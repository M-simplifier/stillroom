{-# LANGUAGE BangPatterns #-}

-- | The authored journey world, evaluated in Double. Raster sampling and
-- vegetation RNG live separately; a query cannot select or mutate another seed.
module Stillroom.Landscape.Field
  ( Habitat (..), HabitatMix, habitat, habitatWeight, dominantHabitat,
    worldHash, worldRandom, worldNoise, smooth, ground, groundWithRail,
    naturalGround, shoreDistance, railX, railY, railSlopeX, railNearestZ,
    railDistance, railControlY, railBlend, forestDensity, treeSpecies,
    treePlace, boulderPlace, standable, interest,
  ) where

import Data.Bits (shiftR, xor)
import Data.List (foldl')
import Data.Word (Word32)
import Stillroom.Journey.World (Seed, seedValue)

data Habitat = Meadow | Birchwood | Pines | Heath | Wetland | Dunes
  deriving (Eq, Ord, Show, Enum, Bounded)

data HabitatMix = HabitatMix !Double !Double !Double !Double !Double !Double !Habitat
  deriving (Eq, Show)

habitatWeight :: HabitatMix -> Habitat -> Double
habitatWeight (HabitatMix a _ _ _ _ _ _) Meadow = a
habitatWeight (HabitatMix _ a _ _ _ _ _) Birchwood = a
habitatWeight (HabitatMix _ _ a _ _ _ _) Pines = a
habitatWeight (HabitatMix _ _ _ a _ _ _) Heath = a
habitatWeight (HabitatMix _ _ _ _ a _ _) Wetland = a
habitatWeight (HabitatMix _ _ _ _ _ a _) Dunes = a

dominantHabitat :: HabitatMix -> Habitat
dominantHabitat (HabitatMix _ _ _ _ _ _ h) = h

clamp :: Double -> Double -> Double -> Double
clamp a b x = max a (min b x)

smooth :: Double -> Double -> Double -> Double
smooth a b x = let t = clamp 0 1 ((x-a)/(b-a)) in t*t*(3-2*t)

worldHash :: Seed -> Int -> Int -> Word32
worldHash seed x z =
  let mixed = fromIntegral x * 0x9e3779b9 `xor` (fromIntegral z * 0x85ebca6b) `xor` seedValue seed
      first = (mixed `xor` (mixed `shiftR` 16)) * 0x7feb352d
      second = (first `xor` (first `shiftR` 15)) * 0x846ca68b
   in second `xor` (second `shiftR` 16)

worldRandom :: Seed -> Int -> Int -> Double
worldRandom s x z = fromIntegral (worldHash s x z `shiftR` 8) * (1/16777216)

worldNoise :: Seed -> Double -> Double -> Double
worldNoise s x z =
  let i = floor x
      j = floor z
      u0 = x-fromIntegral i
      v0 = z-fromIntegral j
      u = u0*u0*(3-2*u0)
      v = v0*v0*(3-2*v0)
      a = worldRandom s i j*(1-u)+worldRandom s (i+1) j*u
      b = worldRandom s i (j+1)*(1-u)+worldRandom s (i+1) (j+1)*u
   in a*(1-v)+b*v

habitat :: Seed -> Double -> Double -> HabitatMix
habitat s x0 z0 =
  let x = x0+22*(worldNoise s (x0*0.003+39) (z0*0.003-21)-0.5)
      -- This warp intentionally uses the already warped x, as in world.h.
      z = z0+22*(worldNoise s (x*0.003-81) (z0*0.003+47)-0.5)
      cx = floor (x/190)
      cz = floor (z/190)
      sites =
        [ let dx = x-(fromIntegral i+0.5)*190-76*(worldRandom s (i+323) (j-71)-0.5)
              dz = z-(fromIntegral j+0.5)*190-76*(worldRandom s (i-99) (j+653)-0.5)
           in (sqrt (dx*dx+dz*dz), toEnum (fromIntegral (worldHash s (i-173) (j+281) `mod` 6)))
        | j <- [cz-1..cz+1], i <- [cx-1..cx+1] ]
      best = foldl' (\nearest (distance,_) -> min nearest distance) 1e9 sites
      add (!a,!b,!c,!d,!e,!f,!total) (distance,h) =
        let w = 1-smooth 0 48 (distance-best)
            q = w*w
         in case h of
           Meadow -> (a+q,b,c,d,e,f,total+q)
           Birchwood -> (a,b+q,c,d,e,f,total+q)
           Pines -> (a,b,c+q,d,e,f,total+q)
           Heath -> (a,b,c,d+q,e,f,total+q)
           Wetland -> (a,b,c,d,e+q,f,total+q)
           Dunes -> (a,b,c,d,e,f+q,total+q)
      (meadow,birch,pine,heath,wetland,dunes,totalWeight) = foldl' add (0,0,0,0,0,0,0) sites
      weights =
        [ (Meadow,meadow/totalWeight), (Birchwood,birch/totalWeight),
          (Pines,pine/totalWeight), (Heath,heath/totalWeight),
          (Wetland,wetland/totalWeight), (Dunes,dunes/totalWeight)
        ]
      dominant = fst (foldl' (\prior candidate -> if snd candidate>snd prior then candidate else prior) (Meadow,meadow/totalWeight) weights)
   in HabitatMix (meadow/totalWeight) (birch/totalWeight) (pine/totalWeight)
        (heath/totalWeight) (wetland/totalWeight) (dunes/totalWeight) dominant

shoreDistance :: Seed -> Double -> Double -> Double
shoreDistance s x z =
  let ix = floor ((x+120)/240)
      iz = floor ((z+120)/240)
      lake best (i,j) =
        let lx = fromIntegral i*240+90*(worldRandom s (i+91) (j-12)-0.5)
            lz = fromIntegral j*240+90*(worldRandom s (i-41) (j+77)-0.5)
            dx = x-lx
            dz = z-lz
            wet = habitatWeight (habitat s lx lz) Wetland
         in if dx*dx+dz*dz > 260*260 || (worldRandom s (i+213) (j-774) > 0.68+0.32*wet && (i/=0 || j/=0))
            then best
            else let rx = 24+24*worldRandom s (i+141) (j-20)+28*wet
                     rz = 17+18*worldRandom s (i-108) (j+49)+23*wet
                     a = worldRandom s (i+67) (j-81)*3.141592653589793
                     u = dx*cos a+dz*sin a
                     v = -dx*sin a+dz*cos a
                     d = (sqrt (u*u/(rx*rx)+v*v/(rz*rz))-1)*min rx rz
                  in min best (d+2.5*(worldNoise s (x*0.035) (z*0.035)-0.5))
   in foldl' lake 240 [(i,j) | j <- [iz-1..iz+1], i <- [ix-1..ix+1]]

naturalGround :: Seed -> Double -> Double -> Double
naturalGround s x z =
  let ecology = habitat s x z
      wet = habitatWeight ecology Wetland
      dune = habitatWeight ecology Dunes
      warp = 45*(worldNoise s (x*0.003-4) (z*0.003+18)-0.5)
      upland = smooth 0.30 0.73 (worldNoise s (x*0.0026+2.3) (z*0.0029-7.6))
      ridge = 1-abs (2*worldNoise s ((x+warp)*0.0073+16) ((z-warp)*0.0061-31)-1)
      mountains = 63*upland*ridge*ridge*ridge
      hills = 4+14*worldNoise s (x*0.0041-29) (z*0.0043+5)
                +3*worldNoise s (x*0.016+37) (z*0.014-72)
      plain0 = (hills+mountains)*(1-0.88*wet)+0.8*wet
      dunes = 2.3*(1+sin ((x+warp)*0.055+3*worldNoise s (x*0.008) (z*0.008)))
      plain = plain0*(1-0.48*dune)+dunes*dune+0.20*worldNoise s (x*0.19+53) (z*0.19-26)
      shore = shoreDistance s x z
      edge = shore*0.25
      blend = smooth 5 70 shore
   in max (-4.5) (edge*(1-blend)+plain*blend)

railBlend :: Double -> Double -> Double -> Double -> Double -> Double
railBlend a b c d t =
  let u = 1-t
   in (u*u*u*a+(3*t*t*t-6*t*t+4)*b+(-3*t*t*t+3*t*t+3*t+1)*c+t*t*t*d)/6

railX :: Seed -> Double -> Double
railX s z =
  let f = z/256
      i = floor f
      t = f-fromIntegral i
      control k = 280*(worldRandom s k 1729-0.5)
   in railBlend (control (i-1)) (control i) (control (i+1)) (control (i+2)) t

railSlopeX :: Seed -> Double -> Double
railSlopeX s z = (railX s (z+0.1)-railX s (z-0.1))/0.2

railControlY :: Seed -> Int -> Double
railControlY s i = let z = fromIntegral i*256 in 2+min 24 (max 0 (naturalGround s (railX s z) z)*0.65)

railY :: Seed -> Double -> Double
railY s z =
  let f = z/256
      i = floor f
      t = f-fromIntegral i
   in railBlend (railControlY s (i-1)) (railControlY s i) (railControlY s (i+1)) (railControlY s (i+2)) t

railNearestZ :: Seed -> Double -> Double -> Double
railNearestZ s x z = let d = railSlopeX s z in z+(x-railX s z)*d/(1+d*d)

railDistance :: Seed -> Double -> Double -> Double
railDistance s x z = let d = railSlopeX s z in abs (x-railX s z)/sqrt (1+d*d)

-- | A bounded chunk may share precomputed rail controls through this argument.
-- It changes evaluation cost only; all geometry still uses the same field.
groundWithRail :: Seed -> (Double -> Double) -> Double -> Double -> Double
groundWithRail s trackY x z =
  let naturalHeight = naturalGround s x z
      distance = railDistance s x z
   in if distance>=18 then naturalHeight else
      let bed = trackY (railNearestZ s x z)-0.36
          influence =
            if bed>naturalHeight
              then (1-smooth 2.1 7 distance)*(1-smooth 1.2 3.0 (bed-naturalHeight))
              else 1-smooth 2.1 18 distance
       in naturalHeight+(bed-naturalHeight)*influence

ground :: Seed -> Double -> Double -> Double
ground s = groundWithRail s (railY s)

forestDensity :: Seed -> Double -> Double -> Double
forestDensity s x z =
  let ecology = habitat s x z
      w = habitatWeight ecology
      d = 0.05*w Meadow+0.78*w Birchwood+0.91*w Pines+0.055*w Heath+0.18*w Wetland+0.025*w Dunes
   in d*(0.55+0.45*worldNoise s (x*0.024+2) (z*0.024-1))

treeSpecies :: Seed -> Double -> Double -> Habitat
treeSpecies s x z =
  let ecology = habitat s x z
      coniferWeight = habitatWeight ecology Pines+0.55*habitatWeight ecology Heath
      speciesSample = worldRandom s (floor (x*3)+412) (floor (z*3)-817)
   in if speciesSample<coniferWeight then Pines else Birchwood

treePlace :: Seed -> Int -> Int -> Maybe (Double,Double,Double)
treePlace s ix iz =
  let x = fromIntegral ix*8+1+worldRandom s (ix+713) (iz-127)*6
      z = fromIntegral iz*8+1+worldRandom s (ix-117) (iz+821)*6
      h =
        if treeSpecies s x z==Pines
          then 9+worldRandom s (ix+31) (iz-89)*8
          else 6+worldRandom s (ix+31) (iz-89)*5
   in if railDistance s x z<6 || worldRandom s ix iz>forestDensity s x z || ground s x z<0.75
        then Nothing
        else Just (x,z,h)

boulderPlace :: Seed -> Int -> Int -> Maybe (Double,Double,Double)
boulderPlace s ix iz =
  let x = fromIntegral ix*12+2+8*worldRandom s (ix+921) (iz-81)
      z = fromIntegral iz*12+2+8*worldRandom s (ix-71) (iz+982)
      ecology = habitat s x z
      chance = 0.035+0.22*habitatWeight ecology Heath+0.08*habitatWeight ecology Pines
   in if railDistance s x z<5 || worldRandom s (ix-553) (iz+91)>chance || ground s x z<0.4
        then Nothing
        else Just (x,z,0.65+1.65*worldRandom s (ix+325) (iz-274))

standable :: Seed -> Double -> Double -> Bool
standable s x z
  | isNaN x || isNaN z || isInfinite x || isInfinite z || abs x>10000000 || abs z>10000000 || ground s x z<0.16 = False
  | dx*dx+dz*dz>0.075 = False
  | otherwise =
      all clearTree [(i,j) | i <- [ix-1..ix+1], j <- [iz-1..iz+1]]
        && all clearRock [(i,j) | i <- [rx-1..rx+1], j <- [rz-1..rz+1]]
  where
    dx = ground s (x+0.15) z-ground s (x-0.15) z
    dz = ground s x (z+0.15)-ground s x (z-0.15)
    ix = floor (x/8)
    iz = floor (z/8)
    rx = floor (x/12)
    rz = floor (z/12)
    clearTree (i,j) = case treePlace s i j of
      Nothing -> True
      Just (tx,tz,h) ->
        let radius = 0.34+h*(if treeSpecies s tx tz==Pines then 0.10 else 0.025)
         in (x-tx)*(x-tx)+(z-tz)*(z-tz)>=radius*radius
    clearRock (i,j) = case boulderPlace s i j of
      Nothing -> True
      Just (tx,tz,r) -> (x-tx)*(x-tx)+(z-tz)*(z-tz)>=(r*1.1+0.27)*(r*1.1+0.27)

interest :: Seed -> Double -> Double -> Double
interest s x z =
  let d = (shoreDistance s x z-12)/22
      ecology = habitat s x z
      ecotone = foldl' (\a h -> let w = habitatWeight ecology h in a-w*w) 1 [minBound..maxBound]
   in 0.35*exp (-d*d)+0.25*ecotone+0.25*clamp 0 1 (ground s x z/55)+0.15*(1-forestDensity s x z)
