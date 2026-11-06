module Stillroom.Landscape.Ecology
  ( habitatTint, terrainTint, coarseHeight, patchHeight, vegetation, distantTree ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.STRef (modifySTRef')
import Data.Vector.Storable qualified as V
import Stillroom.Journey.World (Seed)
import Stillroom.Landscape.Field qualified as F
import Stillroom.Landscape.Internal
import Stillroom.Landscape.Math
import Stillroom.Landscape.Plants (blade, tube)

habitatTint :: F.HabitatMix -> Bool -> Colour
habitatTint mix grass =
  let palette = if grass
        then [(0.24,0.36,0.073),(0.40,0.32,0.085),(0.12,0.23,0.14),(0.29,0.22,0.28),(0.30,0.37,0.11),(0.50,0.39,0.19)]
        else [(0.13,0.205,0.062),(0.205,0.171,0.064),(0.057,0.102,0.071),(0.13,0.145,0.085),(0.14,0.178,0.075),(0.40,0.302,0.164)]
      add (r,g,b) (h,(x,y,z)) =
        let w = F.habitatWeight mix h
            accumulate a c = realToFrac (realToFrac a+w*realToFrac (c :: Float)) :: Float
         in (accumulate r x,accumulate g y,accumulate b z)
      (red,green,blue) = foldl add (0,0,0) (zip [F.Meadow .. F.Dunes] palette)
   in tint red green blue 1

terrainTint :: Seed -> Float -> Float -> Colour
terrainTint seed x z = habitatTint (F.habitat seed (realToFrac x) (realToFrac z)) False

coarseHeight :: Seed -> Float -> Float -> Float
coarseHeight seed = coarseHeightUsing seed (F.railY seed)

coarseHeightUsing :: Seed -> (Double -> Double) -> Float -> Float -> Float
coarseHeightUsing seed trackY xf zf =
  let x = realToFrac xf; z = realToFrac zf
      ox = fromInteger (floor (x/8))*8; oz = fromInteger (floor (z/8))*8
      u = (x-ox)/8; v = (z-oz)/8
      height = F.groundWithRail seed trackY
      a = height ox oz; b = height (ox+8) oz
      c = height ox (oz+8); d = height (ox+8) (oz+8)
   in realToFrac (triangleHeight a b c d u v)

-- | Interpolate the actual near terrain triangles. The diagonal and Float
-- arithmetic match the mesh, rather than promising analytical field equality.
patchHeight :: V.Vector Float -> Float -> Float -> Int -> Float
patchHeight heights x0 z0 spacing =
  let x = clampF 0 32 x0; z = clampF 0 32 z0
      ix = min (32 `div` spacing-1) (floor (x/fromIntegral spacing))
      iz = min (32 `div` spacing-1) (floor (z/fromIntegral spacing))
      u = (x-fromIntegral (ix*spacing))/fromIntegral spacing
      v = (z-fromIntegral (iz*spacing))/fromIntegral spacing
      cell = iz*spacing*33+ix*spacing
      a = heights V.! cell; b = heights V.! (cell+spacing)
      c = heights V.! (cell+spacing*33); d = heights V.! (cell+spacing*34)
   in triangleHeight a b c d u v

blossom :: Build s -> Float -> Float -> Float -> Float -> Colour -> ST s ()
blossom s x z y h color = do
  radius <- range s 0.045 0.085; a <- range s 0 (2*piF)
  let top = V3 x (y+h) z; center = Vertex top (V3 0 1 0) (V2 1 y) (V2 x z) color
      rim angle = Vertex (add3 top (V3 (radius*cos angle) (-0.012) (radius*sin angle))) (V3 0 1 0) (V2 1 y) (V2 x z) color
  forM_ [0..6 :: Int] $ \i -> do
    let p = a+fromIntegral (i*2)*piF/7; q = a+fromIntegral ((i+1)*2)*piF/7
    tri s Grass center (rim p) (rim q)

vegetation :: Build s -> Seed -> Float -> Float -> V.Vector Float -> ST s ()
vegetation s seed ox oz heights =
  forM_ [1..6500 :: Int] $ \_ -> do
    x <- range s ox (ox+32); z <- range s oz (oz+32)
    let y = patchHeight heights (x-ox) (z-oz) 1
    when (y>=0.18 && F.railDistance seed (realToFrac x) (realToFrac z)>=2.8) $ do
      let mix = F.habitat seed (realToFrac x) (realToFrac z); w = F.habitatWeight mix
          cover = realToFrac ((realToFrac (0.72 :: Float) :: Double)-realToFrac (0.42 :: Float)*w F.Pines-realToFrac (0.10 :: Float)*w F.Heath-realToFrac (0.54 :: Float)*w F.Dunes) :: Float
          patch = 0.22+0.78*(realToFrac (F.worldNoise seed (realToFrac x*0.14) (realToFrac z*0.14)) :: Float)
      accept <- randf s
      when (accept<=patch*cover) $ do
        initialH <- range s 0.22 0.49
        shade <- range s 0.78 1.12
        let h = realToFrac (realToFrac initialH*(1+2.2*w F.Wetland+0.65*w F.Dunes))
            Colour r g b alpha = habitatTint mix True
            byte n = fromIntegral (truncate (fromIntegral n*shade) :: Int)
            color = Colour (byte r) (byte g) (byte b) alpha
        forM_ [1..3 :: Int] $ \_ -> do
          bx <- (x+) <$> range s (-0.08) 0.08; bz <- (z+) <$> range s (-0.08) 0.08
          scale <- range s 0.8 1.2; width <- range s 0.018 0.033
          blade s bx bz (patchHeight heights (bx-ox) (bz-oz) 1) (h*scale) width color
        bloomChance <- randf s
        when (realToFrac bloomChance<0.15*w F.Meadow+0.50*w F.Heath) $ do
          bloom <- if w F.Heath>0.45 then pure (tint 0.43 0.20 0.48 1) else do
            choice <- randf s
            pure (if choice>0.45 then tint 0.90 0.84 0.53 1 else tint 0.52 0.41 0.72 1)
          blossom s x z y (h+0.025) bloom

crown :: Build s -> V3 -> Float -> Float -> Colour -> Bool -> ST s ()
crown s (V3 cx cy cz) radius vertical color cone =
  forM_ [0..2 :: Int] $ \i -> do
    let angle = fromIntegral i*piF/3+cx*0.03
        side = V3 (cos angle) 0 (sin angle); front = V3 (-sin angle) 0 (cos angle)
        vertex :: Int -> Vertex
        vertex k =
          let u = if odd k then 1 else -1; w = if k `div` 2/=0 then 1 else -1
              p = V3 (cx+cos angle*u*radius) (cy+vertical*(w+1)*0.5) (cz+sin angle*u*radius)
              n = normalize3 (add3 (add3 (scale3 side u) (scale3 front 0.45)) (V3 0 (w*0.7) 0))
           in vert p n (V2 u w) (-1) (if cone then 1 else 0) color
    tri s Foliage (vertex (0 :: Int)) (vertex 2) (vertex 1)
    tri s Foliage (vertex (1 :: Int)) (vertex 2) (vertex 3)

distantTree :: Build s -> Seed -> Float -> Float -> Float -> F.Habitat -> ST s ()
distantTree s seed x z h species = do
  let root = V3 x (coarseHeightUsing seed (buildRailY s) x z) z
  modifySTRef' (buildTreeCount s) (+1)
  tube s root (add3 root (V3 0 (h*0.88) 0)) (h*0.023) 0.025 (-1) (tint 0.30 0.27 0.19 1) 4
  if species==F.Pines then
    forM_ [0..2 :: Int] $ \k ->
      crown s (add3 root (V3 (h*0.015) (h*(0.2+0.21*fromIntegral k)) 0)) (h*(0.21-0.048*fromIntegral k)) (h*(0.45-0.06*fromIntegral k)) (tint 0.058 0.135 0.090 0.75) True
  else do
    let mix = F.habitat seed (realToFrac x) (realToFrac z)
        gold = realToFrac (F.habitatWeight mix F.Birchwood*(0.65+0.35*F.worldNoise seed (realToFrac x*0.009) (realToFrac z*0.009))) :: Float
        color = tint (realToFrac (0.12+0.42*realToFrac gold :: Double)) (realToFrac (0.255+0.09*realToFrac gold :: Double)) (realToFrac (0.075*(1-0.55*realToFrac gold) :: Double)) 0.8
    forM_ [0..5 :: Int] $ \k -> do
      let a = fromIntegral k*2.39996+x*0.01
      crown s (add3 root (V3 (cos a*h*0.09) (h*(0.38+0.075*fromIntegral k)) (sin a*h*0.09))) (h*(0.20-0.013*fromIntegral k)) (h*0.26) color False
