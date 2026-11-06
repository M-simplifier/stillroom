{-# LANGUAGE BangPatterns #-}

module Stillroom.Landscape.Fixed (fixedSceneGeometry, fixedTerrain) where

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Vector.Storable qualified as V
import Stillroom.Landscape.Internal
import Stillroom.Landscape.Math
import Stillroom.Landscape.Plants

fixedTerrain :: Scene -> Float -> Float -> Float
fixedTerrain scene x z =
  let alpine = scene==AlpineLake
      rx = (x-(if alpine then 0 else 5))/(if alpine then 24 else 18.5)
      rz = (z+(if alpine then 32 else 25))/(if alpine then 32 else 26.5)
      radius = sqrt (rx*rx+rz*rz)
      shore0 = (radius-1)*5.5+0.21*sin (x*0.24+z*0.17)+0.12*sin (z*0.52-x*0.11)
      river = 2+6*sin ((z+10)*0.045)+3*sin (z*0.10)
      shore = if scene==MeadowStream then max ((abs (x-river)-(3.5+1*sin (z*0.025)))*0.48) (-1.1) else shore0
      land = 0.32+noise (x*0.075) (z*0.075)*1.0+0.14*noise (x*0.41) (z*0.41)
      height = min shore land
   in if z>=(-57) then height else
      let a0 = clampF 0 1 ((-z-57)/48); a = a0*a0*(3-2*a0)
          hills0 = 11*exp (-(((x+22)/70)**2)-(((z+130)/57)**2))+15*exp (-(((x-67)/55)**2)-(((z+154)/61)**2))
          warp = 6*(noise (x*0.035) (z*0.031)-0.5); xx = x+warp
          peaks = [(-36,-119,42,42),(45,-151,64,60),(9,-161,40,48),(-79,-164,36,55),(83,-181,49,54)]
          ridge (p,(px,pz,ph,pw)) =
            let dx = (xx-px)/pw; dz = (z-pz)/(pw*1.12)
                radial = clampF 0 1 (1-sqrt (dx*dx+dz*dz))
                angle = atan2 dz dx
                drainage = 0.5+0.5*sin (angle*7+noise (xx*0.08) (z*0.09)*2.7+fromIntegral (p :: Int))
             in ph*(radial**0.86)*(1-0.20*drainage*(1-radial))
          hills1 = foldl max 0 (map ridge (zip [0..] peaks))
          ribs = 1-abs (2*noise (xx*0.11) (z*0.095)-1)
          hills2 = hills1*(0.86+0.14*ribs)
          hills = if alpine then hills2+2.2*noise (xx*0.23) (z*0.20)*clampF 0 1 (hills2/15) else hills0
       in height+a*hills*(0.82+0.18*noise (x*0.055) (z*0.042))

fixedSceneGeometry :: Scene -> Either GenerationError Geometry
fixedSceneGeometry scene = runST $ do
  let height = fixedTerrain scene
      nx = 220; nz = 190
  s <- newBuild (Fixed scene) (0x73419a21+fromIntegral (fromEnum scene)*1949) height (const 0)
  let grid = V.generate ((nx+1)*(nz+1)) $ \k ->
        let i = k `mod` (nx+1); j = k `div` (nx+1)
            x = -130+fromIntegral i*(260/fromIntegral nx)
            z = -200+fromIntegral j*(224/fromIntegral nz)
         in vert (surface s x z) (terrainNormal s x z) (V2 0 0) (-1) 0 white
  forM_ [0..nz-1] $ \j ->
    forM_ [0..nx-1] $ \i -> do
      let k = j*(nx+1)+i; a = grid V.! k; b = grid V.! (k+1)
          c = grid V.! (k+nx+1); d = grid V.! (k+nx+2)
      tri s Terrain a c b; tri s Terrain b c d
  case scene of
    BirchLake -> forM_ [(-10,-7,8.6),(-15,-20,10),(-19,-29,9.5),(24,-20,12.8),(27,-31,10),(-23,-40,9)] $ \(x,z,h) -> tree s x z h False
    AlpineLake -> forM_ [(-13,-3,9),(-20,-10,11),(-27,-24,12),(18,-6,11),(27,-29,13)] $ \(x,z,h) -> conifer s x z h
    MeadowStream -> do
      tree s (-14) (-25) 7 False
      tree s (-17) (-30) 8.5 False
      tree s 24 (-48) 7 True
  forM_ [((-30,-55),11),((-15,-73),7),((28,-58),15),((48,-81),9)] $ \((gx,gz),count) ->
    forM_ [0..count-1 :: Int] $ \i -> do
      angle <- range s 0 (2*piF); radius <- sqrt <$> randf s
      let x = gx+cos angle*radius*12; z = gz+sin angle*radius*8
      if scene==AlpineLake then range s 6 12 >>= conifer s x z
      else when (scene==BirchLake || i `rem` 3==0) $ range s 5.2 12.0 >>= \h -> tree s x z h True
  forM_ [1..180000 :: Int] $ \_ -> do
    x <- range s (-37) 37; z <- range s (-65) 14
    when (z<=8) $
      when (abs (x-0.7)<=(8.3-z)*0.88+2.0) $ do
        let dist = sqrt (x*x+(z-8)*(z-8))
            fade = clampF 0 1 ((44-dist)/14)
            patch = 0.28+0.72*noise (x*0.19) (z*0.19)
            wet = clampF 0 1 ((height x z-0.08)/0.38)
        when (dist<=44) $ do
          choice <- randf s
          when (choice<=fade*patch*wet/(1+dist*0.035)) $ do
            alpineChoice <- if scene==AlpineLake then randf s else pure 0
            when (scene/=AlpineLake || alpineChoice<=0.28) $ do
              h0 <- range s 0.16 0.47
              let h1 = h0*(1+0.35*noise (x*0.2) (z*0.2))
                  h = if height x z<0.4 then h1*1.4 else h1
              forM_ [1..3 :: Int] $ \_ -> do
                bx <- (x+) <$> range s (-0.10) 0.10
                bz <- (z+) <$> range s (-0.10) 0.10
                scale <- range s 0.7 1.2; width <- range s 0.009 0.020
                _ <- randf s -- The old phase argument is ignored, but consumes RNG.
                fixedBlade s bx bz (h*scale) width
  let rocks !attempt !count
        | attempt>=1800 || count>=(if scene==AlpineLake then 90 else 45) = pure ()
        | otherwise = do
            x <- range s (-35) 35; z <- range s (-68) 4
            let y = height x z
            if y>0.02 && y<0.45 then do
              radius <- range s 0.10 (if scene==AlpineLake then 0.65 else 0.34)
              rock s x z radius
              rocks (attempt+1) (count+1)
            else rocks (attempt+1) count
  rocks (0 :: Int) (0 :: Int)
  let heightSamples = V.generate (512*512) $ \k ->
        height (-130+(fromIntegral (k `mod` 512)+0.5)/512*260) (-200+(fromIntegral (k `div` 512)+0.5)/512*224)
  finishGeometry s (plane 260 224 100 100 0 (-88)) heightSamples 512 512
