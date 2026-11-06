module Stillroom.Landscape.Railway (railChunk) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Stillroom.Journey.World (Seed)
import Stillroom.Landscape.Field qualified as F
import Stillroom.Landscape.Internal
import Stillroom.Landscape.Math

railPoint :: Build s -> Seed -> Float -> Float -> Float -> V3
railPoint s seed z side above =
  let zd = realToFrac z; d = F.railSlopeX seed zd; m = realToFrac (sqrt (1+d*d))
   in V3 (realToFrac (F.railX seed zd)+side/m) (realToFrac (buildRailY s zd)+above) (z-side*realToFrac d/m)

railQuad :: Build s -> V3 -> V3 -> V3 -> V3 -> Colour -> ST s ()
railQuad s a c d e color = do
  let n = normalize3 (cross3 (sub3 c a) (sub3 d a)); vertex p = vert p n (V2 2 0) (-1) 0 color
  tri s Terrain (vertex a) (vertex c) (vertex d)
  tri s Terrain (vertex a) (vertex d) (vertex e)

railPrism :: Build s -> (Int -> V3) -> Colour -> ST s ()
railPrism s corner color =
  forM_ [(0,3,2,1),(4,5,6,7),(0,1,5,4),(3,7,6,2),(0,4,7,3),(1,2,6,5)] $ \(a,b,c,d) -> railQuad s (corner a) (corner b) (corner c) (corner d) color

railBox :: Build s -> Seed -> Float -> Float -> Float -> Float -> Float -> Float -> Colour -> ST s ()
railBox s seed z0 z1 x0 x1 y0 y1 color =
  let a = railPoint s seed z0 x0 y0; b = railPoint s seed z0 x1 y0
      c = railPoint s seed z1 x1 y0; d = railPoint s seed z1 x0 y0
      e = railPoint s seed z0 x0 y1; f = railPoint s seed z0 x1 y1
      g = railPoint s seed z1 x1 y1; h = railPoint s seed z1 x0 y1
      corner k = case k of 0 -> a; 1 -> b; 2 -> c; 3 -> d; 4 -> e; 5 -> f; 6 -> g; _ -> h
   in railPrism s corner color

railChunk :: Build s -> Seed -> Int -> Int -> Int -> ChunkDetail -> ST s ()
railChunk s seed cx cz extent detail = do
  let far = detail==FarChunk; step = if far then 4 else 2
      ballast = tint 0.26 0.245 0.21 1; steel = tint 0.31 0.34 0.33 1
      wood = tint 0.22 0.16 0.10 1; beam = tint 0.18 0.235 0.22 1
      box = railBox s seed
  forM_ [cz*extent,cz*extent+step..(cz+1)*extent-1] $ \iz -> do
    let z = fromIntegral iz
    when (floor (F.railX seed (realToFrac (z+fromIntegral step*0.5))/fromIntegral extent)==cx) $ do
      box z (z+fromIntegral step) (-1.65) 1.65 (-0.32) (-0.20) ballast
      forM_ [-1,1 :: Int] $ \side -> do
        let x = fromIntegral side*0.54
        box z (z+fromIntegral step) (x-0.035) (x+0.035) (-0.13) (-0.035) wood
        box z (z+fromIntegral step) (x-0.055) (x+0.055) (-0.045) 0.015 steel
      when (not far) $ forM_ [0,1 :: Int] $ \t -> box (z+fromIntegral t) (z+fromIntegral t+0.17) (-0.91) 0.91 (-0.21) (-0.12) wood
      let gap = buildRailY s (realToFrac (z+1))-F.groundWithRail seed (buildRailY s) (F.railX seed (realToFrac (z+1))) (realToFrac (z+1))
      when (gap>1.6) $ do
        forM_ [-1,1 :: Int] $ \side -> do
          let x = fromIntegral side*1.42
          box z (z+fromIntegral step) (x-0.10) (x+0.10) (-0.67) (-0.29) beam
          box z (z+fromIntegral step) (x-0.035) (x+0.035) 0.64 0.70 beam
          when (not far) $ box z (z+0.07) (x-0.04) (x+0.04) (-0.27) 0.70 beam
        when (iz `rem` 8==0) $ forM_ [-1,1 :: Int] $ \side -> do
          let V3 ax ay az = railPoint s seed z (fromIntegral side*1.25) (-0.56)
              foot = realToFrac (F.groundWithRail seed (buildRailY s) (realToFrac ax) (realToFrac az))-0.35
              corner k = V3 (ax+(if k `mod` 4==1 || k `mod` 4==2 then 0.15 else -0.15)) (if k>=4 then ay else foot) (az+(if k `mod` 4>=2 then 0.15 else -0.15))
          railPrism s corner beam
