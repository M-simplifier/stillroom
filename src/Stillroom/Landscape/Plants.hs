{-# LANGUAGE BangPatterns #-}

-- | Authored branch, leaf, needle, stone and blade shapes. The two historical
-- scene families have deliberately different silhouettes and sampling budgets.
module Stillroom.Landscape.Plants
  ( tube, leaf, tree, conifer, rock, blade, fixedBlade ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.STRef (modifySTRef')
import Stillroom.Landscape.Field qualified as F
import Stillroom.Landscape.Internal
import Stillroom.Landscape.Math

tube :: Build s -> V3 -> V3 -> Float -> Float -> Int -> Colour -> Int -> ST s ()
tube s a@(V3 _ ay _) b@(V3 _ by _) ra rb bone color sides = do
  let axis@(V3 _ y _) = normalize3 (sub3 b a)
      u = normalize3 (cross3 axis (if abs y<0.9 then V3 0 1 0 else V3 1 0 0))
      v = cross3 axis u
  forM_ [0..sides-1] $ \j -> do
    let p = fromIntegral (j*2)*piF/fromIntegral sides
        q = fromIntegral ((j+1)*2)*piF/fromIntegral sides
        n0 = add3 (scale3 u (cos p)) (scale3 v (sin p))
        n1 = add3 (scale3 u (cos q)) (scale3 v (sin q))
        a0 = vert (add3 a (scale3 n0 ra)) n0 (V2 (fromIntegral j/fromIntegral sides) ay) bone 0 color
        a1 = vert (add3 a (scale3 n1 ra)) n1 (V2 (fromIntegral (j+1)/fromIntegral sides) ay) bone 0 color
        b0 = vert (add3 b (scale3 n0 rb)) n0 (V2 (fromIntegral j/fromIntegral sides) by) bone 0 color
        b1 = vert (add3 b (scale3 n1 rb)) n1 (V2 (fromIntegral (j+1)/fromIntegral sides) by) bone 0 color
    tri s Wood a0 b0 a1
    tri s Wood a1 b0 b1

leaf :: Build s -> V3 -> V3 -> Float -> Int -> Colour -> ST s ()
leaf s base direction lengthV bone color = do
  sx <- range s (-0.5) 0.5; sz <- range s (-0.5) 0.5
  phase <- (\a -> a*2*piF) <$> randf s
  let dir = normalize3 direction
      side = normalize3 (cross3 dir (V3 sx 1 sz))
      n = normalize3 (cross3 side dir)
      mid = add3 base (scale3 dir (lengthV*0.48))
      foldV = add3 mid (scale3 n (lengthV*0.055))
      profile = [(0,0),(0.28,-0.30),(0.70,-0.27),(1,0),(0.70,0.27),(0.28,0.30)]
      rim (x,y) = vert (add3 base (add3 (scale3 dir (x*lengthV)) (scale3 side (y*lengthV)))) n (V2 x (y/0.30)) bone phase color
      center = vert foldV n (V2 0.48 0) bone phase color
      vertices = map rim profile
  forM_ (zip vertices (drop 1 vertices++take 1 vertices)) $ \(a,b) -> tri s Foliage a b center

branch :: Build s -> V3 -> V3 -> Float -> Float -> Int -> Int -> Float -> Float -> Bool -> ST s ()
branch s base direction len radius depth parent hue leafSize lod = do
  phase <- range s 0 6.28
  bone <- newBone s base parent (if depth==0 then 0.018 else 0.012) phase
  bx <- (*len) <$> range s (-0.12) 0.12
  bz <- (*len) <$> range s (-0.12) 0.12
  let dir = normalize3 direction
      end = add3 base (add3 (scale3 dir len) (V3 bx (0.12*len) bz))
      V3 mx my mz = lerp3 base end 0.52
      mid = V3 mx (my-len*0.07) mz
      bark = tint (0.43+hue*0.1) (0.41+hue*0.08) (0.32+hue*0.05) 1
  tube s base mid radius (radius*0.67) bone bark 5
  tube s mid end (radius*0.67) (radius*0.24) bone bark 5
  if depth>0 then do
    let count = if lod then 2 else 3 :: Int
    forM_ [0..count-1] $ \i -> do
      a <- range s (-piF) piF
      dy <- range s 0.05 0.6
      reach <- range s 0.48 0.68
      let t = 0.44+0.5*fromIntegral i/fromIntegral (count-1)
          d = normalize3 (add3 (scale3 dir 0.55) (V3 (cos a*0.72) dy (sin a*0.72)))
      branch s (lerp3 base end t) d (len*reach) (radius*0.43) (depth-1) bone hue leafSize lod
  else do
    let journey = case buildLandscape s of Journey _ -> True; _ -> False
        count = if journey then (if lod then 64 else 80) else (if lod then 36 else 56) :: Int
        jitter = if journey then 0.30 else 0.12
        jitterY = if journey then 0.25 else 0.10
    forM_ [0..count-1] $ \j -> do
      t <- range s 0.10 1.05
      px <- range s (-jitter) jitter; py <- range s (-jitterY) jitterY; pz <- range s (-jitter) jitter
      dy <- range s (-0.18) 0.65
      r <- range s (if journey then 0.075 else 0.10) (if journey then 0.15 else 0.22)
      g <- range s (if journey then 0.18 else 0.21) (if journey then 0.29 else 0.36)
      b <- range s (if journey then 0.045 else 0.035) (if journey then 0.10 else 0.085)
      alpha <- range s 0.5 1.0
      size <- range s 0.7 1.35
      let a = fromIntegral j*2.39996
          p = add3 (lerp3 base end t) (V3 px py pz)
          d = normalize3 (add3 (scale3 dir 0.28) (V3 (cos a) dy (sin a)))
          c = if journey then tint (r+hue*0.42) (g+hue*0.09) (b*(1-0.55*hue)) alpha else tint (r+hue*0.03) (g+hue*0.02) b alpha
      leaf s p d (leafSize*size) bone c

tree :: Build s -> Float -> Float -> Float -> Bool -> ST s ()
tree s x z h lod = do
  let root@(V3 _ y _) = surface s x z
  when (y>=0.12) $ do
    modifySTRef' (buildTreeCount s) (+1)
    hue <- case buildLandscape s of
      Fixed _ -> range s 0 1
      Journey seed -> pure (realToFrac (F.habitatWeight (F.habitat seed (realToFrac x) (realToFrac z)) F.Birchwood*(0.65+0.35*F.worldNoise seed (realToFrac x*0.009) (realToFrac z*0.009))))
    lean <- range s (-0.10) 0.10; phase <- range s 0 6.28
    trunk <- newBone s root (-1) 0.0045 phase
    let advance !prior i = do
          let t = fromIntegral i/12
              next = add3 root (V3 (lean*h*t*t) (h*t) (sin (t*3+phase)*h*0.025*t))
              r = h*0.023*((1-fromIntegral (i-1)/13)**1.15)
          tube s prior next r (h*0.023*((1-fromIntegral i/13)**1.15)) trunk (tint 0.57 0.55 0.46 1) 7
          when (i>=4 && i<=11 && (not lod || even i)) $ do
            dy <- range s 0.38 0.75
            reach <- range s 0.85 1.25
            let a = fromIntegral i*2.39996+phase
                len = h*(0.39-0.14*t)*reach
                leafSize = case buildLandscape s of Journey _ -> if lod then 0.36 else 0.30; Fixed _ -> if lod then 0.65 else 0.39
            branch s prior (V3 (cos a) dy (sin a)) len (r*0.59) 2 trunk hue leafSize lod
          if i<12 then advance next (i+1) else pure ()
    advance root (1 :: Int)
    branch s (add3 root (V3 (lean*h*0.8) (h*0.84) 0)) (V3 0.05 1 0.1) (h*0.22) (h*0.025*0.18) 2 trunk hue 0.28 lod

needleMass :: Build s -> V3 -> Float -> Float -> Int -> Colour -> ST s ()
needleMass s center radius vertical bone color = do
  let journey = case buildLandscape s of Journey _ -> True; _ -> False
      count = if journey then 20 else 28 :: Int
  forM_ [0..count-1] $ \i -> do
    let y = 1-2*(fromIntegral i+0.5)/fromIntegral count
        a = fromIntegral i*2.39996; ring = sqrt (1-y*y)
        outward@(V3 vx vy vz) = V3 (cos a*ring) y (sin a*ring)
        base = add3 center (V3 (vx*radius*0.52) (vy*vertical*0.52) (vz*radius*0.52))
        side = normalize3 (cross3 outward (V3 0.05 1 0.13))
        n = normalize3 (cross3 side outward)
    forM_ [-2..2 :: Int] $ \j -> do
      let V3 dx dy dz = normalize3 (add3 outward (scale3 side (fromIntegral j*0.3)))
          tip = add3 base (V3 (dx*radius*(if journey then 0.64 else 0.80)) (dy*vertical*0.80) (dz*radius*(if journey then 0.64 else 0.80)))
          width = radius*(if journey then 0.065 else 0.115)
          left = vert (sub3 base (scale3 side width)) n (V2 0.25 (-1)) bone a color
          right = vert (add3 base (scale3 side width)) n (V2 0.25 1) bone a color
      tri s Foliage left right (vert tip n (V2 1 0) bone a color)

conifer :: Build s -> Float -> Float -> Float -> ST s ()
conifer s x z h = do
  let root@(V3 _ y _) = surface s x z
  when (y>=0.14) $ do
    modifySTRef' (buildTreeCount s) (+1)
    trunkPhase <- range s 0 6.28
    trunk <- newBone s root (-1) 0.003 trunkPhase
    tube s root (add3 root (V3 (h*0.025) h 0)) (h*0.025) 0.025 trunk (tint 0.19 0.15 0.10 1) 7
    phase <- range s 0 6.28
    let journey = case buildLandscape s of Journey _ -> True; _ -> False
        distant = journey || z<(-45)
        levels = if journey then 11 else if distant then 9 else 14 :: Int
    forM_ [0..levels-1] $ \level -> do
      let f = (if journey then 0.28 else 0.17)+fromIntegral level*((if journey then 0.67 else 0.78)/fromIntegral (levels-1))
          y0 = h*f
          count = if journey then 4 else if distant then 3 else 5 :: Int
      forM_ [0..count-1] $ \j -> do
        by <- range s (-0.024) 0.024
        reach <- range s 0.28 0.42
        sag <- range s 0.15 0.35
        let a = phase+fromIntegral (j*2)*piF/fromIntegral count+fromIntegral level*1.31
            base = add3 root (V3 (h*0.025*f) (y0+by*h) 0)
            len = h*(1-f)*reach
            tip = add3 base (V3 (cos a*len) (-len*sag) (sin a*len))
        bone <- newBone s base trunk 0.009 (phase+fromIntegral level*0.7+fromIntegral j)
        tube s base tip (h*0.010*(1-f)) 0.008 bone (tint 0.23 0.18 0.10 1) 4
        forM_ [0..(if distant then 1 else 2) :: Int] $ \m -> do
          vscale <- range s 0.80 1.65
          let along = 0.37+fromIntegral m*(if distant then 0.35 else 0.24)
              V3 cx cy cz = lerp3 base tip along
              center = V3 cx (cy+len*0.10) cz
              radius = len*(1-along)*0.48+0.13
          needleMass s center radius (radius*vscale) bone (tint 0.045 0.115 0.073 0.68)
        when (not distant) $
          forM_ [0..7 :: Int] $ \k -> do
            let t = 0.16+fromIntegral k*(0.76/7); p = lerp3 base tip t
            forM_ [-1,1 :: Int] $ \side -> do
              let sf = fromIntegral side
                  d = V3 (cos (a+sf*0.90)) 0.15 (sin (a+sf*0.90))
                  reachV = len*(1-t)*0.48+0.12; end = add3 p (scale3 d reachV)
              forM_ [0..5 :: Int] $ \n -> do
                ay <- range s (-0.20) 0.24
                ny <- range s (-0.2) 0.55
                size <- range s 0.24 0.42
                r <- range s 0.035 0.075; g <- range s 0.10 0.18; b <- range s 0.075 0.12
                let V3 ax atY az = lerp3 p end (fromIntegral n/6)
                    needle = V3 (cos (a+sf*(0.5+fromIntegral n*0.28))) ny (sin (a+sf*(0.5+fromIntegral n*0.28)))
                leaf s (V3 ax (atY+ay*len) az) needle size bone (tint r g b 0.75)

rock :: Build s -> Float -> Float -> Float -> ST s ()
rock s x z radius = do
  let V3 rx ry rz = surface s x z; root = V3 rx (ry-radius*0.22) rz
  forM_ [0..4 :: Int] $ \j ->
    forM_ [0..8 :: Int] $ \i -> do
      let vertex k =
            let u = fromIntegral ((i+k `mod` 2)*2)*piF/9
                vv = fromIntegral (j+k `div` 2)*piF/5
                n@(V3 nx ny nz) = V3 (cos u*sin vv) (cos vv) (sin u*sin vv)
                r = radius*(0.85+0.25*noise (nx*3+rx) (nz*3+rz))
                p = add3 root (V3 (nx*r) (ny*r*0.62) (nz*r*0.85))
             in vert p n (V2 1 0) (-1) 0 white
      tri s Terrain (vertex 0) (vertex 2) (vertex 1)
      tri s Terrain (vertex 1) (vertex 2) (vertex 3)

blade :: Build s -> Float -> Float -> Float -> Float -> Float -> Colour -> ST s ()
blade s x z y h width color = bladeShape s True x z y h width color

fixedBlade :: Build s -> Float -> Float -> Float -> Float -> ST s ()
fixedBlade s x z h width = do
  let y = buildHeight s x z
  when (y>=0.10) $ do
    -- Angle/curve precede the colour samples in the authored fixed scene.
    a <- range s 0 (2*piF); curve <- range s (-0.22) 0.22
    r <- range s 0.14 0.29; g <- range s 0.24 0.38; b <- range s 0.045 0.11
    emitBlade s False x z y h width a curve (tint r g b 1)

bladeShape :: Build s -> Bool -> Float -> Float -> Float -> Float -> Float -> Colour -> ST s ()
bladeShape s journey x z y h width color = when (y>=0.10) $ do
  a <- range s 0 (2*piF); curve <- range s (-0.22) 0.22
  emitBlade s journey x z y h width a curve color

emitBlade :: Build s -> Bool -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Colour -> ST s ()
emitBlade s journey x z y h width a curve color = do
  let root = V3 x y z; side = V3 (cos a) 0 (sin a); n = V3 (-sin a) 0.18 (cos a)
      vertex i sign =
        let t = fromIntegral i/3; w = width*(1-t)*(0.7+0.3*sin (t*piF))
            p = add3 root (V3 (curve*t*t) (h*t) (-curve*0.28*t*t))
         in Vertex (add3 p (scale3 side (sign*w))) n (V2 t (if journey then y else sign)) (V2 x z) color
  forM_ [0..2 :: Int] $ \i -> do
    tri s Grass (vertex i (-1)) (vertex i 1) (vertex (i+1) (-1))
    when (i<2) $ tri s Grass (vertex i 1) (vertex (i+1) 1) (vertex (i+1) (-1))
  modifySTRef' (buildGrassCount s) (+1)
