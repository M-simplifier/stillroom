-- | Stillroom's authored railcar. Dimensions, palette and curved wheels
-- live here; the graphics core only knows surfaces and their interpretation.
module Stillroom.Journey.Railcar
  ( Material (..), RailcarError (..), railcarBoxes, railcarFaces, railcarMesh,
  )
where

import Stillroom.Graphics.Mesh
import Stillroom.Graphics.BoxUnion (Box, Face, boundary, cuboid, triangulate)

data Material = Paint | Dark | Wood | Brass | Cream deriving (Eq, Show)
data RailcarError = InvalidPart !Int deriving (Eq, Show)

railcarBoxes :: Either RailcarError [Box Material]
railcarBoxes = traverse makePart (zip [0..] partSpecifications)
  where
    makePart (index, (lo,hi,material)) =
      maybe (Left (InvalidPart index)) Right (cuboid lo hi material)

-- Original decimal dimensions and part order, represented exactly.
partSpecifications :: [((Rational,Rational,Rational), (Rational,Rational,Rational), Material)]
partSpecifications =
  [at 0 0.37 0 1.70 0.22 4.9 Dark, at 0 0.56 0 2.18 0.16 5.2 Paint]
  ++ [at 0 0.655 (-2.45+k*0.20) 2.06 0.035 0.19 Wood | k <- [0..24]]
  ++ concat
    [ concat
        [ [at (s*1.04) 0.95 (e*1.66) 0.095 0.63 1.78 Paint,
           at (s*1.04) 1.29 (e*1.66) 0.13 0.07 1.80 Wood,
           at (s*1.035) 1.79 (e*2.46) 0.075 2.32 0.075 Brass]
        | e <- [-1,1]]
      ++ [at (s*1.20) 0.34 0 0.42 0.09 0.86 Wood,
          at (s*0.84) 1.02 1.15 0.35 0.12 1.60 Wood,
          at (s*0.97) 1.36 1.15 0.065 0.65 1.60 Wood,
          at (s*1.045) 2.91 0 0.09 0.13 5.25 Brass]
    | s <- [-1,1]]
  ++ concat
    [ [at 0 0.98 (e*2.46) 2.07 0.65 0.10 Paint,
       at 0 1.33 (e*2.46) 2.14 0.09 0.13 Wood,
       at 0 0.41 (e*2.72) 0.75 0.12 0.22 Dark,
       at 0 2.91 (e*2.46) 2.17 0.13 0.10 Brass]
    | e <- [-1,1]]
  ++ [let x = -1.20+k*0.15
          y = 2.96+0.14*(1-x*x/(1.2*1.2))
       in at (x+0.075) y 0 0.151 0.07 5.48 Cream | k <- [0..15]]
  where
    at x y z w h d material = ((x-w/2,y-h/2,z-d/2), (x+w/2,y+h/2,z+d/2), material)

railcarFaces :: Either RailcarError [Face Material]
railcarFaces = boundary <$> railcarBoxes

railcarMesh :: Either RailcarError (Mesh Float RGBA)
railcarMesh = do
  faces <- railcarFaces
  let boxes = map (fmap (mapVertex realToFrac color)) (concatMap triangulate faces)
      wheels = concat [wheel (s*0.54) z | s <- [-1,1], z <- [-1.65,1.65]]
  pure (boxes ++ wheels)

color :: Material -> RGBA
color Paint = RGBA 26 62 57 255
color Dark = RGBA 16 30 30 255
color Wood = RGBA 119 76 35 255
color Brass = RGBA 135 99 45 255
color Cream = RGBA 165 163 130 255

-- Preserve the original cap offset and silhouette. Caps and tread are
-- separate surfaces, not a claim that this wheel is a closed CSG solid.
wheel :: Double -> Double -> Mesh Float RGBA
wheel x z = concatMap segment [0..19 :: Int]
  where
    ring i = let angle = fromIntegral (i `mod` 20)*2*pi/20
              in (0.28+0.29*sin angle,z+0.29*cos angle)
    vertex (px,py,pz) (nx,ny,nz) rgb =
      mapVertex realToFrac id (Vertex (V3 px py pz) (V3 nx ny nz) rgb)
    dark = RGBA 25 33 35 255
    rim = RGBA 73 81 79 255
    segment i =
      let (ya,za) = ring i
          (yb,zb) = ring (i+1)
          a = (x-0.075,ya,za)
          b = (x+0.075,ya,za)
          c = (x+0.075,yb,zb)
          d = (x-0.075,yb,zb)
          dy = yb-ya
          dz = zb-za
          len = sqrt (dy*dy+dz*dz)
          outward = (0,-dz/len,dy/len)
          v p = vertex p outward dark
          tread = [Triangle (v a) (v b) (v c), Triangle (v a) (v c) (v d)]
          cap sign =
            let px = x+sign*0.077
                n = (sign,0,0)
                center = vertex (px,0.28,z) n rim
                va = vertex (px,ya,za) n dark
                vb = vertex (px,yb,zb) n dark
             in if sign < 0 then Triangle center va vb else Triangle center vb va
       in tread ++ [cap (-1),cap 1]
