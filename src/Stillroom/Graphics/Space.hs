{-# LANGUAGE RoleAnnotations #-}

-- | Typed coordinate spaces and homogeneous maps. Composition can only join
-- matching spaces. All arithmetic remains Double until a backend lowers it.
--
-- These are numerical operations, not a claim of real-number arithmetic:
-- identity/composition are checked with a tolerance; zero directions and
-- degenerate lenses produce non-finite maps which 'observe' rejects.
module Stillroom.Graphics.Space
  ( World, Render, Object, View, Clip,
    Point (..), Direction (..), Transform,
    identity, compose, transform, matrixColumns,
    pointList, directionList, pointOf, relative,
    plus, scale, dot, cross, unit, look, perspective, ortho, objectTransform,
  )
where

import Data.List (transpose)

data World
data Render
data Object
data Clip
data View

type role Point nominal
data Point space = Point !Double !Double !Double deriving (Eq, Show)
type role Direction nominal
data Direction space = Direction !Double !Double !Double deriving (Eq, Show)
type role Transform nominal nominal
-- Row-major internally; packet encoding is explicitly OpenGL column-major.
data Transform from to = Transform !Row !Row !Row !Row deriving (Eq, Show)
data Row = Row !Double !Double !Double !Double deriving (Eq, Show)


identity :: Transform space space
identity = Transform (Row 1 0 0 0) (Row 0 1 0 0) (Row 0 0 1 0) (Row 0 0 0 1)

pointList :: Point s -> [Double]
pointList (Point x y z) = [x,y,z]

directionList :: Direction s -> [Double]
directionList (Direction x y z) = [x,y,z]

pointOf :: Direction s -> Point s
pointOf (Direction x y z) = Point x y z

relative :: Point World -> Point World -> Point Render
relative (Point ox oy oz) (Point x y z) = Point (x-ox) (y-oy) (z-oz)

plus :: Direction s -> Direction s -> Direction s
plus (Direction x y z) (Direction a b c) = Direction (x+a) (y+b) (z+c)

scale :: Double -> Direction s -> Direction s
scale a (Direction x y z) = Direction (a*x) (a*y) (a*z)

dot :: Direction s -> Direction s -> Double
dot (Direction x y z) (Direction a b c) = x*a + y*b + z*c

cross :: Direction s -> Direction s -> Direction s
cross (Direction x y z) (Direction a b c) = Direction (y*c-z*b) (z*a-x*c) (x*b-y*a)

unit :: Direction s -> Direction s
unit a = scale (1 / sqrt (dot a a)) a

rows :: Transform a b -> [Row]
rows (Transform a b c d) = [a,b,c,d]

rowList :: Row -> [Double]
rowList (Row a b c d) = [a,b,c,d]

matrixColumns :: Transform a b -> [Double]
matrixColumns = concat . transpose . map rowList . rows

rowDot :: Row -> Row -> Double
rowDot (Row a b c d) (Row w x y z) = a*w + b*x + c*y + d*z

compose :: Transform b c -> Transform a b -> Transform a c
compose a (Transform (Row b0 b4 b8 b12) (Row b1 b5 b9 b13) (Row b2 b6 b10 b14) (Row b3 b7 b11 b15)) =
  let col0 = Row b0 b1 b2 b3; col1 = Row b4 b5 b6 b7
      col2 = Row b8 b9 b10 b11; col3 = Row b12 b13 b14 b15
      multiply r = Row (rowDot r col0) (rowDot r col1) (rowDot r col2) (rowDot r col3)
      Transform r0 r1 r2 r3 = a
   in Transform (multiply r0) (multiply r1) (multiply r2) (multiply r3)

transform :: Transform a b -> Point a -> (Double, Double, Double, Double)
transform (Transform a b c d) (Point x y z) =
  let p = Row x y z 1 in (rowDot a p, rowDot b p, rowDot c p, rowDot d p)

look :: Point Render -> Direction Render -> Transform Render View
look (Point x y z) forward =
  let backward = scale (-1) (unit forward)
      right@(Direction rx ry rz) = unit (cross (Direction 0 1 0) backward)
      up@(Direction ux uy uz) = cross backward right
      Direction bx by bz = backward
      p = Direction x y z
   in Transform (Row rx ry rz (-dot right p)) (Row ux uy uz (-dot up p))
                (Row bx by bz (-dot backward p)) (Row 0 0 0 1)

perspective :: Double -> Double -> Double -> Double -> Transform View Clip
perspective fov aspect near far =
  let f = 1 / tan (fov / 2)
   in Transform (Row (f/aspect) 0 0 0) (Row 0 f 0 0)
                (Row 0 0 (-(far+near)/(far-near)) (-2*far*near/(far-near)))
                (Row 0 0 (-1) 0)

ortho :: Double -> Double -> Double -> Transform View Clip
ortho half near far =
  Transform (Row (1/half) 0 0 0) (Row 0 (1/half) 0 0)
            (Row 0 0 (-2/(far-near)) (-(far+near)/(far-near))) (Row 0 0 0 1)

objectTransform :: Point Render -> Double -> Double -> Transform Object Render
objectTransform (Point x y z) heading grade =
  let c = cos heading; s = sin heading; cg = cos grade; sg = sin grade
   in Transform (Row c (s*sg) (s*cg) x) (Row 0 cg (-sg) y)
                (Row (-s) (c*sg) (c*cg) z) (Row 0 0 0 1)
