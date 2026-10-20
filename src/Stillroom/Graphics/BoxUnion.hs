-- | Exact axis-aligned box-union boundaries. A surface has one owner: covered interior
-- faces disappear, and the later solid owns coincident exterior material.
-- Rational topology is lowered to Float only after the boundary is compiled.
--
-- Assemble parts before taking their boundary. Concatenating the boundaries
-- of touching parts retains their internal faces, so 'boundary' does not
-- preserve list concatenation. Assembly order also owns material precedence.
module Stillroom.Graphics.BoxUnion
  ( Rect, rectangle, rectBounds, rectArea, difference,
    Axis (..), Side (..), Box, cuboid, bounds,
    Face (..), boundary, triangulate,
  )
where

import Data.List (foldl')
import Stillroom.Graphics.Mesh

data Rect = Rect !Rational !Rational !Rational !Rational deriving (Eq, Show)
rectangle :: Rational -> Rational -> Rational -> Rational -> Maybe Rect
rectangle a b c d
  | a < b && c < d = Just (Rect a b c d)
  | otherwise = Nothing
rectBounds :: Rect -> (Rational, Rational, Rational, Rational)
rectBounds (Rect a b c d) = (a,b,c,d)
rectArea :: Rect -> Rational
rectArea (Rect a b c d) = (b-a)*(d-c)

-- Disjoint rectangular difference, with exact comparisons at touching edges.
difference :: Rect -> Rect -> [Rect]
difference original@(Rect a b c d) (Rect e f g h)
  | x0 >= x1 || y0 >= y1 = [original]
  | otherwise = [Rect l r lo hi | (l,r,lo,hi) <-
      [(a,x0,c,d),(x1,b,c,d),(x0,x1,c,y0),(x0,x1,y1,d)], l < r, lo < hi]
  where
    x0 = max a e; x1 = min b f; y0 = max c g; y1 = min d h

data Axis = X | Y | Z deriving (Eq, Show, Enum, Bounded)
data Side = Negative | Positive deriving (Eq, Show)
type Point3 = (Rational, Rational, Rational)
data Box material = Box !Point3 !Point3 !material deriving (Eq, Show)
cuboid :: Point3 -> Point3 -> material -> Maybe (Box material)
cuboid lo@(x,y,z) hi@(a,b,c) material
  | x<a && y<b && z<c = Just (Box lo hi material)
  | otherwise = Nothing
bounds :: Box material -> (Point3, Point3)
bounds (Box a b _) = (a,b)
data Face material = Face
  { axis :: !Axis, side :: !Side, plane :: !Rational,
    region :: !Rect, material :: !material, owner :: !Int
  } deriving (Eq, Show)

component :: Axis -> Point3 -> Rational
component X (x,_,_) = x
component Y (_,y,_) = y
component Z (_,_,z) = z
otherAxes :: Axis -> (Axis, Axis)
otherAxes X = (Y,Z)
otherAxes Y = (Z,X)
otherAxes Z = (X,Y)

boundary :: [Box material] -> [Face material]
boundary solids = concat
  [ [Face a sign p r mat index | r <- foldl' subtractOwner [initial] cutters]
  | (index, Box lo hi mat) <- numbered
  , a <- [X,Y,Z], sign <- [Negative,Positive]
  , let p = component a (if sign == Negative then lo else hi)
        (u,v) = otherAxes a
        initial = Rect (component u lo) (component u hi) (component v lo) (component v hi)
        cutters =
          [ Rect (component u low) (component u high) (component v low) (component v high)
          | (j, Box low high _) <- numbered, j /= index
          , let l = component a low; h = component a high
          , if sign == Positive then (l <= p && p < h) || (p == h && j > index)
              else (l < p && p <= h) || (p == l && j > index)
          ]
  ]
  where
    numbered = zip [0..] solids
    subtractOwner faces cutter = concatMap (`difference` cutter) faces

-- | A boundary face becomes two consistently outward triangles. Exact
-- coordinates and material names survive triangulation; a backend decides
-- when and how to approximate them.
triangulate :: Face material -> Mesh Rational material
triangulate (Face a sign p (Rect x0 x1 y0 y1) mat _) =
  let point u v = case a of
        X -> V3 p u v
        Y -> V3 v p u
        Z -> V3 u v p
      n = if sign == Positive then 1 else -1
      outward = case a of X -> V3 n 0 0; Y -> V3 0 n 0; Z -> V3 0 0 n
      vertex u v = Vertex (point u v) outward mat
      a0 = vertex x0 y0
      a1 = vertex x1 y0
      a2 = vertex x1 y1
      a3 = vertex x0 y1
   in if sign == Positive
        then [Triangle a0 a1 a2, Triangle a0 a2 a3]
        else [Triangle a0 a2 a1, Triangle a0 a3 a2]
