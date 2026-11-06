{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Graphics.Encoding (point, direction, transform) where

import Raylib.Types (Matrix (..), Vector3, pattern Vector3)
import Stillroom.Graphics.Space qualified as S

point :: S.Point space -> Vector3
point (S.Point x y z) = Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

direction :: S.Direction space -> Vector3
direction (S.Direction x y z) = Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

-- Matrix is the pinned binding's native row-layout. The source exposes its
-- components in OpenGL column order, and never exposes a mutable native array.
transform :: S.Transform from to -> Matrix
transform value = case S.matrixElements value of
  ((a, b, c, d), (e, f, g, h), (i, j, k, l), (m, n, o, p)) ->
    Matrix
      (cvt a)
      (cvt e)
      (cvt i)
      (cvt m)
      (cvt b)
      (cvt f)
      (cvt j)
      (cvt n)
      (cvt c)
      (cvt g)
      (cvt k)
      (cvt o)
      (cvt d)
      (cvt h)
      (cvt l)
      (cvt p)
  where
    cvt = realToFrac
