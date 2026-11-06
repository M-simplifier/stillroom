module Stillroom.Landscape.Math
  ( V2 (..), V3 (..), Colour (..), add3, sub3, scale3, cross3,
    normalize3, lerp3, tint, white, clampF, lerpF, triangleHeight, noise, piF,
  ) where

import Data.Word (Word8)

data V2 = V2 !Float !Float deriving (Eq, Show)
data V3 = V3 !Float !Float !Float deriving (Eq, Show)
data Colour = Colour !Word8 !Word8 !Word8 !Word8 deriving (Eq, Show)

piF :: Float
piF = 3.14159265358979323846

add3, sub3, cross3 :: V3 -> V3 -> V3
add3 (V3 x y z) (V3 a b c) = V3 (x+a) (y+b) (z+c)
sub3 (V3 x y z) (V3 a b c) = V3 (x-a) (y-b) (z-c)
cross3 (V3 x y z) (V3 a b c) = V3 (y*c-z*b) (z*a-x*c) (x*b-y*a)

scale3 :: V3 -> Float -> V3
scale3 (V3 x y z) a = V3 (x*a) (y*a) (z*a)

normalize3 :: V3 -> V3
normalize3 v@(V3 x y z) =
  let lengthV = sqrt (x*x+y*y+z*z)
   in if lengthV>0 then scale3 v (1/lengthV) else v

lerp3 :: V3 -> V3 -> Float -> V3
lerp3 (V3 x y z) (V3 a b c) t = V3 (x+t*(a-x)) (y+t*(b-y)) (z+t*(c-z))

clampF :: Float -> Float -> Float -> Float
clampF a b x = max a (min b x)

lerpF :: Float -> Float -> Float -> Float
lerpF a b t = a+t*(b-a)

triangleHeight :: (Ord a, Num a) => a -> a -> a -> a -> a -> a -> a
triangleHeight a b c d u v =
  if u+v<=1 then a+(b-a)*u+(c-a)*v else d+(c-d)*(1-u)+(b-d)*(1-v)

tint :: Float -> Float -> Float -> Float -> Colour
tint r g b a = Colour (byte r) (byte g) (byte b) (byte a)
  where byte x = fromIntegral (truncate (clampF 0 1 x*255) :: Int)

white :: Colour
white = Colour 255 255 255 255

noise :: Float -> Float -> Float
noise x z =
  let ix = fromInteger (floor x); iz = fromInteger (floor z)
      fx0 = x-ix; fz0 = z-iz
      fx = fx0*fx0*(3-2*fx0); fz = fz0*fz0*(3-2*fz0)
   in lerpF (lerpF (hash2 ix iz) (hash2 (ix+1) iz) fx) (lerpF (hash2 ix (iz+1)) (hash2 (ix+1) (iz+1)) fx) fz
  where
    hash2 a b = let n = sin (a*127.1+b*311.7)*43758.5453 in n-fromInteger (floor n)
