{-# LANGUAGE DataKinds #-}

module Stillroom.Runtime.Graphics.Shader
  ( Scalar, V2, V3, V4, F, Build, build, share, shareVertex, repeatN
  , v2, v3, v4, x2, y2, x3, y3, z3, x4, y4, z4, w4, xy, xz, rgb, rgba
  , add2, sub2, mul2, scale2, add3, sub3, mul3, scale3, divide3
  , splat2, splat3, absF, floorF, fractF, sinF, cosF, expF, sqrtF
  , powF, minF, maxF, stepF, mixF, smooth, clampF, floor2, fract2, floor3, fract3
  , exp3, pow3, mix3, clamp3, reflect3, rotate2, less, lessEqual, greater, unit
  ) where

import Data.Word (Word32)
import GameIO.Graphics.Plan (Vec2, Vec3, Vec4)
import GameIO.Graphics.Shader

type Scalar local stage = Expr local stage Float
type V2 local stage = Expr local stage Vec2
type V3 local stage = Expr local stage Vec3
type V4 local stage = Expr local stage Vec4
type F local a = Expr local 'FragmentStage a

-- Give a shared expression one shader value even when several later formulas
-- consume it. The result is still a closed expression over its local type.
newtype Build result a = Build ((a -> result) -> result)

instance Functor (Build result) where
  fmap f (Build action) = Build (\use -> action (use . f))

instance Applicative (Build result) where
  pure value = Build ($ value)
  Build function <*> Build argument = Build (\use -> function (\f -> argument (use . f)))

instance Monad (Build result) where
  Build action >>= next = Build (\use -> action (\value -> let Build rest = next value in rest use))

build :: Build result result -> result
build (Build action) = action id

share :: Expr local stage a -> Build (Expr local stage result) (Expr local stage a)
share value = Build (letExpr value)

shareVertex :: Expr local 'VertexStage a -> Build (VertexBody local) (Expr local 'VertexStage a)
shareVertex value = Build (letVertex value)

repeatN :: FloatShape a -> Word32 -> Expr local stage a
        -> (Scalar local stage -> Expr local stage a -> Expr local stage a) -> Expr local stage a
repeatN shape count initial update = case loopLimit count of
  Just limit -> foldCount shape limit (UnsignedLiteral count) initial (update . UnsignedFloat)
  Nothing -> error "Stillroom shader loop exceeds the supported bound"

v2 :: Scalar local stage -> Scalar local stage -> V2 local stage
v2 = MakeVec2

v3 :: Scalar local stage -> Scalar local stage -> Scalar local stage -> V3 local stage
v3 = MakeVec3

v4 :: Scalar local stage -> Scalar local stage -> Scalar local stage -> Scalar local stage -> V4 local stage
v4 = MakeVec4

x2, y2 :: V2 local stage -> Scalar local stage
x2 = Component X2
y2 = Component Y2

x3, y3, z3 :: V3 local stage -> Scalar local stage
x3 = Component X3
y3 = Component Y3
z3 = Component Z3

x4, y4, z4, w4 :: V4 local stage -> Scalar local stage
x4 = Component X4
y4 = Component Y4
z4 = Component Z4
w4 = Component W4

xy, xz :: V3 local stage -> V2 local stage
xy xyz = letExpr xyz (\p -> v2 (x3 p) (y3 p))
xz xyz = letExpr xyz (\p -> v2 (x3 p) (z3 p))

rgb :: V4 local stage -> V3 local stage
rgb rgbaValue = letExpr rgbaValue (\p -> v3 (x4 p) (y4 p) (z4 p))

rgba :: V3 local stage -> Scalar local stage -> V4 local stage
rgba xyz alpha = letExpr xyz (\p -> v4 (x3 p) (y3 p) (z3 p) alpha)

add2, sub2, mul2 :: V2 local stage -> V2 local stage -> V2 local stage
add2 = FloatBinary Float2 AddF
sub2 = FloatBinary Float2 SubtractF
mul2 = FloatBinary Float2 MultiplyF

scale2 :: V2 local stage -> Scalar local stage -> V2 local stage
scale2 = ScaleVector Vector2

add3, sub3, mul3, divide3 :: V3 local stage -> V3 local stage -> V3 local stage
add3 = FloatBinary Float3 AddF
sub3 = FloatBinary Float3 SubtractF
mul3 = FloatBinary Float3 MultiplyF
divide3 = FloatBinary Float3 DivideF

scale3 :: V3 local stage -> Scalar local stage -> V3 local stage
scale3 = ScaleVector Vector3

splat2 :: Scalar local stage -> V2 local stage
splat2 value = letExpr value (\x -> v2 x x)

splat3 :: Scalar local stage -> V3 local stage
splat3 value = letExpr value (\x -> v3 x x x)

absF, floorF, fractF, sinF, cosF, expF, sqrtF :: Scalar local stage -> Scalar local stage
absF = FloatUnary Float1 Absolute
floorF = FloatUnary Float1 FloorF
fractF = FloatUnary Float1 Fract
sinF = FloatUnary Float1 Sin
cosF = FloatUnary Float1 Cos
expF = FloatUnary Float1 Exp
sqrtF = FloatUnary Float1 Sqrt

powF, minF, maxF, stepF :: Scalar local stage -> Scalar local stage -> Scalar local stage
powF = FloatBinary Float1 Power
minF = FloatBinary Float1 Minimum
maxF = FloatBinary Float1 Maximum
stepF = FloatBinary Float1 Step

mixF, smooth, clampF :: Scalar local stage -> Scalar local stage -> Scalar local stage -> Scalar local stage
mixF = FloatTernary Float1 Mix
smooth = FloatTernary Float1 Smoothstep
clampF = FloatTernary Float1 Clamp

floor2, fract2 :: V2 local stage -> V2 local stage
floor2 = FloatUnary Float2 FloorF
fract2 = FloatUnary Float2 Fract

floor3, fract3, exp3 :: V3 local stage -> V3 local stage
floor3 = FloatUnary Float3 FloorF
fract3 = FloatUnary Float3 Fract
exp3 = FloatUnary Float3 Exp

pow3 :: V3 local stage -> Scalar local stage -> V3 local stage
pow3 color power = FloatBinary Float3 Power color (splat3 power)

mix3 :: V3 local stage -> V3 local stage -> Scalar local stage -> V3 local stage
mix3 first second ratio = FloatTernary Float3 Mix first second (splat3 ratio)

clamp3 :: V3 local stage -> Scalar local stage -> Scalar local stage -> V3 local stage
clamp3 color lower upper = FloatTernary Float3 Clamp color (splat3 lower) (splat3 upper)

reflect3 :: V3 local stage -> V3 local stage -> V3 local stage
reflect3 ray normal = sub3 ray (scale3 normal (2 * Dot Vector3 normal ray))

rotate2 :: Scalar local stage -> Scalar local stage -> V2 local stage -> V2 local stage
rotate2 cosine sine value = letExpr value $ \p ->
  v2 (cosine * x2 p + sine * y2 p) (cosine * y2 p - sine * x2 p)

less, lessEqual, greater :: Scalar local stage -> Scalar local stage -> Expr local stage Bool
less = Compare Less
lessEqual = Compare LessEqual
greater = Compare Greater

unit :: Scalar local stage -> Scalar local stage
unit value = clampF value 0 1
