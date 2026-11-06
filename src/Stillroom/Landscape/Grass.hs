module Stillroom.Landscape.Grass
  ( GrassBlade, GrassCorner, GrassUV (..), GrassVertex (..),
    grassBlade, bladeRoot, bladeHeight, bladeWidth, bladeSide, bladeCurve,
    bladeColor, bladeVertexOffset, grassCorners, grassVertex, foldGrassVertices,
    grassShapeGLSL, grassPositionErrorBound,
  ) where

import Data.Vector.Storable qualified as V
import Data.Word (Word8, Word32)
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff)
import Stillroom.Landscape.Grass.Kernel (bladeCorner, grassShapeGLSL)

data GrassUV = JourneyGrass | FixedGrass deriving (Eq, Show)

-- The final Word32 is CPU diagnostic order, not a vertex shader attribute.
-- It preserves the original mixed blade/blossom Grass stream without retaining
-- the expanded blade vertices. All offsets count vertices, not bytes.
data GrassBlade = GrassBlade
  !Float !Float !Float !Float !Float !Float !Float !Float
  !Word8 !Word8 !Word8 !Word8 !Word32
  deriving (Eq, Show)

data GrassCorner = GrassCorner !Float !Float !Float deriving (Eq, Show)

data GrassVertex = GrassVertex
  { grassPosition :: !(Float,Float,Float), grassNormal :: !(Float,Float,Float),
    grassTexCoord :: !(Float,Float), grassExtra :: !(Float,Float),
    grassVertexColor :: !(Word8,Word8,Word8,Word8)
  } deriving (Eq, Show)

grassBlade
  :: Word32 -> (Float,Float,Float) -> Float -> Float -> Float -> Float
  -> (Word8,Word8,Word8,Word8) -> GrassBlade
grassBlade offset (x,y,z) height width angle curve (r,g,b,a) =
  GrassBlade x y z height width (cos angle) (sin angle) curve r g b a offset

bladeRoot :: GrassBlade -> (Float,Float,Float)
bladeRoot (GrassBlade x y z _ _ _ _ _ _ _ _ _ _) = (x,y,z)

bladeHeight, bladeWidth, bladeCurve :: GrassBlade -> Float
bladeHeight (GrassBlade _ _ _ height _ _ _ _ _ _ _ _ _) = height
bladeWidth (GrassBlade _ _ _ _ width _ _ _ _ _ _ _ _) = width
bladeCurve (GrassBlade _ _ _ _ _ _ _ curve _ _ _ _ _) = curve

bladeSide :: GrassBlade -> (Float,Float)
bladeSide (GrassBlade _ _ _ _ _ x z _ _ _ _ _ _) = (x,z)

bladeColor :: GrassBlade -> (Word8,Word8,Word8,Word8)
bladeColor (GrassBlade _ _ _ _ _ _ _ _ r g b a _) = (r,g,b,a)

bladeVertexOffset :: GrassBlade -> Word32
bladeVertexOffset (GrassBlade _ _ _ _ _ _ _ _ _ _ _ _ offset) = offset

grassCorners :: V.Vector GrassCorner
grassCorners = V.fromList
  [ corner 0 (-1), corner 0 1, corner 1 (-1)
  , corner 0 1, corner 1 1, corner 1 (-1)
  , corner 1 (-1), corner 1 1, corner 2 (-1)
  , corner 1 1, corner 2 1, corner 2 (-1)
  , corner 2 (-1), corner 2 1, corner 3 (-1)
  ]
  where
    corner :: Int -> Float -> GrassCorner
    corner i sideSign =
      let t = fromIntegral i/3
          piF = 3.14159265358979323846 :: Float
       in GrassCorner t (0.7+0.3*sin (t*piF)) sideSign

grassVertex :: GrassUV -> GrassBlade -> GrassCorner -> GrassVertex
grassVertex mode (GrassBlade x y z height width sideX sideZ curve r g b a _) (GrassCorner t profile sideSign) =
  let uvV = case mode of JourneyGrass -> y; FixedGrass -> sideSign
      (position,normal,texCoord,extra) = bladeCorner (x,y,z) height width (sideX,sideZ) curve (t,profile,sideSign) uvV
   in GrassVertex position normal texCoord extra (r,g,b,a)
{-# INLINE grassVertex #-}

foldGrassVertices :: (a -> GrassVertex -> a) -> a -> GrassUV -> GrassBlade -> a
foldGrassVertices combine initial mode blade = V.foldl' (\acc corner -> combine acc (grassVertex mode blade corner)) initial grassCorners
{-# INLINE foldGrassVertices #-}

-- Conservative pre-wind absolute position tolerance for finite authored inputs.
-- Sixteen ULPs at this enclosing magnitude cover the short multiply/add chains,
-- including permitted GPU contraction. Normals, UVs, extra and color use only
-- stored inputs/constants and are exact before the existing wind shader runs.
grassPositionErrorBound :: GrassBlade -> Float
grassPositionErrorBound blade =
  let (x,y,z) = bladeRoot blade
      magnitude = maximum [1,abs x,abs y,abs z]+abs (bladeHeight blade)+2*abs (bladeWidth blade)+2*abs (bladeCurve blade)
      (_,power) = decodeFloat magnitude
   in 16*encodeFloat 1 power

instance Storable GrassBlade where
  sizeOf _ = 40
  alignment _ = 4
  peek pointer = do
    x <- peekByteOff pointer 0; y <- peekByteOff pointer 4; z <- peekByteOff pointer 8
    height <- peekByteOff pointer 12; width <- peekByteOff pointer 16
    sideX <- peekByteOff pointer 20; sideZ <- peekByteOff pointer 24; curve <- peekByteOff pointer 28
    r <- peekByteOff pointer 32; g <- peekByteOff pointer 33; b <- peekByteOff pointer 34; a <- peekByteOff pointer 35
    offset <- peekByteOff pointer 36
    pure (GrassBlade x y z height width sideX sideZ curve r g b a offset)
  poke pointer (GrassBlade x y z height width sideX sideZ curve r g b a offset) = do
    pokeByteOff pointer 0 x; pokeByteOff pointer 4 y; pokeByteOff pointer 8 z; pokeByteOff pointer 12 height
    pokeByteOff pointer 16 width; pokeByteOff pointer 20 sideX; pokeByteOff pointer 24 sideZ; pokeByteOff pointer 28 curve
    pokeByteOff pointer 32 r; pokeByteOff pointer 33 g; pokeByteOff pointer 34 b; pokeByteOff pointer 35 a
    pokeByteOff pointer 36 offset

instance Storable GrassCorner where
  sizeOf _ = 12
  alignment _ = 4
  peek pointer = GrassCorner <$> peekByteOff pointer 0 <*> peekByteOff pointer 4 <*> peekByteOff pointer 8
  poke pointer (GrassCorner t profile sideSign) = do
    pokeByteOff pointer 0 t; pokeByteOff pointer 4 profile; pokeByteOff pointer 8 sideSign
