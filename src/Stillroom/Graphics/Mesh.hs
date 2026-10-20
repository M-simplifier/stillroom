{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Geometry before a backend chooses a byte layout.
--
-- A triangle always contains three vertices. Coordinates and appearance remain
-- independent parameters: exact solid boundaries can use Rational and named
-- materials, while a sampled curved surface can use Double.
module Stillroom.Graphics.Mesh
  ( V3 (..), Vertex (..), Triangle (..), Mesh, RGBA (..), mapVertex,
  )
where

import Data.Word (Word8)

data V3 a = V3 !a !a !a
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Vertex scalar material = Vertex
  { position :: !(V3 scalar),
    normal :: !(V3 scalar),
    appearance :: !material
  }
  deriving (Eq, Show)

data Triangle vertex = Triangle !vertex !vertex !vertex
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Mesh scalar material = [Triangle (Vertex scalar material)]

data RGBA = RGBA !Word8 !Word8 !Word8 !Word8 deriving (Eq, Show)

-- | Interpret coordinates and appearance without changing connectivity.
-- Mapping identity preserves a vertex; successive maps compose componentwise.
mapVertex :: (a -> b) -> (m -> n) -> Vertex a m -> Vertex b n
mapVertex coordinate material (Vertex p n m) =
  Vertex (fmap coordinate p) (fmap coordinate n) (material m)
