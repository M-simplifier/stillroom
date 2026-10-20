-- | The native renderer's vertex format. This is a lowering boundary, not a
-- shape definition: geometric topology has already been chosen.
module Stillroom.Graphics.Encoding.Mesh
  ( MeshPacket, MeshError (..), lowerMesh, meshValues, meshVertexCount,
  )
where

import Data.Foldable (toList)
import Data.Vector.Storable qualified as V
import Stillroom.Graphics.Mesh

newtype MeshPacket = MeshPacket (V.Vector Float)

data MeshError = EmptyMesh | MeshTooLarge !Int | NonFiniteMesh
  deriving (Eq, Show)

-- | Ten floats per vertex: position, outward normal, and four colour bytes.
-- Retain this result and upload once; no geometry needs regenerating per frame.
lowerMesh :: Mesh Float RGBA -> Either MeshError MeshPacket
lowerMesh triangles
  | vertexCount == 0 = Left EmptyMesh
  | vertexCount > 100000 = Left (MeshTooLarge vertexCount)
  | not (V.all finite values) = Left NonFiniteMesh
  | otherwise = Right (MeshPacket values)
  where
    vertexCount = 3 * length triangles
    values = V.fromList (concatMap (concatMap encodeVertex . toList) triangles)
    finite x = not (isNaN x || isInfinite x)
    encodeVertex (Vertex (V3 x y z) (V3 nx ny nz) (RGBA r g b a)) =
      [x,y,z,nx,ny,nz,fromIntegral r,fromIntegral g,fromIntegral b,fromIntegral a]

meshValues :: MeshPacket -> V.Vector Float
meshValues (MeshPacket values) = values

meshVertexCount :: MeshPacket -> Int
meshVertexCount (MeshPacket values) = V.length values `div` 10
