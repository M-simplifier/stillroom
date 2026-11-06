-- | Pure, retained CPU geometry. Construction establishes matching attribute
-- lengths, triangle topology, bounded bone IDs and the original material order.
-- The runtime owns every GPU allocation and may retain these immutable arrays
-- across allocation failure or context rebuilding.
module Stillroom.Landscape.Geometry
  ( Geometry, MeshData, Material (..), Scene (..), ChunkDetail (..), GenerationError (..),
    fixedSceneGeometry, seededChunkGeometry, fixedTerrain,
    chunkSpan, chunkCells, chunkSamplingStep, rasterGround, triangleHeight,
    materialMesh, waterMesh, meshPositions, meshNormals, meshTexCoords,
    meshExtra, meshColors, meshIndices, meshVertexCount, meshTriangleCount,
    bonePixels, boneTextureHeight, heightPixels, heightWidth, heightHeight,
    boneCount, treeCount, grassCount, geometrySignature,
  ) where

import Data.Vector.Storable (Vector)
import Data.Word (Word8, Word16, Word64)
import Stillroom.Landscape.Chunk
import Stillroom.Landscape.Fixed
import Stillroom.Landscape.Internal (Geometry, MeshData, Material (..), Scene (..), ChunkDetail (..), GenerationError (..))
import Stillroom.Landscape.Internal qualified as Internal
import Stillroom.Landscape.Math qualified as Math

triangleHeight :: Float -> Float -> Float -> Float -> Float -> Float -> Float
triangleHeight = Math.triangleHeight

materialMesh :: Geometry -> Material -> MeshData
materialMesh = Internal.materialMesh

waterMesh :: Geometry -> MeshData
waterMesh = Internal.waterMesh

meshPositions, meshNormals, meshTexCoords, meshExtra :: MeshData -> Vector Float
meshPositions = Internal.meshPositions
meshNormals = Internal.meshNormals
meshTexCoords = Internal.meshTexCoords
meshExtra = Internal.meshExtra

meshColors :: MeshData -> Vector Word8
meshColors = Internal.meshColors

meshIndices :: MeshData -> Vector Word16
meshIndices = Internal.meshIndices

meshVertexCount, meshTriangleCount :: MeshData -> Int
meshVertexCount = Internal.meshVertexCount
meshTriangleCount = Internal.meshTriangleCount

bonePixels, heightPixels :: Geometry -> Vector Float
bonePixels = Internal.bonePixels
heightPixels = Internal.heightPixels

boneTextureHeight, heightWidth, heightHeight, boneCount, treeCount, grassCount :: Geometry -> Int
boneTextureHeight = Internal.boneTextureHeight
heightWidth = Internal.heightWidth
heightHeight = Internal.heightHeight
boneCount = Internal.boneCount
treeCount = Internal.treeCount
grassCount = Internal.grassCount

geometrySignature :: Geometry -> Word64
geometrySignature = Internal.geometrySignature
