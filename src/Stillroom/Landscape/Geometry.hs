-- | Pure, retained CPU geometry. Construction establishes matching attribute
-- lengths, triangle topology, bounded bone IDs and the original material order.
-- The runtime owns every GPU allocation and may retain these immutable arrays
-- across allocation failure or context rebuilding.
module Stillroom.Landscape.Geometry
  ( Geometry, MeshData, Material (..), Scene (..), ChunkDetail (..), GenerationError (..),
    fixedSceneGeometry, seededChunkGeometry, fixedTerrain,
    chunkSpan, chunkCells, chunkSamplingStep, rasterGround, triangleHeight,
    materialMesh, materialVertexCount, diagnosticMaterialMesh, grassBlades, grassUV,
    waterMesh, meshPositions, meshNormals, meshTexCoords,
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
import Stillroom.Landscape.Grass qualified as Grass
import Stillroom.Landscape.Math qualified as Math

triangleHeight :: Float -> Float -> Float -> Float -> Float -> Float -> Float
triangleHeight = Math.triangleHeight

materialMesh :: Geometry -> Material -> MeshData
materialMesh = Internal.materialMesh

materialVertexCount :: Geometry -> Material -> Int
materialVertexCount = Internal.materialVertexCount

-- Explicit compatibility expansion for frozen byte acquisition, never upload.
diagnosticMaterialMesh :: Geometry -> Material -> MeshData
diagnosticMaterialMesh = Internal.diagnosticMaterialMesh

grassBlades :: Geometry -> Vector Grass.GrassBlade
grassBlades = Internal.grassBlades

grassUV :: Geometry -> Grass.GrassUV
grassUV = Internal.grassUV

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

-- | Explicit diagnostic traversal of the completed render buffers. Geometry
-- construction already packs every stream; ordinary rendering needs no hash.
geometrySignature :: Geometry -> Word64
geometrySignature = Internal.geometrySignature
