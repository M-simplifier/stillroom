module Stillroom.Runtime.Graphics.WorldGeometry
  ( Anchor(..), Geometry(..), uploadGeometry, uploadCar, releaseGeometry, grassTemplate
  , relativeTo, absoluteAnchor
  ) where

import Control.Exception (bracketOnError, evaluate, throwIO)
import qualified Data.ByteString as BS
import Data.Vector.Storable qualified as V
import Data.Word (Word8, Word32, Word64)
import Foreign (castPtr, sizeOf)
import GameIO.Graphics qualified as GPU
import Stillroom.Graphics.Encoding.Mesh qualified as Mesh
import Stillroom.Landscape.Geometry qualified as Land
import Stillroom.Landscape.Grass qualified as Grass
import Stillroom.Runtime.Graphics.Scene (Scope, closeScope, newScope, own)

data Anchor = Anchor !Double !Double deriving (Eq, Show)

data Geometry = Geometry
  { landMeshes :: ![(Land.Material, GPU.Mesh)]
  , grassInstances :: !(Maybe GPU.Instances)
  , waterMesh :: !(Maybe (GPU.Mesh, GPU.Texture))
  , boneTexture :: !GPU.Texture
  , boneHeight :: !Int
  , landAnchor :: !Anchor
  , treeCount :: !Int
  , grassCount :: !Int
  , instanceCount :: !Int
  , triangleCount :: !Int
  , signature :: !(Maybe Word64)
  , geometryScope :: !Scope
  }

relativeTo :: Anchor -> (Double, Double, Double) -> GPU.Vec3
relativeTo (Anchor x z) (ox, oy, oz) = GPU.Vec3 (realToFrac (x-ox)) (realToFrac (negate oy)) (realToFrac (z-oz))

absoluteAnchor :: Anchor -> GPU.Vec3
absoluteAnchor (Anchor x z) = GPU.Vec3 (realToFrac x) 0 (realToFrac z)

grassTemplate :: GPU.Renderer -> Scope -> IO GPU.InstanceTemplate
grassTemplate renderer scope = own scope (GPU.createInstanceTemplate renderer corners) (GPU.destroyInstanceTemplate renderer)
  where
    corners = [GPU.Vec3 t profile side | corner <- V.toList Grass.grassCorners
      , let (t, profile, side) = Grass.grassCornerValues corner]

uploadGeometry :: GPU.Renderer -> Anchor -> Bool -> Land.Geometry -> IO Geometry
uploadGeometry renderer anchor fingerprint geometry = bracketOnError newScope closeScope $ \scope -> do
  identity <- if fingerprint then Just <$> evaluate (Land.geometrySignature geometry) else pure Nothing
  let materials = [Land.Terrain, Land.Wood, Land.Foliage, Land.Grass]
  let meshParts = [(kind, Land.materialMesh geometry kind) | kind <- materials]
  meshes <- sequence [fmap ((,) kind) <$> uploadMesh renderer scope anchor mesh | (kind, mesh) <- meshParts]
  let blades = Land.grassBlades geometry
  instances <- if V.null blades then pure Nothing else
    Just <$> own scope (GPU.createInstances renderer (map (encodeBlade anchor) (V.toList blades))) (GPU.destroyInstances renderer)
  let water = Land.waterMesh geometry
  shore <- uploadMesh renderer scope anchor water
  waterWithHeight <- traverse (\mesh -> do
    height <- floatTexture scope GPU.Linear GPU.R32Float (Land.heightWidth geometry)
      (Land.heightHeight geometry) (Land.heightPixels geometry)
    pure (mesh, height)) shore
  let boneRows = Land.boneTextureHeight geometry
  bones <- floatTexture scope GPU.Nearest GPU.RGBA32Float 256 boneRows (Land.bonePixels geometry)
  let triangles = sum (map (Land.meshTriangleCount . snd) meshParts) + 5 * V.length blades + Land.meshTriangleCount water
  pure (Geometry [mesh | Just mesh <- meshes] instances waterWithHeight bones boneRows anchor
    (Land.treeCount geometry) (Land.grassCount geometry) (V.length blades) triangles identity scope)
  where
    floatTexture scope filterMode format width height values = do
      packed <- V.unsafeWith values $ \ptr -> BS.packCStringLen (castPtr ptr, V.length values * sizeOf (0 :: Float))
      own scope (GPU.createTextureFloat32 renderer filterMode format width height packed) (GPU.destroyTexture renderer)

releaseGeometry :: Geometry -> IO ()
releaseGeometry = closeScope . geometryScope

uploadMesh :: GPU.Renderer -> Scope -> Anchor -> Land.MeshData -> IO (Maybe GPU.Mesh)
uploadMesh renderer scope anchor source
  | count == 0 = pure Nothing
  | V.length positions /= count*3 || V.length normals /= count*3 ||
      not (optionalSize uv (count*2) && optionalSize extra (count*2) && optionalSize colors (count*4)) =
      throwIO (userError "Invalid Stillroom vertex attribute lengths")
  | otherwise = Just <$> residentMesh renderer scope vertices indices
  where
    count = Land.meshVertexCount source
    positions = Land.meshPositions source
    normals = Land.meshNormals source
    uv = Land.meshTexCoords source
    extra = Land.meshExtra source
    colors = Land.meshColors source
    optionalSize array expected = V.null array || V.length array == expected
    pair array i = if V.null array then GPU.Vec2 0 0 else GPU.Vec2 (array V.! (i*2)) (array V.! (i*2+1))
    triple array i = (array V.! (i*3), array V.! (i*3+1), array V.! (i*3+2))
    normal i = let (x, y, z) = triple normals i in GPU.Vec3 x y z
    tint i
      | V.null colors = GPU.Color 1 1 1 1
      | otherwise = GPU.Color (unorm (colors V.! (i*4))) (unorm (colors V.! (i*4+1)))
          (unorm (colors V.! (i*4+2))) (unorm (colors V.! (i*4+3)))
    vertices = [GPU.VertexWithDetails (localPosition anchor (triple positions i)) (pair uv i) (tint i)
      (normal i) (pair extra i) | i <- [0..count-1]]
    indices = if V.null (Land.meshIndices source) then [0..fromIntegral count-1]
      else map fromIntegral (V.toList (Land.meshIndices source))

uploadCar :: GPU.Renderer -> Scope -> Mesh.MeshPacket -> IO GPU.Mesh
uploadCar renderer scope packet = residentMesh renderer scope vertices [0..fromIntegral count-1]
  where
    values = Mesh.meshValues packet
    count = Mesh.meshVertexCount packet
    vertices = [let field n = values V.! (i*10+n) in
      GPU.VertexWithDetails (GPU.Vec3 (field 0) (field 1) (field 2)) (GPU.Vec2 2 0)
        (GPU.Color (field 6/255) (field 7/255) (field 8/255) (field 9/255))
        (GPU.Vec3 (field 3) (field 4) (field 5)) (GPU.Vec2 (-1) 0) | i <- [0..count-1]]

residentMesh :: GPU.Renderer -> Scope -> [GPU.Vertex] -> [Word32] -> IO GPU.Mesh
residentMesh renderer scope vertices indices = do
  mesh <- either (throwIO . userError . show) pure (GPU.meshData vertices indices)
  own scope (GPU.createMesh renderer mesh) (GPU.destroyMesh renderer)

localPosition :: Anchor -> (Float, Float, Float) -> GPU.Vec3
localPosition (Anchor x z) (px, py, pz) =
  GPU.Vec3 (realToFrac (realToFrac px-x)) py (realToFrac (realToFrac pz-z))

encodeBlade :: Anchor -> Grass.GrassBlade -> GPU.InstanceData
encodeBlade anchor blade = GPU.InstanceData (GPU.Vec4 x y z (Grass.bladeHeight blade))
  (GPU.Vec4 (Grass.bladeWidth blade) sideX sideZ (Grass.bladeCurve blade))
  (Grass.bladeColor blade) (Grass.bladeVertexOffset blade)
  where
    GPU.Vec3 x y z = localPosition anchor (Grass.bladeRoot blade)
    (sideX, sideZ) = Grass.bladeSide blade

unorm :: Word8 -> Float
unorm channel = fromIntegral channel / 255
