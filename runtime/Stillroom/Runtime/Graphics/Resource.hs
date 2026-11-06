{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Graphics.Resource
  ( GraphicsError (..),
    Scope,
    newScope,
    closeScope,
    own,
    Program,
    loadProgram,
    programId,
    uniformLocation,
    float,
    int,
    vec2,
    vec3,
    vec4,
    matrix,
    texture,
    shaderMode,
    nativeTransform,
    matrixTransform,
    MaterialHandle,
    loadMaterial,
    useMaterial,
    GpuMesh,
    uploadMesh,
    uploadRailcar,
    drawMesh,
    triangleCount,
    GpuGeometry (..),
    FingerprintMode (..),
    uploadGeometry,
    uploadGeometryWithSignature,
    releaseGeometry,
    geometryTriangles,
    ParentState,
    saveParent,
    restoreParent,
    textureMode,
    mode3D,
    white,
    black,
    identity,
  )
where

import Control.Exception
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Data (Typeable)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Vector.Storable qualified as V
import Foreign hiding (void)
import Foreign.C
import Raylib.Core qualified as R
import Raylib.Core.Models qualified as M
import Raylib.Core.Textures qualified as T
import Raylib.Types
import Raylib.Util.Math (matrixIdentity)
import Raylib.Util.RLGL qualified as RL
import Stillroom.Graphics.Encoding.Mesh qualified as Packet
import Stillroom.Graphics.Space qualified as Space
import Stillroom.Landscape.Geometry qualified as G
import Stillroom.Runtime.Graphics.Encoding qualified as Encoding
import Stillroom.Runtime.Graphics.GL qualified as GL

data GraphicsError = ShaderUnavailable !String | GeometryUnavailable !String | TextureUnavailable | MaterialUnavailable
  deriving (Show, Typeable)

instance Exception GraphicsError

-- Explicit, idempotent context-thread destruction. The scope has no GC finalizer.
newtype Scope = Scope (IORef [IO ()])

newScope :: IO Scope
newScope = Scope <$> newIORef []

closeScope :: Scope -> IO ()
closeScope (Scope ref) = atomicModifyIORef' ref (\xs -> ([], xs)) >>= foldr finally (pure ())

own :: Scope -> IO a -> (a -> IO ()) -> IO a
own (Scope ref) acquire release = mask_ $ do
  value <- acquire
  modifyIORef' ref (release value :)
  pure value

data Program = Program !(Ptr Shader) !Integer !(IORef (Map.Map String Int))

programId :: Program -> Integer
programId (Program _ n _) = n

loadProgram :: Scope -> BS.ByteString -> Maybe BS.ByteString -> BS.ByteString -> String -> IO Program
loadProgram scope common vertex fragment required = own scope acquire release
  where
    full body = BS.concat [BS.pack [35, 118, 101, 114, 115, 105, 111, 110, 32, 51, 51, 48, 32, 99, 111, 114, 101, 10], common, BS.singleton 10, body]
    withVertex f = maybe (f nullPtr) (\body -> BS.useAsCString (full body) f) vertex
    acquire = mask_ $ do
      p <- withVertex $ \vs -> BS.useAsCString (full fragment) (R.c'loadShaderFromMemory vs)
      when (p == nullPtr) (throwIO (ShaderUnavailable required))
      n <- fromIntegral <$> peek (p'shader'id p)
      def <- RL.rlGetShaderIdDefault
      valid <- R.c'isShaderValid p
      loc <- withCString required (R.c'getShaderLocation p)
      if n == 0 || n == def || valid == 0 || loc < 0
        then do
          when (n /= 0 && n /= def) (R.c'unloadShader p)
          free p
          throwIO (ShaderUnavailable required)
        else Program p n <$> newIORef Map.empty
    release (Program p _ _) = R.c'unloadShader p `finally` free p

uniformLocation :: Program -> String -> IO Int
uniformLocation (Program p _ ref) name = do
  cache <- readIORef ref
  case Map.lookup name cache of
    Just location -> pure location
    Nothing -> do
      location <- fromIntegral <$> withCString name (R.c'getShaderLocation p)
      modifyIORef' ref (Map.insert name location)
      pure location

uniform :: (Storable a) => CInt -> Program -> String -> a -> IO ()
uniform kind program@(Program p _ _) name value = do
  location <- uniformLocation program name
  when (location >= 0) $ with value $ \v -> R.c'setShaderValue p (fromIntegral location) (castPtr v) kind

float :: Program -> String -> Float -> IO ()
float p n = uniform 0 p n . (realToFrac :: Float -> CFloat)

int :: Program -> String -> Int -> IO ()
int p n = uniform 4 p n . (fromIntegral :: Int -> CInt)

vec2 :: Program -> String -> Vector2 -> IO ()
vec2 = uniform 1

vec3 :: Program -> String -> Vector3 -> IO ()
vec3 = uniform 2

vec4 :: Program -> String -> Vector4 -> IO ()
vec4 = uniform 3

matrix :: Program -> String -> Matrix -> IO ()
matrix program@(Program p _ _) name value = do
  location <- uniformLocation program name
  when (location >= 0) $ with value (R.c'setShaderValueMatrix p (fromIntegral location))

nativeTransform :: Space.Transform from to -> IO Matrix
nativeTransform = pure . Encoding.transform

matrixTransform :: Program -> String -> Space.Transform from to -> IO ()
matrixTransform program name value = nativeTransform value >>= matrix program name

texture :: Program -> String -> Texture -> Int -> IO ()
texture program name image slot = do
  RL.rlEnableShader (programId program)
  RL.rlActiveTextureSlot slot
  RL.rlEnableTexture (texture'id image)
  location <- uniformLocation program name
  with (fromIntegral slot :: CInt) $ \p -> RL.c'rlSetUniform (fromIntegral location) (castPtr p) 4 1
  RL.rlActiveTextureSlot 0

shaderMode :: Program -> IO a -> IO a
shaderMode (Program p _ _) = bracket_ (R.c'beginShaderMode p) R.endShaderMode

newtype MaterialHandle = MaterialHandle (Ptr Material)

loadMaterial :: Scope -> IO MaterialHandle
loadMaterial scope = own scope acquire release
  where
    acquire = do
      p <- M.c'loadMaterialDefault
      when (p == nullPtr) (throwIO MaterialUnavailable)
      maps <- peek (p'material'maps p)
      when (maps == nullPtr) (free p >> throwIO MaterialUnavailable)
      pure (MaterialHandle p)
    -- Default textures and the program copied into this material are borrowed.
    release (MaterialHandle p) = (peek (p'material'maps p) >>= R.c'memFree . castPtr) `finally` free p

useMaterial :: MaterialHandle -> Program -> IO ()
useMaterial (MaterialHandle p) (Program shader _ _) = copyBytes (p'material'shader p) shader nativeShaderSize

nativeShaderSize, nativeMeshSize :: Int
nativeShaderSize = sizeOf (Shader 0 [])
nativeMeshSize = sizeOf (Mesh 0 0 [] Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0 0 Nothing)

data GpuMesh = GpuMesh !(Ptr Mesh) !Int

triangleCount :: GpuMesh -> Int
triangleCount (GpuMesh _ n) = n

uploadMesh :: Scope -> G.MeshData -> IO (Maybe GpuMesh)
uploadMesh scope mesh =
  uploadStreams
    scope
    (G.meshVertexCount mesh)
    (G.meshTriangleCount mesh)
    (G.meshPositions mesh)
    (G.meshNormals mesh)
    (G.meshTexCoords mesh)
    (G.meshExtra mesh)
    (G.meshColors mesh)
    (G.meshIndices mesh)

uploadStreams :: Scope -> Int -> Int -> V.Vector Float -> V.Vector Float -> V.Vector Float -> V.Vector Float -> V.Vector Word8 -> V.Vector Word16 -> IO (Maybe GpuMesh)
uploadStreams scope count triangles positions normals uv extra colors indices
  | count == 0 = pure Nothing
  | count < 0 || V.length positions /= count * 3 || V.length normals /= count * 3 = throwIO (GeometryUnavailable "Invalid mesh streams")
  | otherwise = Just <$> own scope acquire release
  where
    acquire = mask_ $ do
      p <- callocBytes nativeMeshSize
      -- DrawMesh selects indexed drawing from this CPU pointer even after
      -- upload. Unlike the other attribute arrays, indices stay owned by the
      -- native mesh until UnloadMesh frees them on the context thread.
      indexData <-
        if V.null indices
          then pure nullPtr
          else do
            ip <- R.c'memAlloc (fromIntegral (V.length indices * sizeOf (0 :: Word16)))
            when (ip == nullPtr) (free p >> throwIO (GeometryUnavailable "Index allocation failed"))
            V.unsafeWith indices (\source -> copyBytes ip (castPtr source) (V.length indices * sizeOf (0 :: Word16)))
            pure (castPtr ip)
      poke (p'mesh'indices p) indexData
      let detach = do
            poke (p'mesh'vertices p) nullPtr
            poke (p'mesh'normals p) nullPtr
            poke (p'mesh'texcoords p) nullPtr
            poke (p'mesh'texcoords2 p) nullPtr
            poke (p'mesh'colors p) nullPtr
          upload = V.unsafeWith positions $ \vp -> V.unsafeWith normals $ \np ->
            optional uv $ \tp -> optional extra $ \ep -> optional colors $ \cp -> do
              poke (p'mesh'vertexCount p) (fromIntegral count)
              poke (p'mesh'triangleCount p) (fromIntegral triangles)
              poke (p'mesh'vertices p) (castPtr vp)
              poke (p'mesh'normals p) (castPtr np)
              poke (p'mesh'texcoords p) (castPtr tp)
              poke (p'mesh'texcoords2 p) (castPtr ep)
              poke (p'mesh'colors p) (castPtr cp)
              M.c'uploadMesh p 0
      (upload `finally` detach) `onException` (M.c'unloadMesh p `finally` free p)
      vao <- peek (p'mesh'vaoId p)
      if vao == 0 then M.c'unloadMesh p >> free p >> throwIO (GeometryUnavailable "Mesh upload failed") else pure (GpuMesh p triangles)
    release (GpuMesh p _) = M.c'unloadMesh p `finally` free p
    optional :: (Storable a) => V.Vector a -> (Ptr a -> IO b) -> IO b
    optional xs action = if V.null xs then action nullPtr else V.unsafeWith xs action

uploadRailcar :: Scope -> Packet.MeshPacket -> IO GpuMesh
uploadRailcar scope packet = do
  let values = Packet.meshValues packet
      count = Packet.meshVertexCount packet
      stream stride offset width = V.generate (count * width) $ \i -> values V.! ((i `div` width) * stride + offset + i `mod` width)
      positions = stream 10 0 3
      normals = stream 10 3 3
      uv = V.generate (count * 2) $ \i -> if even i then 2 else 0
      extra = V.generate (count * 2) $ \i -> if even i then -1 else 0
      colors = V.map (fromIntegral . (truncate :: Float -> Int)) (stream 10 6 4)
  result <- uploadStreams scope count (count `div` 3) positions normals uv extra colors V.empty
  maybe (throwIO (GeometryUnavailable "Empty railcar")) pure result

drawMesh :: MaterialHandle -> GpuMesh -> IO ()
drawMesh (MaterialHandle material) (GpuMesh mesh _) = with matrixIdentity (M.c'drawMesh mesh material)

data FingerprintMode = SkipFingerprints | CaptureFingerprints deriving (Eq, Show)

data GpuGeometry = GpuGeometry
  { gpuMaterials :: ![Maybe GpuMesh],
    gpuWater :: !(Maybe GpuMesh),
    gpuBones :: !Texture,
    gpuHeight :: !(Maybe Texture),
    gpuTrees :: !Int,
    gpuGrass :: !Int,
    gpuBonesCount :: !Int,
    gpuSignature :: !(Maybe Word64),
    geometryScope :: !Scope
  }

uploadGeometry :: G.Geometry -> IO GpuGeometry
uploadGeometry = uploadGeometryWithSignature Nothing

uploadGeometryWithSignature :: Maybe Word64 -> G.Geometry -> IO GpuGeometry
uploadGeometryWithSignature fingerprint geometry = bracketOnError newScope closeScope $ \scope -> do
  -- A diagnostic caller may supply a lazy hash. Finish it while CPU buffers
  -- are present rather than retaining them through a thunk after upload.
  signature <- traverse evaluate fingerprint
  meshes <- mapM (uploadMesh scope . G.materialMesh geometry) [G.Terrain, G.Wood, G.Foliage, G.Grass]
  water <- uploadMesh scope (G.waterMesh geometry)
  bones <- pixels scope 256 (G.boneTextureHeight geometry) 10 (G.bonePixels geometry) TextureFilterPoint
  height <-
    if G.meshVertexCount (G.waterMesh geometry) == 0 || V.null (G.heightPixels geometry)
      then pure Nothing
      else
        Just
          <$> pixels
            scope
            (G.heightWidth geometry)
            (G.heightHeight geometry)
            8
            (G.heightPixels geometry)
            TextureFilterBilinear
  pure (GpuGeometry meshes water bones height (G.treeCount geometry) (G.grassCount geometry) (G.boneCount geometry) signature scope)
  where
    pixels scope width height format values filtering =
      own
        scope
        ( do
            n <- V.unsafeWith values $ \p -> fromIntegral <$> RL.c'rlLoadTexture (castPtr p) (fromIntegral width) (fromIntegral height) format 1
            when (n == 0) (throwIO TextureUnavailable)
            let t = Texture n width height 1 (toEnum (fromIntegral format))
            _ <- T.setTextureFilter t filtering
            _ <- T.setTextureWrap t TextureWrapClamp
            pure t
        )
        (RL.rlUnloadTexture . texture'id)

releaseGeometry :: GpuGeometry -> IO ()
releaseGeometry = closeScope . geometryScope

geometryTriangles :: GpuGeometry -> Int
geometryTriangles g = sum (map (maybe 0 triangleCount) (gpuMaterials g)) + maybe 0 triangleCount (gpuWater g)

data ParentState = ParentState !Integer ![Int] !Matrix !Matrix !Int !Int

saveParent :: IO ParentState
saveParent = do
  RL.rlDrawRenderBatchActive
  fb <- RL.rlGetActiveFramebuffer
  viewport <- GL.integers 0x0ba2 4
  ParentState fb viewport <$> RL.rlGetMatrixModelview <*> RL.rlGetMatrixProjection <*> RL.rlGetFramebufferWidth <*> RL.rlGetFramebufferHeight

restoreParent :: ParentState -> IO ()
restoreParent (ParentState fb viewport view projection width height) = do
  RL.rlEnableFramebuffer fb
  case viewport of
    [x, y, w, h] -> RL.rlViewport x y w h
    _ -> pure ()
  RL.rlSetFramebufferWidth width
  RL.rlSetFramebufferHeight height
  RL.rlSetMatrixModelview view
  RL.rlSetMatrixProjection projection

textureMode :: RenderTexture -> IO a -> IO a
textureMode target = bracket_ (R.beginTextureMode target) R.endTextureMode

mode3D :: Camera3D -> IO a -> IO a
mode3D camera = bracket_ (R.beginMode3D camera) R.endMode3D

white, black :: Color
white = Color 255 255 255 255
black = Color 0 0 0 255

identity :: Matrix
identity = matrixIdentity
