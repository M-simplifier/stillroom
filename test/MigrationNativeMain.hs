{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Vector.Storable qualified as V
import Foreign
import Foreign.C (CUShort)
import Raylib.Core qualified as R
import Raylib.Core.Models qualified as M
import Raylib.Core.Shapes qualified as S
import Raylib.Core.Textures qualified as T
import Raylib.Types
import Raylib.Util.RLGL qualified as RL
import Raylib.Util.Math (matrixMultiply)
import Stillroom.Graphics.Encoding.Mesh qualified as Packet
import Stillroom.Journey.Railcar qualified as Car
import Stillroom.Journey.World qualified as W
import Stillroom.Landscape.Geometry qualified as G
import Stillroom.Landscape.Grass qualified as Grass
import Stillroom.Runtime.Graphics.GL qualified as GL
import Stillroom.Runtime.Graphics.Resource qualified as Resource
import Stillroom.Runtime.Graphics.Target qualified as Target

assertion :: String -> Bool -> IO ()
assertion name passed = do
  putStrLn (name ++ ": " ++ show passed)
  unless passed (fail name)

scopeChecks :: IO ()
scopeChecks = do
  events <- newIORef ([] :: [Int])
  scope <- Resource.newScope
  forM_ [1,2,3] $ \n -> Resource.own scope (pure n) (\value -> modifyIORef' events (value:))
  Resource.closeScope scope
  Resource.closeScope scope
  released <- readIORef events
  assertion "scope releases in reverse acquisition order exactly once" (released == [1,2,3])
  failedScope <- Resource.newScope
  _ <- Resource.own failedScope (pure ()) (\() -> modifyIORef' events (9:))
  failed <- try (Resource.own failedScope (throwIO (userError "deliberate acquisition failure") :: IO ()) (\() -> modifyIORef' events (99:))) :: IO (Either IOException ())
  Resource.closeScope failedScope
  after <- readIORef events
  assertion "failed acquisition preserves earlier cleanup and owns no nonexistent handle" (either (const True) (const False) failed && after == [9,1,2,3])

copyFloats :: Ptr Vector3 -> Int -> IO (V.Vector Float)
copyFloats pointer count = V.generateM count (peekElemOff (castPtr pointer :: Ptr Float))

comparePlane :: String -> Float -> Float -> Int -> Float -> Float -> G.MeshData -> IO ()
comparePlane name width depth resolution ox oz expected =
  bracket (M.c'genMeshPlane (realToFrac width) (realToFrac depth) (fromIntegral resolution) (fromIntegral resolution))
    (\pointer -> M.c'unloadMesh pointer `finally` free pointer) $ \pointer -> do
      vertices <- fromIntegral <$> peek (p'mesh'vertexCount pointer)
      triangles <- fromIntegral <$> peek (p'mesh'triangleCount pointer)
      positions <- (`copyFloats` (vertices*3)) =<< peek (p'mesh'vertices pointer)
      normals <- (`copyFloats` (vertices*3)) =<< peek (p'mesh'normals pointer)
      uvPointer <- peek (p'mesh'texcoords pointer)
      uv <- V.generateM (vertices*2) (peekElemOff (castPtr uvPointer :: Ptr Float))
      indicesPointer <- peek (p'mesh'indices pointer)
      indices <- V.generateM (triangles*3) (\i -> fromIntegral <$> peekElemOff (indicesPointer :: Ptr CUShort) i :: IO Word16)
      let moved = V.imap (\i value -> case i `mod` 3 of 0 -> value+ox; 2 -> value+oz; _ -> value) positions
      assertion (name ++ " exact pinned raylib water vertices/normals/UV/indices")
        (vertices == G.meshVertexCount expected && triangles == G.meshTriangleCount expected &&
         moved == G.meshPositions expected && normals == G.meshNormals expected &&
         uv == G.meshTexCoords expected && indices == G.meshIndices expected)

indexedDraw :: G.MeshData -> IO ()
indexedDraw mesh = bracket Resource.newScope Resource.closeScope $ \scope -> do
  uploaded <- Resource.uploadMesh scope mesh >>= maybe (fail "Empty indexed plane") pure
  material <- Resource.loadMaterial scope
  let camera = Camera3D (Vector3 16 48 16) (Vector3 16 0 16) (Vector3 0 0 (-1)) 64 CameraOrthographic
      draw = bracket_ R.beginDrawing R.endDrawing $ do
        R.clearBackground (Color 255 0 255 255)
        bracket_ (R.beginMode3D camera) R.endMode3D (Resource.drawMesh material uploaded)
  -- Capture the completed swapped framebuffer after the same draw twice.
  draw
  draw
  bracket T.c'loadImageFromScreen (\p -> T.c'unloadImage p `finally` free p) $ \image -> do
    width <- fromIntegral <$> peek (p'image'width image)
    height <- fromIntegral <$> peek (p'image'height image)
    pixels <- peek (p'image'data image)
    colors <- peekArray (width*height) (castPtr pixels :: Ptr Color)
    let drawn = length [() | Color r g b _ <- colors, r/=255 || g/=0 || b/=255]
    assertion "indexed water renders a filled surface after CPU buffers leave upload" (drawn>width*height `div` 6)

targetChecks :: IO ()
targetChecks = do
  previous <- RL.rlGetActiveFramebuffer
  let acquire = Target.createSurface 64 64 >>= maybe (fail "Reference surface allocation failed") pure
  bracket acquire Target.releaseSurface $ \parent ->
    bracket_ (RL.rlEnableFramebuffer (renderTexture'id parent)) (RL.rlEnableFramebuffer previous) $ do
      bracket acquire Target.releaseSurface $ \_ -> do
        active <- RL.rlGetActiveFramebuffer
        assertion "nested surface creation restores the parent framebuffer" (active == renderTexture'id parent)
      active <- RL.rlGetActiveFramebuffer
      assertion "releasing an unrelated child preserves the parent framebuffer" (active == renderTexture'id parent)
      (do
        Target.allocationLimitsForCheck 1 1
        rejected <- Target.createSurface 64 64
        activeAfterFailure <- RL.rlGetActiveFramebuffer
        assertion "rejected surface allocation preserves the parent framebuffer" (case rejected of Nothing -> activeAfterFailure == renderTexture'id parent; Just _ -> False))
        `finally` Target.allocationLimitsForCheck 0 0

surfacePixels :: RenderTexture -> IO BS.ByteString
surfacePixels surface = bracket acquire release $ \image -> do
  pixels <- peek (p'image'data image)
  width <- fromIntegral <$> peek (p'image'width image)
  height <- fromIntegral <$> peek (p'image'height image)
  BS.packCStringLen (castPtr pixels, width * height * 4)
  where
    acquire = with (renderTexture'texture surface) T.c'loadImageFromTexture
    release image = T.c'unloadImage image `finally` free image

retainedChecks :: G.MeshData -> IO ()
retainedChecks plane = bracket Resource.newScope Resource.closeScope $ \scope -> do
  material <- Resource.loadMaterial scope
  let fragment = "in vec4 tint;out vec4 finalColor;void main(){finalColor=tint;}"
      explicit = "in vec3 vertexPosition;in vec4 vertexColor;uniform mat4 uClipFromRender;out vec4 tint;void main(){tint=vertexColor;gl_Position=uClipFromRender*vec4(vertexPosition,1.0);}"
      standard = "in vec3 vertexPosition;in vec4 vertexColor;uniform mat4 mvp;out vec4 tint;void main(){tint=vertexColor;gl_Position=mvp*vec4(vertexPosition,1.0);}"
      mixed = "in vec3 vertexPosition;in vec4 vertexColor;uniform mat4 uClipFromRender;uniform mat4 mvp;out vec4 tint;void main(){tint=vertexColor;gl_Position=(mvp+uClipFromRender)*vec4(vertexPosition,1.0)*0.5;}"
  program <- Resource.loadProgram scope "" (Just explicit) fragment "uClipFromRender"
  fallback <- Resource.loadProgram scope "" (Just standard) fragment "mvp"
  mixedProgram <- Resource.loadProgram scope "" (Just mixed) fragment "mvp"
  mesh <- Resource.uploadMesh scope plane >>= maybe (fail "Missing retained plane") pure
  carMesh <- either (fail . show) pure Car.railcarMesh
  packet <- either (fail . show) pure (Packet.lowerMesh carMesh)
  railcar <- Resource.uploadRailcar scope packet
  let acquire = Target.createSurface 64 64 >>= maybe (fail "Retained test target") pure
  bracket acquire Target.releaseSurface $ \surface -> do
    let draw camera selected retained expectedRetained geometry = Resource.textureMode surface $ do
          R.clearBackground (Color 255 0 255 255)
          Resource.mode3D camera $ do
            clip <- matrixMultiply <$> RL.rlGetMatrixModelview <*> RL.rlGetMatrixProjection
            Resource.matrix selected "uClipFromRender" clip
            Resource.useMaterial material selected
            let submit = Resource.doubleSided material (Resource.drawMesh material geometry)
            if retained
              then Resource.withMeshPass material selected $ do
                active <- Resource.meshPassActive material
                assertion "mesh pass selects the expected submission path" (active == expectedRetained)
                submit
              else submit
        compareDraw name camera geometry = do
          draw camera program False False geometry
          reference <- surfacePixels surface
          draw camera program True True geometry
          actual <- surfacePixels surface
          assertion (name ++ " retained pixels equal generic DrawMesh") (actual == reference)
          let background = BS.pack [255,0,255,255]
              drawn = length [() | i <- [0,4..BS.length reference-4], BS.take 4 (BS.drop i reference) /= background]
          assertion (name ++ " comparison contains visible geometry") (drawn > 32)
    compareDraw "indexed plane" (Camera3D (Vector3 16 48 16) (Vector3 16 0 16) (Vector3 0 0 (-1)) 64 CameraOrthographic) mesh
    compareDraw "non-indexed railcar" (Camera3D (Vector3 8 8 8) (Vector3 0 0 0) (Vector3 0 1 0) 16 CameraOrthographic) railcar
    Resource.referenceMeshesForCheck material True
    draw (Camera3D (Vector3 16 48 16) (Vector3 16 0 16) (Vector3 0 0 (-1)) 64 CameraOrthographic) program True False mesh
    Resource.referenceMeshesForCheck material False
    forM_ [fallback,mixedProgram] $ \selected -> do
      let camera = Camera3D (Vector3 16 48 16) (Vector3 16 0 16) (Vector3 0 0 (-1)) 64 CameraOrthographic
      draw camera selected False False mesh
      reference <- surfacePixels surface
      draw camera selected True False mesh
      actual <- surfacePixels surface
      assertion "standard matrix shader draws through unchanged generic fallback" (actual == reference)
    Resource.useMaterial material program
    nested <- try (Resource.withMeshPass material program (Resource.withMeshPass material program (pure ()))) :: IO (Either Resource.GraphicsError ())
    assertion "nested retained pass is rejected" (case nested of Left (Resource.GeometryUnavailable _) -> True; _ -> False)
    changed <- try (Resource.withMeshPass material program (Resource.useMaterial material fallback)) :: IO (Either Resource.GraphicsError ())
    assertion "material change inside retained pass is rejected" (case changed of Left (Resource.GeometryUnavailable _) -> True; _ -> False)
    switched <- try (Resource.withMeshPass material program (Resource.referenceMeshesForCheck material True)) :: IO (Either Resource.GraphicsError ())
    assertion "diagnostic override cannot change during retained submission" (case switched of Left (Resource.GeometryUnavailable _) -> True; _ -> False)
    failed <- try (Resource.withMeshPass material program (Resource.drawMesh material mesh >> throwIO (userError "deliberate draw failure"))) :: IO (Either IOException ())
    active <- Resource.meshPassActive material
    assertion "failed draw retires retained pass state" (either (const True) (const False) failed && not active)
    state <- mapM (`GL.integers` 1) [0x8b8d,0x85b5,0x8894,0x8895,0x84e0,0x0b44]
    assertion "failed draw restores shader, VAO, buffers, texture slot and culling" (state == [[0],[0],[0],[0],[0x84c0],[1]])
    Resource.textureMode surface $ do
      R.clearBackground (Color 255 0 255 255)
      S.drawRectangle 0 0 64 64 Resource.white
    ui <- surfacePixels surface
    assertion "raylib UI draws after retained failure" (BS.take 4 ui == BS.pack [255,255,255,255])
    assertion "retained and fallback checks leave no GL error" =<< ((==0) <$> GL.getError)
  forM_ [("journey","land.vs","land.fs","uTime",True), ("journey","water.vs","water.fs","uReflection",True), ("nature","land.vs","land.fs","uTime",False), ("nature","water.vs","water.fs","uReflection",False)] $ \(folder,vs,fs,required,expected) -> do
    let source file = BS.readFile ("assets/rest/" ++ folder ++ "/" ++ file)
    common <- source "common.glsl"
    vertex <- source vs
    fragmentSource <- source fs
    let shared = if vs=="land.vs" then common<>"\n"<>BS8.pack Grass.grassShapeGLSL else common
    selected <- Resource.loadProgram scope shared (Just vertex) fragmentSource required
    Resource.withMeshPass material selected $ do
      active <- Resource.meshPassActive material
      assertion (folder ++ "/" ++ vs ++ " selects intended production submission") (active == expected)

main :: IO ()
main = do
  scopeChecks
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowHidden,WindowUndecorated,WindowAlwaysRun]
  bracket (R.initWindow 64 64 "Stillroom migration native check") (R.closeWindow . Just) $ \_ -> bracket Resource.newScope Resource.closeScope $ \templateScope -> do
    template <- Resource.loadGrassTemplate templateScope
    targetChecks
    fixed <- either (fail . show) pure (G.fixedSceneGeometry G.BirchLake)
    comparePlane "fixed" 260 224 100 0 (-88) (G.waterMesh fixed)
    forM_ [(G.NearChunk,0,0),(G.FarChunk,0,0)] $ \(detail,cx,cz) -> do
      geometry <- either (fail . show) pure (G.seededChunkGeometry W.firstSeed cx cz detail)
      let extent = fromIntegral (G.chunkSpan detail)
      assertion "selected reference chunk includes water" (G.meshVertexCount (G.waterMesh geometry)>0)
      comparePlane (show detail) extent extent 16 (fromIntegral cx*extent+extent*0.5) (fromIntegral cz*extent+extent*0.5) (G.waterMesh geometry)
      if detail == G.NearChunk then do
        indexedDraw (G.waterMesh geometry)
        retainedChecks (G.waterMesh geometry)
        bracket (Resource.uploadGeometry template geometry) Resource.releaseGeometry $ \gpu -> do
          assertion "default upload retains one compact instance per authored blade" (Resource.grassInstanceCount gpu==G.grassCount geometry && Resource.grassInstanceCount gpu>0)
          referenceTemplate <- Resource.loadGrassTemplateExpandedForCheck templateScope
          bracket (Resource.uploadGeometry referenceTemplate geometry) Resource.releaseGeometry $ \reference -> do
            assertion "expanded reference is explicit and preserves logical triangle/blade counts"
              (Resource.grassInstanceCount reference==0 && Resource.gpuGrass reference==Resource.gpuGrass gpu && Resource.geometryTriangles reference==Resource.geometryTriangles gpu)
      else pure ()
    small <- either (fail . show) pure (G.seededChunkGeometry (W.seed 1) 0 0 G.FarChunk)
    bracket (Resource.uploadGeometry template small) Resource.releaseGeometry $ \gpu ->
      assertion "ordinary upload omits diagnostic fingerprints" (Resource.gpuSignature gpu == Nothing)
    forM_ [1..8 :: Int] $ \_ -> do
      gpu <- Resource.uploadGeometryWithSignature template (Just (G.geometrySignature small)) small
      assertion "diagnostic upload keeps its CPU signature" (Resource.gpuSignature gpu == Just (G.geometrySignature small))
      Resource.releaseGeometry gpu
      Resource.releaseGeometry gpu
      GL.finish
      clean <- (==0) <$> GL.getError
      assertion "repeated acquire/release leaves no GL error" clean
    bracket Resource.newScope Resource.closeScope $ \scope -> do
      failed <- try (Resource.loadProgram scope "" Nothing "deliberately invalid GLSL" "missing") :: IO (Either Resource.GraphicsError Resource.Program)
      assertion "shader failure reports a typed error and keeps the context usable" (case failed of Left (Resource.ShaderUnavailable _) -> True; _ -> False)
    GL.clearErrors
    bracket (Resource.uploadGeometry template small) Resource.releaseGeometry $ \_ -> pure ()
    assertion "GPU allocation succeeds after shader failure" =<< ((==0) <$> GL.getError)
