{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Monad (forM_, unless)
import Data.IORef
import Data.Vector.Storable qualified as V
import Data.Word (Word16)
import Foreign
import Foreign.C (CUShort)
import Raylib.Core qualified as R
import Raylib.Core.Models qualified as M
import Raylib.Core.Textures qualified as T
import Raylib.Types
import Raylib.Util.RLGL qualified as RL
import Stillroom.Journey.World qualified as W
import Stillroom.Landscape.Geometry qualified as G
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

main :: IO ()
main = do
  scopeChecks
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowHidden,WindowUndecorated,WindowAlwaysRun]
  bracket (R.initWindow 64 64 "Stillroom migration native check") (R.closeWindow . Just) $ \_ -> do
    targetChecks
    fixed <- either (fail . show) pure (G.fixedSceneGeometry G.BirchLake)
    comparePlane "fixed" 260 224 100 0 (-88) (G.waterMesh fixed)
    forM_ [(G.NearChunk,0,0),(G.FarChunk,0,0)] $ \(detail,cx,cz) -> do
      geometry <- either (fail . show) pure (G.seededChunkGeometry W.firstSeed cx cz detail)
      let span = fromIntegral (G.chunkSpan detail)
      assertion "selected reference chunk includes water" (G.meshVertexCount (G.waterMesh geometry)>0)
      comparePlane (show detail) span span 16 (fromIntegral cx*span+span*0.5) (fromIntegral cz*span+span*0.5) (G.waterMesh geometry)
      if detail == G.NearChunk then indexedDraw (G.waterMesh geometry) else pure ()
    small <- either (fail . show) pure (G.seededChunkGeometry (W.seed 1) 0 0 G.FarChunk)
    bracket (Resource.uploadGeometry small) Resource.releaseGeometry $ \gpu ->
      assertion "ordinary upload omits diagnostic fingerprints" (Resource.gpuSignature gpu == Nothing)
    forM_ [1..8 :: Int] $ \_ -> do
      gpu <- Resource.uploadGeometryWithSignature (Just (G.geometrySignature small)) small
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
    bracket (Resource.uploadGeometry small) Resource.releaseGeometry $ \_ -> pure ()
    assertion "GPU allocation succeeds after shader failure" =<< ((==0) <$> GL.getError)
