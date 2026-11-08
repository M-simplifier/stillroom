module Main (main) where

import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Vector.Storable qualified as V
import GameIO.Graphics qualified as GPU
import GameIO.Graphics.Shader
import Stillroom.Journey.World qualified as Seed
import Stillroom.Landscape.Geometry qualified as Land
import Stillroom.Landscape.Grass qualified as Grass
import Stillroom.Runtime.Graphics.CheckIO
import Stillroom.Runtime.Graphics.Scene
import Stillroom.Runtime.Graphics.WorldGeometry qualified as Geometry
import Stillroom.Runtime.Graphics.WorldShaders

main :: IO ()
main = do
  scopeChecks
  withHiddenRenderer "Stillroom Vulkan migration check" 180 120 $ \_ renderer -> do
    geometryChecks renderer
    grassCheck renderer
    targetCheck renderer

assertion :: String -> Bool -> IO ()
assertion name passed = do
  putStrLn (name ++ " " ++ show passed)
  unless passed (fail name)

scopeChecks :: IO ()
scopeChecks = do
  events <- newIORef ([] :: [Int])
  scope <- newScope
  forM_ [1,2,3] $ \n -> own scope (pure n) (\value -> modifyIORef' events (value:))
  closeScope scope
  closeScope scope
  released <- readIORef events
  assertion "LIFO cleanup executes exactly once" (released == [1,2,3])
  failedScope <- newScope
  _ <- own failedScope (pure ()) (\() -> modifyIORef' events (9:))
  failed <- try (own failedScope (throwIO (userError "injected acquisition failure"))
    (\() -> modifyIORef' events (99:))) :: IO (Either IOException ())
  closeScope failedScope
  after <- readIORef events
  assertion "acquisition failure leaves prior cleanup in scope" (either (const True) (const False) failed && after == [9,1,2,3])

geometryChecks :: GPU.Renderer -> IO ()
geometryChecks renderer = do
  near <- either (fail . show) pure (Land.seededChunkGeometry Seed.firstSeed 0 0 Land.NearChunk)
  let water = Land.waterMesh near
  assertion "indexed water fixture omits optional extra and color streams"
    (Land.meshVertexCount water > 0 && not (V.null (Land.meshIndices water)) &&
      V.null (Land.meshExtra water) && V.null (Land.meshColors water))
  let anchor = Geometry.Anchor 0 0
  bracket (Geometry.uploadGeometry renderer anchor False near) Geometry.releaseGeometry $ \resident -> do
    assertion "authored grass is retained as one compact instance per blade"
      (Geometry.instanceCount resident == V.length (Land.grassBlades near) && Geometry.instanceCount resident > 0 && GPU.instanceStride == 40)
    assertion "normal upload omits fingerprints" (Geometry.signature resident == Nothing)
    plane <- maybe (fail "water did not upload") (pure . fst) (Geometry.waterMesh resident)
    camera <- either (fail . show) pure (GPU.lookAtMat4 (GPU.Vec3 16 40 16) (GPU.Vec3 16 0 16) (GPU.Vec3 0 0 (-1)))
    projection <- either (fail . show) pure (GPU.orthographicMat4 32 1.5 0.25 100)
    pixels <- capture renderer [] [GPU.WorldDraw plane Nothing (GPU.multiplyMat4 projection camera)] []
    assertion "indexed optional-stream water fills the framebuffer" (drawnPixels pixels > 2000)
  far <- either (fail . show) pure (Land.seededChunkGeometry (Seed.seed 1) 0 0 Land.FarChunk)
  forM_ [1..3 :: Int] $ \_ -> do
    uploaded <- Geometry.uploadGeometry renderer anchor True far
    assertion "diagnostic resident preserves its authored signature" (Geometry.signature uploaded == Just (Land.geometrySignature far))
    Geometry.releaseGeometry uploaded
    Geometry.releaseGeometry uploaded

grassCheck :: GPU.Renderer -> IO ()
grassCheck renderer = withScope $ \scope -> do
  let blades = [Grass.grassBlade 0 (-0.48,-0.39,0.28) 0.65 0.13 0.26 0.08 (31,143,227,255),
        Grass.grassBlade 15 (0.31,-0.25,0.53) 0.56 0.085 (-0.38) (-0.044) (201,67,42,255)]
  let expanded = [lowerCorner (Grass.grassVertex Grass.FixedGrass blade corner) |
        blade <- blades, corner <- V.toList Grass.grassCorners]
  meshData <- either (fail . show) pure (GPU.meshData expanded [0..fromIntegral (length expanded)-1])
  mesh <- own scope (GPU.createMesh renderer meshData) (GPU.destroyMesh renderer)
  template <- Geometry.grassTemplate renderer scope
  let instanceRecord blade =
        let (x,y,z) = Grass.bladeRoot blade
            (sx,sz) = Grass.bladeSide blade
        in GPU.InstanceData (GPU.Vec4 x y z (Grass.bladeHeight blade))
             (GPU.Vec4 (Grass.bladeWidth blade) sx sz (Grass.bladeCurve blade))
             (Grass.bladeColor blade) (Grass.bladeVertexOffset blade)
  records <- own scope (GPU.createInstances renderer (map instanceRecord blades)) (GPU.destroyInstances renderer)
  let material instanced = own scope (GPU.createMaterial renderer
        (GPU.MaterialConfig GPU.WorldDepth (landVertex FixedWorld Blades instanced) (FragmentShader InterpolatedColor)))
        (GPU.destroyMaterial renderer)
  instanced <- material True
  reference <- material False
  let inputs = GPU.ShaderInputs [] (worldParameters parameters)
  actual <- capture renderer [] [GPU.WorldInstances template records instanced inputs GPU.identityMat4] []
  expected <- capture renderer [] [GPU.WorldDrawWith mesh reference inputs GPU.identityMat4] []
  assertion "instanced GPU grass matches CPU-expanded position, tint and coverage"
    (GPU.pixelRGBA8 actual == GPU.pixelRGBA8 expected && drawnPixels actual > 200)
  where
    unorm byte = fromIntegral byte/255
    lowerCorner v = case (Grass.grassPosition v, Grass.grassNormal v, Grass.grassTexCoord v, Grass.grassExtra v, Grass.grassVertexColor v) of
      ((x,y,z),(nx,ny,nz),(u,w),(a,b),(r,g,blue,alpha)) ->
        GPU.VertexWithDetails (GPU.Vec3 x y z) (GPU.Vec2 u w) (GPU.Color (unorm r) (unorm g) (unorm blue) (unorm alpha))
          (GPU.Vec3 nx ny nz) (GPU.Vec2 a b)
    parameters = WorldParameters
      {worldClock = 1.37, worldQuality = 1, worldLandscape = 0, worldClip = False
      , worldEye = GPU.Vec3 0 0 0, worldFar = False, worldSun = GPU.Vec3 0 1 0, worldRasterHeight = 120
      , worldAnchor = GPU.Vec3 0 0 0, worldBoneRows = 1, worldOffset = GPU.Vec3 0 0 0, worldReference = GPU.Vec3 0 0 0
      , worldShadowUnits = GPU.Vec4 0 0 0 0, worldShadowWidth = 1, worldReflectionWidth = 1, worldReflectionHeight = 1
      , worldHeightRect = GPU.Vec4 0 0 0 0, worldLightMatrix = GPU.identityMat4
      , worldReflectionMatrix = GPU.identityMat4, worldObjectMatrix = GPU.identityMat4}

targetCheck :: GPU.Renderer -> IO ()
targetCheck renderer = bracket (GPU.createRenderTarget renderer GPU.RGBA8 32 20) (GPU.destroyRenderTarget renderer) $ \target -> do
  let color = GPU.Color (61/255) (103/255) (177/255) 1
  GPU.renderOffscreen renderer [GPU.RenderPass target (GPU.Frame color [] [])]
  rejected <- try (GPU.createRenderTarget renderer GPU.RGBA8 0 20)
  assertion "invalid target has a typed size error" $ case rejected of
    Left (GPU.InvalidTargetSize 0 20) -> True
    _ -> False
  let overlay = textured (GPU.renderTargetTexture target) (wholeSurface 180 120) (GPU.Vec4 0 0 1 1)
  pixels <- capture renderer [] [] [overlay]
  assertion "previous completed offscreen version survives rejected allocation"
    (BS.take 4 (GPU.pixelRGBA8 pixels) == BS.pack [61,103,177,255])

capture :: GPU.Renderer -> [GPU.RenderPass] -> [GPU.WorldDraw] -> [GPU.OverlayBatch] -> IO GPU.FramePixels
capture renderer passes world overlay = do
  (status, pixels) <- GPU.renderPassesReadback renderer passes (GPU.Frame (GPU.Color 0 0 0 1) world overlay)
  case (status, pixels) of
    (GPU.FramePresented, Just image) -> pure image
    _ -> fail ("Vulkan readback " ++ show status)

drawnPixels :: GPU.FramePixels -> Int
drawnPixels = length . filter (/=0) . everyPixel . BS.unpack . GPU.pixelRGBA8
  where
    everyPixel (red:_:_:_:rest) = red : everyPixel rest
    everyPixel _ = []
