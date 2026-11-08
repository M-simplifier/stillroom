{-# LANGUAGE OverloadedStrings #-}

module Stillroom.Runtime.Graphics.Journey
  ( World, FingerprintMode(..), openWithFingerprints, close, ready, problem, quality
  , prepare, frameTargets, render, executedFrame, clipProjectionError, signature, farSignature, info
  , setAllocationLimitsForCheck
  ) where

import Control.Exception (bracketOnError, evaluate, finally, mask_, throwIO)
import Control.Monad (forM_, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.IntMap.Strict qualified as Map
import Data.List (find, minimumBy)
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Data.Word (Word64)
import GameIO.Graphics qualified as GPU
import GHC.Clock (getMonotonicTimeNSec)
import Stillroom.Graphics qualified as G
import Stillroom.Graphics.Encoding.Mesh qualified as Mesh
import Stillroom.Graphics.Observation qualified as O
import Stillroom.Journey.Frame qualified as F
import Stillroom.Journey.World qualified as Seed
import Stillroom.Landscape.Geometry qualified as Land
import Stillroom.Runtime.Graphics.Scene
import Stillroom.Runtime.Graphics.WorldGeometry qualified as Geometry
import Stillroom.Runtime.Graphics.WorldRenderer qualified as Render
import Stillroom.Runtime.Graphics.WorldShaders (WorldStyle(JourneyWorld))

data FingerprintMode = SkipFingerprints | CaptureFingerprints deriving (Eq, Show)

data Chunk = Chunk !Int !Int !Geometry.Geometry

data Counts = Counts
  { generated :: !Int, evicted :: !Int, farGenerated :: !Int, farEvicted :: !Int
  , frames :: !Int, lastGenerationMs :: !Double, maxGenerationMs :: !Double
  , cpuGenerationMs :: !Double, uploadMs :: !Double, streamMs :: !Double
  , nearMissing :: !Bool
  }

data World = World
  { gpu :: !GPU.Renderer
  , renderer :: !Render.WorldRenderer
  , seed :: !Seed.Seed
  , car :: !GPU.Mesh
  , carTriangles :: !Int
  , nearChunks :: !(IORef (Map.IntMap Chunk))
  , farChunks :: !(IORef (Map.IntMap Chunk))
  , counts :: !(IORef Counts)
  , desired :: !(IORef G.Tier)
  , issue :: !(IORef (Maybe String))
  , executed :: !(IORef (Maybe F.FramePlan))
  , fingerprintMode :: !FingerprintMode
  , scope :: !Scope
  }

openWithFingerprints :: GPU.Renderer -> FingerprintMode -> Seed.Seed -> Mesh.MeshPacket -> IO World
openWithFingerprints device fingerprints worldSeed carMesh = bracketOnError newScope closeScope $ \resources -> do
  native <- own resources (Render.openWorldRenderer device JourneyWorld) Render.closeWorldRenderer
  railcar <- Geometry.uploadCar device resources carMesh
  World device native worldSeed railcar (Mesh.meshVertexCount carMesh `div` 3)
    <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef (Counts 0 0 0 0 0 0 0 0 0 0 False)
    <*> newIORef G.Balanced <*> newIORef Nothing <*> newIORef Nothing <*> pure fingerprints <*> pure resources

close :: World -> IO ()
close world = mask_ $ do
  near <- atomicModifyIORef' (nearChunks world) (\m -> (Map.empty, m))
  far <- atomicModifyIORef' (farChunks world) (\m -> (Map.empty, m))
  let release (Chunk _ _ geometry) = Geometry.releaseGeometry geometry
  foldr (finally . release) (closeScope (scope world)) (Map.elems near ++ Map.elems far)

ready :: World -> IO Bool
ready world = (== Nothing) <$> readIORef (issue world)

problem :: World -> IO String
problem world = maybe "" id <$> readIORef (issue world)

quality :: World -> G.Tier -> IO ()
quality world = writeIORef (desired world)

setAllocationLimitsForCheck :: World -> Int -> Int -> IO ()
setAllocationLimitsForCheck = Render.setAllocationLimitsForCheck . renderer

chunkAt :: Int -> Int -> Map.IntMap Chunk -> Maybe Chunk
chunkAt x z = find (\(Chunk cx cz _) -> cx == x && cz == z) . Map.elems

signature, farSignature :: World -> Int -> Int -> IO (Maybe Word64)
signature world x z = chunkSignature world x z (nearChunks world)
farSignature world x z = chunkSignature world x z (farChunks world)

chunkSignature :: World -> Int -> Int -> IORef (Map.IntMap Chunk) -> IO (Maybe Word64)
chunkSignature world x z cache = do
  when (fingerprintMode world /= CaptureFingerprints) (throwIO (userError "Geometry fingerprints were not requested"))
  found <- chunkAt x z <$> readIORef cache
  pure (found >>= \(Chunk _ _ geometry) -> Geometry.signature geometry)

-- Keep the original stable slot order and nearest-first streaming. Each new
-- immutable chunk is resident before the corresponding old one is released.
streamOne :: World -> Bool -> Double -> Double -> IO Bool
streamOne world far x z = mask_ $ do
  existing <- readIORef cache
  let centerX = floor (x / fromIntegral extent)
  let centerZ = floor (z / fromIntegral extent)
  let outside (_, Chunk a b _) = abs (a-centerX) > radius || abs (b-centerZ) > radius
  let missing = [(centerX+i, centerZ+j, i*i+j*j) | j <- [-radius..radius], i <- [-radius..radius],
        case chunkAt (centerX+i) (centerZ+j) existing of Nothing -> True; Just _ -> False]
  let slot = case find (`Map.notMember` existing) [0..capacity-1] of
        Just index -> Just index
        Nothing -> fst <$> find outside (Map.toAscList existing)
  case missing of
    [] -> pure True
    _ -> case slot of
      Nothing -> writeIORef (issue world) (Just "No bounded journey stream slot") >> pure False
      Just index -> do
        let (cx,cz,_) = minimumBy (comparing (\(_,_,d) -> d)) missing
        let detail = if far then Land.FarChunk else Land.NearChunk
        let anchor = Geometry.Anchor (fromIntegral cx*fromIntegral extent) (fromIntegral cz*fromIntegral extent)
        started <- getMonotonicTimeNSec
        case Land.seededChunkGeometry (seed world) cx cz detail of
          Left failure -> writeIORef (issue world) (Just (show failure)) >> pure False
          Right source -> do
            generatedGeometry <- evaluate source
            cpu <- getMonotonicTimeNSec
            resident <- Geometry.uploadGeometry (gpu world) anchor (fingerprintMode world == CaptureFingerprints) generatedGeometry
            uploaded <- getMonotonicTimeNSec
            writeIORef cache (Map.insert index (Chunk cx cz resident) existing)
            forM_ (Map.lookup index existing) (\(Chunk _ _ old) -> Geometry.releaseGeometry old)
            ended <- getMonotonicTimeNSec
            let elapsed = milliseconds (ended-started)
            let replaced = if Map.member index existing then 1 else 0
            modifyIORef' (counts world) $ \s ->
              let timed = s {lastGenerationMs = elapsed, maxGenerationMs = max elapsed (maxGenerationMs s),
                    cpuGenerationMs = cpuGenerationMs s + milliseconds (cpu-started),
                    uploadMs = uploadMs s + milliseconds (uploaded-cpu), streamMs = streamMs s + elapsed}
              in if far then timed {farGenerated = farGenerated s+1, farEvicted = farEvicted s+replaced}
                 else timed {generated = generated s+1, evicted = evicted s+replaced}
            pure True
  where
    cache = if far then farChunks world else nearChunks world
    extent = if far then 128 else 32 :: Int
    radius = if far then 4 else 3 :: Int
    capacity = if far then 81 else 49
    milliseconds ns = fromIntegral ns / 1000000

stream :: World -> Double -> Double -> IO Bool
stream world x z = do
  near <- streamOne world False x z
  if near then streamOne world True x z else pure False

prepare :: World -> Double -> Double -> IO Int
prepare world x z = do
  healthy <- ready world
  streamed <- if healthy then stream world x z else pure False
  if streamed then do
    near <- Map.size <$> readIORef (nearChunks world)
    far <- Map.size <$> readIORef (farChunks world)
    pure (130-near-far)
  else pure (-1)

frameTargets :: World -> Double -> Double -> Int -> Int -> IO (Maybe F.Targets)
frameTargets world x z width height = do
  healthy <- ready world
  if not healthy || width < 1 || height < 1 then pure Nothing else do
    streamed <- stream world x z
    near <- readIORef (nearChunks world)
    let current = chunkAt (floor (x/32)) (floor (z/32)) near
    modifyIORef' (counts world) (\countsValue -> countsValue {nearMissing = isNothing current})
    case current of
      Just _ | streamed -> do
        tier <- readIORef (desired world)
        allocated <- Render.prepareTargets (renderer world) tier False width height
        when (isNothing allocated) (writeIORef (issue world) (Just "Journey render targets unavailable"))
        pure (Render.targets <$> allocated)
      _ -> pure Nothing

render :: World -> F.FramePlan -> SceneRect -> IO SceneDraw
render world frame rectangle = do
  images <- Render.currentTargets (renderer world)
  case images of
    Just targets | Render.targets targets == F.targets frame -> do
      far <- Map.elems <$> readIORef (farChunks world)
      near <- Map.elems <$> readIORef (nearChunks world)
      let context = F.geometry frame
      let reference@(F.Point rx _ rz) = F.referenceEye context
      let origin = F.renderOrigin context
      let units = GPU.Vec4 (realToFrac (F.shadowTexel frame)) (realToFrac F.shadowDepthSpan)
            (realToFrac F.shadowBaseBias) (realToFrac F.shadowSlopeBias)
      let chooseFar (Chunk x z geometry) =
            let dx = max 0 (abs (rx-(fromIntegral x+0.5)*128)-64)
                dz = max 0 (abs (rz-(fromIntegral z+0.5)*128)-64)
            in Render.ChunkDraw geometry True (dx*dx+dz*dz <= 560*560) False False
      let chooseNear (Chunk x z geometry) =
            let dx = rx-(fromIntegral x+0.5)*32
                dz = rz-(fromIntegral z+0.5)*32
                distance = dx*dx+dz*dz
                mx = max 0 (abs dx-16)
                mz = max 0 (abs dz-16)
            in Render.ChunkDraw geometry False (distance <= 132*132) (distance <= 112*112) (mx*mx+mz*mz < 2500)
      let worldFrame = Render.WorldFrame 0 (F.semanticTime context) (F.sunDirection frame) origin
            (F.relative origin reference) (F.shadowPass frame) (F.reflectionPass frame) (F.mainPass frame)
            units (map chooseFar far ++ map chooseNear near) (Just (car world, F.carTransform frame))
      let draw = Render.worldDraw (renderer world) targets worldFrame rectangle
      pure draw {scenePresented = \status -> when (status == GPU.FramePresented) $ do
        writeIORef (executed world) (Just frame)
        modifyIORef' (counts world) (\s -> s {frames = frames s+1})}
    _ -> writeIORef (issue world) (Just "Journey observations do not match the allocated images") >> pure mempty

executedFrame :: World -> IO (Maybe F.FramePlan)
executedFrame = readIORef . executed

-- Independent clip-coordinate reference over asymmetric points in the already
-- submitted observations. Actual fragment and shadow placement is checked by
-- framebuffer captures, separately from this lowering error.
clipProjectionError :: F.FramePlan -> Float
clipProjectionError frame = maximum
  [maximum (zipWith (\observed reference -> abs (observed-reference)) gpuClip expected) |
    (pass, observation) <- [(F.Main,F.mainPass frame),(F.Shadow,F.shadowPass frame),(F.Reflection,F.reflectionPass frame)],
    point@(F.Point x y z) <- [F.Point 0.71 1.62 (-2.37), F.Point (-15.15) 3.31 19.27, F.Point 68.7 (-5.8) (-41.2)],
    let (cx,cy,cz,cw) = F.clipPoint frame pass point,
    let expected = map realToFrac [cx,-cy,(cz+cw)*0.5,cw],
    let GPU.Vec4 a b c d = GPU.transformVec4 (Render.projectClip (O.clipFromRender observation))
          (GPU.Vec4 (realToFrac x) (realToFrac y) (realToFrac z) 1),
    let gpuClip = [a,b,c,d]]

info :: World -> IO String
info world = do
  near <- map chunkGeometry . Map.elems <$> readIORef (nearChunks world)
  far <- map chunkGeometry . Map.elems <$> readIORef (farChunks world)
  s <- readIORef (counts world)
  images <- Render.currentTargets (renderer world)
  metrics <- GPU.rendererLastMetrics (gpu world)
  let measure view = maybe 0 (view . Render.targets) images
  let instances = sum (map Geometry.instanceCount near)
  pure $ BL.unpack $ encode $ object
    ["quality" .= maybe (-1) (fromEnum . Render.selectedTier) images,
     "rasterWidth" .= measure F.rasterWidth, "rasterHeight" .= measure F.rasterHeight,
     "shadowWidth" .= measure F.shadowResolution, "reflectionWidth" .= measure F.reflectionWidth,
     "farResident" .= length far, "farCapacity" .= (81::Int), "farTriangles" .= sum (map Geometry.triangleCount far),
     "farGenerated" .= farGenerated s, "farEvicted" .= farEvicted s, "resident" .= length near, "capacity" .= (49::Int),
     "trees" .= sum (map Geometry.treeCount near), "blades" .= sum (map Geometry.grassCount near),
     "grassRenderingPath" .= ("instanced"::String), "grassInstances" .= instances,
     "grassInstanceBytes" .= (GPU.instanceStride*instances), "grassSharedCornerBytes" .= (GPU.cornerStride*15),
     "triangles" .= sum (map Geometry.triangleCount near), "carTriangles" .= carTriangles world,
     "generated" .= generated s, "evicted" .= evicted s, "frames" .= frames s,
     "maxGenerationMs" .= maxGenerationMs s, "lastGenerationMs" .= lastGenerationMs s,
     "cpuGenerationMs" .= cpuGenerationMs s, "uploadMs" .= uploadMs s, "streamMs" .= streamMs s,
     "geometryFingerprints" .= (fingerprintMode world == CaptureFingerprints),
     "nearMissing" .= (if nearMissing s then 1 else 0 :: Int),
     "gpuMs" .= (fmap (/1000000) (metrics >>= GPU.frameGPUNSec)),
     "depthFormat" .= ("D32_SFLOAT"::String), "depthBits" .= (32::Int), "renderer" .= GPU.rendererAdapterName (gpu world)]
  where
    chunkGeometry (Chunk _ _ geometry) = geometry
