{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Graphics.Journey
  ( World,
    open,
    close,
    ready,
    problem,
    quality,
    prepare,
    frameTargets,
    render,
    frameCopy,
    frameUniforms,
    signature,
    farSignature,
    info,
  )
where

import Control.Exception
import Control.Monad (forM_, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.IntMap.Strict qualified as Map
import Data.List (find, minimumBy)
import Data.Ord (comparing)
import Data.Vector.Storable qualified as V
import Data.Word (Word64)
import Raylib.Core qualified as R
import Raylib.Types
import Raylib.Util.RLGL qualified as RL
import Stillroom.Graphics.Encoding.Mesh qualified as Mesh
import Stillroom.Graphics.Observation qualified as O
import Stillroom.Journey.Frame qualified as F
import Stillroom.Journey.World qualified as Seed
import Stillroom.Landscape.Geometry qualified as G
import Stillroom.Runtime.Graphics.Encoding qualified as Encoding
import Stillroom.Runtime.Graphics.GL qualified as GL
import Stillroom.Runtime.Graphics.Renderer
import Stillroom.Runtime.Graphics.Resource
import Stillroom.Runtime.Graphics.Target qualified as Target
import Stillroom.Runtime.Journey.Packet qualified as Packet

data Chunk = Chunk !Int !Int !GpuGeometry

data Counts = Counts
  { generated :: !Int,
    evicted :: !Int,
    farGenerated :: !Int,
    farEvicted :: !Int,
    frames :: !Int,
    lastGenerationMs :: !Double,
    maxGenerationMs :: !Double,
    nearMissing :: !Bool,
    cpuGenerationMs :: !Double,
    uploadMs :: !Double,
    streamMs :: !Double,
    submitMs :: !Double
  }

data World = World
  { renderer :: !Renderer,
    seed :: !Seed.Seed,
    car :: !GpuMesh,
    nearChunks :: !(IORef (Map.IntMap Chunk)),
    farChunks :: !(IORef (Map.IntMap Chunk)),
    counts :: !(IORef Counts),
    executed :: !(IORef (Maybe F.FramePlan)),
    worldScope :: !Scope
  }

open :: Seed.Seed -> [BS.ByteString] -> Mesh.MeshPacket -> IO World
open worldSeed sources carMesh = bracketOnError newScope closeScope $ \scope -> do
  r <- own scope (loadRenderer sources) releaseRenderer
  railcar <- uploadRailcar scope carMesh
  near <- newIORef Map.empty
  far <- newIORef Map.empty
  statistics <- newIORef emptyCounts
  frame <- newIORef Nothing
  pure
    World
      { renderer = r,
        seed = worldSeed,
        car = railcar,
        nearChunks = near,
        farChunks = far,
        counts = statistics,
        executed = frame,
        worldScope = scope
      }

emptyCounts :: Counts
emptyCounts =
  Counts
    { generated = 0,
      evicted = 0,
      farGenerated = 0,
      farEvicted = 0,
      frames = 0,
      lastGenerationMs = 0,
      maxGenerationMs = 0,
      nearMissing = False,
      cpuGenerationMs = 0,
      uploadMs = 0,
      streamMs = 0,
      submitMs = 0
    }

close :: World -> IO ()
close world = mask_ $ do
  near <- atomicModifyIORef' (nearChunks world) (\m -> (Map.empty, m))
  far <- atomicModifyIORef' (farChunks world) (\m -> (Map.empty, m))
  let release (Chunk _ _ geometry) = releaseGeometry geometry
  mapM_ release near `finally` (mapM_ release far `finally` closeScope (worldScope world))

ready :: World -> IO Bool
ready = healthy . renderer

problem :: World -> IO String
problem world = maybe "" id <$> readIORef (issue (renderer world))

quality :: World -> Int -> IO Bool
quality world = setQuality (renderer world)

chunkAt :: Int -> Int -> Map.IntMap Chunk -> Maybe Chunk
chunkAt x z = find (\(Chunk chunkX chunkZ _) -> chunkX == x && chunkZ == z) . Map.elems

signature :: World -> Int -> Int -> IO Word64
signature world x z = maybe 0 (\(Chunk _ _ g) -> gpuSignature g) . chunkAt x z <$> readIORef (nearChunks world)

farSignature :: World -> Int -> Int -> IO Word64
farSignature world x z = maybe 0 (\(Chunk _ _ g) -> gpuSignature g) . chunkAt x z <$> readIORef (farChunks world)

-- A replacement is generated and uploaded before the previous slot is retired.
-- The two tables remain bounded at 49 near and 81 far geometries.
streamOne :: World -> Bool -> Double -> Double -> IO Bool
streamOne world far x z = mask_ $ do
  existing <- readIORef ref
  let centerX = floor (x / fromIntegral extent)
      centerZ = floor (z / fromIntegral extent)
      missing =
        [ (centerX + i, centerZ + j, i * i + j * j)
          | j <- [-radius .. radius],
            i <- [-radius .. radius],
            case chunkAt (centerX + i) (centerZ + j) existing of
              Nothing -> True
              Just _ -> False
        ]
      outside (_, Chunk a b _) = abs (a - centerX) > radius || abs (b - centerZ) > radius
      vacant = find (`Map.notMember` existing) [0 .. capacity - 1]
      slot = case vacant of
        Just index -> Just index
        Nothing -> fst <$> find outside (Map.toAscList existing)
  case missing of
    [] -> pure True
    _ -> case slot of
      Nothing -> failRenderer (renderer world) "No bounded stream slot" >> pure False
      Just index -> do
        let (cx, cz, _) = minimumBy (comparing (\(_, _, d) -> d)) missing
        began <- R.getTime
        built <- try $ do
          geometry <-
            either
              (throwIO . GeometryUnavailable . show)
              evaluate
              (G.seededChunkGeometry (seed world) cx cz (if far then G.FarChunk else G.NearChunk))
          -- Geometry has strict packed buffers and a strict byte signature:
          -- this receipt forces CPU generation before the upload clock starts.
          _ <- evaluate (G.geometrySignature geometry)
          cpuFinished <- R.getTime
          gpu <- uploadGeometry geometry
          uploaded <- R.getTime
          pure (gpu, (cpuFinished - began) * 1000, (uploaded - cpuFinished) * 1000)
        case built of
          Left (e :: GraphicsError) -> failRenderer (renderer world) (show e) >> pure False
          Right (geometry, cpuElapsed, uploadElapsed) -> do
            writeIORef ref (Map.insert index (Chunk cx cz geometry) existing)
            forM_ (Map.lookup index existing) (\(Chunk _ _ old) -> releaseGeometry old)
            ended <- R.getTime
            let elapsed = (ended - began) * 1000
                replaced = if Map.member index existing then 1 else 0
            modifyIORef' (counts world) $ \s ->
              let timed =
                    s
                      { lastGenerationMs = elapsed,
                        maxGenerationMs = max elapsed (maxGenerationMs s),
                        cpuGenerationMs = cpuGenerationMs s + cpuElapsed,
                        uploadMs = uploadMs s + uploadElapsed,
                        streamMs = streamMs s + elapsed
                      }
               in if far
                    then timed {farGenerated = farGenerated s + 1, farEvicted = farEvicted s + replaced}
                    else timed {generated = generated s + 1, evicted = evicted s + replaced}
            pure True
  where
    ref = if far then farChunks world else nearChunks world
    extent = if far then 128 else 32 :: Int
    radius = if far then 4 else 3 :: Int
    capacity = if far then 81 else 49

stream :: World -> Double -> Double -> IO Bool
stream world x z = do
  nearReady <- streamOne world False x z
  if nearReady then streamOne world True x z else pure False

prepare :: World -> Double -> Double -> IO Int
prepare world x z = do
  ok <- ready world
  if not ok
    then pure (-1)
    else do
      streamed <- stream world x z
      if not streamed
        then pure (-1)
        else do
          nearCount <- Map.size <$> readIORef (nearChunks world)
          farCount <- Map.size <$> readIORef (farChunks world)
          pure (130 - nearCount - farCount)

frameTargets :: World -> Double -> Double -> Int -> Int -> IO (Maybe F.Targets)
frameTargets world x z width height = do
  ok <- ready world
  if not ok || width < 1 || height < 1
    then pure Nothing
    else do
      streamed <- stream world x z
      near <- readIORef (nearChunks world)
      let missing = case chunkAt (floor (x / 32)) (floor (z / 32)) near of
            Nothing -> True
            Just _ -> False
      modifyIORef' (counts world) (\s -> s {nearMissing = missing})
      allocated <- if streamed && not missing then Target.budgetTargets (targets (renderer world)) (fromIntegral width) (fromIntegral height) False else pure False
      if not allocated
        then pure Nothing
        else do
          ts <- Target.readTargets (targets (renderer world))
          pure $ case (Target.shadowTarget ts, Target.reflectionTarget ts) of
            (Just shadow, Just reflection) ->
              Just
                ( F.Targets
                    (Target.rasterWidth ts)
                    (Target.rasterHeight ts)
                    (texture'width (renderTexture'depth shadow))
                    (texture'width (renderTexture'texture reflection))
                    (texture'height (renderTexture'texture reflection))
                )
            _ -> Nothing

beginPass :: O.Observation target -> IO ()
beginPass observation = do
  view <- nativeTransform (O.viewMatrix observation)
  projection <- nativeTransform (O.projectionMatrix observation)
  RL.rlDrawRenderBatchActive
  RL.rlMatrixMode RLProjection
  RL.rlPushMatrix
  RL.rlSetMatrixProjection projection
  RL.rlMatrixMode RLModelView
  RL.rlSetMatrixModelview view
  RL.rlEnableDepthTest

pass :: O.Observation target -> IO a -> IO a
pass observation = bracket_ (beginPass observation) R.endMode3D

allLand :: World -> F.FramePlan -> Texture -> Bool -> Bool -> O.Observation target -> IO ()
allLand world frame shadow shadowPass clip observation = do
  let r = renderer world
      p = landProgram r
      F.Point rx _ rz = F.referenceEye (F.geometry frame)
      sun = Encoding.direction (F.sunDirection frame)
      time = realToFrac (F.semanticTime (F.geometry frame))
      eye = Encoding.point (O.passEye observation)
  light <- nativeTransform (O.clipFromRender (F.shadowPass frame))
  prepareWorld r 0 sun light shadow shadowPass clip eye
  let draw visibility (Chunk _ _ geometry) = drawGeometry r geometry time visibility shadowPass
  when (not shadowPass) $ do
    int p "uFarPass" 1
    chunks <- Map.elems <$> readIORef (farChunks world)
    forM_ chunks $ \chunk@(Chunk x z _) -> do
      let dx = max 0 (abs (rx - (fromIntegral x + 0.5) * 128) - 64)
          dz = max 0 (abs (rz - (fromIntegral z + 0.5) * 128) - 64)
      when (dx * dx + dz * dz <= 560 * 560) (draw 3 chunk)
  int p "uFarPass" 0
  chunks <- Map.elems <$> readIORef (nearChunks world)
  forM_ chunks $ \chunk@(Chunk x z _) -> do
    let dx = rx - (fromIntegral x + 0.5) * 32
        dz = rz - (fromIntegral z + 0.5) * 32
        distance = sqrt (dx * dx + dz * dz)
        mx = max 0 (abs dx - 16)
        mz = max 0 (abs dz - 16)
    when (distance <= if shadowPass then 112 else 132) (draw (if mx * mx + mz * mz < 2500 then 7 else 3) chunk)

drawCar :: World -> F.FramePlan -> Texture -> Bool -> Bool -> O.Observation target -> IO ()
drawCar world frame shadow shadowPass clip observation = do
  let r = renderer world
      p = landProgram r
      F.Point cx cy cz = O.passEye observation
      F.Point x y z = F.objectPoint frame (F.Point 0 0 0)
      distance = sqrt ((cx - x) * (cx - x) + (cy - y) * (cy - y) + (cz - z) * (cz - z))
  when (distance <= 540) $ do
    int p "uFarPass" 0
    int p "uKind" 4
    int p "uShadowPass" (if shadowPass then 1 else 0)
    int p "uClip" (if clip then 1 else 0)
    vec3 p "uEye" (Encoding.point (O.passEye observation))
    vec3 p "uSun" (Encoding.direction (F.sunDirection frame))
    matrixTransform p "uLightVP" (O.clipFromRender (F.shadowPass frame))
    matrixTransform p "uObject" (F.carTransform frame)
    texture p "uShadow" shadow 9
    useMaterial (material r) p
    RL.rlDisableBackfaceCulling
    drawMesh (material r) (car world) `finally` RL.rlEnableBackfaceCulling

render :: World -> F.FramePlan -> Rectangle -> IO Bool
render world frame rectangle@(Rectangle _ _ width height) = do
  ok <- ready world
  ts <- Target.readTargets (targets r)
  let planned = F.targets frame
      valid =
        F.rasterWidth planned == Target.rasterWidth ts
          && F.rasterHeight planned == Target.rasterHeight ts
          && F.shadowResolution planned == maybe 0 (texture'width . renderTexture'depth) (Target.shadowTarget ts)
          && F.reflectionWidth planned == maybe 0 (texture'width . renderTexture'texture) (Target.reflectionTarget ts)
          && F.reflectionHeight planned == maybe 0 (texture'height . renderTexture'texture) (Target.reflectionTarget ts)
  if not ok || width < 1 || height < 1
    then pure False
    else
      if not valid
        then failRenderer r "Frame observations do not match allocated targets" >> pure False
        else case (Target.shadowTarget ts, Target.reflectionTarget ts, Target.hdrTarget ts) of
          (Just shadow, Just reflection, Just hdr) -> do
            began <- R.getTime
            bracket saveParent restoreParent $ \_ -> do
              setShaderQuality r (Target.currentQuality ts)
              renderPasses world frame shadow reflection hdr
            composite r hdr rectangle
            submitted <- R.getTime
            writeIORef (executed world) (Just frame)
            modifyIORef' (counts world) (\s -> s {frames = frames s + 1, submitMs = submitMs s + (submitted - began) * 1000})
            e <- GL.getError
            when (e /= 0) (failRenderer r ("OpenGL error " ++ show e))
            ready world
          _ -> failRenderer r "Journey targets unavailable" >> pure False
  where
    r = renderer world

renderPasses :: World -> F.FramePlan -> RenderTexture -> RenderTexture -> RenderTexture -> IO ()
renderPasses world frame shadow reflection hdr = do
  let r = renderer world
      land = landProgram r
      water = waterProgram r
      context = F.geometry frame
      sun = Encoding.direction (F.sunDirection frame)
      shadowImage = renderTexture'depth shadow
      worldPass shadowPass clip observation = allLand world frame shadowImage shadowPass clip observation >> drawCar world frame shadowImage shadowPass clip observation
      main = F.mainPass frame
      light = F.shadowPass frame
      mirror = F.reflectionPass frame
  vec3 land "uReferenceEye" (Encoding.point (F.referenceEye context))
  vec3 land "uOrigin" (Encoding.point (F.renderOrigin context))
  vec3 water "uOrigin" (Encoding.point (F.renderOrigin context))
  vec4 land "uShadowUnits" (Vector4 (realToFrac (F.shadowTexel frame)) (realToFrac F.shadowDepthSpan) (realToFrac F.shadowBaseBias) (realToFrac F.shadowSlopeBias))
  textureMode shadow $ do
    R.clearBackground white
    pass light $ matrixTransform land "uClipFromRender" (O.clipFromRender light) >> worldPass True False light
  textureMode reflection $ do
    R.clearBackground black
    plannedSky r 0 sun mirror
    pass mirror $ matrixTransform land "uClipFromRender" (O.clipFromRender mirror) >> worldPass False True mirror
  textureMode hdr $ do
    R.clearBackground black
    plannedSky r 0 sun main
    pass main $ do
      matrixTransform land "uClipFromRender" (O.clipFromRender main)
      worldPass False False main
      useMaterial (material r) water
      matrixTransform water "uClipFromRender" (O.clipFromRender main)
      vec3 water "uEye" (Encoding.point (O.passEye main))
      vec3 water "uSun" sun
      float water "uTime" (realToFrac (F.semanticTime context))
      int water "uLandscape" 2
      matrixTransform water "uReflectionVP" (O.clipFromRender mirror)
      texture water "uReflection" (renderTexture'texture reflection) 10
      RL.rlDisableBackfaceCulling
      ( forM_ [True, False] $ \far -> do
          int water "uFarPass" (if far then 1 else 0)
          chunks <- Map.elems <$> readIORef (if far then farChunks world else nearChunks world)
          let extent = if far then 128 else 32
              texel = if far then 8 else 1
              F.Point originX _ originZ = F.renderOrigin context
          forM_ chunks $ \(Chunk x z geometry) -> case (gpuWater geometry, gpuHeight geometry) of
            (Just mesh, Just height) -> do
              vec4
                water
                "uHeightRect"
                ( Vector4
                    (realToFrac (fromIntegral x * extent - originX - texel * 0.5))
                    (realToFrac (fromIntegral z * extent - originZ - texel * 0.5))
                    (realToFrac (extent + texel))
                    (realToFrac (extent + texel))
                )
              texture water "uHeight" height 11
              drawMesh (material r) mesh
            _ -> pure ()
        )
        `finally` RL.rlEnableBackfaceCulling

-- Serialization is only a diagnostic view; production execution consumes the
-- typed plan and observations above and has no packet offsets.
frameCopy :: World -> IO (V.Vector Float)
frameCopy world = do
  frame <- readIORef (executed world)
  pure $ case frame >>= either (const Nothing) Just . Packet.lowerFrame of
    Nothing -> V.empty
    Just packet -> Packet.packetValues packet

frameUniforms :: World -> IO (Maybe (V.Vector Float))
frameUniforms world = do
  frame <- readIORef (executed world)
  case frame of
    Nothing -> pure Nothing
    Just _ -> do
      let p = landProgram (renderer world)
      parts <-
        mapM
          (\(name, n) -> uniformLocation p name >>= \loc -> GL.readUniform (programId p) loc n)
          [("uObject", 16), ("uLightVP", 16), ("uClipFromRender", 16), ("uReferenceEye", 3), ("uOrigin", 3)]
      pure (V.fromList . concat <$> sequence parts)

info :: World -> IO String
info world = do
  near <- Map.elems <$> readIORef (nearChunks world)
  far <- Map.elems <$> readIORef (farChunks world)
  s <- readIORef (counts world)
  ts <- Target.readTargets (targets (renderer world))
  receipt <- frameCopy world
  let geometries = map (\(Chunk _ _ g) -> g)
      landTriangles g = sum (map (maybe 0 triangleCount) (gpuMaterials g))
      shadow = maybe 0 (texture'id . renderTexture'depth) (Target.shadowTarget ts)
  depth <- GL.depthDescription shadow
  name <- GL.rendererName
  pure $
    BL.unpack $
      encode $
        object
          [ "quality" .= Target.currentQuality ts,
            "rasterWidth" .= Target.rasterWidth ts,
            "rasterHeight" .= Target.rasterHeight ts,
            "shadowWidth" .= maybe 0 (texture'width . renderTexture'depth) (Target.shadowTarget ts),
            "reflectionWidth" .= maybe 0 (texture'width . renderTexture'texture) (Target.reflectionTarget ts),
            "farResident" .= length far,
            "farCapacity" .= (81 :: Int),
            "farTriangles" .= sum (map landTriangles (geometries far)),
            "farGenerated" .= farGenerated s,
            "farEvicted" .= farEvicted s,
            "resident" .= length near,
            "capacity" .= (49 :: Int),
            "trees" .= sum (map gpuTrees (geometries near)),
            "blades" .= sum (map gpuGrass (geometries near)),
            "triangles" .= sum (map landTriangles (geometries near)),
            "generated" .= generated s,
            "evicted" .= evicted s,
            "frames" .= frames s,
            "maxGenerationMs" .= maxGenerationMs s,
            "lastGenerationMs" .= lastGenerationMs s,
            "cpuGenerationMs" .= cpuGenerationMs s,
            "uploadMs" .= uploadMs s,
            "streamMs" .= streamMs s,
            "submitMs" .= submitMs s,
            "submitBoundary" .= ("parent capture through postprocess submission; excludes EndDrawing/present" :: String),
            "nearMissing" .= (if nearMissing s then 1 else 0 :: Int),
            "depthFormat" .= at 0 depth,
            "depthBits" .= at 1 depth,
            "depthType" .= at 2 depth,
            "carTriangles" .= triangleCount (car world),
            "frameVersion" .= (if V.null receipt then 0 else receipt V.! 0),
            "renderer" .= name
          ]
  where
    at n xs = case drop n xs of
      value : _ -> value
      [] -> 0
