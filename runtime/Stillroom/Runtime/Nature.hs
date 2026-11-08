{-# LANGUAGE OverloadedStrings #-}

module Stillroom.Runtime.Nature
  ( Nature, withNature, available, draw, info, reference, quality, landscapeIndex, setAllocationLimitsForCheck ) where

import Control.Exception (bracket, evaluate, finally, mask_, throwIO)
import Control.Monad (when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IntMap.Strict qualified as Map
import Data.IORef
import GameIO.Graphics qualified as GPU
import GHC.Clock (getMonotonicTimeNSec)
import Stillroom.Domain (RestScene(..))
import Stillroom.Graphics qualified as G
import Stillroom.Graphics.Observation qualified as O
import Stillroom.Journey.Frame qualified as F
import Stillroom.Landscape.Geometry qualified as Land
import Stillroom.Runtime.Graphics.Scene
import Stillroom.Runtime.Graphics.WorldGeometry qualified as Geometry
import Stillroom.Runtime.Graphics.WorldRenderer qualified as World
import Stillroom.Runtime.Graphics.WorldShaders (WorldStyle(FixedWorld))

data Nature = Nature
  { gpu :: !GPU.Renderer
  , world :: !World.WorldRenderer
  , cache :: !(IORef (Map.IntMap Geometry.Geometry))
  , generationMs :: !(IORef (Map.IntMap Double))
  , desired :: !(IORef G.Tier)
  , fullRaster :: !(IORef Bool)
  , active :: !(IORef Int)
  , previous :: !(IORef (Maybe (Int, Integer, Int)))
  , drawCount :: !(IORef Int)
  , healthy :: !(IORef Bool)
  }

withNature :: GPU.Renderer -> (Nature -> IO a) -> IO a
withNature renderer use = bracket (World.openWorldRenderer renderer FixedWorld) World.closeWorldRenderer $ \native ->
  bracket (Nature renderer native <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef G.Balanced
    <*> newIORef False <*> newIORef (-1) <*> newIORef Nothing <*> newIORef 0 <*> newIORef True) release use
  where
    release nature = do
      geometries <- atomicModifyIORef' (cache nature) (\m -> (Map.empty, m))
      foldr (finally . Geometry.releaseGeometry) (pure ()) geometries

available :: Nature -> IO Bool
available = readIORef . healthy

quality :: Nature -> G.Tier -> IO ()
quality nature = writeIORef (desired nature)

reference :: Nature -> Bool -> IO ()
reference nature = writeIORef (fullRaster nature)

setAllocationLimitsForCheck :: Nature -> Int -> Int -> IO ()
setAllocationLimitsForCheck = World.setAllocationLimitsForCheck . world

landscapeIndex :: RestScene -> Maybe Int
landscapeIndex BirchLake = Just 0
landscapeIndex AlpineLake = Just 1
landscapeIndex MeadowStream = Just 2
landscapeIndex _ = Nothing

draw :: Nature -> RestScene -> Double -> SceneRect -> IO SceneDraw
draw nature scene elapsed rect@(SceneRect _ _ width height) = case landscapeIndex scene of
  Nothing -> pure mempty
  Just index
    | any (\v -> isNaN v || isInfinite v) [elapsed, realToFrac width, realToFrac height] || width < 1 || height < 1 -> pure mempty
    | otherwise -> do
      geometry <- geometryFor nature index
      writeIORef (active nature) index
      q <- readIORef (desired nature)
      full <- readIORef (fullRaster nature)
      chosen <- World.prepareTargets (world nature) q full (floor width) (floor height)
      case chosen of
        Nothing -> writeIORef (healthy nature) False >> pure mempty
        Just images -> do
          writeIORef (healthy nature) True
          let tick = floor (max 0 elapsed * 30) :: Integer
          let key = (index, tick, World.targetRevision images)
          frame <- either (throwIO . userError . show) pure (fixedFrame index (fromIntegral tick/30) (World.targets images) geometry)
          let rendered = World.worldDraw (world nature) images frame rect
          old <- readIORef (previous nature)
          if old == Just key then pure rendered {scenePasses = []} else
            pure rendered {scenePresented = \status -> when (status == GPU.FramePresented) $ do
              writeIORef (previous nature) (Just key)
              modifyIORef' (drawCount nature) (+1)}

geometryFor :: Nature -> Int -> IO Geometry.Geometry
geometryFor nature index = mask_ $ do
  existing <- readIORef (cache nature)
  case Map.lookup index existing of
    Just geometry -> pure geometry
    Nothing -> do
      begin <- getMonotonicTimeNSec
      let scene = case index of 1 -> Land.AlpineLake; 2 -> Land.MeadowStream; _ -> Land.BirchLake
      prepared <- either (throwIO . userError . show) evaluate (Land.fixedSceneGeometry scene)
      geometry <- Geometry.uploadGeometry (gpu nature) (Geometry.Anchor 0 0) False prepared
      end <- getMonotonicTimeNSec
      writeIORef (cache nature) (Map.insert index geometry existing)
      modifyIORef' (generationMs nature) (Map.insert index (fromIntegral (end-begin)/1000000))
      pure geometry

fixedFrame :: Int -> Double -> F.Targets -> Geometry.Geometry -> Either O.ObservationError World.WorldFrame
fixedFrame index elapsed targets geometry = do
  let (eye, aim, fov) = case index of
        1 -> (F.Point 1 3 13, F.Point 0 3.5 (-34), 49)
        2 -> (F.Point 0 2.5 10, F.Point 3 1.6 (-35), 52)
        _ -> (F.Point 0.7 2.5 8.3, F.Point 1.3 1.55 (-27), 49)
  let sun = F.unit (F.Direction (-0.58) 0.68 0.45)
  let F.Direction sx sy sz = F.scale 95 sun
  let center = F.Point 0 0 (-25)
  let light = F.Point sx sy (sz-25)
  let mirrorY (F.Point x y z) = F.Point x (-y) z
  let direction (F.Point x y z) (F.Point a b c) = F.Direction (a-x) (b-y) (c-z)
  let camera p target w h = O.observe (F.ImageSize w h) p (direction p target)
        (F.perspective (fov*pi/180) (fromIntegral w/fromIntegral h) 0.01 1000)
  let resolution = F.shadowResolution targets
  shadow <- O.observe (F.ImageSize resolution resolution) light (direction light center) (F.ortho 52.5 0.01 1000)
  reflection <- camera (mirrorY eye) (mirrorY aim) (F.reflectionWidth targets) (F.reflectionHeight targets)
  main <- camera eye aim (F.rasterWidth targets) (F.rasterHeight targets)
  pure (World.WorldFrame index elapsed sun (F.Point 0 0 0) eye shadow reflection main (GPU.Vec4 0 0 0 0)
    [World.ChunkDraw geometry False True True True] Nothing)

info :: Nature -> IO String
info nature = do
  index <- readIORef (active nature)
  geometry <- Map.lookup index <$> readIORef (cache nature)
  images <- World.currentTargets (world nature)
  timings <- readIORef (generationMs nature)
  count <- readIORef (drawCount nature)
  let instances = maybe 0 Geometry.instanceCount geometry
  pure $ BL.unpack $ encode $ object
    ["quality" .= maybe (-1) (fromEnum . World.selectedTier) images,
     "shadowWidth" .= maybe 0 (F.shadowResolution . World.targets) images,
     "reflectionWidth" .= maybe 0 (F.reflectionWidth . World.targets) images,
     "scene" .= index, "trees" .= maybe 0 Geometry.treeCount geometry,
     "blades" .= maybe 0 Geometry.grassCount geometry, "grassRenderingPath" .= ("instanced" :: String),
     "grassInstances" .= instances, "grassInstanceBytes" .= (GPU.instanceStride * instances),
     "grassSharedCornerBytes" .= (15 * GPU.cornerStride),
     "triangles" .= maybe 0 Geometry.triangleCount geometry, "draws" .= count,
     "width" .= maybe 0 (F.rasterWidth . World.targets) images,
     "height" .= maybe 0 (F.rasterHeight . World.targets) images,
     "generationMs" .= map (\i -> Map.findWithDefault 0 i timings) [0,1,2],
     "renderer" .= GPU.rendererAdapterName (gpu nature)]
