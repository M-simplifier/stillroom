-- Feed the original quality governor completed GPU observations from the same
-- full-size scene. UI frames, previews and unrelated captures carry different
-- submission numbers and cannot enter its sample window.
module Stillroom.Runtime.Graphics (Graphics, withGraphics, choose, notePresented, diagnostics) where

import Control.Monad (when)
import Data.IORef
import Data.Word (Word64)
import GameIO.Graphics qualified as GPU
import Stillroom.Domain (RestScene)
import Stillroom.Graphics

type Context = (GraphicsQuality, RestScene, Int, Int)

data Graphics = Graphics
  !GPU.Renderer !(IORef (Maybe Context, Governor)) !(IORef (Maybe (Context, Word64))) !(IORef (Int, Double))

withGraphics :: GPU.Renderer -> (Graphics -> IO a) -> IO a
withGraphics renderer action = do
  state <- newIORef (Nothing, initialGovernor)
  pending <- newIORef Nothing
  samples <- newIORef (0, 0)
  action (Graphics renderer state pending samples)

choose :: Graphics -> GraphicsQuality -> RestScene -> Int -> Int -> Bool -> IO Tier
choose (Graphics _ state _ _) preference scene width height adaptive = do
  (key, old) <- readIORef state
  let nextKey = (preference, scene, width, height)
  let same = key == Just nextKey
  let governor = if same then old else initialGovernor
  writeIORef state (Just nextKey, governor)
  pure (maybe (if adaptive || same then tier governor else Balanced) id (fixedTier preference))

notePresented :: Graphics -> Bool -> GPU.FrameStatus -> IO ()
notePresented (Graphics renderer state pending samples) adaptive status = when (status == GPU.FramePresented) $ do
  (context, governor) <- readIORef state
  serial <- GPU.rendererLastSubmission renderer
  earlier <- readIORef pending
  completed <- GPU.rendererLastMetrics renderer
  let next = case context of
        Just key@(Automatic, _, _, _) | adaptive -> Just (key, serial)
        _ -> Nothing
  writeIORef pending next
  case (next, earlier, completed) of
    (Just (key, _), Just (previous, index), Just metrics)
      | key == previous, GPU.frameSubmissionIndex metrics == index
      , Just nanoseconds <- GPU.frameGPUNSec metrics, nanoseconds > 0 -> do
          let ms = nanoseconds / 1000000
          writeIORef state (context, observe ms governor)
          modifyIORef' samples (\(count, _) -> (count+1, ms))
    _ -> pure ()

diagnostics :: Graphics -> IO (Bool, Tier, Int, Double)
diagnostics (Graphics renderer state _ samples) = do
  (_, governor) <- readIORef state
  (count, latest) <- readIORef samples
  metrics <- GPU.rendererLastMetrics renderer
  let hasGPUTime = case metrics >>= GPU.frameGPUNSec of Just _ -> True; Nothing -> False
  pure (hasGPUTime, tier governor, count, latest)
