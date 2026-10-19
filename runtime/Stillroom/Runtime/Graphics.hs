{-# LANGUAGE ForeignFunctionInterface #-}
-- | The effect boundary for quality. Only completed queries affect the pure
-- governor; settings previews and frozen/hidden frames cannot bias it upward.
module Stillroom.Runtime.Graphics (Graphics, withGraphics, choose, measured, diagnostics) where

import Control.Exception (bracket, bracket_)
import Data.IORef
import Foreign (Ptr, nullPtr)
import Foreign.C (CDouble (..))
import Stillroom.Domain (RestScene)
import Stillroom.Graphics

data Timer
data Graphics = Graphics !(Ptr Timer) !(IORef (Maybe (GraphicsQuality, RestScene, Int, Int), Governor)) !(IORef (Int, Double))
foreign import ccall safe "sr_graphics_open" open :: IO (Ptr Timer)
foreign import ccall safe "sr_graphics_close" close :: Ptr Timer -> IO ()
foreign import ccall unsafe "sr_graphics_sample" sample :: Ptr Timer -> IO CDouble
foreign import ccall safe "sr_graphics_begin" begin :: Ptr Timer -> IO ()
foreign import ccall safe "sr_graphics_end" end :: Ptr Timer -> IO ()

withGraphics :: (Graphics -> IO a) -> IO a
withGraphics action = bracket open close $ \timer -> do
  state <- newIORef (Nothing, initialGovernor)
  counts <- newIORef (0, 0)
  action (Graphics timer state counts)

choose :: Graphics -> GraphicsQuality -> RestScene -> Int -> Int -> Bool -> IO Tier
choose (Graphics timer state counts) preference scene w h adaptive = do
  (oldKey, old) <- readIORef state
  let key = (preference, scene, w, h)
      same = oldKey == Just key
      current = if same then old else initialGovernor
  ms <- realToFrac <$> sample timer
  let updated = if same && adaptive && preference == Automatic then observe ms current else current
  if adaptive && preference == Automatic && ms > 0 && same
    then modifyIORef' counts (\(n, _) -> (n + 1, ms))
    else pure ()
  -- Previews don't replace the full-size context. The settling interval also
  -- excludes any query submitted before a preference/viewport transition.
  writeIORef state (Just key, updated)
  pure (maybe (if adaptive || same then tier updated else Balanced) id (fixedTier preference))

measured :: Graphics -> Bool -> IO a -> IO a
measured (Graphics timer _ _) enabled = if enabled then bracket_ (begin timer) (end timer) else id

diagnostics :: Graphics -> IO (Bool, Tier, Int, Double)
diagnostics (Graphics timer state counts) = do
  (_, g) <- readIORef state
  (n, ms) <- readIORef counts
  pure (timer /= nullPtr, tier g, n, ms)
