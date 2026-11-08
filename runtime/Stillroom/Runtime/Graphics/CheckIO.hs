module Stillroom.Runtime.Graphics.CheckIO (withHiddenRenderer, presentScene, readScene, clockSeconds) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import GameIO.Graphics qualified as GPU
import GameIO.Platform qualified as Platform
import GHC.Clock (getMonotonicTimeNSec)
import Stillroom.Runtime.Graphics.Scene

withHiddenRenderer :: String -> Int -> Int -> (Platform.Window -> GPU.Renderer -> IO a) -> IO a
withHiddenRenderer title width height use =
  Platform.withWindowVisibility False (Platform.WindowConfig title width height) $ \window ->
    GPU.withRenderer window (use window)

presentScene :: GPU.Renderer -> IO SceneDraw -> IO ()
presentScene renderer plan = present (120 :: Int)
  where
    present 0 = fail "Scene check did not acquire a presentable surface"
    present attempts = do
      draw <- plan
      status <- GPU.renderPasses renderer (scenePasses draw) (frame draw)
      scenePresented draw status
      unless (status == GPU.FramePresented) (threadDelay 1000 >> present (attempts-1))

readScene :: GPU.Renderer -> IO SceneDraw -> IO (GPU.FrameStatus, Maybe GPU.FramePixels)
readScene renderer plan = do
  draw <- plan
  outcome@(status, _) <- GPU.renderPassesReadback renderer (scenePasses draw) (frame draw)
  scenePresented draw status
  pure outcome

frame :: SceneDraw -> GPU.Frame
frame draw = GPU.Frame (GPU.Color 0 0 0 1) [] (sceneOverlay draw)

clockSeconds :: IO Double
clockSeconds = (/1e9) . fromIntegral <$> getMonotonicTimeNSec
