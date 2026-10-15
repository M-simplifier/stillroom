{-# LANGUAGE OverloadedStrings #-}

-- | A bounded benchmark of the actual renderer, without audio or frame pacing.
-- It never reads or writes the user's preferences. Compare identical builds,
-- window sizes and workloads; this is not a battery-life or GPU-time meter.
module Stillroom.Runtime.Benchmark (runBenchmark) where

import Control.Exception (bracket)
import Control.Monad (forM, replicateM_, unless)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import GHC.Stats
import Raylib.Core qualified as R
import Raylib.Types
import Stillroom.Domain
import Stillroom.Runtime.Render qualified as View
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Session
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Mem (performGC)

runBenchmark :: FilePath -> IO Bool
runBenchmark output = do
  enabled <- getRTSStatsEnabled
  unless enabled (ioError (userError "Run --benchmark OUTPUT +RTS -T -RTS"))
  createDirectoryIfMissing True output
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowHidden, WindowAlwaysRun]
  bracket (R.initWindow 760 500 "Stillroom renderer benchmark") (R.closeWindow . Just) $ \window -> do
    R.setTargetFPS 0
    font <- View.loadTypeface window [title]
    assets <- Rest.loadRestAssets window
    rows <- forM [1 .. 3 :: Int] $ \repetition -> forM cases $ \(name, w, h, state) -> do
      R.setWindowSize w h
      let draw :: Int -> IO ()
          draw i = do
            R.beginDrawing
            View.render font assets w h state {restSeconds = if restMotion state == Still then 0 else fromIntegral i / 60} Nothing "1 / 4   Mist lake" title Nothing (-1, -1)
            R.endDrawing
      replicateM_ 8 (draw 0)
      performGC
      before <- getRTSStats
      cpuBefore <- getCPUTime
      wallBefore <- R.getTime
      samples <- forM [1 .. frames] $ \i -> do
        start <- R.getTime
        draw i
        end <- R.getTime
        pure ((end - start) * 1000)
      wallAfter <- R.getTime
      cpuAfter <- getCPUTime
      performGC
      after <- getRTSStats
      let count = fromIntegral frames :: Double
      pure $ object
        [ "case" .= name, "repetition" .= repetition, "width" .= w, "height" .= h,
          "frames" .= frames,
          "cpu_ms_per_frame" .= (fromIntegral (cpuAfter - cpuBefore) / 1e9 / count :: Double),
          "wall_ms_per_frame" .= ((wallAfter - wallBefore) * 1000 / count),
          "wall_median_ms" .= percentile 0.5 samples,
          "wall_p95_ms" .= percentile 0.95 samples,
          "allocated_bytes_per_frame" .= (fromIntegral (allocated_bytes after - allocated_bytes before) / count :: Double)
        ]
    BL.writeFile (output </> "renderer-benchmark.json") $ encode $ object
      [ "workload" .= ("actual View.render; no audio; hidden OpenGL window; no FPS target or VSync request; warm caches; 3 repetitions" :: String),
        "results" .= concat rows
      ]
    pure True
  where
    title = "A quiet afternoon / 静かな午後"
    frames = 60 :: Int
    base = (initialSession defaultVolume) {windowMode = Compact, language = Japanese}
    cases :: [(String, Int, Int, Session)]
    cases =
      [ ("player", 760, 500, base),
        ("display-settings", 760, 500, base {settingsOpen = True, settingsPage = DisplaySettings}),
        ("rest-gallery", 760, 500, base {settingsOpen = True, settingsPage = RestSettings}),
        ("quiet-lake", 1920, 1080, base {windowMode = Fullscreen, idleSeconds = 10, background = Lake}),
        ("water-over-lake", 1920, 1080, base {viewingMode = Resting, background = Lake, restHintSeconds = 0}),
        ("still-water", 1920, 1080, base {viewingMode = Resting, restMotion = Still, restHintSeconds = 0})
      ]

percentile :: Double -> [Double] -> Double
percentile fraction values = case drop index (sort values) of
  value : _ -> value
  [] -> 0
  where
    index = max 0 (ceiling (fraction * fromIntegral (length values)) - 1)
