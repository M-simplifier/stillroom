{-# LANGUAGE OverloadedStrings #-}

-- | A bounded benchmark of the actual renderer, without audio or frame pacing.
-- It never reads or writes the user's preferences. Compare identical builds,
-- window sizes and workloads; this is not a battery-life or GPU-time meter.
module Stillroom.Runtime.Benchmark (runBenchmark) where

import Control.Exception (bracket)
import Control.Monad (forM, replicateM_, unless, void)
import Data.Aeson (FromJSON (..), Value, decode, encode, object, withObject, (.:), (.=))
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (sort)
import GHC.Stats
import GameIO.Graphics qualified as GPU
import GameIO.Platform qualified as Platform
import Stillroom.Domain
import Stillroom.Runtime.Graphics.CheckIO
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
  withHiddenRenderer "Stillroom renderer benchmark" 760 500 $ \window renderer ->
    bracket (View.loadTypeface renderer [title]) (View.unloadTypeface renderer) $ \font -> Rest.withRestAssets window renderer $ \assets -> do
      ready <- Rest.proceduralReady assets
      unless (ready && Rest.thumbnailsReady assets) $
        ioError (userError "Renderer benchmark requires the procedural shader and all thumbnails.")
      rows <- forM [1 .. 3 :: Int] $ \repetition -> forM cases $ \(name, w, h, state) -> do
        Platform.resizeWindow window w h
        let draw :: Int -> IO ()
            draw i = void $ View.render renderer False font assets w h
              state {restSeconds = if restMotion state == Still then 0 else fromIntegral i / 60}
              Nothing "1 / 4   Mist lake" title Nothing (-1, -1)
        replicateM_ 8 (draw 0)
        performGC
        before <- getRTSStats
        cpuBefore <- getCPUTime
        wallBefore <- clockSeconds
        samples <- forM [1 .. frames] $ \i -> do
          start <- clockSeconds
          draw i
          end <- clockSeconds
          pure ((end - start) * 1000)
        wallAfter <- clockSeconds
        cpuAfter <- getCPUTime
        performGC
        after <- getRTSStats
        let count = fromIntegral frames :: Double
        pure $
          object
            [ "case" .= name,
              "repetition" .= repetition,
              "width" .= w,
              "height" .= h,
              "frames" .= frames,
              "cpu_ms_per_frame" .= (fromIntegral (cpuAfter - cpuBefore) / 1e9 / count :: Double),
              "wall_ms_per_frame" .= ((wallAfter - wallBefore) * 1000 / count),
              "wall_median_ms" .= percentile 0.5 samples,
              "wall_p95_ms" .= percentile 0.95 samples,
              "allocated_bytes_per_frame" .= (fromIntegral (allocated_bytes after - allocated_bytes before) / count :: Double)
            ]
      (landscapes, contracts) <- landscapeBenchmark window renderer font assets
      BL.writeFile (output </> "renderer-benchmark.json")
        $ encode
        $ object
          [ "workload" .= ("actual View.render; hidden Vulkan window; audio and pacing disabled; warm caches; 3 repetitions" :: String),
            "results" .= concat rows,
            "landscapeWorkload" .= ("actual View.render with Vulkan graphics fence and GPU timestamps; same scene, camera and 30 Hz clock; hidden window; reference uses full-size reflection and uncapped raster; standard uses half-size reflection and max 1600x900 raster; both use 2048 shadow; 2 repetitions, 30 frames each" :: String),
            "landscapeResults" .= landscapes,
            "landscapeContracts" .= contracts
          ]
      pure True
  where
    title = "A quiet afternoon / 静かな午後"
    frames = 60 :: Int
    base = (initialSession defaultVolume) {windowMode = Compact, language = Japanese, graphicsQuality = Standard}
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

data LandscapeStats = LandscapeStats {drawCount :: Int, rasterWidth :: Int, rasterHeight :: Int}

instance FromJSON LandscapeStats where
  parseJSON = withObject "LandscapeStats" $ \v -> LandscapeStats <$> v .: "draws" <*> v .: "width" <*> v .: "height"

landscapeBenchmark :: Platform.Window -> GPU.Renderer -> View.Typeface -> Rest.RestAssets -> IO ([Value], Value)
landscapeBenchmark window renderer font assets = do
  let state scene = (initialSession defaultVolume) {graphicsQuality = Standard, viewingMode = Resting, restScene = scene, restHintSeconds = 0, restShade = Original}
      draw w h scene clock = do
        void (View.render renderer False font assets w h (state scene) {restSeconds = clock} Nothing "" "" Nothing (-1, -1))
        GPU.waitRenderer renderer
      stats = do
        raw <- Rest.landscapeInfo assets
        maybe (ioError (userError "Invalid landscape diagnostics")) pure (decode (BLC.pack raw))
      count = 30 :: Int
  rows <- forM [1, 2 :: Int] $ \repetition -> forM [BirchLake, AlpineLake, MeadowStream] $ \scene ->
    forM [(1280, 720), (1920, 1080)] $ \(w, h) -> forM ["reference", "balanced", "still" :: String] $ \profile -> do
      Rest.landscapeReference assets (profile == "reference")
      Platform.resizeWindow window w h
      replicateM_ 8 (draw w h scene 12)
      actual <- Platform.windowFramebufferSize window
      unless (actual == (w, h)) (ioError (userError "Benchmark surface size differs from workload"))
      performGC
      before <- getRTSStats
      cpuBefore <- getCPUTime
      samples <- forM [1 .. count] $ \i -> do
        start <- clockSeconds
        draw w h scene (if profile == "still" then 12 else 12 + fromIntegral i / 30 + 0.000001)
        end <- clockSeconds
        metrics <- GPU.rendererLastMetrics renderer
        pure ((end - start) * 1000, fmap (/1000000) (metrics >>= GPU.frameGPUNSec))
      cpuAfter <- getCPUTime
      performGC
      after <- getRTSStats
      details <- Rest.landscapeInfo assets
      ready <- Rest.proceduralReady assets
      unless ready (ioError (userError "Landscape benchmark failed"))
      pure $
        object
          [ "scene" .= show scene,
            "profile" .= profile,
            "repetition" .= repetition,
            "width" .= w,
            "height" .= h,
            "frames" .= count,
            "cpu_ms_per_frame" .= (fromIntegral (cpuAfter - cpuBefore) / 1e9 / fromIntegral count :: Double),
            "wall_median_ms" .= percentile 0.5 (map fst samples),
            "wall_p95_ms" .= percentile 0.95 (map fst samples),
            "gpu_ms" .= map snd samples,
            "allocated_bytes_per_frame" .= (fromIntegral (allocated_bytes after - allocated_bytes before) / fromIntegral count :: Double),
            "native" .= (decode (BLC.pack details) :: Maybe Value)
          ]
  Rest.landscapeReference assets False
  Platform.resizeWindow window 1280 720
  draw 1280 720 BirchLake 12
  before <- stats
  replicateM_ 3 (draw 1280 720 BirchLake 12)
  held <- stats
  draw 1280 720 BirchLake 13
  moved <- stats
  Platform.resizeWindow window 1920 1080
  draw 1920 1080 BirchLake 13
  resized <- stats
  draw 1920 1080 AlpineLake 13
  switched <- stats
  let frozen = drawCount held == drawCount before
      advances = drawCount moved == drawCount held + 1
      resize = drawCount resized == drawCount moved + 1 && rasterWidth resized == 1600 && rasterHeight resized == 900
      selection = drawCount switched == drawCount resized + 1
      contracts = object ["frozen_reuses_scene" .= frozen, "clock_redraws" .= advances, "resize_invalidates_and_bounds_raster" .= resize, "selection_invalidates" .= selection]
  unless (and [frozen, advances, resize, selection]) (ioError (userError "Landscape cache contract failed"))
  pure (concatMap (concatMap concat) rows, contracts)
