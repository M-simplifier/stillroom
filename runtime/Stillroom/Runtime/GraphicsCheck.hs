{-# LANGUAGE OverloadedStrings #-}
-- Product-path graphics checks with independent state and native Vulkan images.
module Stillroom.Runtime.GraphicsCheck (runGraphicsCheck) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, throwIO)
import Control.Monad (forM, replicateM_, unless)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort)
import GameIO.Platform qualified as Platform
import Stillroom.Domain
import Stillroom.Journey.Ride qualified as J
import Stillroom.Runtime.Capture (screenshot)
import Stillroom.Runtime.Graphics.CheckIO
import Stillroom.Runtime.Graphics.Scene (wholeSurface)
import Stillroom.Runtime.Journey qualified as Journey
import Stillroom.Runtime.Nature qualified as Nature
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Session
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

runGraphicsCheck :: FilePath -> IO Bool
runGraphicsCheck folder = do
  createDirectoryIfMissing True folder
  withHiddenRenderer "Stillroom graphics check" 1600 900 $ \window renderer -> Rest.withRestAssets window renderer $ \assets -> do
    let state scene quality = (initialSession defaultVolume) {restScene = scene, graphicsQuality = quality, restShade = Original, restSeconds = 30, viewingMode = Resting}
        frame w h s = do
          current <- Platform.windowFramebufferSize window
          unless (current == (w,h)) (Platform.resizeWindow window w h)
          presentScene renderer (Rest.renderRest assets w h s)
        capture w h s path = screenshot path (readScene renderer (Rest.renderRest assets w h s))
        healthy s = Rest.restIssue assets s >>= \issue -> unless (issue == Nothing) (throwIO (userError (show issue)))
        allocationLimits = Rest.setAllocationLimitsForCheck assets
    manual <- forM [scene | scene <- [minBound .. maxBound], scene /= RailJourney] $ \scene -> do
      levels <- forM [Lightweight, Standard, High, Ultra] $ \quality -> do
        let s = (state scene quality) {restMotion = Still}
        replicateM_ 3 (frame 1600 900 s)
        healthy s
        capture 1600 900 s (folder </> show scene ++ "-" ++ show quality ++ ".png")
        before <- Rest.landscapeInfo assets
        replicateM_ 3 (frame 1600 900 s)
        after <- Rest.landscapeInfo assets
        unless (before == after) (throwIO (userError "Frozen nature was rendered again"))
        pure (object ["quality" .= show quality, "frozenCache" .= True, "native" .= after])
      -- Supersampled → low and resize must invalidate targets even when frozen.
      frame 800 450 ((state scene Lightweight) {restMotion = Still})
      healthy (state scene Lightweight)
      pure (object ["scene" .= show scene, "levels" .= levels])
    automatic <- forM [BirchLake, RailJourney] $ \scene -> do
      let s = state scene Automatic
      -- Prepare on the real renderer, outside the measurement series.
      replicateM_ 82 (frame 1600 900 (state scene Standard))
      healthy s
      initial <- Journey.model (Rest.journey assets)
      (_, _, initialCount, _) <- Rest.graphicsInfo assets
      measurements <- forM [1..510 :: Int] $ \i -> do
        start <- clockSeconds
        if scene == RailJourney then Journey.advanceForCheck (Rest.journey assets) 2 J.neutral else pure ()
        frame 1600 900 s {restSeconds = 30 + fromIntegral i / 30}
        submitted <- clockSeconds
        threadDelay (max 0 (ceiling ((start+1/30-submitted)*1000000)))
        ended <- clockSeconds
        (available, tier, count, ms) <- Rest.graphicsInfo assets
        pure (available, tier, count, ms, (ended - start) * 1000)
      healthy s
      final <- Journey.model (Rest.journey assets)
      let previousCounts = initialCount : [n | (_, _, n, _, _) <- measurements]
          observations = [ms | (previous, (_, _, count, ms, _)) <- zip previousCounts measurements, count > previous, ms > 0]
          levels = nub [show t | (_, t, _, _, _) <- measurements]
          available = or [a | (a, _, _, _, _) <- measurements]
          paced = sort [ms | (_, _, _, _, ms) <- measurements]
          percentile :: Double -> Double
          percentile p = case drop (floor (p * fromIntegral (length paced - 1))) paced of x : _ -> x; [] -> 0
      unless (not available || length observations > 400) (throwIO (userError "GPU query ring stopped producing results"))
      unless (J.worldSeed initial == J.worldSeed final) (throwIO (userError "Automatic quality changed the journey seed"))
      capture 1600 900 s (folder </> show scene ++ "-Automatic.png")
      pure (object ["scene" .= show scene, "timerAvailable" .= available, "tiers" .= levels,
                    "observations" .= length observations, "gpuMs" .= observations,
                    "pacedFrameMedianMs" .= percentile 0.5, "pacedFrameP95Ms" .= percentile 0.95])
    recovery <- forM [BirchLake, RailJourney, TidalLight] $ \scene -> do
      frame 1600 900 (state scene Standard)
      before <- Journey.model (Rest.journey assets)
      let limits = if scene == RailJourney then allocationLimits 0 1536 else allocationLimits 1536 0
      (do
        limits
        replicateM_ 3 (frame 1920 1080 (state scene Ultra))
        healthy (state scene Ultra)
        -- Even when every replacement is rejected, the valid old image remains.
        allocationLimits 1 1
        replicateM_ 3 (frame 1800 1000 (state scene Ultra))
        healthy (state scene Ultra)
        capture 1800 1000 (state scene Ultra) (folder </> show scene ++ "-allocation-recovery.png")) `finally` allocationLimits 0 0
      frame 800 450 (state scene Lightweight)
      healthy (state scene Lightweight)
      after <- Journey.model (Rest.journey assets)
      unless (before == after) (throwIO (userError "Allocation recovery changed journey state"))
      pure (show scene)
    (do
      Nature.withNature renderer $ \native -> do
        Nature.setAllocationLimitsForCheck native 0 1536
        presentScene renderer (Nature.draw native BirchLake 30 (wholeSurface 800 450))
        Nature.available native >>= \ok -> unless ok (throwIO (userError "Initial nature shadow fallback failed"))
      Journey.unload (Rest.journey assets)
      allocationLimits 0 1536
      replicateM_ 82 (frame 800 450 (state RailJourney Standard))
      healthy (state RailJourney Standard)) `finally` allocationLimits 0 0
    BL.writeFile (folder </> "graphics-check.json") (encode (object ["passed" .= True, "manual" .= manual, "automatic" .= automatic, "allocationRecovery" .= recovery, "initialShadowFallback" .= True]))
    pure True
