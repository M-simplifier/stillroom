{-# LANGUAGE OverloadedStrings #-}

-- Deterministic production-renderer capture and offline film, disposable
-- state, no personal settings or audio device. Quality never changes the world.
module Stillroom.Runtime.GraphicsCapture (runQualityCapture, runQualityFilm) where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_, replicateM_, unless)
import Data.Aeson (decodeStrict', encode, object, (.=), Value)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import GameIO.Graphics qualified as GPU
import GameIO.Platform qualified as Platform
import Stillroom.Domain (Language (Japanese))
import Stillroom.Graphics qualified as G
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.Capture (screenshot)
import Stillroom.Runtime.Journey qualified as Runtime
import Stillroom.Landscape.Field qualified as Field
import Stillroom.Runtime.Graphics.CheckIO
import Stillroom.Runtime.Graphics.Scene (wholeSurface)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hSetBinaryMode, stdout)

lakeProgress :: Double
lakeProgress = fromMaybe 450 $ listToMaybe
  [s - 12 | s <- [100,102 .. 3000], Field.naturalGround World.firstSeed
      (J.trackX World.firstSeed s) (-s) < -1]

pose :: Double -> Double -> J.Journey
pose progress offset =
  let j = J.at World.firstSeed progress
      c = (J.car j) {J.speed = 3.6, J.pace = J.Easy}
   in j {J.car = c, J.person = J.passenger World.firstSeed c offset (-0.045) 1800}

lake :: J.Journey
lake = pose (lakeProgress+18) (-1.0)

healthy :: Runtime.JourneyRuntime -> IO ()
healthy runtime = Runtime.status runtime Japanese >>= \issue ->
  unless (issue == Nothing) (throwIO (userError (show issue)))

frame :: GPU.Renderer -> Runtime.JourneyRuntime -> Int -> Int -> IO ()
frame renderer runtime width height = presentScene renderer (Runtime.draw runtime (wholeSurface width height))

warm :: GPU.Renderer -> Runtime.JourneyRuntime -> Int -> Int -> IO ()
warm renderer runtime width height = replicateM_ 82 (frame renderer runtime width height) >> healthy runtime

runQualityCapture :: FilePath -> IO Bool
runQualityCapture folder = do
  createDirectoryIfMissing True folder
  withHiddenRenderer "Stillroom quality capture" 1600 900 $ \window renderer -> do
    Runtime.withJourneyForCheck window renderer $ \runtime -> do
      entries <- forM [("lake",lake), ("meadow",pose 60 0), ("shore",pose (lakeProgress+40) (-1.1))] $ \(name,start) -> do
        Runtime.restoreModel runtime start
        warm renderer runtime 1600 900
        before <- Runtime.model runtime
        let camera = J.view before
            cx = floor (W.posX camera/32)
            cz = floor (W.posZ camera/32)
            fx = floor (W.posX camera/128)
            fz = floor (W.posZ camera/128)
            fingerprints = (,) <$> (fst <$> Runtime.fingerprintsForCheck runtime cx cz)
                               <*> (snd <$> Runtime.fingerprintsForCheck runtime fx fz)
        fingerprint <- fingerprints
        unless (isJust (fst fingerprint) && isJust (snd fingerprint)) (throwIO (userError "Comparison geometry was not resident"))
        levels <- forM [G.Economy,G.Balanced,G.Detailed,G.Supersampled] $ \quality -> do
          Runtime.quality runtime quality
          replicateM_ 3 (frame renderer runtime 1600 900)
          healthy runtime
          submitted <- Runtime.frameMatchesForCheck runtime
          projectionError <- Runtime.frameProjectionErrorForCheck runtime
          unless (submitted && maybe False (<0.001) projectionError)
            (throwIO (userError ("Frame projection disagreement: " ++ show projectionError)))
          screenshot (folder </> name ++ "-" ++ show quality ++ ".png")
            (readScene renderer (Runtime.draw runtime (wholeSurface 1600 900)))
          times <- forM [1..12 :: Int] $ \_ -> do
            GPU.waitRenderer renderer
            began <- clockSeconds
            frame renderer runtime 1600 900
            GPU.waitRenderer renderer
            ended <- clockSeconds
            metrics <- GPU.rendererLastMetrics renderer
            pure ((ended-began)*1000, fmap (/1000000) (metrics >>= GPU.frameGPUNSec))
          after <- Runtime.model runtime
          unchanged <- (==fingerprint) <$> fingerprints
          unless (after==before && unchanged) (throwIO (userError "Quality changed the journey or geometry"))
          statistics <- Runtime.info runtime
          pure (object ["quality" .= show quality,"frameMs" .= map fst times,"sortedMs" .= sort (map fst times),
                        "gpuMs" .= map snd times, "worldAndPoseUnchanged" .= True,"plannedFrameSubmitted" .= submitted,
                        "maxClipProjectionError" .= projectionError,"renderer" .= (decodeStrict' (BC.pack statistics) :: Maybe Value)])
        pure (object ["scene" .= (name :: String),"progress" .= J.progress (J.car start),"levels" .= levels])
      -- Verify downshifting reallocates the native targets and allows resizing.
      Runtime.quality runtime G.Balanced
      Platform.resizeWindow window 800 450
      frame renderer runtime 800 450
      healthy runtime
      Runtime.quality runtime G.Supersampled
      Platform.resizeWindow window 1600 900
      frame renderer runtime 1600 900
      healthy runtime
      BL.writeFile (folder </> "quality-check.json") (encode (object ["passed" .= True,"results" .= entries,
        "resizeAndSwitchBack" .= True,"audioOpened" .= False,"personalSettingsRead" .= False]))
    pure True

runQualityFilm :: G.Tier -> IO Bool
runQualityFilm quality = do
  hSetBinaryMode stdout True
  withHiddenRenderer "Stillroom quality film" 1280 720 $ \window renderer -> do
    Runtime.withJourney window renderer $ \runtime -> do
      Runtime.restoreModel runtime lake
      Runtime.quality runtime quality
      warm renderer runtime 1280 720
      forM_ [0..479 :: Int] $ \_ -> do
        (status, pixels) <- readScene renderer (Runtime.draw runtime (wholeSurface 1280 720))
        healthy runtime
        case (status, pixels) of
          (GPU.FramePresented, Just rgba) | GPU.pixelWidth rgba == 1280 && GPU.pixelHeight rgba == 720 ->
            BS.hPut stdout (GPU.pixelRGBA8 rgba)
          _ -> throwIO (userError "Film capture lost its 1280×720 surface")
        Runtime.advanceForCheck runtime 2 J.neutral
    pure True
