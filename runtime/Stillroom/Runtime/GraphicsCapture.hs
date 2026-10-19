{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- Deterministic production-renderer capture and offline film, disposable
-- state, no personal settings or audio device. Quality never changes the world.
module Stillroom.Runtime.GraphicsCapture (runQualityCapture, runQualityFilm) where

import Control.Exception (bracket, bracket_, finally, throwIO)
import Control.Monad (forM, forM_, replicateM_, unless)
import Data.Aeson (decodeStrict', encode, object, (.=), Value)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Foreign (castPtr, free, peek)
import Foreign.C (CDouble (..), CUInt (..))
import Raylib.Core qualified as R
import Raylib.Core.Textures (c'loadImageFromScreen)
import Raylib.Internal.Foreign (c'free)
import Raylib.Types
import Stillroom.Domain (Language (Japanese))
import Stillroom.Graphics qualified as G
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.Capture (screenshot)
import Stillroom.Runtime.Journey qualified as Runtime
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hSetBinaryMode, stdout)

foreign import ccall unsafe "sr_car_natural_ground" naturalGround :: CUInt -> CDouble -> CDouble -> CDouble
foreign import ccall safe "sr_nature_sync" finishGPU :: IO ()

lakeProgress :: Double
lakeProgress = fromMaybe 450 $ listToMaybe
  [s - 12 | s <- [100,102 .. 3000], naturalGround (fromIntegral (World.seedValue World.firstSeed))
      (realToFrac (J.trackX World.firstSeed s)) (realToFrac (-s)) < -1]

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

drawScene :: Runtime.JourneyRuntime -> Int -> Int -> IO ()
drawScene runtime width height = Runtime.draw runtime (Rectangle 0 0 (fromIntegral width) (fromIntegral height))

frame :: Runtime.JourneyRuntime -> Int -> Int -> IO ()
frame runtime width height = bracket_ R.beginDrawing R.endDrawing (drawScene runtime width height)

warm :: Runtime.JourneyRuntime -> Int -> Int -> IO ()
warm runtime width height = replicateM_ 82 (frame runtime width height) >> healthy runtime

runQualityCapture :: FilePath -> IO Bool
runQualityCapture folder = do
  createDirectoryIfMissing True folder
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowHidden, WindowUndecorated, WindowAlwaysRun]
  bracket (R.initWindow 1600 900 "Stillroom quality capture") (R.closeWindow . Just) $ \_ -> do
    R.setTargetFPS 0
    Runtime.withJourney $ \runtime -> do
      entries <- forM [("lake",lake), ("meadow",pose 60 0), ("shore",pose (lakeProgress+40) (-1.1))] $ \(name,start) -> do
        Runtime.restoreModel runtime start
        warm runtime 1600 900
        before <- Runtime.model runtime
        let camera = J.view before
            cx = floor (W.posX camera/32)
            cz = floor (W.posZ camera/32)
            fx = floor (W.posX camera/128)
            fz = floor (W.posZ camera/128)
            fingerprints = (,) <$> (fst <$> Runtime.fingerprintsForCheck runtime cx cz)
                               <*> (snd <$> Runtime.fingerprintsForCheck runtime fx fz)
        fingerprint <- fingerprints
        unless (fst fingerprint /= 0 && snd fingerprint /= 0) (throwIO (userError "Comparison geometry was not resident"))
        levels <- forM [G.Economy,G.Balanced,G.Detailed,G.Supersampled] $ \quality -> do
          Runtime.quality runtime quality
          replicateM_ 3 (frame runtime 1600 900)
          healthy runtime
          screenshot (folder </> name ++ "-" ++ show quality ++ ".png")
          times <- forM [1..12 :: Int] $ \_ -> do
            finishGPU
            began <- R.getTime
            frame runtime 1600 900
            finishGPU
            ended <- R.getTime
            pure ((ended-began)*1000)
          after <- Runtime.model runtime
          unchanged <- (==fingerprint) <$> fingerprints
          unless (after==before && unchanged) (throwIO (userError "Quality changed the journey or geometry"))
          renderer <- Runtime.info runtime
          pure (object ["quality" .= show quality,"frameMs" .= times,"sortedMs" .= sort times,
                        "worldAndPoseUnchanged" .= True,"renderer" .= (decodeStrict' (BC.pack renderer) :: Maybe Value)])
        pure (object ["scene" .= (name :: String),"progress" .= J.progress (J.car start),"levels" .= levels])
      -- Verify downshifting reallocates the native targets and allows resizing.
      Runtime.quality runtime G.Balanced
      frame runtime 800 450
      healthy runtime
      Runtime.quality runtime G.Supersampled
      frame runtime 1600 900
      healthy runtime
      BL.writeFile (folder </> "quality-check.json") (encode (object ["passed" .= True,"results" .= entries,
        "resizeAndSwitchBack" .= True,"audioOpened" .= False,"personalSettingsRead" .= False]))
    pure True

rgba :: IO BS.ByteString
rgba = bracket c'loadImageFromScreen release $ \picture -> do
  width <- fromIntegral <$> peek (p'image'width picture)
  height <- fromIntegral <$> peek (p'image'height picture)
  pixels <- peek (p'image'data picture)
  BS.packCStringLen (castPtr pixels,width*height*4)
  where
    release picture = (c'free . castPtr =<< peek (p'image'data picture)) `finally` free picture

runQualityFilm :: G.Tier -> IO Bool
runQualityFilm quality = do
  hSetBinaryMode stdout True
  R.setTraceLogLevel LogNone
  R.setConfigFlags [WindowHidden,WindowUndecorated,WindowAlwaysRun]
  bracket (R.initWindow 1280 720 "Stillroom quality film") (R.closeWindow . Just) $ \_ -> do
    R.setTargetFPS 0
    Runtime.withJourney $ \runtime -> do
      Runtime.restoreModel runtime lake
      Runtime.quality runtime quality
      warm runtime 1280 720
      forM_ [0..479 :: Int] $ \_ -> do
        -- Both buffers receive the same semantic timestamp. Capture has no
        -- dependence on wall-clock speed or the previous swapped frame.
        frame runtime 1280 720
        frame runtime 1280 720
        healthy runtime
        rgba >>= BS.hPut stdout
        Runtime.advanceForCheck runtime 2 J.neutral
    pure True
