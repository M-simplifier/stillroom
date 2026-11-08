module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import GameIO.Audio qualified as Device
import Stillroom.Audio.Environment (environmentInput)
import Stillroom.Domain
import Stillroom.Runtime.Audio qualified as Audio
import Stillroom.Runtime.Audio.Builtin (loadBuiltinSourcesWithAssets)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  (music, ambient) <- loadBuiltinSourcesWithAssets pure 413091
  let tracks = Track (TrackId 1) "Linen" (Procedural Linen) :|
        [Track (TrackId 2) "Moss" (Procedural Moss)]
  Audio.withEngineLayersStarting Paused music ambient tracks 413091 defaultVolume $ \engine -> do
    initial <- Audio.snapshot engine
    check "Paused startup keeps the exact starting position" (Audio.position initial == 0 && not (Audio.audible initial))
    Audio.setTransport engine Playing
    _ <- waitFor engine (\snapshot -> Audio.position snapshot > 0.18)
    Audio.setAmbience engine (Just (environmentInput True 8 0.8 (-2)))
    threadDelay 600000
    moving <- Audio.diagnosticsForCheck engine
    check "Journey ambience supplies new PCM and ducks the music"
      (Audio.ambienceEnabled moving && maybe False (> 8192) (Audio.ambienceNextFrame moving)
        && Audio.musicGainMultiplier moving < 0.9)
    Audio.setTransport engine Paused
    stopped <- Audio.snapshot engine
    still <- Audio.diagnosticsForCheck engine
    threadDelay 180000
    paused <- Audio.snapshot engine
    later <- Audio.diagnosticsForCheck engine
    check "Pause holds music and environment cursors"
      (Audio.position stopped == Audio.position paused && Audio.ambienceNextFrame still == Audio.ambienceNextFrame later)
    Audio.setTransport engine Playing
    Audio.seekForCheck engine 176
    crossfade <- Audio.snapshot engine
    check "The finite built-in track crosses into the prepared deck" (Audio.crossfading crossfade)
    advanced <- waitFor engine ((> Audio.transitionCount crossfade) . Audio.transitionCount)
    check "The two-deck handover advances once" (Audio.transitionCount advanced == Audio.transitionCount crossfade + 1)
    observedDevice engine
  case args of
    [] -> pure ()
    [wave, mp3, vorbis] -> checkRecordings music ambient wave mp3 vorbis
    _ -> fail "Usage: stillroom-audio-check [WAV MP3 OGG]"

checkRecordings :: Audio.BlockSource -> Audio.AmbienceSource -> FilePath -> FilePath -> FilePath -> IO ()
checkRecordings music ambient wave mp3 vorbis = do
  let files = Track (TrackId 11) "WAV" (LocalFile wave) :|
        [Track (TrackId 12) "MP3" (LocalFile mp3), Track (TrackId 13) "Ogg Vorbis" (LocalFile vorbis)]
  Audio.withEngineLayers music ambient files 703 defaultVolume $ \engine -> do
    first <- waitFor engine ((> 0.12) . Audio.position)
    Audio.next engine
    second <- waitFor engine ((> 0.12) . Audio.position)
    Audio.next engine
    third <- waitFor engine ((> 0.12) . Audio.position)
    check "Stillroom plays all three native recording formats"
      (Set.fromList (map Audio.title [first, second, third]) == Set.fromList ["WAV", "MP3", "Ogg Vorbis"])
    let broken = Track (TrackId 21) "Missing" (LocalFile "assets/audio/music/missing.mp3") :| []
    rejected <- try (Audio.replaceLibrary engine broken 20)
    case rejected of
      Left Audio.NoPlayableTracks -> pure ()
      _ -> fail "A missing recording did not produce NoPlayableTracks"
    preserved <- Audio.snapshot engine
    check "Failed playlist replacement keeps the active deck" (Audio.title preserved == Audio.title third)
    observedDevice engine

waitFor :: Audio.Engine -> (Audio.AudioSnapshot -> Bool) -> IO Audio.AudioSnapshot
waitFor engine predicate = go (650 :: Int)
  where
    go remaining = do
      snapshot <- Audio.snapshot engine
      if predicate snapshot then pure snapshot
      else if remaining == 0 then fail ("Timed out waiting for audio state " ++ show snapshot)
      else threadDelay 10000 >> go (remaining - 1)

observedDevice :: Audio.Engine -> IO ()
observedDevice engine = do
  (pid, stats) <- Audio.deviceForCheck engine
  check "A separate worker supplied the native endpoint" (pid /= Nothing && Device.submittedPackets stats > 10)
  check "The supplier kept the stream queues fed" (Device.streamStarvedFrames stats == 0)
  print (pid, stats)

check :: String -> Bool -> IO ()
check message condition = unless condition (fail message)
