{-# LANGUAGE NamedFieldPuns #-}

module Stillroom.Runtime.Audio
  ( Engine,
    AudioFailure (..),
    AudioSnapshot (..),
    withEngine,
    pump,
    setTransport,
    setVolume,
    next,
    replaceLibrary,
    snapshot,
    seekForCheck,
  )
where

import Control.Exception
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Vector.Storable qualified as V
import Foreign
import Foreign.C (withCString)
import Raylib.Core (c'memFree, getTime)
import Raylib.Core.Audio qualified as A
import Raylib.Internal.Foreign (pop, withFreeable)
import Raylib.Types (AudioStream, Music (..), MusicContextType (..))
import Stillroom.Domain
import Stillroom.Shuffle
import Stillroom.Synthesis
import System.Directory (getFileSize)
import System.FilePath (takeExtension)

data Voice = Recorded !Music !(Ptr ()) | Generated !AudioStream !Preset !Integer

data Deck = Deck
  { deckTrack :: !Track,
    voice :: !Voice,
    deckLength :: !Double,
    accumulated :: !Double,
    runningSince :: !(Maybe Double)
  }

data State = State
  { current :: !Deck,
    prepared :: !Deck,
    bag :: !(Bag Track),
    librarySize :: !Int,
    rejected :: !(Set.Set TrackId),
    playing :: !Transport,
    master :: !Volume,
    blending :: !Bool,
    warning :: !(Maybe String),
    transitions :: !Int
  }

newtype Engine = Engine (IORef State)

data AudioSnapshot = AudioSnapshot
  { title :: !String,
    position :: !Double,
    duration :: !Double,
    audible :: !Bool,
    crossfading :: !Bool,
    transitionCount :: !Int,
    audioWarning :: !(Maybe String)
  }
  deriving (Show)

data AudioFailure = InvalidRecording | RecordingTooLarge | NoPlayableTracks
  deriving (Show)

instance Exception AudioFailure

-- Both decks and the audio device have one owner. The bracket owns the latest
-- state even when a later decoder or window operation raises an exception.
withEngine :: NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngine tracks seed initialVolume action = bracket acquire release action
  where
    acquire = mask $ \restore -> do
      A.initAudioDevice
      ready <- A.isAudioDeviceReady
      when (not ready) (A.closeAudioDevice Nothing >> throwIO (userError "No audio output device is available."))
      A.setAudioStreamBufferSizeDefault 4096
      let initialBag = newBag seed tracks; n = NE.length tracks
      (first, b1, bad1) <- restore (selectDeck n Set.empty initialBag) `onException` A.closeAudioDevice Nothing
      (second, b2, bad2) <- restore (selectDeck n bad1 b1) `onException` (closeDeck first >> A.closeAudioDevice Nothing)
      ref <- newIORef (State first second b2 n bad2 Paused initialVolume False Nothing 0)
      let engine = Engine ref
      restore (setTransport engine Playing) `onException` (closeDeck first >> closeDeck second >> A.closeAudioDevice Nothing)
      pure engine
    release (Engine ref) = do
      s <- readIORef ref
      closeDeck (current s) `finally` (closeDeck (prepared s) `finally` A.closeAudioDevice Nothing)

closeDeck :: Deck -> IO ()
closeDeck Deck {voice = Recorded music bytes} = releaseMusic music `finally` free bytes
closeDeck Deck {voice = Generated stream _ _} = withFreeable stream A.c'unloadAudioStream

-- Compatibility with the exact raylib revision bundled by h-raylib 5.6.0.0:
-- its WAV destructor omits the allocated context. Keep this repair isolated
-- and recheck on dependency updates. FLAC is disabled in this pinned build.
releaseMusic :: Music -> IO ()
releaseMusic music = do
  withFreeable music A.c'unloadMusicStream
  when (music'ctxType music == MusicAudioWAV) (c'memFree (music'ctxData music))

openDeck :: Track -> IO Deck
openDeck track = case trackSource track of
  Procedural preset -> do
    stream <- A.loadAudioStream (fromIntegral sampleRate) 32 2
    valid <- A.isAudioStreamValid stream
    if not valid
      then throwIO (userError "Cannot create an audio stream.")
      else do
        A.setAudioStreamVolume stream 0
        pure (Deck track (Generated stream preset 0) (secondsValue (presetDuration preset)) 0 Nothing)
  LocalFile file -> mask $ \restore -> do
    size <- getFileSize file
    when (size > 128 * 1024 * 1024) (throwIO RecordingTooLarge)
    bytes <- restore (BS.readFile file)
    buffer <- mallocBytes (BS.length bytes)
    music <-
      ( do
          BS.useAsCString bytes $ \source -> copyBytes buffer (castPtr source) (BS.length bytes)
          withCString (map toLower (takeExtension file)) (\extension -> A.c'loadMusicStreamFromMemory extension (castPtr buffer) (fromIntegral (BS.length bytes))) >>= pop
      )
        `onException` free buffer
    let release = releaseMusic music `finally` free buffer
    ( do
        valid <- A.isMusicValid music
        when (not valid) (throwIO InvalidRecording)
        lengthSeconds <- realToFrac <$> A.getMusicTimeLength music
        when (isNaN lengthSeconds || isInfinite lengthSeconds || lengthSeconds <= 0) (throwIO InvalidRecording)
        A.setMusicVolume music 0
        pure (Deck track (Recorded (music {music'looping = False}) buffer) lengthSeconds 0 Nothing)
      )
      `onException` release

-- Only synchronous decoding/file errors are recoverable. Cancellation is never
-- swallowed. Every attempt removes a failing TrackId, so failure is bounded.
selectDeck :: Int -> Set.Set TrackId -> Bag Track -> IO (Deck, Bag Track, Set.Set TrackId)
selectDeck remaining bad source
  | remaining <= 0 = throwIO NoPlayableTracks
  | otherwise = do
      let (track, rest) = draw source
      if Set.member (trackId track) bad
        then reject track bad rest
        else do
          result <- tryJust recoverable (openDeck track)
          case result of
            Right deck -> pure (deck, rest, bad)
            Left _ -> reject track (Set.insert (trackId track) bad) rest
  where
    reject track failures rest = case exclude track rest of
      Nothing -> throwIO NoPlayableTracks
      Just filtered -> selectDeck (remaining - 1) failures filtered
    recoverable exception = case fromException exception :: Maybe AsyncException of
      Just _ -> Nothing
      Nothing -> Just exception

startDeck :: Deck -> IO Deck
startDeck deck@Deck {runningSince = Just _} = pure deck
startDeck deck = do
  now <- getTime
  case voice deck of
    Recorded music _ -> if accumulated deck > 0 then A.resumeMusicStream music else A.playMusicStream music
    Generated stream _ _ -> if accumulated deck > 0 then A.resumeAudioStream stream else A.playAudioStream stream
  pure deck {runningSince = Just now}

pauseDeck :: Deck -> IO Deck
pauseDeck deck@Deck {runningSince = Nothing} = pure deck
pauseDeck deck@Deck {runningSince = Just since} = do
  now <- getTime
  case voice deck of
    Recorded music _ -> A.pauseMusicStream music
    Generated stream _ _ -> A.pauseAudioStream stream
  pure deck {accumulated = accumulated deck + now - since, runningSince = Nothing}

deckPosition :: Deck -> IO Double
deckPosition deck = case voice deck of
  Recorded music _ -> do
    nativePlaying <- A.isMusicStreamPlaying music
    if runningSince deck /= Nothing && not nativePlaying
      then pure (deckLength deck)
      else realToFrac <$> A.getMusicTimePlayed music
  Generated _ _ _ -> do now <- getTime; pure (accumulated deck + maybe 0 (now -) (runningSince deck))

feed :: Deck -> IO Deck
feed deck@Deck {runningSince = Nothing} = pure deck
feed deck@Deck {voice = Recorded music _} = A.updateMusicStream music >> pure deck
feed deck@Deck {voice = Generated stream preset offset} = do
  available <- A.isAudioStreamProcessed stream
  if not available
    then pure deck
    else do
      let samples = renderBlock preset offset 4096
      V.unsafeWith samples $ \pointer -> A.updateAudioStream stream (castPtr pointer) 4096
      pure deck {voice = Generated stream preset (offset + 4096)}

gain :: Deck -> Double -> IO ()
gain deck value = case voice deck of
  Recorded music _ -> A.setMusicVolume music (realToFrac value)
  Generated stream _ _ -> A.setAudioStreamVolume stream (realToFrac value)

setTransport :: Engine -> Transport -> IO ()
setTransport (Engine ref) requested = do
  s <- readIORef ref
  if playing s == requested
    then pure ()
    else do
      first <- if requested == Playing then startDeck (current s) else pauseDeck (current s)
      second <- if requested == Playing && blending s then startDeck (prepared s) else pauseDeck (prepared s)
      writeIORef ref s {current = first, prepared = second, playing = requested}

setVolume :: Engine -> Volume -> IO ()
setVolume (Engine ref) value = modifyIORef' ref (\s -> s {master = value})

next :: Engine -> IO ()
next (Engine ref) = mask_ $ do
  s <- readIORef ref
  -- Acquire replacement before releasing the old deck: a failure leaves the
  -- existing state valid and owned by the enclosing bracket.
  (newDeck, rest, bad) <- selectDeck (librarySize s) (rejected s) (bag s)
  first <- (if playing s == Playing then startDeck (prepared s) else pure (prepared s)) `onException` closeDeck newDeck
  writeIORef ref s {current = first, prepared = newDeck, bag = rest, rejected = bad, blending = False, transitions = transitions s + 1}
  closeDeck (current s)

replaceLibrary :: Engine -> NonEmpty Track -> Int -> IO ()
replaceLibrary (Engine ref) tracks seed = mask_ $ do
  s <- readIORef ref
  let count = NE.length tracks
  (a, b1, bad1) <- selectDeck count Set.empty (newBag seed tracks)
  (b, b2, bad2) <- selectDeck count bad1 b1 `onException` closeDeck a
  a' <- (if playing s == Playing then startDeck a else pure a) `onException` (closeDeck a >> closeDeck b)
  writeIORef
    ref
    s
      { current = a',
        prepared = b,
        bag = b2,
        librarySize = count,
        rejected = bad2,
        blending = False,
        warning = if Set.null bad2 then Nothing else Just "Some music files could not be decoded."
      }
  closeDeck (current s) `finally` closeDeck (prepared s)

pump :: Engine -> IO ()
pump engine@(Engine ref) = do
  s <- readIORef ref
  if playing s == Paused
    then pure ()
    else do
      first <- feed (current s)
      elapsed <- deckPosition first
      let timeValue = either (const zeroSeconds) id . seconds
          curve = mixBetween (timeValue (deckLength first)) (timeValue (deckLength (prepared s))) (timeValue elapsed)
          level = 0.66 * volumeValue (master s)
          attack = fadeIn (timeValue elapsed)
      case curve of
        Advance -> next engine
        Solo -> do
          gain first (level * attack)
          gain (prepared s) 0
          writeIORef ref s {current = first}
        Crossfade outgoing incoming -> do
          second <- startDeck (prepared s) >>= feed
          gain first (level * outgoing * attack)
          gain second (level * incoming)
          writeIORef ref s {current = first, prepared = second, blending = True}

snapshot :: Engine -> IO AudioSnapshot
snapshot (Engine ref) = do
  s <- readIORef ref
  elapsed <- deckPosition (current s)
  pure
    ( AudioSnapshot
        (trackTitle (deckTrack (current s)))
        elapsed
        (deckLength (current s))
        (playing s == Playing)
        (blending s)
        (transitions s)
        (warning s)
    )

-- Used by the native acceptance harness; the regular player has no seek UI.
seekForCheck :: Engine -> Double -> IO ()
seekForCheck (Engine ref) target = do
  s <- readIORef ref
  let deck = current s; bounded = max 0 (min (deckLength deck - 0.01) target)
  now <- getTime
  updated <- case voice deck of
    Recorded music _ -> A.seekMusicStream music (realToFrac bounded) >> pure deck
    Generated stream preset _ ->
      pure
        deck
          { voice = Generated stream preset (floor (bounded * fromIntegral sampleRate)),
            accumulated = bounded,
            runningSince = if playing s == Playing then Just now else Nothing
          }
  writeIORef ref s {current = updated}
