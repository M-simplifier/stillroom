{-# LANGUAGE NamedFieldPuns #-}

module Stillroom.Runtime.Audio
  ( Engine,
    AudioFailure (..),
    AudioSnapshot (..),
    AudioDiagnostics (..),
    BlockSource,
    AmbienceSource,
    withEngine,
    withEngineSource,
    withEngineLayers,
    withEngineLayersStarting,
    pump,
    setTransport,
    setVolume,
    setAmbience,
    next,
    replaceLibrary,
    snapshot,
    seekForCheck,
    diagnosticsForCheck,
    deviceForCheck,
  )
where

import Control.Exception
import Control.Concurrent (MVar, ThreadId, forkIOWithUnmask, killThread, myThreadId, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay, withMVar)
import Control.Monad (when)
import Data.Char (toLower)
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Vector.Storable qualified as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GameIO.Audio qualified as A
import Stillroom.Domain
import Stillroom.Audio.Environment (EnvironmentInput)
import Stillroom.Shuffle
import Stillroom.Synthesis
import System.Directory (getFileSize)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.Timeout (timeout)

-- The supplier generates bounded blocks and sends them to the audio process.
-- Startup, seeks and library changes can also request a block synchronously.
type BlockSource = Preset -> Integer -> Int -> IO (V.Vector Float)

type AmbienceSource = Maybe EnvironmentInput -> Int -> IO (V.Vector Float)

blockFrames, queuedFrames :: Int
blockFrames = 4096
queuedFrames = 2 * blockFrames

data Voice
  = Recorded !A.AudioStream !A.PCMSource !A.PCMReader !Int !Int
  | Generated !A.AudioStream !Preset !Integer !Integer

data Ambience = Ambience {ambienceStream :: !A.AudioStream, ambienceFrames :: !Integer}

data Deck = Deck
  { deckTrack :: !Track,
    voice :: !Voice,
    deckLength :: !(Maybe Double),
    running :: !Bool
  }

data State = State
  { audioDevice :: !A.Audio,
    current :: !Deck,
    prepared :: !Deck,
    bag :: !(Bag Track),
    librarySize :: !Int,
    rejected :: !(Set.Set TrackId),
    playing :: !Transport,
    master :: !Volume,
    blending :: !Bool,
    warning :: !(Maybe String),
    transitions :: !Int,
    supplyPasses :: !Integer,
    ambience :: !(Maybe Ambience),
    environment :: !(Maybe EnvironmentInput),
    musicMultiplier :: !Double,
    gainClock :: !Word64
  }

-- One supplier; synchronous controls and supply serialize through this mutex.
-- The worker's finalizer owns the latest decks and closes the device. No GL or
-- UI API is used by that worker. Cached snapshots never wait for a render block.
data Engine = Engine
  { stateRef :: !(IORef State),
    stateLock :: !(MVar ()),
    blockSource :: BlockSource,
    ambienceSource :: Maybe AmbienceSource,
    latestSnapshot :: !(IORef AudioSnapshot),
    closing :: !(IORef Bool),
    failure :: !(IORef (Maybe SomeException)),
    finished :: !(MVar (Either SomeException ())),
    supplierThread :: !(MVar ThreadId)
  }

data AudioSnapshot = AudioSnapshot
  { title :: !String,
    position :: !Double,
    duration :: !(Maybe Double),
    audible :: !Bool,
    crossfading :: !Bool,
    transitionCount :: !Int,
    audioWarning :: !(Maybe String)
  }
  deriving (Show)

data AudioDiagnostics = AudioDiagnostics
  { supplyPassCount :: !Integer,
    generatedNextFrame :: !(Maybe Integer),
    generatedCompletedFrame :: !(Maybe Integer),
    requestedQueueCapacity :: !Int,
    ambienceNextFrame :: !(Maybe Integer),
    ambienceEnabled :: !Bool,
    musicGainMultiplier :: !Double
  }
  deriving (Eq, Show)

data AudioFailure = InvalidRecording | RecordingTooLarge | NoPlayableTracks | InvalidGeneratedBlock | EngineClosed
  deriving (Show)

instance Exception AudioFailure

withEngine :: NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngine = withEngineSource (\preset offset count -> evaluate (renderBlock preset offset count))

withEngineSource :: BlockSource -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngineSource source = withSources Playing source Nothing

withEngineLayers :: BlockSource -> AmbienceSource -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngineLayers = withEngineLayersStarting Playing

withEngineLayersStarting :: Transport -> BlockSource -> AmbienceSource -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngineLayersStarting initialTransport source ambient = withSources initialTransport source (Just ambient)

withSources :: Transport -> BlockSource -> Maybe AmbienceSource -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withSources initialTransport source ambient tracks seed initialVolume action = bracket acquire release action
  where
    acquire = mask $ \restore -> do
      ready <- newEmptyMVar
      bootFinished <- newEmptyMVar
      supplier <- forkIOWithUnmask $ \unmask -> mask_ $ do
        boot <- try openEngine
        case boot of
          Left e -> putMVar ready (Left (e :: SomeException))
          Right engine -> do
            myThreadId >>= putMVar (supplierThread engine)
            putMVar ready (Right engine)
            runSupplier engine (unmask (supplyLoop engine))
        putMVar bootFinished ()
      -- Acquisition may itself be cancelled while a decoder/source is waiting.
      -- The supplier owns cleanup before the caller observes that failure.
      boot <- restore (takeMVar ready) `onException` (killThread supplier >> readMVar bootFinished)
      either throwIO pure boot
    openEngine = mask_ $ do
      override <- lookupEnv "GAMEIO_AUDIO_WORKER"
      worker <- case override of
        Just path -> pure path
        Nothing -> (</> "gameio-audio-worker.exe") . takeDirectory <$> getExecutablePath
      device <- A.openIsolatedAudio worker A.defaultAudioConfig
      (do
          let initialBag = newBag seed tracks
              n = NE.length tracks
          (first, b1, bad1) <- selectDeck device source n Set.empty initialBag
          (second, b2, bad2) <- selectDeck device source n bad1 b1
          layer <- traverse (openAmbience device) ambient
          now <- getMonotonicTimeNSec
          ref <- newIORef State
            { audioDevice = device,
              current = first,
              prepared = second,
              bag = b2,
              librarySize = n,
              rejected = bad2,
              playing = Paused,
              master = initialVolume,
              blending = False,
              warning = Nothing,
              transitions = 0,
              supplyPasses = 0,
              ambience = layer,
              environment = Nothing,
              musicMultiplier = 1,
              gainClock = now
            }
          mutex <- newMVar ()
          initial <- snapshotState ref
          engine <- Engine ref mutex source ambient <$> newIORef initial <*> newIORef False <*> newIORef Nothing <*> newEmptyMVar <*> newEmptyMVar
          setTransport engine initialTransport
          pure engine
        ) `onException` A.closeAudio device
    release engine = mask_ $ do
      writeIORef (closing engine) True
      -- Let the supplier finish its current pipe transaction before shutdown.
      -- A blocked custom source still has a bounded opportunity to return.
      completed <- timeout 2000000 (readMVar (finished engine))
      case completed of
        Just result -> either throwIO pure result
        Nothing -> do
          readMVar (supplierThread engine) >>= killThread
          readMVar (finished engine) >>= either throwIO pure

closeState :: IORef State -> IO ()
closeState ref = readIORef ref >>= A.closeAudio . audioDevice

runSupplier :: Engine -> IO () -> IO ()
runSupplier engine work = mask_ $ do
  raw <- try work
  stopping <- readIORef (closing engine)
  let outcome = case raw of
        Left e | stopping, Just ThreadKilled <- fromException e -> Right ()
        _ -> raw
  case outcome of Left e -> writeIORef (failure engine) (Just e); Right () -> pure ()
  cleanup <- try $ withMVar (stateLock engine) $ \_ -> do
    writeIORef (closing engine) True
    closeState (stateRef engine)
  let result = case outcome of Left e -> Left e; Right () -> cleanup
  case result of Left e -> writeIORef (failure engine) (Just e); Right () -> pure ()
  putMVar (finished engine) result

supplyLoop :: Engine -> IO ()
supplyLoop engine = do
  continue <- withMVar (stateLock engine) $ \_ -> mask_ $ do
    stop <- readIORef (closing engine)
    if stop then pure False else do
      pumpState (blockSource engine) (ambienceSource engine) (stateRef engine)
      refreshSnapshot engine
      pure True
  when continue (threadDelay 5000 >> supplyLoop engine)

checkEngine :: Engine -> IO ()
checkEngine engine = do
  problem <- readIORef (failure engine)
  mapM_ throwIO problem
  stopped <- readIORef (closing engine)
  when stopped (throwIO EngineClosed)

withState :: Engine -> (IORef State -> IO a) -> IO a
withState engine action = withMVar (stateLock engine) $ \_ -> mask_ $ do
  checkEngine engine
  result <- action (stateRef engine)
  refreshSnapshot engine
  pure result

refreshSnapshot :: Engine -> IO ()
refreshSnapshot engine = snapshotState (stateRef engine) >>= writeIORef (latestSnapshot engine)

closeDeck :: Deck -> IO ()
closeDeck = A.stopVoice . deckVoice

deckVoice :: Deck -> A.Voice
deckVoice deck = A.streamVoice $ case voice deck of
  Recorded stream _ _ _ _ -> stream
  Generated stream _ _ _ -> stream

openDeck :: A.Audio -> BlockSource -> Track -> IO Deck
openDeck device source track = case trackSource track of
  Procedural preset -> openGenerated device source track preset (Just (secondsValue (presetDuration preset)))
  Continuous preset -> openGenerated device source track preset Nothing
  LocalFile file -> mask $ \restore -> do
    size <- getFileSize file
    when (size > 128 * 1024 * 1024) (throwIO RecordingTooLarge)
    recording <- restore $ case map toLower (takeExtension file) of
      ".wav" -> A.loadWavSource file
      ".mp3" -> A.loadMp3Source file
      ".ogg" -> A.loadOggVorbisSource file
      _ -> throwIO InvalidRecording
    decoder <- A.openPCMReader recording 0
    stream <- openPausedStream device (A.sourceSampleRate recording) (A.sourceChannels recording)
    let lengthSeconds = fromIntegral (A.sourceFrames recording) / fromIntegral (A.sourceSampleRate recording)
        deck = Deck track (Recorded stream recording decoder 0 0) (Just lengthSeconds) False
    restore (fill source deck) `onException` closeDeck deck

openGenerated :: A.Audio -> BlockSource -> Track -> Preset -> Maybe Double -> IO Deck
openGenerated device source track preset extent = mask_ $ do
  stream <- openPausedStream device sampleRate A.Stereo
  let deck = Deck track (Generated stream preset 0 0) extent False
  fill source deck `onException` closeDeck deck

openPausedStream :: A.Audio -> Int -> A.Channels -> IO A.AudioStream
openPausedStream device rate channels = mask_ $ do
  stream <- A.openStream device rate channels queuedFrames
  let active = A.streamVoice stream
  A.setVoiceGain active 0 `onException` A.stopVoice active
  pure stream

openAmbience :: A.Audio -> AmbienceSource -> IO Ambience
openAmbience device source = mask_ $ do
  stream <- openPausedStream device sampleRate A.Stereo
  fillAmbience source Nothing (Ambience stream 0)
    `onException` A.stopVoice (A.streamVoice stream)

fillAmbience :: AmbienceSource -> Maybe EnvironmentInput -> Ambience -> IO Ambience
fillAmbience source input layer = do
  cursor <- go (2 :: Int) (ambienceFrames layer)
  pure layer {ambienceFrames = cursor}
  where
    stream = ambienceStream layer
    go remaining cursor
      | remaining <= 0 = pure cursor
      | otherwise = do
          available <- A.streamWritableFrames stream
          if available < blockFrames then pure cursor else do
            samples <- source input blockFrames
            submitBlock stream samples
            go (remaining - 1) (cursor + fromIntegral blockFrames)

-- Only synchronous decoding/file errors are recoverable. Cancellation is never
-- swallowed. Every attempt removes a failing TrackId, so failure is bounded.
selectDeck :: A.Audio -> BlockSource -> Int -> Set.Set TrackId -> Bag Track -> IO (Deck, Bag Track, Set.Set TrackId)
selectDeck device renderer remaining bad source
  | remaining <= 0 = throwIO NoPlayableTracks
  | otherwise = do
      let (track, rest) = draw source
      if Set.member (trackId track) bad
        then reject track bad rest
        else do
          result <- tryJust recoverable (openDeck device renderer track)
          case result of
            Right deck -> pure (deck, rest, bad)
            Left _ -> reject track (Set.insert (trackId track) bad) rest
  where
    reject track failures rest = case exclude track rest of
      Nothing -> throwIO NoPlayableTracks
      Just filtered -> selectDeck device renderer (remaining - 1) failures filtered
    recoverable exception
      | Just (_ :: SomeAsyncException) <- fromException exception = Nothing
      | Just issue <- fromException exception = case issue of
          A.InvalidWav _ -> Just exception
          A.UnsupportedWavFormat _ _ _ -> Just exception
          A.InvalidOgg _ -> Just exception
          A.InvalidVorbis _ -> Just exception
          A.UnsupportedVorbis _ -> Just exception
          A.InvalidMp3 _ -> Just exception
          A.UnsupportedMp3 _ -> Just exception
          A.AudioDecodeTooLarge _ -> Just exception
          A.InvalidSampleRate _ -> Just exception
          A.InvalidFrameCount _ -> Just exception
          A.InvalidSample _ -> Just exception
          A.IncompletePCMFrame -> Just exception
          _ -> Nothing
      | otherwise = Just exception

startDeck :: Deck -> IO Deck
startDeck deck@Deck {running = True} = pure deck
startDeck deck = do
  ifVoiceLive (A.resumeVoice (deckVoice deck))
  pure deck {running = True}

pauseDeck :: Deck -> IO Deck
pauseDeck deck@Deck {running = False} = pure deck
pauseDeck deck = do
  ifVoiceLive (A.pauseVoice (deckVoice deck))
  pure deck {running = False}

deckPosition :: Deck -> IO Double
deckPosition deck = do
  progress <- A.readVoice (deckVoice deck)
  let origin = case voice deck of
        Recorded _ _ _ first _ -> fromIntegral first
        Generated _ _ first _ -> first
      frame = origin + fromIntegral (A.voiceSourceFrame progress)
  pure (fromIntegral frame / fromIntegral (A.voiceSourceRate progress))

feed :: BlockSource -> Deck -> IO Deck
feed _ deck@Deck {running = False} = pure deck
feed source deck = fill source deck

-- May fill a paused stream during startup/seek. At most two halves per pass;
-- queue depth and work are bounded even if the device consumes concurrently.
fill :: BlockSource -> Deck -> IO Deck
fill _ deck@Deck {voice = Recorded stream recording decoder origin offset} = do
  nextFrame <- go (2 :: Int) offset
  pure deck {voice = Recorded stream recording decoder origin nextFrame}
  where
    extent = A.sourceFrames recording
    go remaining cursor
      | remaining <= 0 || cursor >= extent = pure cursor
      | otherwise = do
          available <- A.streamWritableFrames stream
          let count = min blockFrames (extent - cursor)
          if available < count then pure cursor else do
            chunk <- A.readPCMBlock decoder count >>= maybe (throwIO InvalidRecording) pure
            written <- A.writeStream stream chunk
            when (written /= count) (throwIO InvalidRecording)
            let nextFrame = cursor + written
            when (nextFrame == extent) (A.finishStream stream)
            go (remaining - 1) nextFrame
fill source deck@Deck {voice = Generated stream preset origin offset} = do
  nextFrame <- go (2 :: Int) offset
  pure deck {voice = Generated stream preset origin nextFrame}
  where
    go remaining cursor
      | remaining <= 0 = pure cursor
      | otherwise = do
          available <- A.streamWritableFrames stream
          if available < blockFrames then pure cursor else do
            samples <- source preset cursor blockFrames
            submitBlock stream samples
            go (remaining - 1) (cursor + fromIntegral blockFrames)

submitBlock :: A.AudioStream -> V.Vector Float -> IO ()
submitBlock stream samples = do
  when (V.length samples /= 2 * blockFrames) (throwIO InvalidGeneratedBlock)
  pcm <- A.generatePCM sampleRate A.Stereo blockFrames (samples V.!) `catch` \issue -> case issue of
    A.InvalidSample _ -> throwIO InvalidGeneratedBlock
    _ -> throwIO issue
  written <- A.writeStream stream pcm
  -- Each supplier is serialized by stateLock. Capacity can increase while
  -- synthesis runs and only this producer can consume capacity.
  when (written /= blockFrames) (throwIO InvalidGeneratedBlock)

gain :: Deck -> Double -> IO ()
gain deck value = ifVoiceLive (A.setVoiceGain (deckVoice deck) (realToFrac value))

ifVoiceLive :: IO () -> IO ()
ifVoiceLive action = action `catch` \issue -> case issue of
  A.VoiceInactive -> pure ()
  _ -> throwIO issue

setTransport :: Engine -> Transport -> IO ()
setTransport engine requested = withState engine $ \ref -> do
  s <- readIORef ref
  if playing s == requested
    then pure ()
    else do
      -- Set the current effective gains before Resume, including mute changes
      -- made while paused. Pausing/resuming never replays or regenerates PCM.
      applyGains s
      first <- if requested == Playing then startDeck (current s) else pauseDeck (current s)
      second <- if requested == Playing && blending s then startDeck (prepared s) else pauseDeck (prepared s)
      let transport = if requested == Playing then A.resumeVoice else A.pauseVoice
      mapM_ (transport . A.streamVoice . ambienceStream) (ambience s)
      writeIORef ref s {current = first, prepared = second, playing = requested}

setVolume :: Engine -> Volume -> IO ()
setVolume engine value = withState engine $ \ref -> do
  s <- readIORef ref
  let updated = s {master = value}
  applyGains updated
  writeIORef ref updated

setAmbience :: Engine -> Maybe EnvironmentInput -> IO ()
setAmbience engine input = withState engine $ \ref ->
  modifyIORef' ref (\s -> s {environment = if isJust (ambience s) then input else Nothing})

-- A continuous outgoing deck has no automatic handover. A continuous incoming
-- deck cannot end inside an overlap, so the outgoing duration bounds its width.
deckMix :: Deck -> Deck -> Double -> Mix
deckMix outgoing incoming elapsed = case deckLength outgoing of
  Nothing -> Solo
  Just extent ->
    let timeValue = either (const zeroSeconds) id . seconds
     in mixBetween (timeValue extent) (timeValue (maybe extent id (deckLength incoming))) (timeValue elapsed)

applyGains :: State -> IO ()
applyGains s = do
  elapsed <- deckPosition (current s)
  let timeValue = either (const zeroSeconds) id . seconds
      level = 0.66 * volumeValue (master s) * musicMultiplier s
      attack = fadeIn (timeValue elapsed)
      curve = deckMix (current s) (prepared s) elapsed
      (outgoing, incoming) = case curve of Solo -> (attack, 0); Crossfade a b -> (a * attack, b); Advance -> (0, 1)
  gain (current s) (level * outgoing)
  gain (prepared s) (level * incoming)
  mapM_ (\layer -> A.setVoiceGain (A.streamVoice (ambienceStream layer)) (realToFrac (0.66 * volumeValue (master s)))) (ambience s)

next :: Engine -> IO ()
next engine = withState engine (nextState (blockSource engine))

nextState :: BlockSource -> IORef State -> IO ()
nextState source ref = mask_ $ do
  s <- readIORef ref
  -- Acquire replacement before releasing the old deck: a failure leaves the
  -- existing state valid and owned by the enclosing bracket.
  (newDeck, rest, bad) <- selectDeck (audioDevice s) source (librarySize s) (rejected s) (bag s)
  gain (prepared s) (0.66 * volumeValue (master s) * musicMultiplier s) `onException` closeDeck newDeck
  first <- (if playing s == Playing then startDeck (prepared s) else pure (prepared s)) `onException` closeDeck newDeck
  writeIORef ref s {current = first, prepared = newDeck, bag = rest, rejected = bad, blending = False, transitions = transitions s + 1}
  closeDeck (current s)

replaceLibrary :: Engine -> NonEmpty Track -> Int -> IO ()
replaceLibrary engine tracks seed = withState engine $ \ref -> do
  s <- readIORef ref
  let count = NE.length tracks
  (a, b1, bad1) <- selectDeck (audioDevice s) (blockSource engine) count Set.empty (newBag seed tracks)
  (b, b2, bad2) <- selectDeck (audioDevice s) (blockSource engine) count bad1 b1 `onException` closeDeck a
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
pump = checkEngine

pumpState :: BlockSource -> Maybe AmbienceSource -> IORef State -> IO ()
pumpState source ambient ref = do
  previous <- readIORef ref
  now <- getMonotonicTimeNSec
  let dt = min 0.05 (fromIntegral (now - gainClock previous) / 1e9)
      target = if isJust (environment previous) then 0.40 else 1
      delta = max (-0.015) (min 0.015 ((target - musicMultiplier previous) * (1 - exp (negate (dt / 0.35)))))
      eased = if playing previous == Playing then musicMultiplier previous + delta else musicMultiplier previous
      s = previous {gainClock = now, musicMultiplier = eased}
  writeIORef ref s
  if playing s == Paused
    then pure ()
    else do
      first <- feed source (current s)
      layer <- case (ambient, ambience s) of
        (Just renderer, Just active) -> Just <$> fillAmbience renderer (environment s) active
        _ -> pure (ambience s)
      let supplied = s {current = first, ambience = layer, supplyPasses = supplyPasses s + 1}
      writeIORef ref supplied
      elapsed <- deckPosition first
      let curve = deckMix first (prepared s) elapsed
      applyGains supplied
      case curve of
        Advance -> nextState source ref
        Solo -> pure ()
        Crossfade _ _ -> do
          -- applyGains already set both levels before the incoming Resume.
          second <- startDeck (prepared s) >>= feed source
          writeIORef ref supplied {prepared = second, blending = True}

snapshot :: Engine -> IO AudioSnapshot
snapshot engine = checkEngine engine >> readIORef (latestSnapshot engine)

snapshotState :: IORef State -> IO AudioSnapshot
snapshotState ref = do
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
seekForCheck engine target = withState engine $ \ref -> do
  when (isNaN target || isInfinite target) (throwIO (userError "Audio seek must be finite."))
  s <- readIORef ref
  let deck = current s; bounded = max 0 (maybe target (\extent -> min (extent - 0.01) target) (deckLength deck))
  let resetAt = resetDeck (audioDevice s) (blockSource engine)
  updated <- resetAt bounded deck
  second <- resetAt 0 (prepared s) `onException` closeDeck updated
  let reset = s {current = updated, prepared = second, blending = False}
      start = if playing s == Playing then startDeck updated else pure updated
  first <- (applyGains reset >> start) `onException` (closeDeck updated `finally` closeDeck second)
  writeIORef ref reset {current = first}
  closeDeck (current s) `finally` closeDeck (prepared s)
  pumpState (blockSource engine) (ambienceSource engine) ref

-- The replacement owns its own queue. A failed seek leaves the old deck live.
resetDeck :: A.Audio -> BlockSource -> Double -> Deck -> IO Deck
resetDeck device source target deck = do
  replacement <- case voice deck of
    Recorded _ recording _ _ _ -> do
      let rate = A.sourceSampleRate recording
          cursor = floor (target * fromIntegral rate)
      decoder <- A.openPCMReader recording cursor
      stream <- openPausedStream device rate (A.sourceChannels recording)
      pure deck {voice = Recorded stream recording decoder cursor cursor, running = False}
    Generated _ preset _ _ -> do
      replacement <- openPausedStream device sampleRate A.Stereo
      let cursor = floor (target * fromIntegral sampleRate)
      pure deck {voice = Generated replacement preset cursor cursor, running = False}
  fill source replacement `onException` closeDeck replacement

diagnosticsForCheck :: Engine -> IO AudioDiagnostics
diagnosticsForCheck engine = withState engine $ \ref -> do
  s <- readIORef ref
  (nextFrame, completed) <- case voice (current s) of
    Generated stream _ origin cursor -> do
      progress <- A.readVoice (A.streamVoice stream)
      pure (Just cursor, Just (origin + fromIntegral (A.voiceSourceFrame progress)))
    Recorded _ _ _ _ _ -> pure (Nothing, Nothing)
  pure (AudioDiagnostics (supplyPasses s) nextFrame completed queuedFrames (ambienceFrames <$> ambience s) (isJust (environment s)) (musicMultiplier s))

deviceForCheck :: Engine -> IO (Maybe Int, A.AudioStats)
deviceForCheck engine = do
  checkEngine engine
  device <- audioDevice <$> readIORef (stateRef engine)
  stats <- A.readAudioStats device
  pure (A.audioWorkerPid device, stats)
