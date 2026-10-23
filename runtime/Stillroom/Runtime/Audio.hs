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
    pump,
    setTransport,
    setVolume,
    setAmbience,
    next,
    replaceLibrary,
    snapshot,
    seekForCheck,
    diagnosticsForCheck,
  )
where

import Control.Exception
import Control.Concurrent (MVar, ThreadId, forkOSWithUnmask, killThread, myThreadId, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay, withMVar)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Vector.Storable qualified as V
import Foreign
import Foreign.C (withCString)
import GHC.Clock (getMonotonicTimeNSec)
import Raylib.Core (c'memFree)
import Raylib.Core.Audio qualified as A
import Raylib.Internal.Foreign (pop, withFreeable)
import Raylib.Types (AudioStream, Music (..), MusicContextType (..))
import Stillroom.Domain
import Stillroom.Audio.Environment (EnvironmentInput)
import Stillroom.Shuffle
import Stillroom.Synthesis
import System.Directory (getFileSize)
import System.FilePath (takeExtension)

-- Calls are serialized ordinary Haskell code, never the native device callback.
-- Supply uses the bound worker; synchronous startup/seek/library controls can
-- also render. A source must finish each bounded block and support seeks.
type BlockSource = Preset -> Integer -> Int -> IO (V.Vector Float)

type AmbienceSource = Maybe EnvironmentInput -> Int -> IO (V.Vector Float)

blockFrames, queuedFrames :: Int
blockFrames = 4096
queuedFrames = 2 * blockFrames

data Voice = Recorded !Music !(Ptr ()) | Generated !AudioStream !Preset !Integer !Integer

data Ambience = Ambience {ambienceStream :: !AudioStream, ambienceFrames :: !Integer}

data Deck = Deck
  { deckTrack :: !Track,
    voice :: !Voice,
    deckLength :: !(Maybe Double),
    running :: !Bool
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

-- The executable uses -threaded and -N2: a blocking drawing call must not occupy
-- the only capability. Device starvation from OS scheduling, GC or a slow
-- injected source/decoder remains possible; this is not a real-time callback.
withEngineSource :: BlockSource -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngineSource source = withSources source Nothing

withEngineLayers :: BlockSource -> AmbienceSource -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngineLayers source ambient = withSources source (Just ambient)

withSources :: BlockSource -> Maybe AmbienceSource -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withSources source ambient tracks seed initialVolume action = bracket acquire release action
  where
    acquire = mask $ \restore -> do
      ready <- newEmptyMVar
      bootFinished <- newEmptyMVar
      supplier <- forkOSWithUnmask $ \unmask -> mask_ $ do
        boot <- try openEngine
        case boot of
          Left e -> putMVar ready (Left (e :: SomeException))
          Right engine -> do
            myThreadId >>= putMVar (supplierThread engine)
            putMVar ready (Right engine)
            runSupplier engine (unmask (supplyLoop engine))
        putMVar bootFinished ()
      -- Acquisition may itself be cancelled while a decoder/source is waiting.
      -- Its bound worker owns cleanup before the caller observes that failure.
      boot <- restore (takeMVar ready) `onException` (killThread supplier >> readMVar bootFinished)
      either throwIO pure boot
    -- Device initialization and destruction share one bound OS thread. Stream
    -- controls may come from the UI under stateLock; raylib serializes those
    -- buffer operations against its native device thread with its own mutex.
    openEngine = mask_ $ do
      A.initAudioDevice
      ready <- A.isAudioDeviceReady
      when (not ready) (A.closeAudioDevice Nothing >> throwIO (userError "No audio output device is available."))
      A.setAudioStreamBufferSizeDefault blockFrames
      let initialBag = newBag seed tracks; n = NE.length tracks
      (first, b1, bad1) <- selectDeck source n Set.empty initialBag `onException` A.closeAudioDevice Nothing
      (second, b2, bad2) <- selectDeck source n bad1 b1 `onException` (closeDeck first >> A.closeAudioDevice Nothing)
      layer <- traverse openAmbience ambient `onException` (closeDeck first >> closeDeck second >> A.closeAudioDevice Nothing)
      now <- getMonotonicTimeNSec
      ref <- newIORef (State first second b2 n bad2 Paused initialVolume False Nothing 0 0 layer Nothing 1 now)
      let closeOwned = closeState ref
      (do
          mutex <- newMVar ()
          initial <- snapshotState ref
          engine <- Engine ref mutex source ambient <$> newIORef initial <*> newIORef False <*> newIORef Nothing <*> newEmptyMVar <*> newEmptyMVar
          setTransport engine Playing
          pure engine
        ) `onException` closeOwned
    release engine = mask_ $ do
      -- An interrupted waiter cannot abandon resource ownership: the supplier
      -- still observes closing and runs its own finalizer. Normal release joins
      -- it before returning, and never unloads a stream while supply uses it.
      writeIORef (closing engine) True
      -- State commits/native submissions are masked. Cancellation is delivered
      -- at a safe wait or after the current bounded block; a blocked source can
      -- also be interrupted. The worker then closes state under the same mutex.
      readMVar (supplierThread engine) >>= killThread
      readMVar (finished engine) >>= either throwIO pure

closeState :: IORef State -> IO ()
closeState ref = do
  s <- readIORef ref
  closeDeck (current s) `finally` (closeDeck (prepared s) `finally` (mapM_ closeAmbience (ambience s) `finally` A.closeAudioDevice Nothing))

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
closeDeck Deck {voice = Recorded music bytes} = releaseMusic music `finally` free bytes
closeDeck Deck {voice = Generated stream _ _ _} = withFreeable stream A.c'unloadAudioStream

-- Compatibility with the exact raylib revision bundled by h-raylib 5.6.0.0:
-- its WAV destructor omits the allocated context. Keep this repair isolated
-- and recheck on dependency updates. FLAC is disabled in this pinned build.
releaseMusic :: Music -> IO ()
releaseMusic music = do
  withFreeable music A.c'unloadMusicStream
  when (music'ctxType music == MusicAudioWAV) (c'memFree (music'ctxData music))

openDeck :: BlockSource -> Track -> IO Deck
openDeck source track = case trackSource track of
  Procedural preset -> openGenerated source track preset (Just (secondsValue (presetDuration preset)))
  Continuous preset -> openGenerated source track preset Nothing
  LocalFile file -> mask $ \restore -> do
    size <- getFileSize file
    when (size > 128 * 1024 * 1024) (throwIO RecordingTooLarge)
    bytes <- restore (BS.readFile file)
    buffer <- mallocBytes (BS.length bytes)
    music <-
      ( do
          BS.useAsCString bytes $ \input -> copyBytes buffer (castPtr input) (BS.length bytes)
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
        let recording = music {music'looping = False}
        A.playMusicStream recording
        A.pauseMusicStream recording
        A.updateMusicStream recording
        pure (Deck track (Recorded recording buffer) (Just lengthSeconds) False)
      )
      `onException` release

openGenerated :: BlockSource -> Track -> Preset -> Maybe Double -> IO Deck
openGenerated source track preset extent = mask_ $ do
  stream <- openPausedStream
  -- Pinned Play invalidates both queued halves. openPausedStream already did
  -- Play -> Pause; ordinary starts must only Resume after both halves are full.
  fill source (Deck track (Generated stream preset 0 0) extent False)
    `onException` withFreeable stream A.c'unloadAudioStream

openPausedStream :: IO AudioStream
openPausedStream = mask_ $ do
  stream <- A.loadAudioStream (fromIntegral sampleRate) 32 2
  (do
      valid <- A.isAudioStreamValid stream
      when (not valid) (throwIO (userError "Cannot create an audio stream."))
      A.setAudioStreamVolume stream 0
      A.playAudioStream stream
      A.pauseAudioStream stream
      pure stream
    ) `onException` withFreeable stream A.c'unloadAudioStream

openAmbience :: AmbienceSource -> IO Ambience
openAmbience source = mask_ $ do
  stream <- openPausedStream
  fillAmbience source Nothing (Ambience stream 0)
    `onException` withFreeable stream A.c'unloadAudioStream

closeAmbience :: Ambience -> IO ()
closeAmbience layer = withFreeable (ambienceStream layer) A.c'unloadAudioStream

fillAmbience :: AmbienceSource -> Maybe EnvironmentInput -> Ambience -> IO Ambience
fillAmbience source input layer = do
  cursor <- go (2 :: Int) (ambienceFrames layer)
  pure layer {ambienceFrames = cursor}
  where
    stream = ambienceStream layer
    go remaining cursor
      | remaining <= 0 = pure cursor
      | otherwise = do
          available <- A.isAudioStreamProcessed stream
          if not available then pure cursor else do
            samples <- source input blockFrames
            submitBlock stream samples
            go (remaining - 1) (cursor + fromIntegral blockFrames)

-- Only synchronous decoding/file errors are recoverable. Cancellation is never
-- swallowed. Every attempt removes a failing TrackId, so failure is bounded.
selectDeck :: BlockSource -> Int -> Set.Set TrackId -> Bag Track -> IO (Deck, Bag Track, Set.Set TrackId)
selectDeck renderer remaining bad source
  | remaining <= 0 = throwIO NoPlayableTracks
  | otherwise = do
      let (track, rest) = draw source
      if Set.member (trackId track) bad
        then reject track bad rest
        else do
          result <- tryJust recoverable (openDeck renderer track)
          case result of
            Right deck -> pure (deck, rest, bad)
            Left _ -> reject track (Set.insert (trackId track) bad) rest
  where
    reject track failures rest = case exclude track rest of
      Nothing -> throwIO NoPlayableTracks
      Just filtered -> selectDeck renderer (remaining - 1) failures filtered
    recoverable exception = case fromException exception :: Maybe AsyncException of
      Just _ -> Nothing
      Nothing -> Just exception

startDeck :: Deck -> IO Deck
startDeck deck@Deck {running = True} = pure deck
startDeck deck = do
  case voice deck of
    Recorded music _ -> A.resumeMusicStream music
    Generated stream _ _ _ -> A.resumeAudioStream stream
  pure deck {running = True}

pauseDeck :: Deck -> IO Deck
pauseDeck deck@Deck {running = False} = pure deck
pauseDeck deck = do
  case voice deck of
    Recorded music _ -> A.pauseMusicStream music
    Generated stream _ _ _ -> A.pauseAudioStream stream
  pure deck {running = False}

deckPosition :: Deck -> IO Double
deckPosition deck = case voice deck of
  Recorded music _ -> do
    nativePlaying <- A.isMusicStreamPlaying music
    if running deck && not nativePlaying
      then pure (maybe 0 id (deckLength deck))
      else realToFrac <$> A.getMusicTimePlayed music
  -- Public AudioStream APIs expose no consumed-frame cursor. Each refill after
  -- the initial two corresponds to a retired whole subbuffer. This conservative
  -- block clock cannot run ahead merely because the UI or supplier was stalled.
  -- It counts retired source PCM, not the hardware output clock. Normally it
  -- trails by one source block; between observations at most the two queued
  -- blocks can retire. Device/converter latency is not measured. Raylib can
  -- raise the requested half size for a large device period and pad our block;
  -- that silence is deliberately not counted as produced musical PCM.
  Generated _ _ origin nextFrame -> pure (fromIntegral (max origin (nextFrame - fromIntegral queuedFrames)) / fromIntegral sampleRate)

feed :: BlockSource -> Deck -> IO Deck
feed _ deck@Deck {running = False} = pure deck
feed source deck = fill source deck

-- May fill a paused stream during startup/seek. At most two halves per pass;
-- queue depth and work are bounded even if the device consumes concurrently.
fill :: BlockSource -> Deck -> IO Deck
fill _ deck@Deck {voice = Recorded music _} = A.updateMusicStream music >> pure deck
fill source deck@Deck {voice = Generated stream preset origin offset} = do
  nextFrame <- go (2 :: Int) offset
  pure deck {voice = Generated stream preset origin nextFrame}
  where
    go remaining cursor
      | remaining <= 0 = pure cursor
      | otherwise = do
          available <- A.isAudioStreamProcessed stream
          if not available then pure cursor else do
            samples <- source preset cursor blockFrames
            submitBlock stream samples
            go (remaining - 1) (cursor + fromIntegral blockFrames)

submitBlock :: AudioStream -> V.Vector Float -> IO ()
submitBlock stream samples = do
  when (V.length samples /= 2 * blockFrames || V.any (\x -> isNaN x || isInfinite x) samples) (throwIO InvalidGeneratedBlock)
  V.unsafeWith samples $ \pointer -> A.updateAudioStream stream (castPtr pointer) blockFrames

gain :: Deck -> Double -> IO ()
gain deck value = case voice deck of
  Recorded music _ -> A.setMusicVolume music (realToFrac value)
  Generated stream _ _ _ -> A.setAudioStreamVolume stream (realToFrac value)

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
      mapM_ (\layer -> (if requested == Playing then A.resumeAudioStream else A.pauseAudioStream) (ambienceStream layer)) (ambience s)
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
  mapM_ (\layer -> A.setAudioStreamVolume (ambienceStream layer) (realToFrac (0.66 * volumeValue (master s)))) (ambience s)

next :: Engine -> IO ()
next engine = withState engine (nextState (blockSource engine))

nextState :: BlockSource -> IORef State -> IO ()
nextState source ref = mask_ $ do
  s <- readIORef ref
  -- Acquire replacement before releasing the old deck: a failure leaves the
  -- existing state valid and owned by the enclosing bracket.
  (newDeck, rest, bad) <- selectDeck source (librarySize s) (rejected s) (bag s)
  gain (prepared s) (0.66 * volumeValue (master s) * musicMultiplier s) `onException` closeDeck newDeck
  first <- (if playing s == Playing then startDeck (prepared s) else pure (prepared s)) `onException` closeDeck newDeck
  writeIORef ref s {current = first, prepared = newDeck, bag = rest, rejected = bad, blending = False, transitions = transitions s + 1}
  closeDeck (current s)

replaceLibrary :: Engine -> NonEmpty Track -> Int -> IO ()
replaceLibrary engine tracks seed = withState engine $ \ref -> do
  s <- readIORef ref
  let count = NE.length tracks
  (a, b1, bad1) <- selectDeck (blockSource engine) count Set.empty (newBag seed tracks)
  (b, b2, bad2) <- selectDeck (blockSource engine) count bad1 b1 `onException` closeDeck a
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
  updated <- resetDeck (blockSource engine) bounded deck
  second <- resetDeck (blockSource engine) 0 (prepared s)
  let reset = s {current = updated, prepared = second, blending = False}
  writeIORef ref reset
  applyGains reset
  first <- if playing s == Playing then startDeck updated else pure updated
  writeIORef ref reset {current = first}
  pumpState (blockSource engine) (ambienceSource engine) ref

-- Reset invalidates queued PCM before changing the dry cursor. This source API
-- is random-access; stateful renderers must implement their own deterministic
-- reset/reconstruction for the requested offset.
resetDeck :: BlockSource -> Double -> Deck -> IO Deck
resetDeck source target deck = do
  gain deck 0
  case voice deck of
    Recorded music _ -> do
      A.playMusicStream music
      A.pauseMusicStream music
      A.seekMusicStream music (realToFrac target)
      fill source deck {running = False}
    Generated stream preset _ _ -> do
      A.playAudioStream stream
      A.pauseAudioStream stream
      let cursor = floor (target * fromIntegral sampleRate)
      fill source deck {voice = Generated stream preset cursor cursor, running = False}

diagnosticsForCheck :: Engine -> IO AudioDiagnostics
diagnosticsForCheck engine = withState engine $ \ref -> do
  s <- readIORef ref
  let (nextFrame, completed) = case voice (current s) of
        Generated _ _ origin cursor -> (Just cursor, Just (max origin (cursor - fromIntegral queuedFrames)))
        Recorded _ _ -> (Nothing, Nothing)
  pure (AudioDiagnostics (supplyPasses s) nextFrame completed queuedFrames (ambienceFrames <$> ambience s) (isJust (environment s)) (musicMultiplier s))
