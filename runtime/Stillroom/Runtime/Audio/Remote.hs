{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | The desktop owns the window. This child owns the score, sample banks,
-- decoders and two-deck engine. Its device worker owns the WASAPI deadline.
module Stillroom.Runtime.Audio.Remote
  ( Engine,
    Audio.AudioFailure (..),
    Audio.AudioSnapshot (..),
    withEngine,
    setTransport,
    setVolume,
    setAmbience,
    next,
    replaceLibrary,
    snapshot,
    seekForCheck,
    deviceForCheck,
    sourceWorkerPid,
    runAudioService,
  )
where

import Control.Concurrent (MVar, forkIOWithUnmask, modifyMVarMasked, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeAsyncException, SomeException, bracket, catch, displayException, fromException, mask, onException, throwIO, toException, try, tryJust)
import Control.Monad (forM_, unless, void, when)
import Data.Binary (Binary (..), decodeOrFail, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Word (Word16, Word32, Word8)
import GameIO.Audio qualified as Device
import GHC.Generics (Generic)
import Stillroom.Audio.Environment (EnvironmentInput, environmentInput, environmentSensors)
import Stillroom.Domain
import Stillroom.Runtime.Audio qualified as Audio
import Stillroom.Runtime.Audio.Builtin qualified as Builtin
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (NoBuffering), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering, stdin, stdout)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

data Child = Child !Handle !Handle !ProcessHandle
data Engine = Engine !(MVar (Maybe Child)) !(Maybe Int)

data TrackSource = File !FilePath | Score !Bool !Word8
  deriving stock Generic
  deriving anyclass Binary

data TrackValue = TrackValue !Int !String !TrackSource
  deriving stock Generic
  deriving anyclass Binary

data Command
  = Start !Bool !Int !Double ![TrackValue]
  | ChangeTransport !Bool
  | ChangeVolume !Double
  | ChangeEnvironment !(Maybe (Bool, Double, Double, Double))
  | Next
  | Replace !Int ![TrackValue]
  | ReadSnapshot
  | SeekForCheck !Double
  | ReadDeviceStats
  | Stop
  deriving stock Generic
  deriving anyclass Binary

newtype SnapshotValue = SnapshotValue Audio.AudioSnapshot

instance Binary SnapshotValue where
  put (SnapshotValue (Audio.AudioSnapshot title position duration audible fading transitions warning)) =
    put title >> put position >> put duration >> put audible >> put fading >> put transitions >> put warning
  get = SnapshotValue <$> (Audio.AudioSnapshot <$> get <*> get <*> get <*> get <*> get <*> get <*> get)

data Reply
  = Ack
  | NoTracks
  | Failed !String
  | Snapshot !SnapshotValue
  | DeviceStats !(Maybe Int) !Device.AudioStats
  deriving stock Generic
  deriving anyclass Binary

withEngine :: Transport -> NonEmpty Track -> Int -> Volume -> (Engine -> IO a) -> IO a
withEngine transportValue tracks seed level =
  bracket (openEngine (Start (transportValue == Playing) seed (volumeValue level) (encodeTracks tracks))) closeEngine

sourceWorkerPid :: Engine -> Maybe Int
sourceWorkerPid (Engine _ identity) = identity

setTransport :: Engine -> Transport -> IO ()
setTransport engine transportValue = acknowledged =<< call engine (ChangeTransport (transportValue == Playing))

setVolume :: Engine -> Volume -> IO ()
setVolume engine level = acknowledged =<< call engine (ChangeVolume (volumeValue level))

setAmbience :: Engine -> Maybe EnvironmentInput -> IO ()
setAmbience engine sensors = acknowledged =<< call engine (ChangeEnvironment (fmap environmentSensors sensors))

next :: Engine -> IO ()
next engine = acknowledged =<< call engine Next

replaceLibrary :: Engine -> NonEmpty Track -> Int -> IO ()
replaceLibrary engine tracks seed = acknowledged =<< call engine (Replace seed (encodeTracks tracks))

snapshot :: Engine -> IO Audio.AudioSnapshot
snapshot engine = call engine ReadSnapshot >>= \case
  Snapshot (SnapshotValue current) -> pure current
  _ -> protocolFailure "Unexpected snapshot reply"

seekForCheck :: Engine -> Double -> IO ()
seekForCheck engine target = acknowledged =<< call engine (SeekForCheck target)

deviceForCheck :: Engine -> IO (Maybe Int, Device.AudioStats)
deviceForCheck engine = call engine ReadDeviceStats >>= \case
  DeviceStats identity stats -> pure (identity, stats)
  _ -> protocolFailure "Unexpected device statistics reply"

encodeTracks :: NonEmpty Track -> [TrackValue]
encodeTracks = map trackValue . NE.toList
  where
    trackValue (Track (TrackId identity) name source) = TrackValue identity name $ case source of
      LocalFile path -> File path
      Procedural preset -> Score False (fromIntegral (fromEnum preset))
      Continuous preset -> Score True (fromIntegral (fromEnum preset))

decodeTracks :: [TrackValue] -> IO (NonEmpty Track)
decodeTracks values = do
  supplied <- traverse decodeTrack values
  maybe (throwIO Audio.NoPlayableTracks) pure (NE.nonEmpty supplied)
  where
    decodeTrack (TrackValue identity name value) = do
      source <- case value of
        File path -> pure (LocalFile path)
        Score looping ordinal ->
          case drop (fromIntegral ordinal) [minBound .. maxBound] of
            preset : _ -> pure (if looping then Continuous preset else Procedural preset)
            [] -> protocolFailure "Unknown built-in score"
      pure (Track (TrackId identity) name source)

openEngine :: Command -> IO Engine
openEngine command = mask $ \restore -> do
  request <- encodeFrame command
  executable <- getExecutablePath
  let configuration = (proc executable ["--audio-service"])
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = Inherit,
          close_fds = True,
          detach_console = True
        }
  createProcess configuration >>= \case
    (Just send, Just receive, _, process) -> do
      let child = Child send receive process
      flip onException (abort child) $ do
        mapM_ preparePipe [send, receive]
        restore (exchange 30000000 child request >>= checkedReply >>= acknowledged)
        Engine <$> newMVar (Just child) <*> (fmap fromIntegral <$> getPid process)
    (send, receive, _, process) -> do
      ignoreIO (terminateProcess process)
      forM_ send (ignoreIO . hClose)
      forM_ receive (ignoreIO . hClose)
      void (within 3000000 (waitForProcess process))
      protocolFailure "The source process did not provide two pipes"

call :: Engine -> Command -> IO Reply
call (Engine connection _) command = do
  -- A value that fails its local size check has not touched the live pipe.
  request <- encodeFrame command
  send request
  where
    send request = mask $ \restore -> do
      outcome <- modifyMVarMasked connection $ \case
        Nothing -> pure (Nothing, Left (toException Audio.EngineClosed))
        Just child -> do
          result <- try (restore (exchange 5000000 child request))
          case result of
            Right answer -> pure (Just child, Right answer)
            Left issue -> abort child >> pure (Nothing, Left issue)
      either throwIO checkedReply outcome

closeEngine :: Engine -> IO ()
closeEngine (Engine connection _) = mask $ \restore -> do
  outcome <- modifyMVarMasked connection $ \case
    Nothing -> pure (Nothing, Right ())
    Just child@(Child _ _ process) -> do
      result <- try $ restore $ do
        encodeFrame Stop >>= exchange 5000000 child >>= checkedReply >>= acknowledged
        closePipes child
        stopped <- within 3000000 (waitForProcess process)
        unless (stopped == Just ExitSuccess) (protocolFailure "The source process did not shut down cleanly")
      case result of
        Left (_ :: SomeException) -> abort child
        Right () -> pure ()
      pure (Nothing, result)
  case outcome of
    Left issue | Just (_ :: SomeAsyncException) <- fromException issue -> throwIO issue
    _ -> pure ()

abort :: Child -> IO ()
abort child@(Child _ _ process) = do
  ignoreIO (terminateProcess process)
  closePipes child
  void (within 3000000 (waitForProcess process))

closePipes :: Child -> IO ()
closePipes (Child send receive _) = mapM_ (ignoreIO . hClose) [send, receive]

ignoreIO :: IO () -> IO ()
ignoreIO action = catch action (\(_ :: IOException) -> pure ())

exchange :: Int -> Child -> BL.ByteString -> IO Reply
exchange budget (Child send receive _) request = do
  result <- within budget $ do
    writeFrame send request
    receiveFrame receive >>= maybe (protocolFailure "Source pipe closed before the reply") pure
  maybe (protocolFailure "The source process exceeded the command timeout") pure result

-- Windows pipe reads can block an OS thread. Wait on a separate result so
-- termination closes the pipe even if the reader has not returned yet.
within :: Int -> IO a -> IO (Maybe a)
within budget action = do
  done <- newEmptyMVar
  void $ forkIOWithUnmask $ \unmask -> try (unmask action) >>= putMVar done
  result <- timeout budget (takeMVar done)
  traverse (either (throwIO :: SomeException -> IO a) pure) result

checkedReply :: Reply -> IO Reply
checkedReply NoTracks = throwIO Audio.NoPlayableTracks
checkedReply (Failed message) = throwIO (userError message)
checkedReply other = pure other

acknowledged :: Reply -> IO ()
acknowledged Ack = pure ()
acknowledged _ = protocolFailure "Unexpected acknowledgement"

preparePipe :: Handle -> IO ()
preparePipe handle = hSetBinaryMode handle True >> hSetBuffering handle NoBuffering

type Header = (Word32, Word16, Word32)

prefix :: Word32
prefix = 0x53524155

version :: Word16
version = 1

maximumBody :: Word32
maximumBody = 32 * 1024 * 1024

sendFrame :: Binary value => Handle -> value -> IO ()
sendFrame handle value = encodeFrame value >>= writeFrame handle

encodeFrame :: Binary value => value -> IO BL.ByteString
encodeFrame value = do
  let payload = encode value
      bytes = BL.length payload
  when (bytes < 1 || bytes > fromIntegral maximumBody) (protocolFailure "Source control message exceeds its size limit")
  pure (encode ((prefix, version, fromIntegral bytes) :: Header) <> payload)

writeFrame :: Handle -> BL.ByteString -> IO ()
writeFrame handle frame = BL.hPut handle frame >> hFlush handle

receiveFrame :: Binary value => Handle -> IO (Maybe value)
receiveFrame handle = do
  header <- BS.hGet handle 10
  if BS.null header then pure Nothing else do
    (magic, revision, bytes) <- decodeFrame header
    unless (magic == prefix && revision == version && bytes > 0 && bytes <= maximumBody)
      (protocolFailure "Incompatible source control header")
    payload <- BS.hGet handle (fromIntegral bytes)
    unless (BS.length payload == fromIntegral bytes) (protocolFailure "Truncated source control message")
    Just <$> decodeFrame payload

decodeFrame :: Binary value => BS.ByteString -> IO value
decodeFrame bytes = case decodeOrFail (BL.fromStrict bytes) of
  Right (remaining, _, value) | BL.null remaining -> pure value
  _ -> protocolFailure "Malformed source control message"

protocolFailure :: String -> IO a
protocolFailure = throwIO . userError . ("Stillroom audio source: " ++)

-- | Internal mode of the same executable. Standard output is exclusively
-- protocol traffic. A closed parent pipe releases the device worker.
runAudioService :: IO ()
runAudioService = do
  mapM_ preparePipe [stdin, stdout]
  receiveFrame stdin >>= \case
    Nothing -> pure ()
    Just (Start playing seed gain values) -> do
      admitted <- newIORef False
      outcome <- trySync $ do
        tracks <- decodeTracks values
        level <- checkedVolume gain
        (music, ambience) <- Builtin.loadBuiltinSources (fromIntegral seed)
        Audio.withEngineLayersStarting (transport playing) music ambience tracks seed level $ \engine -> do
          sendFrame stdout Ack
          writeIORef admitted True
          serve engine
      case outcome of
        Right True -> sendFrame stdout Ack
        Right False -> pure ()
        Left issue -> do
          opened <- readIORef admitted
          if opened then throwIO issue else sendFrame stdout (replyError issue)
    Just _ -> protocolFailure "Expected source startup"
  where
    serve engine = receiveFrame stdin >>= \case
      Nothing -> pure False
      Just Stop -> pure True
      Just command -> do
        result <- trySync (execute engine command)
        sendFrame stdout (either replyError id result)
        serve engine

execute :: Audio.Engine -> Command -> IO Reply
execute engine command = case command of
  ChangeTransport playing -> Ack <$ Audio.setTransport engine (transport playing)
  ChangeVolume gain -> Ack <$ (checkedVolume gain >>= Audio.setVolume engine)
  ChangeEnvironment settings -> Ack <$ Audio.setAmbience engine (fmap sensorValue settings)
  Next -> Ack <$ Audio.next engine
  Replace seed values -> Ack <$ (decodeTracks values >>= \tracks -> Audio.replaceLibrary engine tracks seed)
  ReadSnapshot -> Snapshot . SnapshotValue <$> Audio.snapshot engine
  SeekForCheck target -> Ack <$ Audio.seekForCheck engine target
  ReadDeviceStats -> uncurry DeviceStats <$> Audio.deviceForCheck engine
  Start {} -> protocolFailure "Source is already running"
  Stop -> protocolFailure "Source shutdown must release its engine"
  where sensorValue (aboard, speed, forest, shore) = environmentInput aboard speed forest shore

transport :: Bool -> Transport
transport playing = if playing then Playing else Paused

checkedVolume :: Double -> IO Volume
checkedVolume = either (protocolFailure . show) pure . volume

trySync :: IO a -> IO (Either SomeException a)
trySync = tryJust (\issue -> case fromException issue :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just issue)

replyError :: SomeException -> Reply
replyError issue = case fromException issue of
  Just Audio.NoPlayableTracks -> NoTracks
  _ -> Failed (displayException issue)
