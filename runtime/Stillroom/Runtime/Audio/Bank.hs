-- | PCM16LE input only. No audio device, CSV, output conversion or fallback.
module Stillroom.Runtime.Audio.Bank
  ( WavError (..), BankLoadError (..)
  , decodePcm16Wav, readPcm16Wav, loadSamples, loadBank
  ) where

import Control.Exception (Exception, IOException, evaluate, try)
import Data.Bits ((.|.), shiftL)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Set qualified as Set
import Data.Vector.Storable qualified as V
import Stillroom.Audio.Mix (Bank, MixError (..), mkBank, sampleRate)

data WavError
  = NotRiffWave
  | RiffSizeMismatch Integer Int
  | TruncatedChunkHeader Int
  | TruncatedChunk String Integer Int
  | MissingChunk String
  | DuplicateChunk String
  | ShortFormat Int
  | UnsupportedFormat Integer Integer Integer Integer
  | InconsistentFormat Integer Integer
  | EmptyPcm
  | MisalignedPcm Int Int
  deriving (Eq, Show)

data BankLoadError
  = SampleReadFailure FilePath String
  | SampleWavFailure FilePath WavError
  | BankValidationFailure MixError
  deriving (Eq, Show)

instance Exception BankLoadError

ascii :: String -> BS.ByteString
ascii = BSC.pack

littleUnsigned :: BS.ByteString -> Integer
littleUnsigned bytes = snd (BS.foldl' step (0 :: Int, 0) bytes)
  where step (bit, value) byte = (bit + 8, value .|. (fromIntegral byte `shiftL` bit))

-- | Strict RIFF/WAVE, format=1, PCM16LE, mono/stereo, 44100 Hz.
-- Mono is duplicated into interleaved stereo. No resampling or normalization.
decodePcm16Wav :: BS.ByteString -> Either WavError (V.Vector Float)
decodePcm16Wav bytes = do
  if BS.length bytes < 12 || BS.take 4 bytes /= ascii "RIFF" || BS.take 4 (BS.drop 8 bytes) /= ascii "WAVE"
    then Left NotRiffWave else pure ()
  let declared = littleUnsigned (BS.take 4 (BS.drop 4 bytes)) + 8
  if declared /= fromIntegral (BS.length bytes)
    then Left (RiffSizeMismatch declared (BS.length bytes)) else pure ()
  chunks <- readChunks (BS.drop 12 bytes)
  fmt <- exactlyOne "fmt " chunks
  pcm <- exactlyOne "data" chunks
  channels <- decodeFormat fmt
  let alignment = channels * 2
  if BS.null pcm then Left EmptyPcm else pure ()
  if BS.length pcm `mod` alignment /= 0 then Left (MisalignedPcm alignment (BS.length pcm)) else pure ()
  pure $ if channels == 2
    then V.unfoldrN (BS.length pcm `div` 2) nextValue pcm
    else V.unfoldrN (BS.length pcm) nextMono (Nothing, pcm)
  where
    -- Validated alignment ensures these total unfolds emit the expected count.
    nextValue remaining = do
      (lo, afterLo) <- BS.uncons remaining
      (hi, rest) <- BS.uncons afterLo
      let unsigned = fromIntegral lo + 256 * fromIntegral hi :: Int
          signed = if unsigned >= 32768 then unsigned - 65536 else unsigned
      pure (fromIntegral signed / 32768, rest)
    nextMono (Just value, remaining) = Just (value, (Nothing, remaining))
    nextMono (Nothing, remaining) = do
      (value, rest) <- nextValue remaining
      pure (value, (Just value, rest))

readChunks :: BS.ByteString -> Either WavError [(BS.ByteString, BS.ByteString)]
readChunks remaining
  | BS.null remaining = Right []
  | BS.length remaining < 8 = Left (TruncatedChunkHeader (BS.length remaining))
  | otherwise = do
      let tag = BS.take 4 remaining
          size = littleUnsigned (BS.take 4 (BS.drop 4 remaining))
          padded = size + size `mod` 2
          body = BS.drop 8 remaining
      if padded > fromIntegral (BS.length body)
        then Left (TruncatedChunk (BSC.unpack tag) padded (BS.length body)) else pure ()
      let payload = BS.take (fromInteger size) body
      rest <- readChunks (BS.drop (fromInteger padded) body)
      pure ((tag, payload) : rest)

exactlyOne :: String -> [(BS.ByteString, BS.ByteString)] -> Either WavError BS.ByteString
exactlyOne name chunks = case [payload | (tag, payload) <- chunks, tag == ascii name] of
  [] -> Left (MissingChunk name)
  [payload] -> Right payload
  _ -> Left (DuplicateChunk name)

decodeFormat :: BS.ByteString -> Either WavError Int
decodeFormat fmt
  | BS.length fmt < 16 = Left (ShortFormat (BS.length fmt))
  | format /= 1 || channels `notElem` [1, 2] || rate /= fromIntegral sampleRate || bits /= 16 =
      Left (UnsupportedFormat format channels rate bits)
  | alignment /= channels * 2 || byteRate /= rate * alignment = Left (InconsistentFormat alignment byteRate)
  | otherwise = Right (fromInteger channels)
  where
    field offset count = littleUnsigned (BS.take count (BS.drop offset fmt))
    format = field 0 2
    channels = field 2 2
    rate = field 4 4
    byteRate = field 8 4
    alignment = field 12 2
    bits = field 14 2

-- | Decode and force PCM before returning, so file IO and sample allocation
-- finish during acquisition rather than on the first audio block.
-- Only synchronous IO failures are caught; cancellation remains interruptible.
readPcm16Wav :: FilePath -> IO (Either BankLoadError (V.Vector Float))
readPcm16Wav path = do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case result of
    Left failure -> pure (Left (SampleReadFailure path (show failure)))
    Right bytes -> case decodePcm16Wav bytes of
      Left failure -> pure (Left (SampleWavFailure path failure))
      Right pcm -> evaluate (V.length pcm) >> pure (Right pcm)

-- | Resolve and read exactly the requested keys, never enumerate a directory.
-- The resolver supplies assetPath policy; this module remains independent of
-- graphics and device ownership. The same decoder serves music and ambience.
loadSamples :: (String -> IO FilePath) -> [String] -> IO (Either BankLoadError [(String, V.Vector Float)])
loadSamples resolve keys = case duplicate keys of
  Just key -> pure (Left (BankValidationFailure (DuplicateSampleKey key)))
  Nothing -> go keys
  where
    go [] = pure (Right [])
    go (key : rest) = do
      path <- resolve key
      result <- readPcm16Wav path
      case result of
        Left failure -> pure (Left failure)
        Right pcm -> do
          remaining <- go rest
          pure (((key, pcm) :) <$> remaining)
    duplicate = findDuplicate Set.empty
    findDuplicate _ [] = Nothing
    findDuplicate seen (key : rest)
      | Set.member key seen = Just key
      | otherwise = findDuplicate (Set.insert key seen) rest

-- | Validate the selected immutable music bank once, after decoding.
loadBank :: (String -> IO FilePath) -> [String] -> IO (Either BankLoadError Bank)
loadBank resolve keys = do
  decoded <- loadSamples resolve keys
  case decoded >>= either (Left . BankValidationFailure) Right . mkBank of
    Left failure -> pure (Left failure)
    Right bank -> evaluate bank >> pure (Right bank)
