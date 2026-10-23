-- | Acquire selected assets once, then supply bounded music/environment blocks.
-- No device ownership or fallback policy. Source closures are called serially.
module Stillroom.Runtime.Audio.Builtin
  ( BuiltinFailure (..), loadBuiltinSources, loadBuiltinSourcesWithAssets
  ) where

import Control.Exception (Exception, evaluate, mask_, throwIO)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Vector.Storable qualified as V
import Data.Word (Word64)
import Stillroom.Audio.Composition (compositionEvents, requiredKeys)
import Stillroom.Audio.Environment
  ( EnvironmentInput, initialEnvironment, prepareEnvironmentBank, renderEnvironment )
import Stillroom.Audio.Mix (MixError, bankTailFrames, renderMix, sampleRate)
import Stillroom.Domain (Preset)
import Stillroom.Runtime.Audio.Bank (loadBank, loadSamples)
import Stillroom.Runtime.Pictures (assetPath)
import System.FilePath ((</>))

-- Structural aliases match Runtime.Audio without importing its device module.
-- This keeps acquisition usable independently and prevents a dependency cycle.
type BlockSource = Preset -> Integer -> Int -> IO (V.Vector Float)
type AmbienceSource = Maybe EnvironmentInput -> Int -> IO (V.Vector Float)

data BuiltinFailure
  = BuiltinMixFailure Preset Integer MixError
  | MusicTailExceedsLookback Integer Integer
  | BuiltinCompositionQueryTooLarge Int
  | BuiltinEnvironmentFailure String
  | InvalidEnvironmentBlock Int Int
  deriving (Show)

instance Exception BuiltinFailure

-- | Use the common portable/Cabal asset resolver. Missing or invalid assets
-- throw their structured BankLoadError; the desktop chooses any fallback.
loadBuiltinSources :: Word64 -> IO (BlockSource, AmbienceSource)
loadBuiltinSources = loadBuiltinSourcesWithAssets assetPath

-- | The resolver seam also permits packaged-asset and isolated acquisition
-- checks without changing the working directory or the device owner.
loadBuiltinSourcesWithAssets :: (FilePath -> IO FilePath) -> Word64 -> IO (BlockSource, AmbienceSource)
loadBuiltinSourcesWithAssets resolve seed = do
  musicBank <- loadBank (resolveSample "music") requiredKeys >>= either throwIO pure
  let lookback = 24 * fromIntegral sampleRate
      actualTail = bankTailFrames musicBank
  when (actualTail > lookback) (throwIO (MusicTailExceedsLookback lookback actualTail))
  environmentSamples <- loadSamples (resolveSample "environment") environmentKeys >>= either throwIO pure
  environmentBank <- either (throwIO . BuiltinEnvironmentFailure) evaluate (prepareEnvironmentBank environmentSamples)
  environmentState <- evaluate initialEnvironment >>= newIORef
  let musicSource preset offset count = do
        -- Composition's documented bound must fail explicitly at this IO
        -- boundary instead of turning an unsupported request into silence.
        when (count > 1048576) (throwIO (BuiltinCompositionQueryTooLarge count))
        either (throwIO . BuiltinMixFailure preset offset) evaluate $
          renderMix musicBank (compositionEvents preset seed offset count) offset count
      ambienceSource input count = mask_ $ do
        when (count < 0 || count > maxBound `div` 2) $
          throwIO (BuiltinEnvironmentFailure ("Invalid environment frame count: " ++ show count))
        previous <- readIORef environmentState
        case renderEnvironment environmentBank input previous count of
          Left failure -> throwIO (BuiltinEnvironmentFailure failure)
          Right (pcm, proposed) -> do
            -- Complete this block before committing its phase/state. Failure
            -- leaves the previous pure state available to the caller.
            realized <- evaluate pcm
            when (V.length realized /= 2 * count || V.any nonFinite realized) $
              throwIO (InvalidEnvironmentBlock count (V.length realized))
            next <- evaluate proposed
            writeIORef environmentState next
            pure realized
  pure (musicSource, ambienceSource)
  where
    resolveSample group key = resolve ("assets" </> "audio" </> group </> (key ++ ".wav"))
    nonFinite value = isNaN value || isInfinite value

environmentKeys :: [String]
environmentKeys = ["wind", "leaves", "water", "wheel", "body-1", "body-2", "body-3"]
