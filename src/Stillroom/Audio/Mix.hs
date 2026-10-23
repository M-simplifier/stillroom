-- | Validated one-shot PCM samples and absolute-frame event mixing.
-- Constructors and record labels stay private; bank checks happen once.
module Stillroom.Audio.Mix
  ( Bank, Event, MixPlan, MixError (..)
  , sampleRate, mkBank, bankKeys, bankTailFrames, mkEvent
  , eventStart, eventKey, eventGain, eventPan
  , prepareMix, planEventCount, renderMix, renderPlan
  ) where

import Control.Monad (forM_)
import Data.Char (isAsciiLower, isAsciiUpper, toUpper)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV

sampleRate :: Int
sampleRate = 44100

newtype Key = Key String deriving (Eq, Ord, Show)
newtype Sample = Sample (V.Vector Float)
data Bank = Bank !(Map.Map Key Sample) !Integer

-- start, key, original gain/pan, precomputed left/right gains
data Event = Event !Integer !Key !Double !Double !Double !Double
data Voice = Voice !Integer !Sample !Double !Double
data MixPlan = MixPlan !(Map.Map Integer [Voice]) !Integer !Int

data MixError
  = InvalidSampleKey String
  | DuplicateSampleKey String
  | EmptySample String
  | OddSampleValueCount String Int
  | InvalidSampleValue String Int Float
  | NegativeEventStart Integer
  | InvalidEventGain Double
  | InvalidEventPan Double
  | MissingSample String
  | NegativeRenderStart Integer
  | InvalidRenderFrameCount Int
  | NonFiniteMixValue Int
  deriving (Eq, Show)

finite :: RealFloat a => a -> Bool
finite x = not (isNaN x || isInfinite x)

parseKey :: String -> Either MixError Key
parseKey raw
  | null raw || any (not . safeChar) raw || reserved = Left (InvalidSampleKey raw)
  | otherwise = Right (Key raw)
  where
    safeChar c = isAsciiLower c || isAsciiUpper c || c >= '0' && c <= '9' || c == '_' || c == '-'
    reserved = map toUpper raw `elem`
      ( ["CON", "PRN", "AUX", "NUL"]
        ++ [prefix ++ show n | prefix <- ["COM", "LPT"], n <- [1 :: Int .. 9]] )

-- | PCM is nonempty, even-length interleaved stereo, finite and in [-1,1].
-- Samples are immutable. Names must be filename-safe ASCII keys.
mkBank :: [(String, V.Vector Float)] -> Either MixError Bank
mkBank entries = do
  samples <- foldl' add (Right Map.empty) entries
  let longest = Map.foldl' (\n sample -> max n (sampleFrames sample)) 0 samples
  pure (Bank samples longest)
  where
    add previous (raw, pcm) = do
      samples <- previous
      key <- parseKey raw
      if Map.member key samples then Left (DuplicateSampleKey raw) else pure ()
      if V.null pcm then Left (EmptySample raw) else pure ()
      if odd (V.length pcm) then Left (OddSampleValueCount raw (V.length pcm)) else pure ()
      case V.findIndex (\x -> not (finite x) || abs x > 1) pcm of
        Just i -> Left (InvalidSampleValue raw i (pcm V.! i))
        Nothing -> Right (Map.insert key (Sample pcm) samples)

bankKeys :: Bank -> [String]
bankKeys (Bank samples _) = [raw | Key raw <- Map.keys samples]

-- | Longest one-shot, cached when the immutable bank is validated.
bankTailFrames :: Bank -> Integer
bankTailFrames (Bank _ longest) = longest

-- | Equal-power stereo balance: L'=gain*cos(theta)*L,
-- R'=gain*sin(theta)*R, theta=(pan+1)*pi/4. No crossfeed.
-- Center attenuates both existing channels by sqrt(1/2); endpoints are exact.
-- These gains are calculated once here, never in the sample loop.
mkEvent :: Integer -> String -> Double -> Double -> Either MixError Event
mkEvent start raw gain pan = do
  key <- parseKey raw
  if start < 0 then Left (NegativeEventStart start) else pure ()
  if not (finite gain) || gain < 0 then Left (InvalidEventGain gain) else pure ()
  if not (finite pan) || pan < -1 || pan > 1 then Left (InvalidEventPan pan) else pure ()
  let (left, right)
        | pan == -1 = (1, 0)
        | pan == 1 = (0, 1)
        | pan == 0 = (sqrt 0.5, sqrt 0.5)
        | otherwise = (cos theta, sin theta)
      theta = (pan + 1) * pi / 4
  pure (Event start key gain pan (gain * left) (gain * right))

eventStart :: Event -> Integer
eventStart (Event start _ _ _ _ _) = start
eventKey :: Event -> String
eventKey (Event _ (Key raw) _ _ _ _) = raw
eventGain :: Event -> Double
eventGain (Event _ _ gain _ _ _) = gain
eventPan :: Event -> Double
eventPan (Event _ _ _ pan _ _) = pan

-- | Resolve every key once, including events outside the first render window.
-- A plan owns the resolved immutable samples, so later queries cannot mismatch
-- a different bank. Summation order is start-frame order, then input order.
prepareMix :: Bank -> [Event] -> Either MixError MixPlan
prepareMix (Bank samples _) events = do
  voices <- traverse resolve events
  let index = Map.fromListWith (++) [(start, [voice]) | voice@(Voice start _ _ _) <- reverse voices]
      longest = foldl' (\n (Voice _ sample _ _) -> max n (sampleFrames sample)) 0 voices
  pure (MixPlan index longest (length events))
  where
    resolve (Event start key@(Key raw) _ _ left right) = case Map.lookup key samples of
      Nothing -> Left (MissingSample raw)
      Just sample -> Right (Voice start sample left right)

planEventCount :: MixPlan -> Int
planEventCount (MixPlan _ _ count) = count

sampleFrames :: Sample -> Integer
sampleFrames (Sample pcm) = fromIntegral (V.length pcm `div` 2)

-- | Convenience checked entry. Prepare once and use 'renderPlan' for playback.
renderMix :: Bank -> [Event] -> Integer -> Int -> Either MixError (V.Vector Float)
renderMix bank events offset frames
  | offset < 0 = Left (NegativeRenderStart offset)
  | frames < 0 || frames > maxBound `div` 2 = Left (InvalidRenderFrameCount frames)
  | otherwise = prepareMix bank events >>= \plan -> renderPlan plan offset frames

-- | Render [offset,offset+frames), adding only intersecting one-shots.
-- No normalization or clipping; accumulation is Double, final PCM is Float.
-- Query validation is O(1); interval lookup is O(log n + nearby events).
-- Finite output is checked, without rescanning the bank or all events.
renderPlan :: MixPlan -> Integer -> Int -> Either MixError (V.Vector Float)
renderPlan (MixPlan starts longest _) offset frames
  | offset < 0 = Left (NegativeRenderStart offset)
  | frames < 0 || frames > maxBound `div` 2 = Left (InvalidRenderFrameCount frames)
  | frames == 0 = Right V.empty
  | otherwise = case V.findIndex (not . finite) output of
      Just i -> Left (NonFiniteMixValue i)
      Nothing -> Right output
  where
    end = offset + fromIntegral frames
    -- A voice starting at offset-longest has already ended. Upper bound is
    -- exclusive too. This avoids traversing the whole score for each block.
    (_, afterOldStarts) = Map.split (offset - longest) starts
    (nearby, _) = Map.split end afterOldStarts
    voices = concat (Map.elems nearby)
    output = V.map realToFrac $ V.create $ do
      mixed <- MV.replicate (2 * frames) (0 :: Double)
      forM_ voices $ \(Voice start sample@(Sample pcm) left right) -> do
        let first = max offset start
            stop = min end (start + sampleFrames sample)
            count = fromInteger (max 0 (stop - first)) :: Int
            sourceFirst = fromInteger (first - start) :: Int
            targetFirst = fromInteger (first - offset) :: Int
            addFrame i
              | i >= count = pure ()
              | otherwise = do
                  -- Overlap arithmetic bounds both indices to their vectors;
                  -- checked indexing remains enabled as a programming guard.
                  let source = 2 * (sourceFirst + i)
                      target = 2 * (targetFirst + i)
                  oldLeft <- MV.read mixed target
                  oldRight <- MV.read mixed (target + 1)
                  MV.write mixed target (oldLeft + realToFrac (pcm V.! source) * left)
                  MV.write mixed (target + 1) (oldRight + realToFrac (pcm V.! (source + 1)) * right)
                  addFrame (i + 1)
        addFrame 0
      pure mixed
