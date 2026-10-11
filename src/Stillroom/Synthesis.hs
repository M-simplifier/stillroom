-- | Original, sample-free music. A preset is a score and a timbre, evaluated
-- from an integer sample clock. Chunk boundaries never change the composition.
-- The source and the rendered preset audio are offered under the MIT license.
module Stillroom.Synthesis (sampleRate, renderBlock, sampleStereo) where

import Control.Monad (forM_)
import Data.Bits (shiftR, xor)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import Data.Word (Word32)
import Stillroom.Domain (Preset (..))

sampleRate :: Int
sampleRate = 44100

-- | Interleaved stereo Float PCM. Negative offsets clamp to the start.
renderBlock :: Preset -> Integer -> Int -> V.Vector Float
renderBlock preset offset frames = V.create $ do
  result <- MV.new (2 * max 0 frames)
  forM_ [0 .. frames - 1] $ \i -> do
    let (left, right) = sampleStereo preset (max 0 offset + fromIntegral i)
    MV.write result (2 * i) (realToFrac left)
    MV.write result (2 * i + 1) (realToFrac right)
  pure result

-- | The output is bounded by construction (sum of bounded voices < 0.5).
-- Slow harmony and sparse repeating figures carry musical variation; the
-- screen and the audio never depend on a random wall-clock callback.
sampleStereo :: Preset -> Integer -> (Double, Double)
sampleStereo preset frame = (envelope * (bed + figureL + pulse), envelope * (bed + figureR + pulse))
  where
    t = fromIntegral (max 0 frame) / fromIntegral sampleRate
    (bpm, tonic, warmth, rhythm) = case preset of
      Linen -> (78, 50, 0.8, 0.032)
      Moss -> (70, 45, 0.3, 0.009)
      Orbit -> (84, 48, 1.0, 0.037)
      Night -> (66, 43, 0.2, 0.006)
    beat = t * bpm / 60
    bar = floor (beat / 4) :: Int
    chordIndex = (bar `div` 8) `mod` 4
    (root, third) = case chordIndex of
      0 -> (tonic, 3)
      1 -> (tonic + 5, 3)
      2 -> (tonic + 3, 4)
      _ -> (tonic - 2, 4)
    -- Each harmonic region eases in; a quiet continuous tonic prevents holes.
    regionPhase = beat - fromIntegral (bar `div` 8) * 32
    regionEnvelope = smooth (min 1 (regionPhase / 2)) * smooth (min 1 ((32 - regionPhase) / 2))
    bed =
      0.023 * osc tonic t
        + regionEnvelope
          * (0.035 * soft root t + 0.024 * soft (root + third + 12) t + 0.021 * soft (root + 7 + 12) t)
    eighth = floor (beat * 2) :: Int
    noteIndex = (eighth `div` 2) `mod` 8
    degree = case noteIndex of
      0 -> 12
      1 -> 19
      2 -> 15
      3 -> 22
      4 -> 19
      5 -> 15
      6 -> 24
      _ -> 19
    noteTime = (beat - fromIntegral (eighth `div` 2)) * 60 / bpm
    figure =
      0.033
        * warmth
        * exp (-2.8 * noteTime)
        * smooth (min 1 (noteTime / 0.035))
        * (osc (root + degree) t + 0.18 * osc (root + degree + 12) t)
    pan = 0.28 * sin (fromIntegral noteIndex * 1.7)
    figureL = figure * (0.8 - pan)
    figureR = figure * (0.8 + pan)
    beatTime = (beat - fromIntegral (floor beat :: Int)) * 60 / bpm
    kick = rhythm * exp (-15 * beatTime) * sin (2 * pi * (48 * beatTime + 1.8 * (1 - exp (-22 * beatTime))))
    halfBeatTime = (beat * 2 - fromIntegral eighth) * 30 / bpm
    hush = 0.0018 * warmth * noise (fromIntegral frame) * exp (-45 * halfBeatTime) * smooth (min 1 (halfBeatTime / 0.007))
    pulse = kick + hush
    envelope = smooth (min 1 (t / 2))

osc :: Double -> Double -> Double
osc midi t = sin (2 * pi * (440 * 2 ** ((midi - 69) / 12)) * t)

soft :: Double -> Double -> Double
soft midi t = 0.8 * osc midi t + 0.12 * osc (midi + 12) t

smooth :: Double -> Double
smooth x = x * x * (3 - 2 * x)

noise :: Word32 -> Double
noise n = fromIntegral (x `mod` 65536) / 32768 - 1
  where
    a = (n `xor` (n `shiftR` 16)) * 2246822519
    x = (a `xor` (a `shiftR` 13)) * 3266489917
