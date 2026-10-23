-- | A score is a window into musical time, not a growing event log.
-- Rational beats locate notes exactly; seeded phrase choices do not depend on
-- callback size, elapsed wall time, or which earlier windows were requested.
module Stillroom.Audio.Composition
  ( compositionEvents, requiredKeys
  ) where

import Data.Bits (shiftR, xor)
import Data.List (nub, sort, sortOn)
import Data.Ratio ((%))
import Data.Word (Word64)
import Stillroom.Audio.Mix (Event, mkEvent)
import Stillroom.Domain (Preset (..))

data Note = Note !Rational !String !Int !Double !Double

tempo :: Preset -> Rational
tempo Linen = 66
tempo Night = 72
tempo Moss = 64
tempo Orbit = 78

-- Low roots and upper voices remain separate. Shared upper tones connect the
-- harmony without a melody or a cadence insisting on the listener's attention.
palette :: Preset -> [[Int]]
palette Linen = [[50,57,61,64,66],[47,54,57,61,64],[43,50,57,59,62],[45,52,57,59,64]]
palette Night = [[48,55,59,62,64],[45,52,55,59,62],[41,48,55,57,60],[43,50,55,57,62]]
palette Moss = [[50,57,60,64,67],[46,53,57,60,65],[48,55,58,62,65],[43,50,57,60,62]]
palette Orbit = [[45,52,55,59,62],[41,48,55,57,60],[48,55,59,62,64],[43,50,55,57,62]]

hash :: Word64 -> Word64
hash n = c `xor` (c `shiftR` 31)
  where
    a = n + 0x9e3779b97f4a7c15
    b = (a `xor` (a `shiftR` 30)) * 0xbf58476d1ce4e5b9
    c = (b `xor` (b `shiftR` 27)) * 0x94d049bb133111eb

unit :: Word64 -> Integer -> Int -> Double
unit seed phrase lane = fromIntegral (hash (seed + fromIntegral phrase * 109 + fromIntegral lane) `mod` 10001) / 10000

harmony :: Preset -> Word64 -> Integer -> [Int]
harmony preset seed bar = palette preset !! (path !! fromIntegral (region `mod` 4))
  where
    region = bar `div` 8
    chapter = region `div` 4
    -- The audition's first passage is retained. Later passages choose nearby
    -- routes through the same vocabulary, with a stable bridge between them.
    path
      | chapter == 0 = [0,1,2,3]
      | otherwise = [[0,1,2,3],[0,2,1,3],[0,1,0,3],[0,2,0,3]]
          !! min 3 (floor (4 * unit seed chapter 71))

master :: Preset -> Double
master preset = 10 ** (db / 20)
  where
    -- Fixed gains from the owner-evaluated -24 LUFS auditions. No reactive
    -- normalization or compressor pumps the dynamics during playback.
    db = case preset of
      Linen -> 4.569207317227811
      Night -> 4.8062235739791204
      Moss -> 4.050815143565721
      Orbit -> 4.564640547853905

barNotes :: Preset -> Word64 -> Integer -> [Note]
barNotes preset seed bar = map humanize (case preset of
  Linen -> pad 0.10 ++ bass 0.31 ++ case local of
    0 -> [n 0 "felt" 1 0.68 (-0.12), n (3%2) "felt" 3 0.43 0.16, n 3 "felt" 2 0.39 (-0.04)]
    1 -> [n (if spacious then 2 else 1) "felt" 4 0.40 0.12]
    2 -> [n (1%2) "felt" 2 0.46 (-0.12), n (if spacious then 3 else 2) "felt" 3 0.39 0.16]
    _ -> [n 1 "felt" (if variation then 2 else 1) 0.41 0.02]
  Night -> pad 0.07 ++ bass 0.34 ++ keys ++ pulse 0.42 0.06
  Moss -> pad 0.15 ++ bass 0.24 ++ case local of
    0 -> [n 0 "wood" 1 0.59 (-0.18), n (5%2) "wood" 3 0.35 0.18]
    1 -> [n 1 "wood" 2 0.43 (-0.06)]
    2 -> [n (1%2) "wood" 4 0.33 0.14, n 3 "wood" 2 0.39 (-0.1)]
    _ -> [n (3%2) "wood" (if variation then 1 else 3) 0.36 0.04]
  Orbit -> pad 0.12 ++ bass 0.32 ++ pulse 0.45 (-0.08) ++
    [n b "pluck" degree amp p | (b,degree,amp,p) <-
      [(0,1,0.34,-0.18),(3%2,3,0.23,0.18),(2,2,0.27,-0.06),
       (if spacious then 3 else 7%2,if variation then 4 else 3,0.20,0.10)]])
  where
    voices = harmony preset seed bar
    local = fromIntegral (bar `mod` 4) :: Int
    origin = fromInteger (bar * 4)
    variation = unit seed (bar `div` 4) 0 > 0.52
    spacious = bar >= 32 && unit seed (bar `div` 4) 31 > 0.62
    n b instrument degree amp p = Note (origin+b) instrument (voices !! degree) amp p
    hit b instrument amp p = Note (origin+b) instrument 0 amp p
    bass amp = [n 0 "bass" 0 amp 0 | local == 0]
    pad amp = [n 0 "air" i amp p | bar `mod` 4 == 0, (i,p) <- [(1,-0.18),(2,0.14),(3,-0.05),(4,0.10)]]
    pulse amp p = [hit b "kick" amp 0 | b <- [0,2]] ++ [hit b "brush" 0.10 p | b <- [1,3]]
    -- Owner feedback: repeated block chords felt restless. Let their voices
    -- arrive separately, leaving space between them while the pulse remains.
    keys = case local of
      0 -> [n 0 "keys" 1 0.45 (-0.12),n (5%4) "keys" 3 0.35 0.12,n (11%4) "keys" 2 0.32 (-0.04)]
      1 -> [n (3%4) "keys" 4 0.30 0.16,n (5%2) "keys" 2 0.28 (-0.06)]
      2 -> [n (1%2) "keys" 2 0.39 (-0.08),n 2 "keys" 1 0.34 0.02,n (13%4) "keys" 3 0.27 0.12]
      _ -> [n 1 "keys" (if variation then 4 else 3) 0.31 0.10]
    humanize (Note b instrument pitch amp p) =
      Note b instrument pitch (amp * master preset * (0.94 + 0.12 * unit seed bar pitch)) p

frameAt :: Preset -> Rational -> Integer
frameAt preset beat = floor (beat * 60 / tempo preset * 44100)

-- | Includes every possible sounding tail (the longest asset is <22 seconds).
-- Work and memory depend on the requested window, never on session age.
-- Invalid/oversized requests are empty; the audio boundary validates its blocks.
compositionEvents :: Preset -> Word64 -> Integer -> Int -> [Event]
compositionEvents preset seed start count
  | start < 0 || count <= 0 || count > 1048576 = []
  | otherwise =
      [event | Note beat instrument pitch gain pan <- sortOn noteBeat notes,
       let frame = frameAt preset beat,
       frame >= beginning, frame < end,
       Right event <- [mkEvent frame (instrument ++ "-" ++ show pitch) gain pan]]
  where
    beginning = max 0 (start - 24 * 44100)
    end = start + fromIntegral count
    barAt frame = floor (fromInteger frame * tempo preset / (44100 * 240))
    notes = concatMap (barNotes preset seed) [barAt beginning .. barAt end]
    noteBeat (Note b _ _ _ _) = b

-- A complete asset contract, including voices reached beyond the first visit.
requiredKeys :: [String]
requiredKeys = sort . nub $
  [instrument ++ "-" ++ show pitch
  | preset <- [Linen,Night,Moss,Orbit], voices <- palette preset,
    (instrument,pitches) <- [(lead preset,tail voices),("air",tail voices),("bass",take 1 voices)],
    pitch <- pitches] ++ ["kick-0","brush-0"]
  where
    lead Linen = "felt"
    lead Night = "keys"
    lead Moss = "wood"
    lead Orbit = "pluck"
