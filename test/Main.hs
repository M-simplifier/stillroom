{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector.Storable qualified as V
import Stillroom.Domain
import Stillroom.Runtime.Storage
import Stillroom.Session
import Stillroom.Shuffle
import Stillroom.Synthesis
import System.Exit (exitFailure)
import Test.QuickCheck

main :: IO ()
main = do
  results <-
    sequence
      [ law "shuffle preserves every track each cycle" propCycles,
        law "shuffle avoids immediate repeats across cycles" propNoRepeat,
        law "volume remains bounded under arbitrary user input" propVolume,
        law "pause/resume is an involution" (once propPause),
        law "compact mode always exposes controls" propCompact,
        law "synthesis is independent of chunk boundaries" propChunks,
        law "sample output is finite and bounded" propSamples,
        law "crossfade has unit power and monotone gains" propFade,
        law "invalid boundary values are rejected" (once propInvalid),
        law "rejected files never reappear" propExclude,
        law "failed files cannot hide the only valid file" propFailureProgress,
        law "short incoming tracks remain alive at handover" propShortIncoming,
        law "a rejected draw preserves the last accepted item" propAcceptedHistory,
        law "saved preferences round-trip" propPreferences,
        law "unknown preference versions are rejected" (once propVersion)
      ]
  unless (all isSuccess results) exitFailure
  where
    law title p = putStrLn title >> quickCheckWithResult stdArgs {maxSuccess = 250} p

drawMany :: (Eq a) => Int -> Bag a -> [a]
drawMany n bag
  | n <= 0 = []
  | otherwise = let (x, b) = draw bag in x : drawMany (n - 1) b

propCycles :: Int -> Positive Int -> Bool
propCycles seed (Positive raw) =
  let n = 1 + raw `mod` 30
      xs = drawMany (n * 4) (newBag seed (0 :| [1 .. n - 1]))
   in all ((== [0 .. n - 1]) . sort) [take n (drop (i * n) xs) | i <- [0 .. 3]]

propNoRepeat :: Int -> Positive Int -> Bool
propNoRepeat seed (Positive raw) =
  let n = 2 + raw `mod` 30; xs = drawMany (n * 5) (newBag seed (0 :| [1 .. n - 1]))
   in and (zipWith (/=) xs (drop 1 xs))

propVolume :: [Double] -> Bool
propVolume xs =
  let v = volumeValue (foldr adjustVolume defaultVolume xs)
   in not (isNaN v) && v >= 0 && v <= 1

propPause :: Bool
propPause =
  let start = initialSession defaultVolume
      (paused, _) = step TogglePlayback start
      (resumed, _) = step TogglePlayback paused
   in transport paused == Paused && transport resumed == Playing

propCompact :: NonNegative Double -> Bool
propCompact (NonNegative t) = controlsVisible ((initialSession defaultVolume) {windowMode = Compact, idleSeconds = t})

propChunks :: Property
propChunks = forAll (chooseInteger (0, 180 * fromIntegral sampleRate)) $ \offset ->
  all (\preset -> renderBlock preset offset 32 == renderBlock preset offset 13 V.++ renderBlock preset (offset + 13) 19) [Linen, Moss, Orbit, Night]

propSamples :: Property
propSamples = forAll (chooseInteger (0, 180 * fromIntegral sampleRate)) $ \n ->
  all bounded [sampleStereo p n | p <- [Linen, Moss, Orbit, Night]]
  where
    bounded (l, r) = all (\x -> not (isNaN x || isInfinite x) && abs x < 0.5) [l, r]

propFade :: Property
propFade = forAll (choose (0, 0.998 :: Double)) $ \fraction ->
  let time = either (const zeroSeconds) id . seconds
   in case (mixAt (time 120) (time (114 + 6 * fraction)), mixAt (time 120) (time (114 + 6 * (fraction + 0.001)))) of
        (Crossfade a b, Crossfade c d) -> abs (a * a + b * b - 1) < 1e-10 && a >= c && b <= d && a >= 0 && b >= 0
        _ -> False

propInvalid :: Bool
propInvalid =
  volume (0 / 0) == Left NotFinite
    && volume 1.1 == Left OutsideUnitInterval
    && seconds (-1) == Left NegativeTime
    && seconds (1 / 0) == Left NotFinite

propExclude :: Int -> Bool
propExclude seed = case exclude (3 :: Int) (newBag seed (0 :| [1 .. 8])) of
  Nothing -> False
  Just bag -> 3 `notElem` drawMany 40 bag

propFailureProgress :: Int -> Bool
propFailureProgress seed = seek (3 :: Int) (newBag seed (0 :| [1, 2 :: Int]))
  where
    seek 0 _ = False
    seek attempts bag =
      let (candidate, rest) = draw bag
       in candidate == 0 || case exclude candidate rest of
            Nothing -> False
            Just available -> seek (attempts - 1) available

propShortIncoming :: Positive Int -> Bool
propShortIncoming (Positive n) =
  let short = fromIntegral (1 + n `mod` 500) / 100
      time = either (const zeroSeconds) id . seconds
   in mixBetween (time 20) (time short) (time (20 - short)) == Solo

propAcceptedHistory :: Int -> Bool
propAcceptedHistory seed =
  let accepted count bag
        | count <= 0 = []
        | otherwise =
            let (item, rest) = draw bag
             in if item == 2
                  then maybe [] (accepted count) (exclude item rest)
                  else item : accepted (count - 1) rest
      items = accepted (30 :: Int) (newBag seed (0 :| [1, 2 :: Int]))
   in length items == 30 && and (zipWith (/=) items (drop 1 items))

propPreferences :: NonNegative Int -> Bool
propPreferences (NonNegative n) =
  let p =
        (defaults Japanese)
          { savedVolume = fromIntegral (n `mod` 101) / 100,
            savedBackground = toEnum (n `mod` 4),
            savedFolder = Just "音楽/静かな部屋"
          }
   in eitherDecode (encode p) == Right p

propVersion :: Bool
propVersion = case (eitherDecode (encode (object ["version" .= (2 :: Int)])) :: Either String Preferences) of
  Left _ -> True
  Right _ -> False
