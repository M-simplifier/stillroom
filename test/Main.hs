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
        law "unknown preference versions are rejected" (once propVersion),
        law "ordinary controls never reconfigure the window" (once propWindowEffects),
        law "built-in music preserves the user's previous folder" (once propRememberFolder),
        law "version 1 settings migrate without losing music or display" (once propMigrate),
        law "picture controls preserve playback and window state" (once propPictureControls),
        law "picture shading cycles through bounded levels" (once propPictureShade)
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
            savedBackground = toEnum (n `mod` 5),
            savedFolder = Just "音楽/静かな部屋",
            savedPicture = Just "user:静かな画像.png",
            savedPictureShade = toEnum (n `mod` 3)
          }
   in eitherDecode (encode p) == Right p

propVersion :: Bool
propVersion = case (eitherDecode (encode (object ["version" .= (3 :: Int)])) :: Either String Preferences) of
  Left _ -> True
  Right _ -> False

propWindowEffects :: Bool
propWindowEffects =
  and
    [ let (updated, effects) = step event ((initialSession defaultVolume) {windowMode = mode})
       in windowMode updated == mode && PlaceWindow `notElem` effects && MoveScreen `notElem` effects
    | mode <- [Fullscreen, Compact],
      event <- [TogglePlayback, ChangeVolume 0.1, NextTrack, ChooseBackground Lake, ToggleSettings, OpenMusic, UsePresets, ToggleLanguage, Activity]
    ]

propRememberFolder :: Bool
propRememberFolder =
  let folder = "音楽/静かな部屋"
      legacy = (defaults Japanese) {savedFolder = Just folder}
      presets = useBuiltInMusic legacy
      selected = useMusicFolder "other" presets
   in savedFolder presets == Nothing
        && previousFolder presets == Just folder
        && eitherDecode (encode presets) == Right presets
        && previousFolder (useBuiltInMusic presets) == Just folder
        && savedFolder selected == Just "other"
        && previousFolder selected == Just "other"

propMigrate :: Bool
propMigrate =
  let legacy = object ["version" .= (1 :: Int), "volume" .= (0.42 :: Double),
        "background" .= ("Lake" :: String), "language" .= ("Japanese" :: String),
        "musicFolder" .= ("音楽/自分の曲" :: String), "screen" .= ("display-2" :: String)]
   in case eitherDecode (encode legacy) of
        Right p -> savedVolume p == 0.42 && savedBackground p == Lake
          && savedLanguage p == Japanese && savedFolder p == Just "音楽/自分の曲"
          && savedScreen p == "display-2" && savedPicture p == Nothing
          && savedPictureShade p == Original
        Left _ -> False

propPictureControls :: Bool
propPictureControls = and
  [ let (after, effects) = step event before
     in transport after == transport before && loudness after == loudness before
       && windowMode after == windowMode before
       && all (`notElem` effects) [PlaceWindow, MoveScreen, SkipTrack]
  | mode <- [Fullscreen, Compact], playing <- [Playing, Paused],
    let before = (initialSession defaultVolume) {windowMode = mode, transport = playing},
    event <- [OpenPicture, BrowsePicture PreviousPicture, BrowsePicture NextPicture,
      RemovePicture, ChooseBackground Picture, CyclePictureShade]
  ]

propPictureShade :: Bool
propPictureShade = all (\shade -> nextShade (nextShade (nextShade shade)) == shade
  && shadeOpacity shade >= 0 && shadeOpacity shade <= 128) [Original, Soft, Dim]
