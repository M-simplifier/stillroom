{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector.Storable qualified as V
import Stillroom.Domain
import Stillroom.Interface qualified as UI
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
        law "picture shading cycles through bounded levels" (once propPictureShade),
        law "rest round-trip preserves music, picture and window" propRestRoundTrip,
        law "mouse activity does not expose rest controls" propRestActivity,
        law "rest comparisons cannot change normal background or audio" (once propRestChoices),
        law "Esc leaves rest before resizing the window" (once propRestEscape),
        law "version 2 settings migrate and resting is never resumed at startup" (once propMigrateRest),
        law "still mode freezes scene time without pausing music" propFreezeScene,
        law "versions 3-5 migrate retired scenes without losing preferences" (once propMigrateRestV3),
        law "rest accepts only elapsed time and exit" propRestExitOnly,
        law "preview and rest use the same motion clock rules" (once propPreviewClock),
        law "every displayed target is on screen, distinct and clickable" propInterfaceTargets,
        law "Tab and Shift-Tab visit every visible action and wrap" propFocusOrder,
        law "rest exposes no pointer or keyboard controls except exit" propRestInterface,
        law "direct selectors preserve playback and choose the requested value" (once propDirectChoices)
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
            savedPictureShade = toEnum (n `mod` 3),
            savedRestScene = toEnum (n `mod` (1 + fromEnum (maxBound :: RestScene))),
            savedRestMotion = toEnum ((n `div` 2) `mod` 2),
            savedRestShade = toEnum ((n `div` 3) `mod` 3)
          }
   in eitherDecode (encode p) == Right p

propVersion :: Bool
propVersion = case (eitherDecode (encode (object ["version" .= (7 :: Int)])) :: Either String Preferences) of
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
  let legacy =
        object
          [ "version" .= (1 :: Int),
            "volume" .= (0.42 :: Double),
            "background" .= ("Lake" :: String),
            "language" .= ("Japanese" :: String),
            "musicFolder" .= ("音楽/自分の曲" :: String),
            "screen" .= ("display-2" :: String)
          ]
   in case eitherDecode (encode legacy) of
        Right p ->
          savedVolume p == 0.42
            && savedBackground p == Lake
            && savedLanguage p == Japanese
            && savedFolder p == Just "音楽/自分の曲"
            && savedScreen p == "display-2"
            && savedPicture p == Nothing
            && savedPictureShade p == Original
        Left _ -> False

propPictureControls :: Bool
propPictureControls =
  and
    [ let (after, effects) = step event before
       in transport after == transport before
            && loudness after == loudness before
            && windowMode after == windowMode before
            && all (`notElem` effects) [PlaceWindow, MoveScreen, SkipTrack]
    | mode <- [Fullscreen, Compact],
      playing <- [Playing, Paused],
      let before = (initialSession defaultVolume) {windowMode = mode, transport = playing},
      event <-
        [ OpenPicture,
          BrowsePicture PreviousPicture,
          BrowsePicture NextPicture,
          RemovePicture,
          ChooseBackground Picture,
          CyclePictureShade
        ]
    ]

propPictureShade :: Bool
propPictureShade =
  all
    ( \shade ->
        nextShade (nextShade (nextShade shade)) == shade
          && shadeOpacity shade >= 0
          && shadeOpacity shade <= 128
    )
    [Original, Soft, Dim]

propRestRoundTrip :: NonNegative Int -> Bool
propRestRoundTrip (NonNegative n) =
  let before =
        (initialSession defaultVolume)
          { background = toEnum (n `mod` 5),
            pictureShade = toEnum (n `mod` 3),
            windowMode = if even n then Fullscreen else Compact,
            transport = if n `mod` 3 == 0 then Paused else Playing
          }
      (resting, enterEffects) = step ToggleRest before
      (after, exitEffects) = step ToggleRest resting
   in viewingMode resting == Resting
        && viewingMode after == Working
        && fromSession after (defaults Japanese) == fromSession before (defaults Japanese)
        && transport after == transport before
        && enterEffects == []
        && exitEffects == []

propRestActivity :: NonNegative Int -> Bool
propRestActivity (NonNegative n) =
  let resting = fst (step ToggleRest ((initialSession defaultVolume) {windowMode = if even n then Fullscreen else Compact}))
      elapsed = either (const zeroSeconds) id (seconds (fromIntegral n))
      after = fst (step Activity (fst (step (Elapse elapsed) resting)))
   in viewingMode after == Resting && not (controlsVisible after)

propRestChoices :: Bool
propRestChoices =
  and
    [ let original = (initialSession defaultVolume) {background = Picture, pictureShade = Dim, viewingMode = mode}
          (after, effects) = step action original
       in background after == Picture
            && pictureShade after == Dim
            && transport after == transport original
            && loudness after == loudness original
            && windowMode after == windowMode original
            && effects == (if mode == Resting then [] else [Persist])
    | mode <- [Working, Resting],
      action <- map ChooseRestScene [minBound .. maxBound] ++ [ToggleRestMotion, CycleRestShade]
    ]

propRestEscape :: Bool
propRestEscape =
  let rest = fst (step ToggleRest (initialSession defaultVolume))
      (after, effects) = step Escape rest
      (compact, nextEffects) = step Escape after
   in viewingMode after == Working
        && windowMode after == Fullscreen
        && effects == []
        && windowMode compact == Compact
        && PlaceWindow `elem` nextEffects

propMigrateRest :: Bool
propMigrateRest =
  let legacy =
        object
          [ "version" .= (2 :: Int),
            "volume" .= (0.42 :: Double),
            "background" .= ("Picture" :: String),
            "picture" .= ("user:my.png" :: String),
            "pictureShade" .= ("Dim" :: String),
            "musicFolder" .= ("my music" :: String)
          ]
   in case eitherDecode (encode legacy) of
        Right p ->
          savedVolume p == 0.42
            && savedPicture p == Just "user:my.png"
            && savedPictureShade p == Dim
            && savedFolder p == Just "my music"
            && savedRestScene p == TidalLight
            && savedRestMotion p == Gentle
            && viewingMode (restoreSession (fromSession (fst (step ToggleRest (restoreSession p))) p)) == Working
        Left _ -> False

propFreezeScene :: NonNegative Int -> Bool
propFreezeScene (NonNegative n) =
  let duration = either (const zeroSeconds) id (seconds (fromIntegral (1 + n `mod` 10000)))
      playing = (initialSession defaultVolume) {settingsOpen = True, settingsPage = RestSettings, restScene = toEnum (n `mod` (1 + fromEnum (maxBound :: RestScene)))}
      advance = fst . step (Elapse duration)
      moved = advance playing
      frozen = fst (step ToggleRestMotion moved)
      (held, effects) = step (Elapse duration) frozen
      resumed = advance (fst (step ToggleRestMotion held))
   in restSeconds held == restSeconds moved
        && restSeconds resumed > restSeconds held
        && transport held == Playing
        && effects == []

propMigrateRestV3 :: Bool
propMigrateRestV3 = and [migrates version name expected | version <- [3, 4, 5], (name, expected) <- [("AfternoonLake", TidalLight), ("WoodlandPond", Shallows), ("Aurora", Aurora)]]
  where
    migrates version name expected =
      let old =
            object
              [ "version" .= (version :: Int),
                "restScene" .= (name :: String),
                "restMotion" .= ("Still" :: String),
                "restShade" .= ("Dim" :: String),
                "volume" .= (0.42 :: Double),
                "screen" .= ("saved display" :: String),
                "language" .= ("Japanese" :: String),
                "picture" .= ("user:my.png" :: String),
                "musicFolder" .= ("my music" :: String)
              ]
       in case eitherDecode (encode old) of
            Right p ->
              savedRestScene p == expected
                && savedVolume p == 0.42
                && savedScreen p == "saved display"
                && savedLanguage p == Japanese
                && savedRestMotion p == Still
                && savedRestShade p == Dim
                && savedPicture p == Just "user:my.png"
                && savedFolder p == Just "my music"
                && viewingMode (restoreSession p) == Working
            Left _ -> False

propRestExitOnly :: NonNegative Int -> Bool
propRestExitOnly (NonNegative n) =
  let resting = (initialSession defaultVolume) {viewingMode = Resting, restSeconds = fromIntegral n, restScene = toEnum (n `mod` (1 + fromEnum (maxBound :: RestScene)))}
      blocked =
        [TogglePlayback, ChangeVolume 0.2, SetVolume defaultVolume, NextTrack, OpenPicture, BrowsePicture NextPicture, RemovePicture, CyclePictureShade, ToggleRestMotion, CycleRestShade, ToggleWindow, ToggleSettings, ShowRestSettings, ShowDisplaySettings, ShowMusicSettings, NextScreen, OpenMusic, UsePresets, ToggleLanguage, Activity]
          ++ map ChooseBackground [minBound .. maxBound]
          ++ map ChooseRestScene [minBound .. maxBound]
          ++ map ShowRestCollection [Landscapes, Atmospheres]
          ++ map ChooseRestMotion [Still, Gentle]
          ++ map ChooseRestShade [Original, Soft, Dim]
          ++ map ChoosePictureShade [Original, Soft, Dim]
   in all (\event -> step event resting == (resting, [])) blocked
        && all (\event -> let (after, effects) = step event resting in viewingMode after == Working && effects == [] && transport after == Playing) [Escape, LeaveRest, ToggleRest]

propPreviewClock :: Bool
propPreviewClock =
  let dt = either (const zeroSeconds) id (seconds 3)
      start = initialSession defaultVolume
      preview = fst (step ShowRestSettings start)
      elapsed = fst (step (Elapse dt) preview)
      still = fst (step ToggleRestMotion elapsed)
      held = fst (step (Elapse dt) still)
      resting = fst (step ToggleRest held)
      heldRest = fst (step (Elapse dt) resting)
   in restSeconds elapsed == 3
        && restSeconds held == 3
        && restSeconds heldRest == 0
        && restMotion heldRest == Still
        && transport heldRest == Playing
        && viewingMode heldRest == Resting

interfaceStates :: [Session]
interfaceStates = [(initialSession defaultVolume) {settingsOpen = opened, settingsPage = page, language = lang, windowMode = Compact, restCollection = collection} | opened <- [False, True], page <- [RestSettings, DisplaySettings, MusicSettings], lang <- [English, Japanese], collection <- [Landscapes, Atmospheres]]

propInterfaceTargets :: Property
propInterfaceTargets = forAll ((,) <$> chooseInt (700, 3840) <*> chooseInt (500, 2160)) $ \(w, h) ->
  let valid s = let cs = UI.controls w h s in all (target s) cs && and [not (overlap (UI.bounds a) (UI.bounds b)) | (i, a) <- zip [0 :: Int ..] cs, b <- drop (i + 1) cs]
      target s c = let UI.Bounds x y cw ch = UI.bounds c in cw >= 24 && ch >= 24 && x >= 0 && y >= 0 && x + cw <= fromIntegral w && y + ch <= fromIntegral h && UI.hitTest w h s (x + cw / 2, y + ch / 2) == Just (UI.action c)
      overlap (UI.Bounds x y cw ch) (UI.Bounds a b aw ah) = min (x + cw) (a + aw) > max x a && min (y + ch) (b + ah) > max y b
   in all valid interfaceStates

propFocusOrder :: NonNegative Int -> Bool
propFocusOrder (NonNegative n) = all valid interfaceStates
  where
    valid s =
      let cs = UI.controls 700 500 s
          expected = map (Just . UI.action) cs
          forward = take (length cs) (tail (iterate (UI.nextFocus False cs) Nothing))
          backward = take (length cs) (tail (iterate (UI.nextFocus True cs) Nothing))
          point = forward !! (n `mod` length cs)
       in forward == expected && backward == reverse expected && UI.nextFocus True cs (UI.nextFocus False cs point) == point

propRestInterface :: Float -> Float -> Bool
propRestInterface x y =
  let s = (initialSession defaultVolume) {viewingMode = Resting, settingsOpen = True}
   in null (UI.controls 700 500 s) && UI.nextFocus False (UI.controls 700 500 s) Nothing == Nothing && UI.hitTest 700 500 s (x, y) == Just (UI.Intent LeaveRest)

propDirectChoices :: Bool
propDirectChoices =
  and [valid (ChooseRestMotion m) (\s -> restMotion s == m) | m <- [Still, Gentle]]
    && and [valid (ChooseRestShade v) (\s -> restShade s == v) && valid (ChoosePictureShade v) (\s -> pictureShade s == v) | v <- [Original, Soft, Dim]]
  where
    before = (initialSession defaultVolume) {transport = Paused, background = Picture, windowMode = Compact}
    valid event chosen = let (after, _) = step event before in chosen after && transport after == Paused && loudness after == loudness before && background after == Picture && windowMode after == Compact
