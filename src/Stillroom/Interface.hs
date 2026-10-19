-- | One pure description drives drawing, pointer targets and keyboard order.
-- Screen geometry has no native handles and is testable without a GPU.
module Stillroom.Interface
  ( Bounds (..),
    UIAction (..),
    Style (..),
    Control (..),
    controls,
    hitTest,
    nextFocus,
    selected,
    contains,
    settingsBounds,
    dockBounds,
    previewBounds,
    galleryCards,
    volumeBounds,
    journeyExitBounds,
    local,
    backgroundName,
  )
where

import Data.List (find, findIndex)
import Stillroom.Domain
import Stillroom.Session

data Bounds = Bounds !Float !Float !Float !Float deriving (Eq, Show)

data UIAction = Intent Event | Quit deriving (Eq, Show)

data Style = Primary | Subtle | Segment | Navigation | Scene RestScene | Swatch Background | Play Transport | Skip | Close deriving (Eq, Show)

data Control = Control {bounds :: !Bounds, caption :: !String, action :: !UIAction, style :: !Style} deriving (Eq, Show)

local :: Session -> String -> String -> String
local s en ja = if language s == Japanese then ja else en

backgroundName :: Session -> Background -> String
backgroundName s choice = case choice of
  Forest -> local s "Quiet green" "静かな緑"
  Slate -> local s "Slate" "スレート"
  Black -> local s "Black" "黒"
  Lake -> local s "Drawn lake" "描かれた湖"
  Picture -> local s "My images" "画像の背景"

settingsBounds :: Int -> Int -> Bounds
settingsBounds w h = Bounds ((fromIntegral w - rw) / 2) ((fromIntegral h - rh) / 2) rw rh
  where
    rw = min 1104 (fromIntegral w - 32)
    rh = min 700 (fromIntegral h - 32)

dockBounds :: Int -> Int -> Bounds
dockBounds w h = Bounds ((fromIntegral w - 620) / 2) (fromIntegral h - 122) 620 98

previewBounds :: Int -> Int -> Bounds
previewBounds w h = Bounds (x + 24) (y + 126) pw (min (pw * 9 / 16) (rh - 358))
  where
    Bounds x y rw rh = settingsBounds w h
    pw = (rw - 48) * 0.56 - 32

galleryCards :: Int -> Int -> RestCollection -> [(RestScene, Bounds)]
galleryCards w h collection =
  [ (scene, Bounds (gx + fromIntegral (i `mod` columns) * (cw + 12)) (y + 126 + fromIntegral (i `div` columns) * (ch + 10)) cw ch)
  | (i, scene) <- zip [0 :: Int ..] scenes
  ]
  where
    Bounds x y rw rh = settingsBounds w h
    rightWidth = (rw - 48) * 0.44
    gx = x + rw - 24 - rightWidth
    scenes = case collection of Journeys -> [RailJourney]; Landscapes -> [BirchLake, AlpineLake, MeadowStream]; Atmospheres -> [TidalLight, AmberSea, Shallows, CloudSea, StarryNight, Aurora, LightVeil, InkFlow]
    columns = if collection == Atmospheres then 2 else 1
    rows = if collection == Atmospheres then 4 else 3 :: Int
    cw = (rightWidth - fromIntegral (columns - 1) * 12) / fromIntegral columns
    ch = (rh - 150 - fromIntegral (rows - 1) * 10) / fromIntegral rows

volumeBounds :: Int -> Int -> Bounds
volumeBounds w h = Bounds (x + 148) (y + 43) 104 36
  where
    Bounds x y _ _ = dockBounds w h

journeyExitBounds :: Int -> Bounds
journeyExitBounds w = Bounds (fromIntegral w - 192) 20 172 36

controls :: Int -> Int -> Session -> [Control]
controls w h s
  | not (controlsVisible s) = []
  | not (settingsOpen s) =
      [ c (Bounds (dx + 18) (dy + 43) 36 36) (local s "Play / pause" "再生・一時停止") TogglePlayback (Play (transport s)),
        c (Bounds (dx + 62) (dy + 43) 36 36) (local s "Next track" "次の曲") NextTrack Skip,
        c (Bounds (dx + 112) (dy + 43) 28 36) "−" (ChangeVolume (-0.025)) Subtle,
        c (Bounds (dx + 260) (dy + 43) 28 36) "+" (ChangeVolume 0.025) Subtle,
        c (Bounds (dx + 318) (dy + 42) 172 38) (local s "Take a pause" "ひと息つく") ToggleRest Primary,
        c (Bounds (dx + 506) (dy + 43) 94 36) (local s "Settings" "設定") ToggleSettings Subtle
      ]
  | otherwise = navigation ++ page
  where
    Bounds dx dy _ _ = dockBounds w h
    Bounds x y rw rh = settingsBounds w h
    Bounds px _ pw _ = previewBounds w h
    rightWidth = (rw - 48) * 0.44
    rx = x + rw - 24 - rightWidth
    c r title event kind = Control r title (Intent event) kind
    navigation =
      [ c (Bounds (x + rw - 388) (y + 23) 80 36) (local s "Rest" "休息") ShowRestSettings Navigation,
        c (Bounds (x + rw - 300) (y + 23) 100 36) (local s "Background" "作業背景") ShowDisplaySettings Navigation,
        c (Bounds (x + rw - 192) (y + 23) 116 36) (local s "Music & app" "音楽・表示") ShowMusicSettings Navigation,
        c (Bounds (x + rw - 60) (y + 23) 36 36) (local s "Close settings" "設定を閉じる") ToggleSettings Close
      ]
    shades left top width currentEvent =
      [ c (Bounds (left + fromIntegral i * (width / 3)) top (width / 3 - 4) 32) name (currentEvent value) Segment
      | (i, (name, value)) <- zip [0 :: Int ..] [("100%", Original), ("75%", Soft), ("50%", Dim)]
      ]
    page = case settingsPage s of
      RestSettings ->
        [ c (Bounds (rx + fromIntegral i * rightWidth / 3) (y + 86) (rightWidth / 3 - 4) 28) title (ShowRestCollection collection) Segment
        | (i, (title, collection)) <- zip [0 :: Int ..] [(local s "Journey" "旅", Journeys), (local s "Nature" "自然", Landscapes), (local s "Light & sky" "光と空", Atmospheres)]
        ]
          ++ [c r (restSceneName (language s) scene) (ChooseRestScene scene) (Scene scene) | (scene, r) <- galleryCards w h (restCollection s)]
          ++ [c (Bounds (px + offset * pw) (y + rh - 153) (width * pw - 3) 28) (graphicsQualityName (language s) quality) (ChooseGraphicsQuality quality) Segment
             | (quality, offset, width) <- zip3 [minBound .. maxBound] (scanl (+) 0 qualityWidths) qualityWidths]
          ++ ( if restScene s == RailJourney
                 then [c (Bounds px (y + rh - 102) (pw * 0.45) 32) (local s "New landscape" "新しい景色") NewJourney Subtle]
                 else
                   [ c (Bounds px (y + rh - 102) (pw * 0.45 / 2 - 4) 32) (local s "Still" "静止") (ChooseRestMotion Still) Segment,
                     c (Bounds (px + pw * 0.45 / 2) (y + rh - 102) (pw * 0.45 / 2 - 4) 32) (local s "Gentle" "ゆらぎ") (ChooseRestMotion Gentle) Segment
                   ]
             )
          ++ shades (px + pw * 0.49) (y + rh - 102) (pw * 0.51) ChooseRestShade
          ++ [c (Bounds px (y + rh - 52) pw 38) (if restScene s == RailJourney then local s "Continue the journey" "旅を続ける" else local s "Rest in this scene" "この景色で、ひと息") ToggleRest Primary]
      DisplaySettings ->
        [ c (Bounds rx (y + 114 + fromIntegral i * 44) rightWidth 38) (backgroundName s choice) (ChooseBackground choice) (Swatch choice)
        | (i, choice) <- zip [0 :: Int ..] [Forest, Slate, Black, Lake, Picture]
        ]
          ++ [ c (Bounds px (y + rh - 102) 40 32) (local s "Previous image" "前の画像") (BrowsePicture PreviousPicture) Subtle,
               c (Bounds (px + 48) (y + rh - 102) (pw - 96) 32) (local s "Add image…" "画像を追加…") OpenPicture Subtle,
               c (Bounds (px + pw - 40) (y + rh - 102) 40 32) (local s "Next image" "次の画像") (BrowsePicture NextPicture) Subtle,
               c (Bounds px (y + rh - 52) pw 38) (local s "Remove imported copy" "登録した画像を解除") RemovePicture Subtle
             ]
          ++ shades rx (y + rh - 52) rightWidth ChoosePictureShade
      MusicSettings ->
        [ c (Bounds px (y + 208) pw 40) (local s "Choose a music folder…" "音楽フォルダーを選ぶ…") OpenMusic Subtle,
          c (Bounds px (y + 258) pw 40) (local s "Use built-in music" "内蔵の音楽を使う") UsePresets Subtle,
          c (Bounds rx (y + 132) rightWidth 40) (local s "Move to another display" "別のディスプレイへ移す") NextScreen Subtle,
          c (Bounds rx (y + 182) rightWidth 40) (if windowMode s == Fullscreen then local s "Use a window" "ウィンドウで使う" else local s "Use fullscreen" "全画面で使う") ToggleWindow Subtle,
          c (Bounds rx (y + 292) rightWidth 40) "日本語 / English" ToggleLanguage Subtle,
          Control (Bounds rx (y + rh - 52) rightWidth 38) (local s "Quit Stillroom" "Stillroomを終了") Quit Subtle
        ]

    -- Keep "Standard" readable even in the smallest English window.
    qualityWidths = if language s == English then [0.18, 0.18, 0.28, 0.18, 0.18] else replicate 5 0.2

contains :: Bounds -> (Float, Float) -> Bool
contains (Bounds x y w h) (px, py) = px >= x && px <= x + w && py >= y && py <= y + h

hitTest :: Int -> Int -> Session -> (Float, Float) -> Maybe UIAction
hitTest w h s point@(mx, _)
  | viewingMode s == Resting && restScene s == RailJourney =
      if contains (journeyExitBounds w) point then Just (Intent LeaveRest) else Nothing
  | viewingMode s == Resting = Just (Intent LeaveRest)
  | not (controlsVisible s) = Just (Intent ToggleRest)
  | not (settingsOpen s),
    contains slider point =
      either (const Nothing) (Just . Intent . SetVolume) (volume (realToFrac (max 0 (min 1 ((mx - sx) / sw)))))
  | otherwise = case find (\control -> contains (bounds control) point) (controls w h s) of
      Just control -> Just (action control)
      Nothing | settingsOpen s || contains (dockBounds w h) point -> Nothing
      Nothing -> Just (Intent ToggleRest)
  where
    slider@(Bounds sx _ sw _) = volumeBounds w h

-- Tab order uses stable actions, not stale integer indices from another page.
nextFocus :: Bool -> [Control] -> Maybe UIAction -> Maybe UIAction
nextFocus backwards available current = case available of
  [] -> Nothing
  _ -> case drop index available of
    control : _ -> Just (action control)
    [] -> Nothing
  where
    count = length available
    position = current >>= (\value -> findIndex ((== value) . action) available)
    index = if count == 0 then 0 else maybe (if backwards then count - 1 else 0) (\i -> (i + if backwards then count - 1 else 1) `mod` count) position

selected :: Session -> UIAction -> Bool
selected s (Intent event) = case event of
  ChooseBackground value -> background s == value
  ChooseRestScene value -> restScene s == value
  ShowRestCollection value -> restCollection s == value
  ChooseRestMotion value -> restMotion s == value
  ChooseGraphicsQuality value -> graphicsQuality s == value
  ChooseRestShade value -> restShade s == value
  ChoosePictureShade value -> pictureShade s == value
  ShowRestSettings -> settingsPage s == RestSettings
  ShowDisplaySettings -> settingsPage s == DisplaySettings
  ShowMusicSettings -> settingsPage s == MusicSettings
  _ -> False
selected _ Quit = False
