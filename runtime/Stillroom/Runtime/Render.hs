{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Render (UIAction (..), loadTypeface, render, hitTest, galleryCards, allInterfaceText) where

import Control.Exception (throwIO)
import Control.Monad (forM_, when)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.List (nub)
import Foreign (castPtr, free, peek, withArray)
import Foreign.C (withCString)
import Paths_stillroom (getDataFileName)
import Raylib.Core (clearBackground)
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures (drawTexturePro, setTextureFilter)
import Raylib.Internal (WindowResources, managed)
import Raylib.Internal.Foreign (pop)
import Raylib.Types (Color (..), Font (..), Rectangle (..), Texture (..), TextureFilter (..), pattern Vector2)
import Stillroom.Domain
import Stillroom.Runtime.Pictures (PictureError (..), pictureErrorText)
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Session
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

data UIAction = Intent Event | Quit deriving (Eq, Show)

data Button = Button Rectangle String UIAction

loadTypeface :: WindowResources -> [String] -> IO Font
loadTypeface window titles = do
  executable <- getExecutablePath
  let relative = "assets" </> "fonts" </> "NotoSansJP.otf"
      beside = takeDirectory executable </> relative
      glyphs = take 4096 (nub ([32 .. 126] ++ map ord (allInterfaceText ++ concat titles)))
  exists <- doesFileExist beside
  file <- if exists then pure beside else getDataFileName relative
  bytes <- BS.readFile file
  font <- managed window $ BS.useAsCStringLen bytes $ \(buffer, size) ->
    withCString ".otf" $ \extension -> withArray (map fromIntegral glyphs) $ \points -> do
      raw <- c'loadFontFromMemory extension (castPtr buffer) (fromIntegral size) 32 points (fromIntegral (length glyphs))
      borrowed <- peek raw
      -- raylib falls back to its shared default font on failure. h-raylib's
      -- pop transfers/frees glyph arrays, so never pop that borrowed fallback.
      if font'baseSize borrowed /= 32
        then free raw >> throwIO (userError "The bundled font could not be loaded.")
        else pop raw
  _ <- setTextureFilter (font'texture font) TextureFilterBilinear
  pure font

allInterfaceText :: String
allInterfaceText =
  "作業モード再生中一時停止次へ音量表示終了霧の湖静かな森砂丘夜の抽象画画像を追加してください内蔵の画像です。登録した画像だけ解除できます。"
    ++ "休息静止景色明るさ背景クリックかEscで戻る動き設定。時間制限はありません。作業へ戻ると通常背景に戻ります。"
    ++ "休息の景色を選ぶ選択中プレビュー休息中はクリックかEscで戻れます。休息の景色を描画できません。クリックかEscで戻ります。"
    ++ concat [restSceneName l scene ++ restSceneDescription l scene | l <- [English, Japanese], scene <- [minBound .. maxBound]]
    ++ concat
      [ caption
      | l <- [English, Japanese],
        t <- [Playing, Paused],
        shade <- [Original, Soft, Dim],
        page <- [DisplaySettings, RestSettings],
        motion <- [Still, Gentle],
        let s = (initialSession defaultVolume) {language = l, transport = t, settingsOpen = True, settingsPage = page, pictureShade = shade, restShade = shade, restMotion = motion},
        Button _ caption _ <- buttons 760 500 s
      ]
    ++ concat
      [ pictureErrorText l issue
      | l <- [English, Japanese],
        issue <- [ImageUnavailable, UnsupportedImage, ImageTooLarge, ImageDecodeFailed]
      ]

label :: Session -> String -> String -> String
label s en ja = if language s == Japanese then ja else en

isRestGallery :: Session -> Bool
isRestGallery s = settingsOpen s && settingsPage s == RestSettings

galleryBounds :: Int -> Int -> Rectangle
galleryBounds w h = Rectangle ((fromIntegral w - rw) / 2) ((fromIntegral h - rh) / 2) rw rh
  where
    rw = min 1060 (fromIntegral w - 32)
    rh = min 700 (fromIntegral h - 32)

galleryCards :: Int -> Int -> [(RestScene, Rectangle)]
galleryCards w h =
  [ (scene, Rectangle (x + 20 + fromIntegral (i `mod` 3) * (cw + 8)) (y + 64 + fromIntegral (i `div` 3) * (ch + 8)) cw ch)
  | (i, scene) <- zip [0 :: Int ..] [TidalLight, AmberSea, Shallows, CloudSea, StarryNight, Aurora, LightVeil, InkFlow]
  ]
  where
    Rectangle x y rw rh = galleryBounds w h
    cw = (rw * 0.58 - 16) / 3
    ch = (rh - 124) / 3 - 8

galleryPreview :: Int -> Int -> Rectangle
galleryPreview w h = Rectangle (x + rw * 0.58 + 40) (y + 86) pw (pw * 9 / 16)
  where
    Rectangle x y rw _ = galleryBounds w h
    pw = rw * 0.42 - 60

galleryButtons :: Int -> Int -> Session -> [Button]
galleryButtons w h s =
  [Button r (restSceneName (language s) scene) (Intent (ChooseRestScene scene)) | (scene, r) <- galleryCards w h]
    ++ [ b px (py + ph + 58) pw (label s "Motion: " "動き: " ++ restMotionName (language s) (restMotion s)) ToggleRestMotion,
         b px (py + ph + 98) pw (label s "Brightness: " "明るさ: " ++ brightness) CycleRestShade,
         b px (y + rh - 54) pw (label s "Start resting" "この景色で休息する") ToggleRest,
         b (x + 20) (y + rh - 54) (gw / 2 - 4) (label s "Display / music" "表示と音楽の設定") ShowDisplaySettings,
         b (x + 24 + gw / 2) (y + rh - 54) (gw / 2 - 4) (label s "Close" "設定を閉じる") ToggleSettings
       ]
  where
    Rectangle x y rw rh = galleryBounds w h
    Rectangle px py pw ph = galleryPreview w h
    gw = rw * 0.58
    b a c width caption event = Button (Rectangle a c width 34) caption (Intent event)
    brightness = case restShade s of Original -> "100%"; Soft -> "75%"; Dim -> "50%"

buttons :: Int -> Int -> Session -> [Button]
buttons w h s | isRestGallery s = galleryButtons w h s
buttons w h s =
  [ b 344 8 150 (label s "Rest  [R]" "休息  [R]") ToggleRest,
    b 20 54 64 (if transport s == Playing then "II" else ">") TogglePlayback,
    b 92 54 70 (label s "Next" "次へ") NextTrack,
    b 188 54 40 "-" (ChangeVolume (-0.025)),
    b 394 54 40 "+" (ChangeVolume 0.025),
    b 454 54 100 (label s "Settings" "設定") ToggleSettings,
    Button (rect 566 54 54 40) "x" Quit
  ]
    ++ if not (settingsOpen s)
      then []
      else
        [ q 20 18 142 (label s "Forest" "静かな緑") (ChooseBackground Forest),
          q 172 18 142 (label s "Slate" "暗い灰色") (ChooseBackground Slate),
          q 324 18 142 (label s "Black" "黒") (ChooseBackground Black),
          q 476 18 142 (label s "Lake" "湖畔") (ChooseBackground Lake),
          q 20 64 142 (label s "Add image [I]" "画像を追加 [I]") OpenPicture,
          q 172 64 142 (label s "< Previous" "< 前の画像") (BrowsePicture PreviousPicture),
          q 324 64 142 (label s "Next >" "次の画像 >") (BrowsePicture NextPicture),
          q 476 64 142 (label s "Show image" "画像を表示") (ChooseBackground Picture),
          q 20 110 294 (label s "Image brightness: " "画像の明るさ: " ++ shadeLabel (pictureShade s)) CyclePictureShade,
          q 324 110 294 (label s "Remove imported image" "登録した画像を解除") RemovePicture,
          q 20 156 294 (label s "Music folder...  [O]" "音楽フォルダー  [O]") OpenMusic,
          q 324 156 294 (label s "Built-in music  [P]" "内蔵プリセット  [P]") UsePresets,
          q 20 202 294 (label s "Move to next screen  [M]" "表示する画面を移動  [M]") NextScreen,
          q 324 202 294 (label s "Fullscreen / window  [F11]" "全画面 / 小さく  [F11]") ToggleWindow,
          q 20 248 192 (label s "Rest settings" "休息の設定") ShowRestSettings,
          q 222 248 192 (label s "日本語 / English" "English / 日本語") ToggleLanguage,
          q 424 248 194 (label s "Close settings" "設定を閉じる") ToggleSettings
        ]
  where
    x = fromIntegral (w - 640) / 2
    y = fromIntegral (h - 160) - 28
    rect a c width height = Rectangle (x + a) (y + c) width height
    b a c width text event = Button (rect a c width 40) text (Intent event)
    q a c width text event = Button (rect a (c - 310) width 38) text (Intent event)
    shadeLabel Original = "100%"
    shadeLabel Soft = "75%"
    shadeLabel Dim = "50%"

hitTest :: Int -> Int -> Session -> (Float, Float) -> Maybe UIAction
hitTest w h s (mx, my)
  | viewingMode s == Resting = Just (Intent LeaveRest)
  | not (controlsVisible s) = Just (Intent ToggleRest)
  | not (isRestGallery s),
    my >= y + 54 && my <= y + 94 && mx >= x + 236 && mx <= x + 386 =
      either (const Nothing) (Just . Intent . SetVolume) (volume (realToFrac (max 0 (min 1 ((mx - x - 236) / 150)))))
  | otherwise = case go (buttons w h s) of
      Just action -> Just action
      Nothing | settingsOpen s || (mx >= x && mx <= x + 640 && my >= y && my <= y + 160) -> Nothing
      Nothing -> Just (Intent ToggleRest)
  where
    x = fromIntegral (w - 640) / 2
    y = fromIntegral (h - 160) - 28
    go [] = Nothing
    go (Button (Rectangle a b rw rh) _ action : rest)
      | mx >= a && mx <= a + rw && my >= b && my <= b + rh = Just action
      | otherwise = go rest

render :: Font -> Rest.RestAssets -> Int -> Int -> Session -> Maybe Texture -> String -> String -> Maybe String -> (Float, Float) -> IO ()
render font restAssets w h s picture pictureCaption track message mouse = do
  drawBackground w h (background s)
  when (background s == Picture) $ forM_ picture $ \texture -> do
    let tw = fromIntegral (texture'width texture)
        th = fromIntegral (texture'height texture)
        wf = fromIntegral w
        hf = fromIntegral h
        scale = max (wf / tw) (hf / th)
        cropW = wf / scale
        cropH = hf / scale
    drawTexturePro
      texture
      (Rectangle ((tw - cropW) / 2) ((th - cropH) / 2) cropW cropH)
      (Rectangle 0 0 wf hf)
      (Vector2 0 0)
      0
      (Color 255 255 255 255)
    drawRectangle 0 0 w h (Color 0 0 0 (fromIntegral (shadeOpacity (pictureShade s))))
  when (viewingMode s == Resting) $ do
    Rest.renderRest restAssets w h s
    let issue = Rest.restIssue restAssets s
    when (restHintSeconds s > 0 || maybe False (const True) issue) $ do
      let hint =
            label
              s
              "Click or Esc to return"
              "クリックかEscで戻る"
          x = fromIntegral (w - 640) / 2
          y = fromIntegral h - 90
      panel (Rectangle x y 640 42)
      fittedText (maybe hint id issue) (x + 20) (y + 12) 600 15 secondary
  when (controlsVisible s && isRestGallery s) $ do
    drawRectangle 0 0 w h (Color 3 9 11 190)
    let bounds@(Rectangle gx gy _ _) = galleryBounds w h
        preview@(Rectangle px py pw ph) = galleryPreview w h
    panel bounds
    text (label s "Choose a place to rest" "休息の景色を選ぶ") (gx + 20) (gy + 20) 23 primary
    text (label s "Preview" "プレビュー") px (gy + 24) 13 secondary
    text (restSceneName (language s) (restScene s)) px (gy + 52) 20 primary
    Rest.renderPreview restAssets h preview s
    wrappedText (maybe (restSceneDescription (language s) (restScene s)) id (Rest.restIssue restAssets s)) px (py + ph + 12) pw 14 secondary
    wrappedText (label s "While resting, click or Esc to return." "休息中は、クリックかEscで戻れます。") px (py + ph + 144) pw 12 secondary
    forM_ (buttons w h s) $ \(Button rectangle@(Rectangle a b rw rh) caption action) -> case action of
      Intent (ChooseRestScene scene) -> do
        let selected = restScene s == scene
            thumbH = min (rh - 28) ((rw - 10) * 9 / 16)
            thumbW = thumbH * 16 / 9
        drawRectangleRounded rectangle 0.08 8 (if selected then Color 68 99 89 255 else Color 27 40 38 255)
        Rest.renderThumbnail restAssets scene (Rectangle (a + (rw - thumbW) / 2) (b + 4) thumbW thumbH)
        fittedText caption (a + 8) (b + rh - 20) (rw - 16) 14 primary
        when selected (drawRectangleLinesEx rectangle 2 (Color 166 210 187 255))
      _ -> drawButton rectangle caption action
  when (controlsVisible s && not (isRestGallery s)) $ do
    let x = fromIntegral (w - 640) / 2; y = fromIntegral (h - 160) - 28
    panel (Rectangle x y 640 160)
    when (settingsOpen s) (panel (Rectangle x (y - 310) 640 300))
    text (label s "Stillroom" "Stillroom / 作業モード") (x + 22) (y + 17) 21 primary
    text (if transport s == Playing then label s "PLAYING" "再生中" else label s "PAUSED" "一時停止") (x + 515) (y + 21) 13 secondary
    forM_ (buttons w h s) $ \(Button rectangle caption action) -> drawButton rectangle caption action
    drawRectangleRounded (Rectangle (x + 236) (y + 72) 150 4) 1 8 (Color 58 72 64 255)
    let filled = 150 * realToFrac (volumeValue (loudness s))
    drawRectangleRounded (Rectangle (x + 236) (y + 72) filled 4) 1 8 (Color 161 185 164 255)
    drawCircle (round (x + 236 + filled)) (round (y + 74)) 5 (Color 205 222 208 255)
    let status = if settingsOpen s && settingsPage s == DisplaySettings && background s == Picture then pictureCaption else track
    fittedText (maybe status id message) (x + 22) (y + 110) 596 15 secondary
    text (label s "R / click background: rest    Space: pause    Esc: window" "R / 背景クリック: 休息    Space: 一時停止    Esc: 小さく") (x + 22) (y + 137) 11 (Color 105 125 113 255)
  where
    primary = Color 222 234 225 255
    secondary = Color 146 167 151 255
    panel r = drawRectangleRounded r 0.06 12 (Color 18 27 26 252)
    text caption x y size color = drawTextEx font caption (Vector2 x y) size 0.2 color
    drawButton rectangle@(Rectangle a b rw rh) caption action = do
      let (mx, my) = mouse
          hovered = mx >= a && mx <= a + rw && my >= b && my <= b + rh
          selected = case action of
            Intent (ChooseBackground choice) -> background s == choice
            Intent (ChooseRestScene choice) -> restScene s == choice
            _ -> False
      let startsRest = action == Intent ToggleRest
      drawRectangleRounded rectangle 0.24 8 (if startsRest then Color 133 178 153 255 else if hovered || selected then Color 55 76 65 255 else Color 34 48 41 255)
      Vector2 tw _ <- measureTextEx font caption 16 0.2
      text caption (a + (rw - tw) / 2) (b + (rh - 16) / 2) 16 (if startsRest then Color 15 34 25 255 else primary)
    wrappedText caption x y width size color = wrap "" caption y
      where
        wrap line [] lineY = text line x lineY size color
        wrap line (c : cs) lineY = do
          Vector2 measured _ <- measureTextEx font (line ++ [c]) size 0.2
          if measured > width && not (null line)
            then text line x lineY size color >> wrap [c] cs (lineY + size + 4)
            else wrap (line ++ [c]) cs lineY
    fittedText caption x y width size color = do
      Vector2 measured _ <- measureTextEx font caption size 0.2
      if measured <= width || length caption < 4
        then text caption x y size color
        else fittedText (take (length caption - 4) caption ++ "...") x y width size color

-- Static scenery, constructed from original geometry. No animation, flashing,
-- stock image, external sample, or account is needed.
drawBackground :: Int -> Int -> Background -> IO ()
drawBackground _ _ Forest = clearBackground (Color 25 35 31 255)
drawBackground _ _ Slate = clearBackground (Color 30 32 34 255)
drawBackground _ _ Black = clearBackground (Color 0 0 0 255)
drawBackground _ _ Picture = clearBackground (Color 25 35 31 255)
drawBackground w h Lake = do
  clearBackground (Color 35 48 49 255)
  drawRectangleGradientV 0 0 w h (Color 53 65 63 255) (Color 21 33 32 255)
  let wf = fromIntegral w
      hf = fromIntegral h
      horizon = hf * 0.48
      ridge shift base amplitude color = forM_ [0 .. 119 :: Int] $ \i -> do
        let x1 = wf * fromIntegral i / 119
            x2 = wf * fromIntegral (i + 1) / 119
            height x = base - amplitude * (0.5 + 0.23 * sin (x / wf * 12 + shift) + 0.19 * sin (x / wf * 23 + shift * 2))
            y1 = height x1
            y2 = height x2
        drawTriangle (Vector2 x1 y1) (Vector2 x1 horizon) (Vector2 x2 y2) color
        drawTriangle (Vector2 x2 y2) (Vector2 x1 horizon) (Vector2 x2 horizon) color
  ridge 1 (hf * 0.40) (hf * 0.13) (Color 43 59 57 255)
  ridge 3 (hf * 0.46) (hf * 0.13) (Color 29 47 42 255)
  drawRectangleGradientV 0 (round horizon) w (h - round horizon) (Color 38 55 52 255) (Color 15 29 25 255)
  forM_ [0 .. 35 :: Int] $ \i -> do
    let y = round (horizon + fromIntegral (i * i) * hf / 2800)
        inset = round (wf * (0.14 + 0.09 * sin (fromIntegral i * 1.7)))
    drawRectangle inset y (max 1 (w - 2 * inset)) 1 (Color 59 73 66 26)
