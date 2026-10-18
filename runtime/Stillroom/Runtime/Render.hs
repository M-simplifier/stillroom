{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Render (UIAction (..), Typeface, loadTypeface, unloadTypeface, render, renderWithFocus, hitTest, galleryCards, allInterfaceText) where

import Control.Monad (forM_, when)
import Data.Char (ord)
import Data.List (nub)
import Paths_stillroom (getDataFileName)
import Raylib.Core.Shapes
import Raylib.Core.Textures (drawTexturePro)
import Raylib.Internal (WindowResources)
import Raylib.Types (Color (..), Rectangle (..), Texture (..), pattern Vector2)
import Stillroom.Domain
import Stillroom.Interface (UIAction (..))
import Stillroom.Interface qualified as UI
import Stillroom.Runtime.Journey qualified as Journey
import Stillroom.Runtime.Pictures (PictureError (..), pictureErrorText)
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Runtime.Typeface (Typeface)
import Stillroom.Runtime.Typeface qualified as Typeface
import Stillroom.Session
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

loadTypeface :: WindowResources -> [String] -> IO Typeface
loadTypeface window titles = do
  executable <- getExecutablePath
  let relative = "assets" </> "fonts" </> "NotoSansJP.otf"
      beside = takeDirectory executable </> relative
      glyphs = take 4096 (nub ([32 .. 126] ++ map ord (allInterfaceText ++ concat titles)))
  exists <- doesFileExist beside
  file <- if exists then pure beside else getDataFileName relative
  Typeface.load window file glyphs

unloadTypeface :: WindowResources -> Typeface -> IO ()
unloadTypeface = Typeface.unload

allInterfaceText :: String
allInterfaceText =
  Journey.interfaceText
    ++ "−…作業モード再生中一時停止次へ音量表示終了霧の湖静かな森砂丘夜の抽象画午後の湖森の池画像を追加してください内蔵の画像です。登録した画像だけ解除できます。"
    ++ "休息静止景色明るさ動き設定背景をクリック、またはRで休息。クリックかEscで戻る休息の景色を描画できません。クリックかEscで戻ります。"
    ++ "作業中の景色音楽再生中の曲表示言語画像の明るさ音は休息中も、そのまま続きます。"
    ++ "眺める。降りて歩く。また乗る。Eで停車してから、近くの地面へ。Escで作業へ。旅の続きは、また次に。"
    ++ concat [restSceneName l scene ++ restSceneDescription l scene | l <- [English, Japanese], scene <- [minBound .. maxBound]]
    ++ concat
      [ UI.caption c
      | l <- [English, Japanese],
        t <- [Playing, Paused],
        page <- [DisplaySettings, RestSettings, MusicSettings],
        opened <- [True, False],
        mode <- [Fullscreen, Compact],
        scene <- [TidalLight, RailJourney],
        collection <- [Landscapes, Atmospheres, Journeys],
        let s = (initialSession defaultVolume) {language = l, transport = t, settingsOpen = opened, settingsPage = page, windowMode = mode, restScene = scene, restCollection = collection},
        c <- UI.controls 760 500 s
      ]
    ++ concat [pictureErrorText l issue | l <- [English, Japanese], issue <- [ImageUnavailable, UnsupportedImage, ImageTooLarge, ImageDecodeFailed]]

rectangle :: UI.Bounds -> Rectangle
rectangle (UI.Bounds x y w h) = Rectangle x y w h

galleryCards :: Int -> Int -> RestCollection -> [(RestScene, Rectangle)]
galleryCards w h collection = [(scene, rectangle r) | (scene, r) <- UI.galleryCards w h collection]

hitTest :: Int -> Int -> Session -> (Float, Float) -> Maybe UIAction
hitTest = UI.hitTest

-- Contrast and selection share semantic roles across pages.
ink, surface, primary, secondary, edge, accent :: Color
ink = Color 16 23 22 255
surface = Color 31 42 38 255
primary = Color 231 235 224 255
secondary = Color 169 181 169 255
edge = Color 64 81 72 255
accent = Color 178 205 180 255

render :: Typeface -> Rest.RestAssets -> Int -> Int -> Session -> Maybe Texture -> String -> String -> Maybe String -> (Float, Float) -> IO ()
render font assets w h s picture pictureCaption track message mouse = renderWithFocus font assets w h s picture pictureCaption track message mouse Nothing

renderWithFocus :: Typeface -> Rest.RestAssets -> Int -> Int -> Session -> Maybe Texture -> String -> String -> Maybe String -> (Float, Float) -> Maybe UIAction -> IO ()
renderWithFocus font assets w h s picture pictureCaption track message mouse focused = do
  when (viewingMode s == Working) $ drawWorking (Rectangle 0 0 (fromIntegral w) (fromIntegral h)) (background s) picture (pictureShade s)
  when (viewingMode s == Resting) $ do
    Rest.renderRest assets w h s
    issue <- Rest.restIssue assets s
    if restScene s == RailJourney
      then do
        let exitBox = rectangle (UI.journeyExitBounds w)
        drawRectangleRounded exitBox 0.2 8 (Color 9 18 17 180)
        centered (label "Return to work  Esc" "作業に戻る  Esc") exitBox 14 primary
        linesToShow <- maybe (Journey.hints (Rest.journey assets) (language s)) (pure . (: [])) issue
        let width = min 820 (fromIntegral w - 32)
            height = fromIntegral (length linesToShow) * 25 + 18
            box = Rectangle ((fromIntegral w - width) / 2) (fromIntegral h - height - 20) width height
        drawRectangleRounded box 0.15 8 (Color 9 18 17 190)
        forM_ (zip [0 :: Int ..] linesToShow) $ \(i, line) ->
          centered line (Rectangle ((fromIntegral w - width) / 2) (fromIntegral h - height - 14 + fromIntegral i * 25) width 25) 13 secondary
      else when (restHintSeconds s > 0 || maybe False (const True) issue) $ do
        let width = min 620 (fromIntegral w - 48)
            left = (fromIntegral w - width) / 2
            r = Rectangle left (fromIntegral h - 78) width 40
        panel r
        centered (maybe (label "Click or Esc to return" "クリックかEscで戻る") id issue) r 14 secondary
  when (controlsVisible s) $ do
    if settingsOpen s then settings else dock
    forM_ (UI.controls w h s) drawControl
  where
    label = UI.local s
    UI.Bounds x y rw rh = UI.settingsBounds w h
    preview@(Rectangle px py pw ph) = rectangle (UI.previewBounds w h)
    rightWidth = (rw - 48) * 0.44
    rx = x + rw - 24 - rightWidth
    panel r = drawRectangleRounded r 0.045 12 ink
    text caption a b size color = Typeface.draw font caption (Vector2 a b) size 0.2 color
    fitted caption a b width size color = fit caption width size >>= \short -> text short a b size color
    fit caption width size = do
      Vector2 measured _ <- Typeface.measure font caption size 0.2
      if measured <= width || length caption < 4 then pure caption else fit (take (length caption - 4) caption ++ "...") width size
    centered caption (Rectangle a b width height) size color = do
      short <- fit caption (width - 16) size
      Vector2 tw _ <- Typeface.measure font short size 0.2
      text short (a + (width - tw) / 2) (b + (height - size) / 2 - 1) size color
    -- Two lines at most. Long filenames use ellipsis instead.
    wrapped caption a b width size color = go "" caption b (2 :: Int)
      where
        go line [] lineY _ = text line a lineY size color
        go line remaining lineY 1 = fitted (line ++ remaining) a lineY width size color
        go line (c : cs) lineY rows = do
          Vector2 measured _ <- Typeface.measure font (line ++ [c]) size 0.2
          if measured > width && not (null line)
            then text line a lineY size color >> go [c] cs (lineY + size + 5) (rows - 1)
            else go (line ++ [c]) cs lineY rows
    rule a b width = drawRectangleRec (Rectangle a b width 1) (Color 45 59 52 255)
    settings = do
      drawRectangle 0 0 w h (Color 3 9 8 170)
      panel (rectangle (UI.settingsBounds w h))
      text "stillroom" (x + 24) (y + 27) 23 primary
      rule (x + 24) (y + 72) (rw - 48)
      case settingsPage s of
        RestSettings -> do
          text (restSceneName (language s) (restScene s)) px (y + 88) 24 primary
          Rest.renderPreview assets h preview s
          issue <- Rest.restIssue assets s
          wrapped (maybe (restSceneDescription (language s) (restScene s)) id issue) px (py + ph + 13) pw 16 secondary
          text (if restScene s == RailJourney then label "Landscape" "旅の風景" else label "Motion" "動き") px (y + rh - 124) 14 secondary
          text (label "Brightness" "明るさ") (px + pw * 0.49) (y + rh - 124) 14 secondary
          when (restCollection s == Journeys) $ do
            let startY = y + 156 + (rh - 170) / 3
            wrapped (label "Ride, wander, and return." "眺める。降りて歩く。また乗る。") rx startY rightWidth 16 primary
            wrapped (label "E stops the railcar before stepping outside." "Eで停車してから、近くの地面へ。") rx (startY + 47) rightWidth 14 secondary
            wrapped (label "Esc returns to work. Your place is saved." "Escで作業へ。旅の続きは、また次に。") rx (startY + 93) rightWidth 14 secondary
        DisplaySettings -> do
          text (label "Your working view" "作業中の景色") px (y + 88) 24 primary
          drawWorking preview (background s) picture (pictureShade s)
          fitted (if background s == Picture then pictureCaption else UI.backgroundName s (background s)) px (py + ph + 13) pw 16 secondary
          text (label "Background" "背景") rx (y + 88) 16 secondary
          text (label "Image brightness" "画像の明るさ") rx (y + rh - 76) 14 secondary
        MusicSettings -> do
          text (label "Music" "音楽") px (y + 88) 24 primary
          text (label "NOW PLAYING" "再生中の曲") px (y + 132) 14 secondary
          wrapped track px (y + 158) pw 18 primary
          wrapped (label "The music stays with you while you rest." "音は休息中も、そのまま続きます。") px (y + 316) pw 16 secondary
          text (label "Display" "表示") rx (y + 88) 24 primary
          text (label "Language" "言語") rx (y + 260) 16 secondary
      forM_ message $ \notice -> fitted notice (x + 24) (min (fromIntegral h - 21) (y + rh + 6)) (rw - 48) 12 primary
    dock = do
      let r@(Rectangle a b width _) = rectangle (UI.dockBounds w h)
          Rectangle sx sy sw sh = rectangle (UI.volumeBounds w h)
          filled = sw * realToFrac (volumeValue (loudness s))
      panel r
      text "stillroom" (a + 20) (b + 12) 18 primary
      fitted (maybe track id message) (a + 135) (b + 15) (width - 155) 13 secondary
      drawRectangleRounded (Rectangle sx (sy + sh / 2 - 1) sw 3) 1 8 edge
      drawRectangleRounded (Rectangle sx (sy + sh / 2 - 1) filled 3) 1 8 accent
      drawCircle (round (sx + filled)) (round (sy + sh / 2)) 4 primary
    drawControl control = do
      let bounds = UI.bounds control
          r@(Rectangle a b cw ch) = rectangle bounds
          action = UI.action control
          chosen = UI.selected s action
          hovered = UI.contains bounds mouse
          fill = if hovered then Color 45 61 52 255 else surface
          title = UI.caption control
          checkMark = do
            drawLineEx (Vector2 (a + cw - 22) (b + ch / 2)) (Vector2 (a + cw - 18) (b + ch / 2 + 4)) 2 accent
            drawLineEx (Vector2 (a + cw - 18) (b + ch / 2 + 4)) (Vector2 (a + cw - 11) (b + ch / 2 - 4)) 2 accent
      case UI.style control of
        UI.Scene scene -> do
          Rest.renderThumbnail assets scene r
          drawRectangleRec (Rectangle a (b + ch - 25) cw 25) (Color 9 17 17 235)
          fitted title (a + 8) (b + ch - 21) (cw - 16) 15 primary
          when (chosen || hovered) (drawRectangleLinesEx r (if chosen then 2 else 1) accent)
        UI.Navigation -> do
          when hovered (drawRectangleRounded r 0.15 8 surface)
          centered title r 16 (if chosen then primary else secondary)
          when chosen (drawRectangleRec (Rectangle (a + 12) (b + ch + 7) (cw - 24) 2) accent)
        UI.Primary -> do
          drawRectangleRounded r 0.18 8 (if hovered then Color 198 223 197 255 else accent)
          centered title r 17 ink
        UI.Segment -> do
          drawRectangleRounded r 0.18 8 (if chosen then Color 61 82 67 255 else fill)
          centered title r 15 (if chosen then primary else secondary)
          when chosen (drawRectangleRec (Rectangle (a + cw / 2 - 8) (b + ch - 3) 16 2) accent)
        UI.Swatch choice -> do
          drawRectangleRounded r 0.16 8 (if chosen then Color 42 58 48 255 else fill)
          let color = case choice of Forest -> Color 44 65 48 255; Slate -> Color 70 74 79 255; Black -> Color 3 5 4 255; Lake -> Color 76 111 111 255; Picture -> Color 145 156 120 255
          drawRectangleRounded (Rectangle (a + 10) (b + 9) 20 20) 0.2 6 color
          fitted title (a + 40) (b + 9) (cw - 74) 16 primary
          when chosen checkMark
        kind -> do
          when (hovered || (settingsOpen s && kind == UI.Subtle)) (drawRectangleRounded r 0.2 8 fill)
          let cx = a + cw / 2
              cy = b + ch / 2
              line ax ay bx by = drawLineEx (Vector2 ax ay) (Vector2 bx by) 1.8 primary
              triangle = drawTriangle (Vector2 (cx - 4) (cy - 7)) (Vector2 (cx - 4) (cy + 7)) (Vector2 (cx + 7) cy) primary
          case kind of
            UI.Play Playing -> forM_ [-4, 3] $ \offset -> drawRectangleRec (Rectangle (cx + offset) (cy - 7) 3 14) primary
            UI.Play Paused -> triangle
            UI.Skip -> triangle >> line (cx + 8) (cy - 7) (cx + 8) (cy + 7)
            UI.Close -> line (cx - 5) (cy - 5) (cx + 5) (cy + 5) >> line (cx - 5) (cy + 5) (cx + 5) (cy - 5)
            _ -> case action of
              Intent (BrowsePicture direction) -> do
                let sign = if direction == PreviousPicture then -1 else 1
                line (cx - sign * 3) (cy - 5) (cx + sign * 3) cy
                line (cx + sign * 3) cy (cx - sign * 3) (cy + 5)
              _ -> centered title r 16 primary
      when (focused == Just action) $ drawRectangleLinesEx (Rectangle (a - 3) (b - 3) (cw + 6) (ch + 6)) 2 primary

-- The same static geometry is used in the working view and its preview.
-- An opaque rest frame skips this pass entirely.
drawWorking :: Rectangle -> Background -> Maybe Texture -> PictureShade -> IO ()
drawWorking r@(Rectangle x y w h) choice picture shade = do
  drawBackground r choice
  when (choice == Picture) $ forM_ picture $ \texture -> do
    let tw = fromIntegral (texture'width texture)
        th = fromIntegral (texture'height texture)
        scale = max (w / tw) (h / th)
        cropW = w / scale
        cropH = h / scale
    drawTexturePro texture (Rectangle ((tw - cropW) / 2) ((th - cropH) / 2) cropW cropH) r (Vector2 0 0) 0 (Color 255 255 255 255)
    drawRectangleRec (Rectangle x y w h) (Color 0 0 0 (fromIntegral (shadeOpacity shade)))

drawBackground :: Rectangle -> Background -> IO ()
drawBackground r Forest = drawRectangleRec r (Color 25 35 31 255)
drawBackground r Slate = drawRectangleRec r (Color 30 32 34 255)
drawBackground r Black = drawRectangleRec r (Color 0 0 0 255)
drawBackground r Picture = drawRectangleRec r (Color 25 35 31 255)
drawBackground (Rectangle ox oy wf hf) Lake = do
  drawRectangleGradientV (round ox) (round oy) (round wf) (round hf) (Color 53 65 63 255) (Color 21 33 32 255)
  let horizon = oy + hf * 0.48
      ridge shift base amplitude color = forM_ [0 .. 118 :: Int] $ \i -> do
        let x1 = ox + wf * fromIntegral i / 119
            x2 = ox + wf * fromIntegral (i + 1) / 119
            height x = oy + base - amplitude * (0.5 + 0.23 * sin ((x - ox) / wf * 12 + shift) + 0.19 * sin ((x - ox) / wf * 23 + shift * 2))
            y1 = height x1
            y2 = height x2
        drawTriangle (Vector2 x1 y1) (Vector2 x1 horizon) (Vector2 x2 y2) color
        drawTriangle (Vector2 x2 y2) (Vector2 x1 horizon) (Vector2 x2 horizon) color
  ridge 1 (hf * 0.40) (hf * 0.13) (Color 43 59 57 255)
  ridge 3 (hf * 0.46) (hf * 0.13) (Color 29 47 42 255)
  drawRectangleGradientV (round ox) (round horizon) (round wf) (round (oy + hf - horizon)) (Color 38 55 52 255) (Color 15 29 25 255)
  forM_ [0 .. 35 :: Int] $ \i -> do
    let y = round (horizon + fromIntegral (i * i) * hf / 2800)
        inset = wf * (0.14 + 0.09 * sin (fromIntegral i * 1.7))
    drawRectangle (round (ox + inset)) y (max 1 (round (wf - 2 * inset))) 1 (Color 59 73 66 26)
