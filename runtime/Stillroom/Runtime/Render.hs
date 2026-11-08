module Stillroom.Runtime.Render
  ( Typeface, loadTypeface, unloadTypeface, render, renderWithFocus, allInterfaceText )
where

import Control.Monad (forM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Data.Word (Word8)
import GameIO.Graphics (Color (..), Vec2 (..))
import GameIO.Graphics qualified as GPU
import Stillroom.Domain
import Stillroom.Interface (UIAction (..))
import Stillroom.Interface qualified as UI
import Stillroom.Runtime.Assets (assetPath)
import Stillroom.Runtime.Journey qualified as Journey
import Stillroom.Runtime.Pictures (PictureError (..), PictureTexture (..), pictureErrorText)
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Runtime.Typeface (Typeface)
import Stillroom.Runtime.Typeface qualified as Typeface
import Stillroom.Session
import System.FilePath ((</>))

loadTypeface :: GPU.Renderer -> [String] -> IO Typeface
loadTypeface renderer titles = do
  file <- assetPath ("assets" </> "fonts" </> "NotoSansJP.otf")
  let characters = take 4096 (distinct Set.empty ([' ' .. '~'] ++ allInterfaceText ++ concat titles))
  Typeface.load renderer file characters
  where
    distinct _ [] = []
    distinct seen (c : cs)
      | Set.member c seen = distinct seen cs
      | otherwise = c : distinct (Set.insert c seen) cs

unloadTypeface :: GPU.Renderer -> Typeface -> IO ()
unloadTypeface = Typeface.unload


allInterfaceText :: String
allInterfaceText =
  Journey.interfaceText
    ++ "−…作業モード再生中一時停止次へ音量表示終了霧の湖静かな森砂丘夜の抽象画午後の湖森の池画像を追加してください内蔵の画像です。登録した画像だけ解除できます。"
    ++ "休息静止景色明るさ動き設定背景をクリック、またはRで休息。クリックかEscで戻る休息の景色を描画できません。クリックかEscで戻ります。"
    ++ " · 画質自動は描画負荷に合わせて調整します。最高画質は消費電力が増えます。"
    ++ "作業中の景色音楽再生中の曲表示言語画像の明るさ音は休息中も、そのまま続きます。"
    ++ "内蔵の音楽は、少しずつ変わりながら続きます。旅の音オフひらけた湖朝霧の森暖かな水面青い稜線"
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

data Rectangle = Rectangle !Float !Float !Float !Float

rectangle :: UI.Bounds -> Rectangle
rectangle (UI.Bounds x y w h) = Rectangle x y w h

sceneRect :: Rectangle -> Rest.SceneRect
sceneRect (Rectangle x y w h) = Rest.SceneRect x y w h

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgba r g b a = Color (channel r) (channel g) (channel b) (channel a)
  where channel value = fromIntegral value / 255

-- Contrast and selection share semantic roles across pages.
ink, surface, primary, secondary, edge, accent :: Color
ink = rgba 16 23 22 255
surface = rgba 31 42 38 255
primary = rgba 231 235 224 255
secondary = rgba 169 181 169 255
edge = rgba 64 81 72 255
accent = rgba 178 205 180 255

render :: GPU.Renderer -> Bool -> Typeface -> Rest.RestAssets -> Int -> Int -> Session -> Maybe PictureTexture -> String -> String -> Maybe String -> (Float, Float) -> IO (GPU.FrameStatus, Maybe GPU.FramePixels)
render renderer readback font assets w h s picture pictureCaption track message mouse =
  renderWithFocus renderer readback font assets w h s picture pictureCaption track message mouse Nothing

renderWithFocus :: GPU.Renderer -> Bool -> Typeface -> Rest.RestAssets -> Int -> Int -> Session -> Maybe PictureTexture -> String -> String -> Maybe String -> (Float, Float) -> Maybe UIAction -> IO (GPU.FrameStatus, Maybe GPU.FramePixels)
renderWithFocus renderer readback font assets w h s picture pictureCaption track message mouse focused = do
  overlays <- newIORef []
  scenes <- newIORef []
  let paint batch = modifyIORef' overlays (batch :)
      addScene build = do
        scene <- build
        modifyIORef' scenes (scene :)
        mapM_ paint (Rest.sceneOverlay scene)
  compose paint addScene font assets w h s picture pictureCaption track message mouse focused
  draws <- reverse <$> readIORef scenes
  frame <- GPU.Frame (rgba 0 0 0 255) [] . reverse <$> readIORef overlays
  let passes = concatMap Rest.scenePasses draws
  result <- if readback
    then GPU.renderPassesReadback renderer passes frame
    else (, Nothing) <$> GPU.renderPasses renderer passes frame
  mapM_ (\scene -> Rest.scenePresented scene (fst result)) draws
  pure result

compose :: (GPU.OverlayBatch -> IO ()) -> (IO Rest.SceneDraw -> IO ()) -> Typeface -> Rest.RestAssets -> Int -> Int -> Session -> Maybe PictureTexture -> String -> String -> Maybe String -> (Float, Float) -> Maybe UIAction -> IO ()
compose paint addScene font assets w h s picture pictureCaption track message mouse focused = do
  when (viewingMode s == Working) $ drawWorking paint (Rectangle 0 0 (fromIntegral w) (fromIntegral h)) (background s) picture (pictureShade s)
  when (viewingMode s == Resting) $ do
    addScene (Rest.renderRest assets w h s)
    issue <- Rest.restIssue assets s
    if restScene s == RailJourney
      then do
        let exitBox = rectangle (UI.journeyExitBounds w)
        drawRectangleRounded exitBox 0.2 8 (rgba 9 18 17 180)
        centered (label "Return to work  Esc" "作業に戻る  Esc") exitBox 14 primary
        linesToShow <- maybe (Journey.hints (Rest.journey assets) (language s)) (pure . (: [])) issue
        let width = min 820 (fromIntegral w - 32)
            height = fromIntegral (length linesToShow) * 25 + 18
            box = Rectangle ((fromIntegral w - width) / 2) (fromIntegral h - height - 20) width height
        drawRectangleRounded box 0.15 8 (rgba 9 18 17 190)
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
    drawRectangleRec bounds color = paint (solid bounds color)
    drawRectangle :: Int -> Int -> Int -> Int -> Color -> IO ()
    drawRectangle a b width height = drawRectangleRec (Rectangle (fromIntegral a) (fromIntegral b) (fromIntegral width) (fromIntegral height))
    drawRectangleRounded = rounded paint
    drawRectangleLinesEx = outline paint
    drawLineEx start end thickness color = paint (strokeSegment start end thickness color)
    drawTriangle a b c color = paint (geometry [a, b, c] color)
    drawCircle :: Int -> Int -> Float -> Color -> IO ()
    drawCircle a b radius color = paint (circle (fromIntegral a) (fromIntegral b) radius color)

    label = UI.local s
    UI.Bounds x y rw rh = UI.settingsBounds w h
    preview@(Rectangle px py pw ph) = rectangle (UI.previewBounds w h)
    rightWidth = (rw - 48) * 0.44
    rx = x + rw - 24 - rightWidth
    panel r = drawRectangleRounded r 0.045 12 ink
    text caption a b size color = Typeface.draw font caption (Vec2 a b) size 0.2 color >>= paint
    fitted caption a b width size color = fit caption width size >>= \short -> text short a b size color
    fit caption width size = do
      Vec2 measured _ <- Typeface.measure font caption size 0.2
      if measured <= width || length caption < 4 then pure caption else fit (take (length caption - 4) caption ++ "...") width size
    centered caption (Rectangle a b width height) size color = do
      short <- fit caption (width - 16) size
      Vec2 tw _ <- Typeface.measure font short size 0.2
      text short (a + (width - tw) / 2) (b + (height - size) / 2 - 1) size color
    -- Two lines at most. Long filenames use ellipsis instead.
    wrapped caption a b width size color = go "" caption b (2 :: Int)
      where
        go line [] lineY _ = text line a lineY size color
        go line remaining lineY 1 = fitted (line ++ remaining) a lineY width size color
        go line (c : cs) lineY rows = do
          Vec2 measured _ <- Typeface.measure font (line ++ [c]) size 0.2
          if measured > width && not (null line)
            then text line a lineY size color >> go [c] cs (lineY + size + 5) (rows - 1)
            else go (line ++ [c]) cs lineY rows
    rule a b width = drawRectangleRec (Rectangle a b width 1) (rgba 45 59 52 255)
    settings = do
      drawRectangle 0 0 w h (rgba 3 9 8 170)
      panel (rectangle (UI.settingsBounds w h))
      text "stillroom" (x + 24) (y + 27) 23 primary
      rule (x + 24) (y + 72) (rw - 48)
      case settingsPage s of
        RestSettings -> do
          text (restSceneName (language s) (restScene s)) px (y + 88) 24 primary
          addScene (Rest.renderPreview assets h (sceneRect preview) s)
          issue <- Rest.restIssue assets s
          wrapped (maybe (restSceneDescription (language s) (restScene s)) id issue) px (py + ph + 13) pw 16 secondary
          text (label "Quality · Auto adapts to rendering load" "画質 · 自動は描画負荷に合わせて調整") px (y + rh - 173) 12 secondary
          text (if restScene s == RailJourney then label "Landscape" "旅の風景" else label "Motion" "動き") px (y + rh - 124) 14 secondary
          text (label "Brightness" "明るさ") (px + pw * 0.49) (y + rh - 124) 14 secondary
          when (restCollection s == Journeys) $ do
            let startY = y + 156 + (rh - 170) / 3
            wrapped (label "Ride, wander, and return." "眺める。降りて歩く。また乗る。") rx startY rightWidth 16 primary
            wrapped (label "E stops the railcar before stepping outside." "Eで停車してから、近くの地面へ。") rx (startY + 47) rightWidth 14 secondary
            wrapped (label "Esc returns to work. Your place is saved." "Escで作業へ。旅の続きは、また次に。") rx (startY + 93) rightWidth 14 secondary
        DisplaySettings -> do
          text (label "Your working view" "作業中の景色") px (y + 88) 24 primary
          drawWorking paint preview (background s) picture (pictureShade s)
          fitted (if background s == Picture then pictureCaption else UI.backgroundName s (background s)) px (py + ph + 13) pw 16 secondary
          text (label "Background" "背景") rx (y + 88) 16 secondary
          text (label "Image brightness" "画像の明るさ") rx (y + rh - 76) 14 secondary
        MusicSettings -> do
          text (label "Music" "音楽") px (y + 88) 24 primary
          text (label "NOW PLAYING" "再生中の曲") px (y + 125) 14 secondary
          fitted track px (y + 149) pw 18 primary
          wrapped (label "Built-in music changes gently as you listen." "内蔵の音楽は、少しずつ変わりながら続きます。") px (y + 358) pw 16 secondary
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
          fill = if hovered then rgba 45 61 52 255 else surface
          title = UI.caption control
          checkMark = do
            drawLineEx (Vec2 (a + cw - 22) (b + ch / 2)) (Vec2 (a + cw - 18) (b + ch / 2 + 4)) 2 accent
            drawLineEx (Vec2 (a + cw - 18) (b + ch / 2 + 4)) (Vec2 (a + cw - 11) (b + ch / 2 - 4)) 2 accent
      case UI.style control of
        UI.Scene scene -> do
          addScene (Rest.renderThumbnail assets scene (sceneRect r))
          drawRectangleRec (Rectangle a (b + ch - 25) cw 25) (rgba 9 17 17 235)
          fitted title (a + 8) (b + ch - 21) (cw - 16) 15 primary
          when (chosen || hovered) (drawRectangleLinesEx r (if chosen then 2 else 1) accent)
        UI.Navigation -> do
          when hovered (drawRectangleRounded r 0.15 8 surface)
          centered title r 16 (if chosen then primary else secondary)
          when chosen (drawRectangleRec (Rectangle (a + 12) (b + ch + 7) (cw - 24) 2) accent)
        UI.Primary -> do
          drawRectangleRounded r 0.18 8 (if hovered then rgba 198 223 197 255 else accent)
          centered title r 17 ink
        UI.Segment -> do
          drawRectangleRounded r 0.18 8 (if chosen then rgba 61 82 67 255 else fill)
          centered title r 15 (if chosen then primary else secondary)
          when chosen (drawRectangleRec (Rectangle (a + cw / 2 - 8) (b + ch - 3) 16 2) accent)
        UI.Swatch choice -> do
          drawRectangleRounded r 0.16 8 (if chosen then rgba 42 58 48 255 else fill)
          let color = case choice of Forest -> rgba 44 65 48 255; Slate -> rgba 70 74 79 255; Black -> rgba 3 5 4 255; Lake -> rgba 76 111 111 255; Picture -> rgba 145 156 120 255
          drawRectangleRounded (Rectangle (a + 10) (b + 9) 20 20) 0.2 6 color
          fitted title (a + 40) (b + 9) (cw - 74) 16 primary
          when chosen checkMark
        kind -> do
          when (hovered || (settingsOpen s && kind == UI.Subtle)) (drawRectangleRounded r 0.2 8 fill)
          let cx = a + cw / 2
              cy = b + ch / 2
              line ax ay bx by = drawLineEx (Vec2 ax ay) (Vec2 bx by) 1.8 primary
              triangle = drawTriangle (Vec2 (cx - 4) (cy - 7)) (Vec2 (cx - 4) (cy + 7)) (Vec2 (cx + 7) cy) primary
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

-- Both working views consume the same picture crop and background geometry.
drawWorking :: (GPU.OverlayBatch -> IO ()) -> Rectangle -> Background -> Maybe PictureTexture -> PictureShade -> IO ()
drawWorking paint r@(Rectangle x y w h) choice picture shade = do
  drawBackground paint r choice
  when (choice == Picture) $ forM_ picture $ \(PictureTexture width height texture) -> do
    let tw = fromIntegral width
        th = fromIntegral height
        scale = max (w / tw) (h / th)
        u = (1 - w / (tw * scale)) / 2
        v = (1 - h / (th * scale)) / 2
    paint (quad (Just texture) r (u, v, 1 - u, 1 - v) (rgba 255 255 255 255) (rgba 255 255 255 255))
    paint (solid (Rectangle x y w h) (rgba 0 0 0 (fromIntegral (shadeOpacity shade))))

drawBackground :: (GPU.OverlayBatch -> IO ()) -> Rectangle -> Background -> IO ()
drawBackground paint r Forest = paint (solid r (rgba 25 35 31 255))
drawBackground paint r Slate = paint (solid r (rgba 30 32 34 255))
drawBackground paint r Black = paint (solid r (rgba 0 0 0 255))
drawBackground paint r Picture = paint (solid r (rgba 25 35 31 255))
drawBackground paint r@(Rectangle ox oy wf hf) Lake = do
  paint (quad Nothing r fullUV (rgba 53 65 63 255) (rgba 21 33 32 255))
  let horizon = oy + hf * 0.48
      ridge shift base amplitude color = paint (GPU.OverlayBatch Nothing (concatMap strip [0 .. 118 :: Int]))
        where
          strip i =
            let x1 = ox + wf * fromIntegral i / 119
                x2 = ox + wf * fromIntegral (i + 1) / 119
                height x = oy + base - amplitude * (0.5 + 0.23 * sin ((x - ox) / wf * 12 + shift) + 0.19 * sin ((x - ox) / wf * 23 + shift * 2))
                y1 = height x1
                y2 = height x2
            in map (point color) [Vec2 x1 y1, Vec2 x1 horizon, Vec2 x2 y2, Vec2 x2 y2, Vec2 x1 horizon, Vec2 x2 horizon]
      point color (Vec2 x y) = GPU.Vertex x y 0 0 0 color
  ridge 1 (hf * 0.40) (hf * 0.13) (rgba 43 59 57 255)
  ridge 3 (hf * 0.46) (hf * 0.13) (rgba 29 47 42 255)
  paint (quad Nothing (Rectangle ox horizon wf (oy + hf - horizon)) fullUV (rgba 38 55 52 255) (rgba 15 29 25 255))
  forM_ [0 .. 35 :: Int] $ \i -> do
    let y = horizon + fromIntegral (i * i) * hf / 2800
        inset = wf * (0.14 + 0.09 * sin (fromIntegral i * 1.7))
    paint (solid (Rectangle (ox + inset) (fromIntegral (round y :: Int)) (max 1 (wf - 2 * inset)) 1) (rgba 59 73 66 26))

fullUV :: (Float, Float, Float, Float)
fullUV = (0, 0, 1, 1)

quad :: Maybe GPU.Texture -> Rectangle -> (Float, Float, Float, Float) -> Color -> Color -> GPU.OverlayBatch
quad texture (Rectangle x y w h) (u0, v0, u1, v1) top bottom =
  let tl = GPU.Vertex x y 0 u0 v0 top
      tr = GPU.Vertex (x + w) y 0 u1 v0 top
      bl = GPU.Vertex x (y + h) 0 u0 v1 bottom
      br = GPU.Vertex (x + w) (y + h) 0 u1 v1 bottom
  in GPU.OverlayBatch texture [tl, tr, br, tl, br, bl]

solid :: Rectangle -> Color -> GPU.OverlayBatch
solid bounds color = quad Nothing bounds fullUV color color

outline :: (GPU.OverlayBatch -> IO ()) -> Rectangle -> Float -> Color -> IO ()
outline paint (Rectangle x y w h) stroke color =
  mapM_ (\bounds -> paint (solid bounds color))
    [ Rectangle x y w stroke,
      Rectangle x (y + h - stroke) w stroke,
      Rectangle x (y + stroke) stroke (h - 2 * stroke),
      Rectangle (x + w - stroke) (y + stroke) stroke (h - 2 * stroke)
    ]

rounded :: (GPU.OverlayBatch -> IO ()) -> Rectangle -> Float -> Int -> Color -> IO ()
rounded paint bounds@(Rectangle x y w h) roundness segments color
  | w <= 0 || h <= 0 = pure ()
  | radius <= 0 = paint (solid bounds color)
  | otherwise = paint (fan (Vec2 (x + w / 2) (y + h / 2)) (concatMap corner corners) color)
  where
    radius = min w h * min 1 roundness / 2
    corners =
      [ (x + w - radius, y + radius, -pi / 2),
        (x + w - radius, y + h - radius, 0),
        (x + radius, y + h - radius, pi / 2),
        (x + radius, y + radius, pi)
      ]
    corner (cx, cy, start) =
      [ let angle = start + (pi / 2) * fromIntegral i / fromIntegral segments
        in Vec2 (cx + radius * cos angle) (cy + radius * sin angle)
      | i <- [0 .. segments]
      ]

fan :: Vec2 -> [Vec2] -> Color -> GPU.OverlayBatch
fan center points color =
  let triangles = concat [[center, a, b] | (a, b) <- zip points (drop 1 points ++ take 1 points)]
  in geometry triangles color

circle :: Float -> Float -> Float -> Color -> GPU.OverlayBatch
circle x y radius = fan (Vec2 x y)
  [let angle = 2 * pi * fromIntegral i / 24 in Vec2 (x + radius * cos angle) (y + radius * sin angle) | i <- [0 .. 23 :: Int]]

strokeSegment :: Vec2 -> Vec2 -> Float -> Color -> GPU.OverlayBatch
strokeSegment (Vec2 x1 y1) (Vec2 x2 y2) thickness =
  let dx = x2 - x1
      dy = y2 - y1
      scale = (thickness / 2) / sqrt (dx * dx + dy * dy)
      ox = dy * scale
      oy = -dx * scale
      a = Vec2 (x1 + ox) (y1 + oy)
      b = Vec2 (x1 - ox) (y1 - oy)
      c = Vec2 (x2 - ox) (y2 - oy)
      d = Vec2 (x2 + ox) (y2 + oy)
  in geometry (if dx == 0 && dy == 0 then [] else [a, b, c, a, c, d])

geometry :: [Vec2] -> Color -> GPU.OverlayBatch
geometry points color = GPU.OverlayBatch Nothing [GPU.Vertex x y 0 0 0 color | Vec2 x y <- points]
