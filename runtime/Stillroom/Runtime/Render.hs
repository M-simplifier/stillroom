{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Render (UIAction (..), loadTypeface, render, hitTest, allInterfaceText) where

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
import Raylib.Core.Textures (setTextureFilter)
import Raylib.Internal (WindowResources, managed)
import Raylib.Internal.Foreign (pop)
import Raylib.Types (Color (..), Font (..), Rectangle (..), TextureFilter (..), pattern Vector2)
import Stillroom.Domain
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
allInterfaceText = "作業モード再生中一時停止次へ音量表示終了静かな緑暗い灰色黒湖畔音楽フォルダー内蔵プリセット画面を移動全小さく読み込んでいます選択閉じる曲を準備設定保存できません曲ありませんこの音楽使用読み込めないため英語日本語"

label :: Session -> String -> String -> String
label s en ja = if language s == Japanese then ja else en

buttons :: Int -> Int -> Session -> [Button]
buttons w h s =
  [ b 20 54 64 (if transport s == Playing then "II" else ">") TogglePlayback,
    b 92 54 70 (label s "Next" "次へ") NextTrack,
    b 188 54 40 "-" (ChangeVolume (-0.025)),
    b 394 54 40 "+" (ChangeVolume 0.025),
    b 454 54 100 (label s "Display" "表示") ToggleSettings,
    Button (rect 566 54 54 40) "x" Quit
  ]
    ++ if not (settingsOpen s)
      then []
      else
        [ q 20 18 142 (label s "Forest" "静かな緑") (ChooseBackground Forest),
          q 172 18 142 (label s "Slate" "暗い灰色") (ChooseBackground Slate),
          q 324 18 142 (label s "Black" "黒") (ChooseBackground Black),
          q 476 18 142 (label s "Lake" "湖畔") (ChooseBackground Lake),
          q 20 76 294 (label s "Music folder...  [O]" "音楽フォルダー  [O]") OpenMusic,
          q 324 76 294 (label s "Built-in music  [P]" "内蔵プリセット  [P]") UsePresets,
          q 20 132 294 (label s "Move to next screen  [M]" "表示する画面を移動  [M]") NextScreen,
          q 324 132 294 (label s "Fullscreen / window  [F11]" "全画面 / 小さく  [F11]") ToggleWindow,
          q 20 188 294 (label s "日本語 / English" "English / 日本語") ToggleLanguage,
          q 324 188 294 (label s "Close settings" "設定を閉じる") ToggleSettings
        ]
  where
    x = fromIntegral (w - 640) / 2
    y = fromIntegral (h - 160) - 28
    rect a c width height = Rectangle (x + a) (y + c) width height
    b a c width text event = Button (rect a c width 40) text (Intent event)
    q a c width text event = Button (rect a (c - 258) width 42) text (Intent event)

hitTest :: Int -> Int -> Session -> (Float, Float) -> Maybe UIAction
hitTest w h s (mx, my)
  | not (controlsVisible s) = Nothing
  | my >= y + 54 && my <= y + 94 && mx >= x + 236 && mx <= x + 386 =
      either (const Nothing) (Just . Intent . SetVolume) (volume (realToFrac (max 0 (min 1 ((mx - x - 236) / 150)))))
  | otherwise = go (buttons w h s)
  where
    x = fromIntegral (w - 640) / 2
    y = fromIntegral (h - 160) - 28
    go [] = Nothing
    go (Button (Rectangle a b rw rh) _ action : rest)
      | mx >= a && mx <= a + rw && my >= b && my <= b + rh = Just action
      | otherwise = go rest

render :: Font -> Int -> Int -> Session -> String -> Maybe String -> (Float, Float) -> IO ()
render font w h s track message mouse = do
  drawBackground w h (background s)
  when (controlsVisible s) $ do
    let x = fromIntegral (w - 640) / 2; y = fromIntegral (h - 160) - 28
    panel (Rectangle x y 640 160)
    when (settingsOpen s) (panel (Rectangle x (y - 258) 640 244))
    text (label s "Stillroom" "Stillroom / 作業モード") (x + 22) (y + 17) 21 primary
    text (if transport s == Playing then label s "PLAYING" "再生中" else label s "PAUSED" "一時停止") (x + 515) (y + 21) 13 secondary
    forM_ (buttons w h s) $ \(Button rectangle@(Rectangle a b rw rh) caption _) -> do
      let (mx, my) = mouse; hovered = mx >= a && mx <= a + rw && my >= b && my <= b + rh
      drawRectangleRounded rectangle 0.24 8 (if hovered then Color 55 76 65 255 else Color 34 48 41 255)
      Vector2 tw _ <- measureTextEx font caption 16 0.2
      text caption (a + (rw - tw) / 2) (b + (rh - 16) / 2) 16 primary
    drawRectangleRounded (Rectangle (x + 236) (y + 72) 150 4) 1 8 (Color 58 72 64 255)
    let filled = 150 * realToFrac (volumeValue (loudness s))
    drawRectangleRounded (Rectangle (x + 236) (y + 72) filled 4) 1 8 (Color 161 185 164 255)
    drawCircle (round (x + 236 + filled)) (round (y + 74)) 5 (Color 205 222 208 255)
    text (take 66 (maybe track id message)) (x + 22) (y + 110) 15 secondary
    text (label s "Space  pause     N  next     Esc  window" "Space  一時停止     N  次へ     Esc  小さく") (x + 22) (y + 137) 11 (Color 105 125 113 255)
  where
    primary = Color 222 234 225 255
    secondary = Color 146 167 151 255
    panel r = drawRectangleRounded r 0.11 12 (Color 18 27 22 246)
    text caption x y size color = drawTextEx font caption (Vector2 x y) size 0.2 color

-- Static scenery, constructed from original geometry. No animation, flashing,
-- stock image, external sample, or account is needed.
drawBackground :: Int -> Int -> Background -> IO ()
drawBackground _ _ Forest = clearBackground (Color 25 35 31 255)
drawBackground _ _ Slate = clearBackground (Color 30 32 34 255)
drawBackground _ _ Black = clearBackground (Color 0 0 0 255)
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
