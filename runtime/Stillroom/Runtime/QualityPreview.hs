{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- An isolated visual study: the released journey reducer/renderer, disposable
-- state, no personal settings or audio device. Quality never changes the world.
module Stillroom.Runtime.QualityPreview (runQualityPreview, runQualityCapture, runQualityFilm) where

import Control.Exception (bracket, bracket_, finally, throwIO)
import Control.Monad (forM, forM_, replicateM_, unless, when)
import Data.Aeson (decodeStrict', encode, object, (.=), Value)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (ord)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Foreign (castPtr, free, peek)
import Foreign.C (CDouble (..), CUInt (..))
import Raylib.Core qualified as R
import Raylib.Core.Shapes (drawRectangleRec)
import Raylib.Core.Textures (c'loadImageFromScreen)
import Raylib.Internal.Foreign (c'free)
import Raylib.Types
import Stillroom.Domain (Language (Japanese))
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.Capture (screenshot)
import Stillroom.Runtime.Journey qualified as Runtime
import Stillroom.Runtime.Pictures qualified as Pictures
import Stillroom.Runtime.Typeface qualified as Typeface
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hSetBinaryMode, stdout)

foreign import ccall unsafe "sr_car_natural_ground" naturalGround :: CUInt -> CDouble -> CDouble -> CDouble
foreign import ccall safe "sr_nature_sync" finishGPU :: IO ()

lakeProgress :: Double
lakeProgress = fromMaybe 450 $ listToMaybe
  [s - 12 | s <- [100,102 .. 3000], naturalGround (fromIntegral (World.seedValue World.firstSeed))
      (realToFrac (J.trackX World.firstSeed s)) (realToFrac (-s)) < -1]

pose :: Double -> Double -> J.Journey
pose progress offset =
  let j = J.at World.firstSeed progress
      c = (J.car j) {J.speed = 3.6, J.pace = J.Easy}
   in j {J.car = c, J.person = J.passenger World.firstSeed c offset (-0.045) 1800}

lake :: J.Journey
lake = pose (lakeProgress+18) (-1.0)

qualityName :: Runtime.PreviewQuality -> String
qualityName Runtime.Standard = "標準"
qualityName Runtime.Detailed = "高画質"
qualityName Runtime.Supersampled = "最高画質"

healthy :: Runtime.JourneyRuntime -> IO ()
healthy runtime = Runtime.status runtime Japanese >>= \issue ->
  unless (issue == Nothing) (throwIO (userError (show issue)))

drawScene :: Runtime.JourneyRuntime -> Int -> Int -> IO ()
drawScene runtime width height = Runtime.draw runtime (Rectangle 0 0 (fromIntegral width) (fromIntegral height))

frame :: Runtime.JourneyRuntime -> Int -> Int -> IO ()
frame runtime width height = bracket_ R.beginDrawing R.endDrawing (drawScene runtime width height)

warm :: Runtime.JourneyRuntime -> Int -> Int -> IO ()
warm runtime width height = replicateM_ 82 (frame runtime width height) >> healthy runtime

runQualityPreview :: IO Bool
runQualityPreview = do
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowResizable, WindowAlwaysRun, VsyncHint]
  bracket (R.initWindow 1280 720 "Stillroom - light and water study") (R.closeWindow . Just) $ \window -> do
    R.setTargetFPS 30
    R.setWindowMinSize 800 450
    fontFile <- Pictures.assetPath ("assets" </> "fonts" </> "NotoSansJP.otf")
    let captions = "STILLROOM·光と水の試作標準高画質最高画質同じ景色のまま比較準備しています静止解除閉じる湖畔へ戻る見回す歩く乗降拡大操作説明一時停止再開" ++ Runtime.interfaceText
    bracket (Typeface.load window fontFile (nub ([32..126] ++ map ord captions))) (Typeface.unload window) $ \font -> Runtime.withJourney $ \runtime -> do
      Runtime.restoreModel runtime lake
      Runtime.previewQuality runtime Runtime.Detailed
      let panel quality paused width = do
            drawRectangleRec (Rectangle 22 20 304 76) (Color 13 26 28 224)
            Typeface.draw font "STILLROOM   /   光と水の試作" (Vector2 39 31) 14 1 (Color 193 209 204 255)
            Typeface.draw font (qualityName quality ++ if paused then "   ·   静止" else "") (Vector2 39 55) 23 1 (Color 239 238 220 255)
            drawRectangleRec (Rectangle 22 (fromIntegral width - 76) 720 54) (Color 13 26 28 210)
            Typeface.draw font "1 標準    2 高画質    3 最高画質    P 静止・解除    F 拡大    Esc 閉じる" (Vector2 38 (fromIntegral width - 63)) 15 0 (Color 225 231 216 255)
            Typeface.draw font "Space 一時停止・再開    E 乗降    WASD 歩く    クリック 見回す    R 湖畔へ戻る" (Vector2 38 (fromIntegral width - 41)) 13 0 (Color 170 192 181 255)
          loading = do
            frame runtime 1280 720
            issue <- Runtime.status runtime Japanese
            pure issue
      replicateM_ 82 loading
      healthy runtime
      let loop quality paused visibleFor = do
            closed <- R.windowShouldClose
            unless closed $ do
              dt <- realToFrac <$> R.getFrameTime
              one <- R.isKeyPressed KeyOne
              two <- R.isKeyPressed KeyTwo
              three <- R.isKeyPressed KeyThree
              freeze <- R.isKeyPressed KeyP
              restart <- R.isKeyPressed KeyR
              full <- R.isKeyPressed KeyF
              help <- R.isKeyPressed KeyH
              when full (R.isWindowMaximized >>= \maximized -> if maximized then R.restoreWindow else R.maximizeWindow)
              when restart (Runtime.restoreModel runtime lake)
              let selected = if one then Runtime.Standard else if two then Runtime.Detailed else if three then Runtime.Supersampled else quality
                  stopped = if freeze then not paused else paused
                  remaining = if or [one,two,three,freeze,restart,help] then 8 else max 0 (visibleFor-dt)
              Runtime.previewQuality runtime selected
              focus <- R.isWindowFocused
              click <- R.isMouseButtonPressed MouseButtonLeft
              Vector2 dx dy <- R.getMouseDelta
              if not focus || stopped then pure () else Runtime.update runtime dt click (dx,dy)
              width <- R.getScreenWidth
              height <- R.getScreenHeight
              bracket_ R.beginDrawing R.endDrawing $ do
                drawScene runtime width height
                when (remaining > 0) (panel selected stopped height)
              issue <- Runtime.status runtime Japanese
              -- Loading after a restart is expected; rendering failures are not.
              case issue of
                Just message | message /= "旅の景色を準備しています…" -> throwIO (userError message)
                _ -> pure ()
              loop selected stopped remaining
      loop Runtime.Detailed False (12 :: Double)
    pure True

runQualityCapture :: FilePath -> IO Bool
runQualityCapture folder = do
  createDirectoryIfMissing True folder
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowHidden, WindowUndecorated, WindowAlwaysRun]
  bracket (R.initWindow 1600 900 "Stillroom quality capture") (R.closeWindow . Just) $ \_ -> do
    R.setTargetFPS 0
    Runtime.withJourney $ \runtime -> do
      entries <- forM [("lake",lake), ("meadow",pose 60 0), ("shore",pose (lakeProgress+40) (-1.1))] $ \(name,start) -> do
        Runtime.restoreModel runtime start
        warm runtime 1600 900
        before <- Runtime.model runtime
        let camera = J.view before
            cx = floor (W.posX camera/32)
            cz = floor (W.posZ camera/32)
            fx = floor (W.posX camera/128)
            fz = floor (W.posZ camera/128)
            fingerprints = (,) <$> (fst <$> Runtime.fingerprintsForCheck runtime cx cz)
                               <*> (snd <$> Runtime.fingerprintsForCheck runtime fx fz)
        fingerprint <- fingerprints
        unless (fst fingerprint /= 0 && snd fingerprint /= 0) (throwIO (userError "Comparison geometry was not resident"))
        levels <- forM [Runtime.Standard,Runtime.Detailed,Runtime.Supersampled] $ \quality -> do
          Runtime.previewQuality runtime quality
          replicateM_ 3 (frame runtime 1600 900)
          healthy runtime
          screenshot (folder </> name ++ "-" ++ show quality ++ ".png")
          times <- forM [1..12 :: Int] $ \_ -> do
            finishGPU
            began <- R.getTime
            frame runtime 1600 900
            finishGPU
            ended <- R.getTime
            pure ((ended-began)*1000)
          after <- Runtime.model runtime
          unchanged <- (==fingerprint) <$> fingerprints
          unless (after==before && unchanged) (throwIO (userError "Quality changed the journey or geometry"))
          renderer <- Runtime.info runtime
          pure (object ["quality" .= show quality,"frameMs" .= times,"sortedMs" .= sort times,
                        "worldAndPoseUnchanged" .= True,"renderer" .= (decodeStrict' (BC.pack renderer) :: Maybe Value)])
        pure (object ["scene" .= (name :: String),"progress" .= J.progress (J.car start),"levels" .= levels])
      -- Verify downshifting reallocates the native targets and allows resizing.
      Runtime.previewQuality runtime Runtime.Standard
      frame runtime 800 450
      healthy runtime
      Runtime.previewQuality runtime Runtime.Supersampled
      frame runtime 1600 900
      healthy runtime
      BL.writeFile (folder </> "quality-check.json") (encode (object ["passed" .= True,"results" .= entries,
        "resizeAndSwitchBack" .= True,"audioOpened" .= False,"personalSettingsRead" .= False]))
    pure True

rgba :: IO BS.ByteString
rgba = bracket c'loadImageFromScreen release $ \picture -> do
  width <- fromIntegral <$> peek (p'image'width picture)
  height <- fromIntegral <$> peek (p'image'height picture)
  pixels <- peek (p'image'data picture)
  BS.packCStringLen (castPtr pixels,width*height*4)
  where
    release picture = (c'free . castPtr =<< peek (p'image'data picture)) `finally` free picture

runQualityFilm :: Runtime.PreviewQuality -> IO Bool
runQualityFilm quality = do
  hSetBinaryMode stdout True
  R.setTraceLogLevel LogNone
  R.setConfigFlags [WindowHidden,WindowUndecorated,WindowAlwaysRun]
  bracket (R.initWindow 1280 720 "Stillroom quality film") (R.closeWindow . Just) $ \_ -> do
    R.setTargetFPS 0
    Runtime.withJourney $ \runtime -> do
      Runtime.restoreModel runtime lake
      Runtime.previewQuality runtime quality
      warm runtime 1280 720
      forM_ [0..479 :: Int] $ \_ -> do
        -- Both buffers receive the same semantic timestamp. Capture has no
        -- dependence on wall-clock speed or the previous swapped frame.
        frame runtime 1280 720
        frame runtime 1280 720
        healthy runtime
        rgba >>= BS.hPut stdout
        Runtime.advanceForCheck runtime 2 J.neutral
    pure True
