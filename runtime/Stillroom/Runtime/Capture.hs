{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Deterministic UI inspection through the production renderer. No audio
-- device, user preferences, native acceptance sequence or personal files.
module Stillroom.Runtime.Capture (runCapture, screenshot) where

import Control.Exception (bracket, bracket_, finally, throwIO)
import Control.Monad (forM, replicateM_, unless, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Foreign (alloca, castPtr, free, nullPtr, peek)
import Foreign.C (withCString)
import Raylib.Core qualified as R
import Raylib.Core.Textures (c'exportImageToMemory, c'loadImageFromScreen, unloadTexture)
import Raylib.Internal.Foreign (c'free)
import Raylib.Types
import Stillroom.Domain
import Stillroom.Interface qualified as UI
import Stillroom.Runtime.Pictures qualified as Pictures
import Stillroom.Runtime.Render qualified as View
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Session
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))

runCapture :: FilePath -> IO Bool
runCapture folder = do
  createDirectoryIfMissing True folder
  let report = folder </> "ui-capture.json"
  exists <- doesFileExist report
  when exists (removeFile report)
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowHidden, WindowUndecorated, WindowAlwaysRun]
  bracket (R.initWindow 700 500 "Stillroom UI capture") (R.closeWindow . Just) $ \window -> do
    R.setTargetFPS 0
    started <- R.getTime
    bracket (View.loadTypeface window ["Linen"]) (View.unloadTypeface window) $ \font -> do
      Rest.withRestAssets window $ \assets -> do
        ready <- Rest.proceduralReady assets
        unless (ready && Rest.thumbnailsReady assets) $
          throwIO (userError "UI capture requires the procedural shader and all thumbnails.")
        entries <- Pictures.discoverPictures (folder </> "backgrounds")
        entry <-
          maybe (throwIO (userError "UI capture requires the bundled forest image.")) pure $
            Pictures.findPicture (Just "bundled:02-quiet-forest.png") (filter ((== "bundled:02-quiet-forest.png") . Pictures.pictureKey) entries)
        prepared <- Pictures.preparePicture entry >>= either throwIO pure
        texture <- Pictures.uploadPicture window prepared
        frames <- forM views $ \(name, lang, page, opened, w, h, scene, mode) -> do
          R.setWindowSize w h
          let state =
                (initialSession defaultVolume)
                  { language = lang,
                    settingsPage = page,
                    settingsOpen = opened,
                    windowMode = Compact,
                    background = Picture,
                    restSeconds = 12,
                    restHintSeconds = 0,
                    restScene = scene,
                    restCollection = sceneCollection scene,
                    viewingMode = mode,
                    restShade = Original
                  }
              caption = "2 / " ++ show (length entries) ++ UI.local state "   Quiet forest" "   静かな森"
              focus = if page == MusicSettings then Just (UI.Intent OpenMusic) else Nothing
              draw =
                bracket_ R.beginDrawing R.endDrawing $
                  View.renderWithFocus font assets w h state (Just texture) caption "Linen" Nothing (-1, -1) focus
              file = name ++ ".png"
          -- Lazy journey loading uses the same bounded steps as the app.
          -- These deterministic captures have no music device to pump.
          when (scene == RailJourney) (replicateM_ 82 draw)
          draw >> draw
          actual <- (,) <$> R.getScreenWidth <*> R.getScreenHeight
          unless (actual == (w, h)) (throwIO (userError "UI capture surface has unexpected dimensions."))
          readyNow <- Rest.proceduralReady assets
          unless readyNow (throwIO (userError "Landscape rendering failed"))
          screenshot (folder </> file)
          pure (object ["file" .= file, "width" .= w, "height" .= h, "language" .= show lang])
        -- Exercise decoding, native texture upload and cover-cropping for every
        -- bundled wallpaper using the production renderer, not a file preview.
        wallpaperFrames <- forM (filter ((== Pictures.Bundled) . Pictures.pictureOrigin) entries) $ \wallpaper -> do
          pixels <- Pictures.preparePicture wallpaper >>= either throwIO pure
          bracket (Pictures.uploadPicture window pixels) (\t -> unloadTexture t window) $ \wallpaperTexture -> do
            R.setWindowSize 1280 720
            let state = (initialSession defaultVolume) {background = Picture, idleSeconds = 10}
                draw = bracket_ R.beginDrawing R.endDrawing $
                  View.render font assets 1280 720 state (Just wallpaperTexture) "" "Linen" Nothing (-1, -1)
                file = "wallpaper-" ++ Pictures.pictureName wallpaper ++ ".png"
            draw >> draw
            screenshot (folder </> file)
            pure (object ["file" .= file, "width" .= (1280 :: Int), "height" .= (720 :: Int), "pictureKey" .= Pictures.pictureKey wallpaper])
        finished <- R.getTime
        BL.writeFile
          report
          ( encode
              ( object
                  [ "completed" .= True,
                    "captures" .= (frames ++ wallpaperFrames),
                    "count" .= length (frames ++ wallpaperFrames),
                    "elapsedSeconds" .= (finished - started),
                    "audioOpened" .= False,
                    "personalSettingsRead" .= False
                  ]
              )
          )
        pure True
  where
    views :: [(String, Language, SettingsPage, Bool, Int, Int, RestScene, ViewingMode)]
    views =
      [ ("ui-" ++ name ++ "-" ++ if opened then show page else "player", lang, page, opened, 700, 500, TidalLight, Working)
      | (lang, name) <- [(English, "en"), (Japanese, "ja")],
        (page, opened) <- [(RestSettings, True), (DisplaySettings, True), (MusicSettings, True), (RestSettings, False)]
      ]
        ++ [("ui-desktop-gallery", Japanese, RestSettings, True, 1280, 800, TidalLight, Working)]
        ++ [ ("nature-" ++ show scene ++ "-" ++ show w, English, RestSettings, False, w, h, scene, Resting)
           | scene <- [BirchLake, AlpineLake, MeadowStream],
             (w, h) <- [(1280, 720), (1920, 1080)]
           ]
        ++ [("nature-gallery-" ++ show lang, lang, RestSettings, True, 700, 500, BirchLake, Working) | lang <- [English, Japanese]]
        ++ [("nature-gallery-desktop", Japanese, RestSettings, True, 1280, 800, BirchLake, Working)]
        ++ [("journey-gallery-" ++ show lang, lang, RestSettings, True, 700, 500, RailJourney, Working) | lang <- [English, Japanese]]
        ++ [("journey-gallery-desktop", Japanese, RestSettings, True, 1280, 800, RailJourney, Working)]
        ++ [("journey-ride", Japanese, RestSettings, False, 1280, 720, RailJourney, Resting)]

-- Encode through native memory; Haskell owns the Unicode output path. Keep
-- decoded and encoded byte buffers out of h-raylib's linked Haskell byte lists.
screenshot :: FilePath -> IO ()
screenshot file = bracket c'loadImageFromScreen release $ \picture ->
  withCString ".png" $ \extension -> alloca $ \size ->
    bracket (c'exportImageToMemory picture extension size) (c'free . castPtr) $ \bytes -> do
      count <- fromIntegral <$> peek size
      when (bytes == nullPtr || count <= 0) (throwIO (userError "Screenshot encoding failed."))
      BS.packCStringLen (castPtr bytes, count) >>= BS.writeFile file
  where
    release raw = (c'free . castPtr =<< peek (p'image'data raw)) `finally` free raw
