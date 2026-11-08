{-# LANGUAGE OverloadedStrings #-}

-- | Deterministic UI inspection through the production Vulkan renderer.
module Stillroom.Runtime.Capture (runCapture, screenshot) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, throwIO)
import Control.Monad (forM, replicateM_, unless, void, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import GameIO.Graphics qualified as GPU
import GameIO.Image (encodePng, rgbaImage)
import GameIO.Platform qualified as Platform
import GHC.Clock (getMonotonicTimeNSec)
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
  Platform.withWindowVisibility False (Platform.WindowConfig "Stillroom UI capture" 700 500) $ \window ->
    GPU.withRenderer window $ \renderer -> do
      started <- getMonotonicTimeNSec
      bracket (View.loadTypeface renderer ["Soft piano"]) (View.unloadTypeface renderer) $ \font ->
        Rest.withRestAssets window renderer $ \assets -> do
          ready <- Rest.proceduralReady assets
          unless (ready && Rest.thumbnailsReady assets) $
            throwIO (userError "UI capture requires the procedural shaders and all thumbnails.")
          entries <- Pictures.discoverPictures (folder </> "backgrounds")
          entry <-
            maybe (throwIO (userError "UI capture requires the bundled open water image.")) pure $
              Pictures.findPicture (Just "bundled:open-water.png") entries
          prepared <- Pictures.preparePicture entry >>= either throwIO pure
          bracket (Pictures.uploadPicture renderer prepared) (Pictures.unloadPicture renderer) $ \texture -> do
            frames <- forM views $ \(name, lang, page, opened, w, h, scene, mode) -> do
              Platform.resizeWindow window w h
              void (Platform.pollWindowEvents window)
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
                  caption = "1 / " ++ show (length entries) ++ UI.local state "   Open water" "   ひらけた水辺"
                  focus = if page == MusicSettings then Just (UI.Intent OpenMusic) else Nothing
                  draw readback =
                    View.renderWithFocus renderer readback font assets w h state (Just texture) caption (presetLabel lang Linen) Nothing (-1, -1) focus
                  file = name ++ ".png"
              -- Let the production Journey loader complete its bounded uploads.
              when (scene == RailJourney) (replicateM_ 82 (void (draw False)))
              actual <- Platform.windowFramebufferSize window
              unless (actual == (w, h)) (throwIO (userError "UI capture surface has unexpected dimensions."))
              screenshot (folder </> file) (draw True)
              readyNow <- Rest.proceduralReady assets
              unless readyNow (throwIO (userError "Landscape rendering failed."))
              pure (object ["file" .= file, "width" .= w, "height" .= h, "language" .= show lang])
            -- Exercise the actual image decoder, upload and cover crop for all
            -- bundled wallpapers.
            wallpaperFrames <- forM (filter ((== Pictures.Bundled) . Pictures.pictureOrigin) entries) $ \wallpaper -> do
              pixels <- Pictures.preparePicture wallpaper >>= either throwIO pure
              bracket (Pictures.uploadPicture renderer pixels) (Pictures.unloadPicture renderer) $ \wallpaperTexture -> do
                Platform.resizeWindow window 1280 720
                void (Platform.pollWindowEvents window)
                let state = (initialSession defaultVolume) {background = Picture, idleSeconds = 10}
                    draw = View.render renderer True font assets 1280 720 state (Just wallpaperTexture) "" "Linen" Nothing (-1, -1)
                    file = "wallpaper-" ++ Pictures.pictureName wallpaper ++ ".png"
                screenshot (folder </> file) draw
                pure (object ["file" .= file, "width" .= (1280 :: Int), "height" .= (720 :: Int), "pictureKey" .= Pictures.pictureKey wallpaper])
            finished <- getMonotonicTimeNSec
            BL.writeFile report $ encode $ object
              [ "completed" .= True,
                "captures" .= (frames ++ wallpaperFrames),
                "count" .= length (frames ++ wallpaperFrames),
                "elapsedSeconds" .= (fromIntegral (finished - started) / 1e9 :: Double),
                "audioOpened" .= False,
                "personalSettingsRead" .= False
              ]
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


-- Wait for a presented frame at the current extent. A deferred acquisition
-- can occur immediately after resize, even when the renderer remains healthy.
screenshot :: FilePath -> IO (GPU.FrameStatus, Maybe GPU.FramePixels) -> IO ()
screenshot file draw = tryFrame (120 :: Int)
  where
    tryFrame 0 = throwIO (userError "Screenshot did not acquire a presentable surface.")
    tryFrame attempts = do
      (status, pixels) <- draw
      case (status, pixels) of
        (GPU.FramePresented, Just (GPU.FramePixels w h bytes)) ->
          either (throwIO . userError . show) (BS.writeFile file . encodePng) (rgbaImage w h bytes)
        (GPU.FramePresented, Nothing) -> throwIO (userError "Screenshot readback returned no pixels.")
        _ -> threadDelay 1000 >> tryFrame (attempts - 1)
