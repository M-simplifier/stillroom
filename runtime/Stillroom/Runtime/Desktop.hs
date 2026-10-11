{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Desktop (runDesktop) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkOS)
import Control.Concurrent.MVar
import Control.Exception hiding (handle)
import Control.Monad (forM_, unless, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.List (find)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Raylib.Core qualified as R
import Raylib.Core.Text (unloadFont)
import Raylib.Core.Textures (exportImageToMemory, loadImageFromScreen, unloadTexture)
import Raylib.Internal (WindowResources)
import Raylib.Types
import Stillroom.Domain
import Stillroom.Runtime.Audio qualified as Audio
import Stillroom.Runtime.Native qualified as Native
import Stillroom.Runtime.Pictures qualified as Pictures
import Stillroom.Runtime.Render qualified as View
import Stillroom.Runtime.Storage
import Stillroom.Session
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

data Desktop = Desktop
  { sessionRef :: !(IORef Session),
    preferencesRef :: !(IORef Preferences),
    noticeRef :: !(IORef (Maybe String)),
    pickerBusy :: !(IORef Bool),
    pickerResult :: !(MVar (Either String (Maybe FilePath))),
    fontRef :: !(IORef Font),
    fontTitlesRef :: !(IORef [String]),
    picturesRef :: !(IORef [Pictures.PictureEntry]),
    pictureRef :: !(IORef (Maybe (Pictures.PictureEntry, Texture))),
    pictureBusy :: !(IORef Bool),
    pictureResult :: !(MVar (Either Pictures.PictureError (Maybe (Pictures.PictureEntry, Pictures.PreparedPicture)))),
    engine :: !Audio.Engine,
    preferencesPath :: !FilePath,
    mutedCheck :: !Bool
  }

runDesktop :: FilePath -> Maybe FilePath -> Maybe FilePath -> IO Bool
runDesktop statePath checkDirectory overrideFolder = do
  languageValue <- Native.systemLanguage
  (loaded, initialNotice) <- readPreferences statePath languageValue
  let preferences = loaded {savedFolder = case overrideFolder of Just p -> Just p; Nothing -> savedFolder loaded}
      prefs = if isJust checkDirectory then preferences {savedVolume = 0, savedWindow = Compact, savedLanguage = English} else preferences
  tracksResult <- maybe (pure (Right presetTracks)) scanFolder (savedFolder prefs)
  let tracks = either (const presetTracks) id tracksResult
      notice = either (const (Just "Music folder unavailable; built-in music is playing.")) (const initialNotice) tracksResult
  R.setTraceLogLevel LogWarning
  R.setConfigFlags ([VsyncHint, WindowAlwaysRun, WindowResizable] ++ [WindowHidden | isJust checkDirectory])
  bracket (R.initWindow 760 500 "Stillroom") (R.closeWindow . Just) $ \window -> do
    R.setExitKey KeyNull
    R.setWindowMinSize 700 500
    Pictures.setAppIcon
    -- Keep presentation synchronized; do not alternate short frames with an
    -- unrelated Haskell sleep while a borderless window covers a display.
    R.setTargetFPS 60
    -- A normal native title bar is available in compact mode; the full-screen
    -- mode uses an undecorated, non-topmost window on exactly one display.
    pictures <- Pictures.discoverPictures (takeDirectory statePath </> "backgrounds")
    let trackTitles = map trackTitle (NE.toList tracks)
    font <- View.loadTypeface window (trackTitles ++ map Pictures.pictureName pictures)
    seed <- round . (* 1000000) <$> getPOSIXTime
    let start chosen startupNotice = Audio.withEngine chosen seed (loudness (restoreSession prefs)) $ \audio -> do
          desktop <-
            Desktop
              <$> newIORef (restoreSession prefs)
              <*> newIORef prefs
              <*> newIORef startupNotice
              <*> newIORef False
              <*> newEmptyMVar
              <*> newIORef font
              <*> newIORef trackTitles
              <*> newIORef pictures
              <*> newIORef Nothing
              <*> newIORef False
              <*> newEmptyMVar
              <*> pure audio
              <*> pure statePath
              <*> pure (isJust checkDirectory)
          forM_ (Pictures.findPicture (savedPicture prefs) pictures) $ \entry -> do
            prepared <- Pictures.preparePicture entry
            case prepared of
              Left issue -> when (savedBackground prefs == Picture) $
                writeIORef (noticeRef desktop) (Just (Pictures.pictureErrorText (savedLanguage prefs) issue))
              Right image -> do
                texture <- Pictures.uploadPicture window image
                writeIORef (pictureRef desktop) (Just (entry, texture))
                modifyIORef' (preferencesRef desktop) (\p -> p {savedPicture = Just (Pictures.pictureKey entry)})
          place desktop False
          now <- R.getTime
          phase <- newIORef (0 :: Int)
          checks <- newIORef ([] :: [(String, Bool)])
          pausePosition <- newIORef 0
          case checkDirectory of
            Just folder -> createDirectoryIfMissing True folder
            Nothing -> pure ()
          loop window desktop checkDirectory phase checks pausePosition now now 0
    -- An unavailable or corrupt selected library is a recoverable startup
    -- condition. Device/window failures remain visible rather than looping.
    result <- tryJust onlyLibraryFailure (start tracks notice)
    case result of
      Right success -> pure success
      Left _ -> start presetTracks (Just "Music files could not be decoded; built-in music is playing.")
  where
    onlyLibraryFailure Audio.NoPlayableTracks = Just ()
    onlyLibraryFailure _ = Nothing

place :: Desktop -> Bool -> IO ()
place desktop moveNext = do
  screens <- Native.monitors
  s <- readIORef (sessionRef desktop)
  prefs <- readIORef (preferencesRef desktop)
  let preferred =
        fromMaybe
          (fromMaybe (Native.Monitor "" 0 0 1280 720 True) (safeFirst screens))
          (find (not . Native.monitorPrimary) screens)
      currentScreen = fromMaybe preferred (find ((== savedScreen prefs) . Native.monitorName) screens)
      afterCurrent = dropWhile ((/= Native.monitorName currentScreen) . Native.monitorName) screens
      chosen = if moveNext then fromMaybe preferred (safeFirst (drop 1 afterCurrent ++ screens)) else currentScreen
      (w, h, x, y) = case windowMode s of
        -- Exact monitor-sized OpenGL windows can be promoted to fullscreen by
        -- the driver. Overscan by one pixel, then clip to this display in Win32.
        Fullscreen -> (Native.monitorWidth chosen + 2, Native.monitorHeight chosen + 2, Native.monitorX chosen - 1, Native.monitorY chosen - 1)
        Compact ->
          ( 760,
            500,
            Native.monitorX chosen + (Native.monitorWidth chosen - 760) `div` 2,
            Native.monitorY chosen + (Native.monitorHeight chosen - 500) `div` 2
          )
  handle <- R.getWindowHandle
  if windowMode s == Fullscreen
    then do
      R.setWindowState [WindowUndecorated]
      -- Set the clip before the single native resize/move. Its WM_SIZE
      -- callback updates raylib's surface size without an intermediate spill.
      Native.positionWindow handle x y w h
    else do
      R.clearWindowState [WindowUndecorated]
      R.setWindowSize w h
      R.setWindowPosition x y
      Native.unclipWindow handle
  writeIORef (preferencesRef desktop) prefs {savedScreen = Native.monitorName chosen}
  Native.setPower (transport s) (windowMode s)
  where
    safeFirst [] = Nothing; safeFirst (x : _) = Just x

save :: Desktop -> IO ()
save desktop = do
  s <- readIORef (sessionRef desktop)
  old <- readIORef (preferencesRef desktop)
  let updated = fromSession s old
  writeIORef (preferencesRef desktop) updated
  result <- writePreferences (preferencesPath desktop) updated
  case result of
    Left _ -> writeIORef (noticeRef desktop) (Just "Settings could not be saved.")
    Right () -> pure ()

apply :: Desktop -> Event -> IO ()
apply desktop event = do
  old <- readIORef (sessionRef desktop)
  let (updated, effects) = step event old
  writeIORef (sessionRef desktop) updated
  forM_ effects $ \effect -> case effect of
    ApplyTransport transportValue -> do
      Audio.setTransport (engine desktop) transportValue
      Native.setPower transportValue (windowMode updated)
    ApplyVolume value -> Audio.setVolume (engine desktop) (if mutedCheck desktop then either (const defaultVolume) id (volume 0) else value)
    SkipTrack -> Audio.next (engine desktop)
    PlaceWindow -> place desktop False
    MoveScreen -> place desktop True
    Persist -> save desktop
    SelectPresets -> do
      seed <- round . (* 1000000) <$> getPOSIXTime
      Audio.replaceLibrary (engine desktop) presetTracks seed
      modifyIORef' (preferencesRef desktop) useBuiltInMusic
      writeIORef (noticeRef desktop) Nothing
    PickMusicFolder -> do
      busy <- readIORef (pickerBusy desktop)
      unless busy $ do
        writeIORef (pickerBusy desktop) True
        handle <- R.getWindowHandle
        prefs <- readIORef (preferencesRef desktop)
        _ <- forkOS $ do
          result <- Native.pickFolder handle (savedFolder prefs <|> previousFolder prefs)
          putMVar (pickerResult desktop) result
        pure ()
    PickPicture -> do
      handle <- R.getWindowHandle
      requestPicture desktop $ do
        chosen <- Native.pickPicture handle
        case chosen of
          Left _ -> pure (Left Pictures.ImageUnavailable)
          Right Nothing -> pure (Right Nothing)
          Right (Just file) -> fmap Just <$> Pictures.importPicture (pictureDirectory desktop) file
    SelectPicture direction -> do
      entries <- readIORef (picturesRef desktop)
      prefs <- readIORef (preferencesRef desktop)
      forM_ (Pictures.adjacentPicture direction (savedPicture prefs) entries) $ \entry ->
        requestPicture desktop $ fmap (Just . (entry,)) <$> Pictures.preparePicture entry
    DeletePicture -> do
      busy <- readIORef (pictureBusy desktop)
      current <- readIORef (pictureRef desktop)
      unless (busy || background updated /= Picture) $ forM_ current $ \(entry, _) -> case Pictures.pictureOrigin entry of
        Pictures.Bundled -> writeIORef (noticeRef desktop) (Just (if language updated == Japanese then "内蔵の画像です。登録した画像だけ解除できます。" else "Built-in images stay available. Only imported copies can be removed."))
        Pictures.Imported -> do
          removed <- Pictures.removePicture entry
          case removed of
            Left issue -> writeIORef (noticeRef desktop) (Just (Pictures.pictureErrorText (language updated) issue))
            Right () -> do
              entries <- Pictures.discoverPictures (pictureDirectory desktop)
              writeIORef (picturesRef desktop) entries
              modifyIORef' (preferencesRef desktop) (\p -> p {savedPicture = Nothing})
              apply desktop (ChooseBackground Forest)
              forM_ (Pictures.findPicture Nothing entries) $ \next ->
                requestPicture desktop $ fmap (Just . (next,)) <$> Pictures.preparePicture next

pictureDirectory :: Desktop -> FilePath
pictureDirectory desktop = takeDirectory (preferencesPath desktop) </> "backgrounds"

requestPicture :: Desktop -> IO (Either Pictures.PictureError (Maybe (Pictures.PictureEntry, Pictures.PreparedPicture))) -> IO ()
requestPicture desktop work = do
  busy <- readIORef (pictureBusy desktop)
  unless busy $ do
    writeIORef (pictureBusy desktop) True
    _ <- forkOS $ do
      result <- work `catch` (\(_ :: IOException) -> pure (Left Pictures.ImageUnavailable))
      putMVar (pictureResult desktop) result
    pure ()

receivePicture :: WindowResources -> Desktop -> IO ()
receivePicture window desktop = do
  completed <- tryTakeMVar (pictureResult desktop)
  forM_ completed $ \result -> do
    writeIORef (pictureBusy desktop) False
    state <- readIORef (sessionRef desktop)
    case result of
      Left issue -> writeIORef (noticeRef desktop) (Just (Pictures.pictureErrorText (language state) issue))
      Right Nothing -> pure ()
      Right (Just (entry, prepared)) -> do
        loaded <- try (Pictures.uploadPicture window prepared)
        case loaded of
          Left issue -> writeIORef (noticeRef desktop) (Just (Pictures.pictureErrorText (language state) issue))
          Right texture -> do
            previous <- atomicModifyIORef' (pictureRef desktop) (\old -> (Just (entry, texture), old))
            forM_ previous (\(_, oldTexture) -> unloadTexture oldTexture window)
            entries <- Pictures.discoverPictures (pictureDirectory desktop)
            oldEntries <- readIORef (picturesRef desktop)
            writeIORef (picturesRef desktop) entries
            when (entries /= oldEntries) $ do
              titles <- readIORef (fontTitlesRef desktop)
              newFont <- View.loadTypeface window (titles ++ map Pictures.pictureName entries)
              oldFont <- atomicModifyIORef' (fontRef desktop) (\old -> (newFont, old))
              unloadFont oldFont window
            modifyIORef' (preferencesRef desktop) (\p -> p {savedPicture = Just (Pictures.pictureKey entry)})
            writeIORef (noticeRef desktop) Nothing
            apply desktop (ChooseBackground Picture)
    apply desktop Activity

keyboard :: IO [Event]
keyboard =
  fmap concat $
    mapM
      (\(key, event) -> do pressed <- R.isKeyPressed key; pure [event | pressed])
      [ (KeySpace, TogglePlayback),
        (KeyN, NextTrack),
        (KeyF11, ToggleWindow),
        (KeyEscape, Escape),
        (KeyM, NextScreen),
        (KeyO, OpenMusic),
        (KeyP, UsePresets),
        (KeyS, ToggleSettings),
        (KeyI, OpenPicture),
        (KeyLeftBracket, BrowsePicture PreviousPicture),
        (KeyRightBracket, BrowsePicture NextPicture),
        (KeyB, CyclePictureShade),
        (KeyEqual, ChangeVolume 0.025),
        (KeyMinus, ChangeVolume (-0.025))
      ]

loop ::
  WindowResources ->
  Desktop ->
  Maybe FilePath ->
  IORef Int ->
  IORef [(String, Bool)] ->
  IORef Double ->
  Double ->
  Double ->
  Int ->
  IO Bool
loop window desktop checkDirectory checkPhase checks pausePosition began previous frame = do
  now <- R.getTime
  let elapsed = either (const zeroSeconds) id (seconds (max 0 (now - previous)))
  apply desktop (Elapse elapsed)
  requested <- Native.activationRequested
  when requested (R.restoreWindow >> R.setWindowFocused >> apply desktop Activity)
  Vector2 dx dy <- R.getMouseDelta
  onScreen <- R.isCursorOnScreen
  when (onScreen && (abs dx + abs dy) > 0) (apply desktop Activity)
  actions <- keyboard
  mapM_ (apply desktop) actions
  sBefore <- readIORef (sessionRef desktop)
  Vector2 mx my <- R.getMousePosition
  w <- R.getScreenWidth
  h <- R.getScreenHeight
  clicked <- R.isMouseButtonPressed MouseButtonLeft
  let click = if clicked then View.hitTest w h sBefore (mx, my) else Nothing
  case click of
    Just (View.Intent event) -> apply desktop event
    _ -> when clicked (apply desktop Activity)
  selected <- tryTakeMVar (pickerResult desktop)
  case selected of
    Nothing -> pure ()
    Just result -> do
      writeIORef (pickerBusy desktop) False
      case result of
        Left issue -> writeIORef (noticeRef desktop) (Just issue)
        Right Nothing -> pure ()
        Right (Just folder) -> do
          library <- scanFolder folder
          case library of
            Left _ -> writeIORef (noticeRef desktop) (Just "No playable music in this folder (MP3, WAV, OGG).")
            Right tracks -> do
              seed <- round . (* 1000000) <$> getPOSIXTime
              replacement <- tryJust synchronous (Audio.replaceLibrary (engine desktop) tracks seed)
              case replacement of
                Left _ -> writeIORef (noticeRef desktop) (Just "These files could not be played. The current music continues.")
                Right () -> do
                  let titles = map trackTitle (NE.toList tracks)
                  writeIORef (fontTitlesRef desktop) titles
                  entries <- readIORef (picturesRef desktop)
                  newFont <- View.loadTypeface window (titles ++ map Pictures.pictureName entries)
                  oldFont <- atomicModifyIORef' (fontRef desktop) (\old -> (newFont, old))
                  unloadFont oldFont window
                  modifyIORef' (preferencesRef desktop) (useMusicFolder folder)
                  writeIORef (noticeRef desktop) Nothing
                  save desktop
          apply desktop Activity
  receivePicture window desktop
  Audio.pump (engine desktop)
  finished <- case checkDirectory of
    Nothing -> pure False
    Just directory -> checkStep desktop directory checkPhase checks pausePosition (now - began)
  state <- readIORef (sessionRef desktop)
  sound <- Audio.snapshot (engine desktop)
  font <- readIORef (fontRef desktop)
  notice <- readIORef (noticeRef desktop)
  picture <- readIORef (pictureRef desktop)
  entries <- readIORef (picturesRef desktop)
  let shown = controlsVisible state
  focused <- R.isWindowFocused
  hidden <- R.isCursorHidden
  let hide = not shown && focused && onScreen
  when (hidden /= hide) (if hide then R.hideCursor else R.showCursor)
  -- A mode/monitor action may have resized the surface since hit testing.
  drawWidth <- R.getScreenWidth
  drawHeight <- R.getScreenHeight
  R.beginDrawing
  let caption = case picture of
        Nothing -> if language state == Japanese then "画像を追加してください" else "Add a background image"
        Just (entry, _) ->
          let position = length (takeWhile ((/= Pictures.pictureKey entry) . Pictures.pictureKey) entries) + 1
              name = case Pictures.pictureKey entry of
                "bundled:01-mist-lake.png" -> if language state == Japanese then "霧の湖" else "Mist lake"
                "bundled:02-quiet-forest.png" -> if language state == Japanese then "静かな森" else "Quiet forest"
                "bundled:03-soft-dunes.png" -> if language state == Japanese then "砂丘" else "Soft dunes"
                "bundled:04-night-field.png" -> if language state == Japanese then "夜の抽象画" else "Night field"
                _ -> Pictures.pictureName entry
           in show position ++ " / " ++ show (length entries) ++ "   " ++ name
  View.render font drawWidth drawHeight state (snd <$> picture) caption (Audio.title sound) (notice `orElse` Audio.audioWarning sound) (mx, my)
  R.endDrawing
  closed <- R.windowShouldClose
  if closed || click == Just View.Quit || finished
    then do
      save desktop
      R.showCursor
      Native.setPower Paused Compact
      results <- readIORef checks
      pure (all snd results)
    else loop window desktop checkDirectory checkPhase checks pausePosition began now (frame + 1)
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y
    synchronous e = case fromException e :: Maybe AsyncException of Just _ -> Nothing; Nothing -> Just e

checkStep :: Desktop -> FilePath -> IORef Int -> IORef [(String, Bool)] -> IORef Double -> Double -> IO Bool
checkStep desktop folder phaseRef checks pausePosition elapsed = do
  phase <- readIORef phaseRef
  sound <- Audio.snapshot (engine desktop)
  let record name success = modifyIORef' checks ((name, success) :)
      advance = writeIORef phaseRef (phase + 1)
  case phase of
    0 | elapsed > 0.7 -> do
      record "autoplay clock advances" (Audio.audible sound && Audio.position sound > 0.1)
      entries <- readIORef (picturesRef desktop)
      record "four bundled pictures are available" (length (filter ((== Pictures.Bundled) . Pictures.pictureOrigin) entries) == 4)
      current <- readIORef (pictureRef desktop)
      record "startup uploads a valid picture texture" (maybe False (\(_, texture) -> texture'width texture > 0 && texture'height texture > 0) current)
      forM_ current $ \(entry, _) -> do
        let source = folder </> "登録の検査.PNG"
        BS.readFile (Pictures.picturePath entry) >>= BS.writeFile source
        requestPicture desktop (fmap Just <$> Pictures.importPicture (pictureDirectory desktop) source)
      advance
      pure False
    1 | elapsed > 1.5 -> do
      current <- readIORef (pictureRef desktop)
      record "Unicode image import copies and displays the image" (maybe False (\(entry, _) -> Pictures.pictureOrigin entry == Pictures.Imported && Pictures.picturePath entry /= folder </> "登録の検査.PNG") current)
      screenshot (folder </> "controls.png")
      apply desktop TogglePlayback
      Audio.snapshot (engine desktop) >>= writeIORef pausePosition . Audio.position
      advance
      pure False
    2 | elapsed > 2.2 -> do
      before <- readIORef pausePosition
      record "pause holds audio position" (abs (Audio.position sound - before) < 0.1 && not (Audio.audible sound))
      apply desktop TogglePlayback
      advance
      pure False
    3 | elapsed > 3.2 -> do
      record "resume advances audio" (Audio.audible sound)
      let corrupt = folder </> "broken.png"
      BS.writeFile corrupt (BS.pack [0, 1, 2, 3])
      requestPicture desktop (fmap Just <$> Pictures.importPicture (pictureDirectory desktop) corrupt)
      Audio.seekForCheck (engine desktop) (Audio.duration sound - 4)
      advance
      pure False
    4 | elapsed > 3.9 -> do
      record "two decks crossfade" (Audio.crossfading sound)
      current <- readIORef (pictureRef desktop)
      notice <- readIORef (noticeRef desktop)
      record "corrupt images preserve the selected texture and report failure" (isJust notice && maybe False ((== Pictures.Imported) . Pictures.pictureOrigin . fst) current)
      advance
      pure False
    5 | elapsed > 8 -> do
      record "automatic next track" (Audio.transitionCount sound >= 1)
      apply desktop ToggleWindow
      handle <- R.getWindowHandle
      prefs <- readIORef (preferencesRef desktop)
      screens <- Native.monitors
      matches <- maybe (pure False) (Native.borderlessMatches handle) (find ((== savedScreen prefs) . Native.monitorName) screens)
      exclusive <- R.isWindowFullscreen
      record "borderless surface clips exactly to the chosen display" (matches && not exclusive)
      modifyIORef' (sessionRef desktop) (\s -> s {idleSeconds = 10})
      state <- readIORef (sessionRef desktop)
      record "fullscreen hides idle controls" (not (controlsVisible state))
      advance
      pure False
    6 | elapsed > 8.6 -> do
      screenshot (folder </> "quiet.png")
      apply desktop Activity
      state <- readIORef (sessionRef desktop)
      record "activity reveals controls" (controlsVisible state)
      apply desktop ToggleWindow
      apply desktop NextScreen
      save desktop
      advance
      pure False
    7 | elapsed > 9.3 -> do
      state <- readIORef (sessionRef desktop)
      record "compact mode stays operable" (windowMode state == Compact && controlsVisible state)
      handle <- R.getWindowHandle
      Native.isUnclipped handle >>= record "compact mode removes the fullscreen clip"
      apply desktop RemovePicture
      advance
      pure False
    8 | elapsed > 10 -> do
      entries <- readIORef (picturesRef desktop)
      sourceExists <- doesFileExist (folder </> "登録の検査.PNG")
      record "unregister removes only the managed copy" (sourceExists && all ((== Pictures.Bundled) . Pictures.pictureOrigin) entries)
      apply desktop (BrowsePicture NextPicture)
      advance
      pure False
    9 | elapsed > 10.7 -> do
      current <- readIORef (pictureRef desktop)
      record "next image uploads the next bundled picture" (maybe False ((== "bundled:02-quiet-forest.png") . Pictures.pictureKey . fst) current)
      apply desktop CyclePictureShade
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "image choice and brightness survive settings reload" (warning == Nothing && reloaded == prefs && savedBackground reloaded == Picture && savedPictureShade reloaded == Soft)
      apply desktop ToggleSettings
      apply desktop ToggleLanguage
      advance
      pure False
    10 | elapsed > 11.4 -> do
      screenshot (folder </> "pictures-settings.png")
      results <- reverse <$> readIORef checks
      BL.writeFile
        (folder </> "native-check.json")
        ( encode
            ( object
                [ "passed" .= all snd results,
                  "checks" .= [object ["name" .= name, "passed" .= ok] | (name, ok) <- results]
                ]
            )
        )
      advance
      pure True
    _ -> pure False

-- Encode through memory so absolute and Unicode output paths stay in Haskell.
screenshot :: FilePath -> IO ()
screenshot file = do
  picture <- loadImageFromScreen
  bytes <- exportImageToMemory picture ".png"
  BS.writeFile file (BS.pack bytes)
