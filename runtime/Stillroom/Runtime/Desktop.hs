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
import Data.List (find, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Raylib.Core qualified as R
import Raylib.Core.Textures (unloadTexture)
import Raylib.Internal (WindowResources)
import Raylib.Types
import Stillroom.Domain
import Stillroom.Interface qualified as UI
import Stillroom.Runtime.Audio qualified as Audio
import Stillroom.Runtime.Capture (screenshot)
import Stillroom.Runtime.Native qualified as Native
import Stillroom.Runtime.Pictures qualified as Pictures
import Stillroom.Runtime.Render qualified as View
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Runtime.Storage
import Stillroom.Session
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

data Desktop = Desktop
  { sessionRef :: !(IORef Session),
    preferencesRef :: !(IORef Preferences),
    noticeRef :: !(IORef (Maybe String)),
    pickerBusy :: !(IORef Bool),
    pickerGeneration :: !(IORef Integer),
    pickerResult :: !(MVar (Integer, Either String (Maybe FilePath))),
    fontRef :: !(IORef View.Typeface),
    fontTitlesRef :: !(IORef [String]),
    picturesRef :: !(IORef [Pictures.PictureEntry]),
    pictureRef :: !(IORef (Maybe (Pictures.PictureEntry, Texture))),
    pictureBusy :: !(IORef Bool),
    pictureGeneration :: !(IORef Integer),
    pictureResult :: !(MVar (Integer, Either Pictures.PictureError (Maybe (Pictures.PictureEntry, Pictures.PreparedPicture)))),
    restAssets :: !Rest.RestAssets,
    focusedRef :: !(IORef Bool),
    controlFocusRef :: !(IORef (Maybe UI.UIAction)),
    checkFrameTimes :: !(IORef [(RestScene, Double)]),
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
    rests <- Rest.loadRestAssets window
    seed <- round . (* 1000000) <$> getPOSIXTime
    let start chosen startupNotice =
          -- A failed library restarts the audio session. Acquire a fresh font
          -- too: an earlier picture/music change may have released the first
          -- font. The current font is always closed before the GL context.
          withSessionFont window (map trackTitle (NE.toList chosen) ++ map Pictures.pictureName pictures)
            $ \liveFont -> Audio.withEngine chosen seed (loudness (restoreSession prefs)) $ \audio -> do
          desktop <-
            Desktop
              <$> newIORef (restoreSession prefs)
              <*> newIORef prefs
              <*> newIORef startupNotice
              <*> newIORef False
              <*> newIORef 0
              <*> newEmptyMVar
              <*> pure liveFont
              <*> newIORef (map trackTitle (NE.toList chosen))
              <*> newIORef pictures
              <*> newIORef Nothing
              <*> newIORef False
              <*> newIORef 0
              <*> newEmptyMVar
              <*> pure rests
              <*> newIORef False
              <*> newIORef Nothing
              <*> newIORef []
              <*> pure audio
              <*> pure statePath
              <*> pure (isJust checkDirectory)
          forM_ (Pictures.findPicture (savedPicture prefs) pictures) $ \entry -> do
            prepared <- Pictures.preparePicture entry
            case prepared of
              Left issue ->
                when (savedBackground prefs == Picture) $
                  writeIORef (noticeRef desktop) (Just (Pictures.pictureErrorText (savedLanguage prefs) issue))
              Right image -> do
                texture <- Pictures.uploadPicture window image
                writeIORef (pictureRef desktop) (Just (entry, texture))
                modifyIORef' (preferencesRef desktop) (\p -> p {savedPicture = Just (Pictures.pictureKey entry)})
          place desktop False
          now <- R.getTime
          phase <- newIORef (0 :: Int, now)
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

withSessionFont :: WindowResources -> [String] -> (IORef View.Typeface -> IO a) -> IO a
withSessionFont window titles = bracket
  (View.loadTypeface window titles >>= newIORef)
  (\ref -> readIORef ref >>= View.unloadTypeface window)

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
  let enteringRest = viewingMode old == Working && viewingMode updated == Resting
      choosingBackground = case event of ChooseBackground _ -> viewingMode old == Working; _ -> False
  -- Pending IO belongs to the choice that requested it. A newer choice or
  -- entering rest invalidates its result before it can change textures/audio.
  when (enteringRest || choosingBackground) (modifyIORef' (pictureGeneration desktop) (+ 1))
  when (enteringRest || SelectPresets `elem` effects) (modifyIORef' (pickerGeneration desktop) (+ 1))
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
        generation <- readIORef (pickerGeneration desktop)
        _ <- forkOS $ do
          result <- Native.pickFolder handle (savedFolder prefs <|> previousFolder prefs)
          putMVar (pickerResult desktop) (generation, result)
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
    generation <- readIORef (pictureGeneration desktop)
    _ <- forkOS $ do
      result <- work `catch` (\(_ :: IOException) -> pure (Left Pictures.ImageUnavailable))
      putMVar (pictureResult desktop) (generation, result)
    pure ()

receivePicture :: WindowResources -> Desktop -> IO ()
receivePicture window desktop = do
  completed <- tryTakeMVar (pictureResult desktop)
  forM_ completed $ \(generation, result) -> do
    writeIORef (pictureBusy desktop) False
    current <- readIORef (pictureGeneration desktop)
    if generation == current
      then accept result
      else case result of
        Right (Just (_, prepared)) -> Pictures.releasePicture prepared
        _ -> pure ()
  where
    accept result = do
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
                View.unloadTypeface window oldFont
              modifyIORef' (preferencesRef desktop) (\p -> p {savedPicture = Just (Pictures.pictureKey entry)})
              writeIORef (noticeRef desktop) Nothing
              apply desktop (ChooseBackground Picture)
      apply desktop Activity

receiveMusic :: WindowResources -> Desktop -> IO ()
receiveMusic window desktop = do
  selected <- tryTakeMVar (pickerResult desktop)
  forM_ selected $ \(generation, result) -> do
    writeIORef (pickerBusy desktop) False
    current <- readIORef (pickerGeneration desktop)
    when (generation == current) $ case result of
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
                View.unloadTypeface window oldFont
                modifyIORef' (preferencesRef desktop) (useMusicFolder folder)
                writeIORef (noticeRef desktop) Nothing
                save desktop
        apply desktop Activity
  where
    synchronous e = case fromException e :: Maybe AsyncException of Just _ -> Nothing; Nothing -> Just e

keyboard :: Session -> Bool -> IO [Event]
keyboard s _ | viewingMode s == Resting = do
  leave <- or <$> mapM R.isKeyPressed [KeyEscape, KeyR]
  pure [LeaveRest | leave]
keyboard s hasFocus = do
  ordinary <-
    pressed
      ([(KeySpace, TogglePlayback) | not hasFocus] ++
      [ (KeyR, ToggleRest),
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
        (KeyB, if restControls then CycleRestShade else CyclePictureShade),
        (KeyEqual, ChangeVolume 0.025),
        (KeyMinus, ChangeVolume (-0.025))
      ])
  restActions <-
    if restControls
      then
        pressed [(KeyA, ToggleRestMotion)]
      else pure []
  pure (ordinary ++ restActions)
  where
    restControls = settingsOpen s && settingsPage s == RestSettings
    pressed = fmap concat . mapM (\(key, event) -> do down <- R.isKeyPressed key; pure [event | down])

loop ::
  WindowResources ->
  Desktop ->
  Maybe FilePath ->
  IORef (Int, Double) ->
  IORef [(String, Bool)] ->
  IORef Double ->
  Double ->
  Double ->
  Int ->
  IO Bool
loop window desktop checkDirectory checkPhase checks pausePosition began previous frame = do
  now <- R.getTime
  let elapsed = either (const zeroSeconds) id (seconds (max 0 (now - previous)))
  when (mutedCheck desktop) $ do
    previousState <- readIORef (sessionRef desktop)
    when
      ( viewingMode previousState == Resting
          && restMotion previousState == Gentle
      )
      $ modifyIORef' (checkFrameTimes desktop) ((restScene previousState, secondsValue elapsed * 1000) :)
  apply desktop (Elapse elapsed)
  requested <- Native.activationRequested
  when requested (R.restoreWindow >> R.setWindowFocused >> apply desktop Activity)
  focused <- R.isWindowFocused
  wasFocused <- atomicModifyIORef' (focusedRef desktop) (\old -> (focused, old))
  when (wasFocused && not focused) (apply desktop LeaveRest)
  -- Hit-test what was visible before this frame's mouse movement wakes controls.
  -- A first click on a quiet screen must never trigger an invisible button.
  sBefore <- readIORef (sessionRef desktop)
  Vector2 dx dy <- R.getMouseDelta
  onScreen <- R.isCursorOnScreen
  when (onScreen && (abs dx + abs dy) > 0) $ do
    apply desktop Activity
    writeIORef (controlFocusRef desktop) Nothing
  w <- R.getScreenWidth
  h <- R.getScreenHeight
  tabbed <- R.isKeyPressed KeyTab
  when (tabbed && viewingMode sBefore == Working) $ do
    apply desktop Activity
    awake <- readIORef (sessionRef desktop)
    backwards <- (||) <$> R.isKeyDown KeyLeftShift <*> R.isKeyDown KeyRightShift
    modifyIORef' (controlFocusRef desktop) (UI.nextFocus backwards (UI.controls w h awake))
  focus <- readIORef (controlFocusRef desktop)
  current <- readIORef (sessionRef desktop)
  let activeFocus = focus >>= (\value -> if any ((== value) . UI.action) (UI.controls w h current) then Just value else Nothing)
  activated <- (||) <$> R.isKeyPressed KeyEnter <*> R.isKeyPressed KeySpace
  let keyboardAction = if activated then activeFocus else Nothing
  actions <- keyboard sBefore (isJust activeFocus)
  mapM_ (apply desktop) actions
  forM_ keyboardAction $ \action -> case action of UI.Intent event -> apply desktop event; UI.Quit -> pure ()
  Vector2 mx my <- R.getMousePosition
  clicked <- R.isMouseButtonPressed MouseButtonLeft
  when clicked (writeIORef (controlFocusRef desktop) Nothing)
  let click = if clicked then View.hitTest w h sBefore (mx, my) else Nothing
  case click of
    Just (View.Intent event) -> apply desktop event
    _ -> when clicked (apply desktop Activity)
  receiveMusic window desktop
  receivePicture window desktop
  Audio.pump (engine desktop)
  finished <- case checkDirectory of
    Nothing -> pure False
    Just directory -> checkStep window desktop directory checkPhase checks pausePosition (now - began)
  state <- readIORef (sessionRef desktop)
  when (viewingMode state == Resting) (writeIORef (controlFocusRef desktop) Nothing)
  drawFocus <- readIORef (controlFocusRef desktop)
  sound <- Audio.snapshot (engine desktop)
  font <- readIORef (fontRef desktop)
  notice <- readIORef (noticeRef desktop)
  picture <- readIORef (pictureRef desktop)
  entries <- readIORef (picturesRef desktop)
  let shown = controlsVisible state
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
  View.renderWithFocus font (restAssets desktop) drawWidth drawHeight state (snd <$> picture) caption (Audio.title sound) (notice `orElse` Audio.audioWarning sound) (mx, my) drawFocus
  R.endDrawing
  closed <- R.windowShouldClose
  if closed || click == Just View.Quit || keyboardAction == Just View.Quit || finished
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

-- Inject completed work deterministically after a newer selection. This uses
-- the real completion boundary without relying on filesystem/decode timing.
checkLateSelections :: WindowResources -> Desktop -> FilePath -> IO [(String, Bool)]
checkLateSelections window desktop folder = do
  entries <- readIORef (picturesRef desktop)
  let snapshot = do
        state <- readIORef (sessionRef desktop)
        prefs <- readIORef (preferencesRef desktop)
        picture <- readIORef (pictureRef desktop)
        notice <- readIORef (noticeRef desktop)
        pure (state, prefs, fmap (\(entry, texture) -> (Pictures.pictureKey entry, texture'id texture)) picture, notice)
      latePicture event = case entries of
        [] -> pure False
        entry : _ -> do
          pixels <- Pictures.preparePicture entry
          case pixels of
            Left _ -> pure False
            Right prepared -> do
              generation <- readIORef (pictureGeneration desktop)
              apply desktop event
              before <- snapshot
              writeIORef (pictureBusy desktop) True
              putMVar (pictureResult desktop) (generation, Right (Just (entry, prepared)))
              receivePicture window desktop
              after <- snapshot
              busy <- readIORef (pictureBusy desktop)
              pure (before == after && not busy)
  backgroundHeld <- latePicture (ChooseBackground Black)
  apply desktop (ChooseBackground Picture)
  restHeld <- latePicture ToggleRest
  apply desktop LeaveRest
  generation <- readIORef (pickerGeneration desktop)
  apply desktop ToggleRest
  before <- snapshot
  writeIORef (pickerBusy desktop) True
  putMVar (pickerResult desktop) (generation, Right (Just (folder </> "late-library")))
  receiveMusic window desktop
  after <- snapshot
  busy <- readIORef (pickerBusy desktop)
  apply desktop LeaveRest
  pure [("late image completion cannot overwrite a newer background", backgroundHeld), ("late image completion cannot change rest or its working picture", restHeld), ("late music picker completion cannot change rest", before == after && not busy)]

checkStep :: WindowResources -> Desktop -> FilePath -> IORef (Int, Double) -> IORef [(String, Bool)] -> IORef Double -> Double -> IO Bool
checkStep window desktop folder phaseRef checks pausePosition elapsed = do
  (phase, lastAdvanced) <- readIORef phaseRef
  now <- R.getTime
  sound <- Audio.snapshot (engine desktop)
  let record name success = modifyIORef' checks ((name, success) :)
      advance = R.getTime >>= \finishedAt -> writeIORef phaseRef (phase + 1, finishedAt)
      -- PNG encoding can take longer than an absolute phase interval. Wait
      -- after it finishes so at least several fresh frames reach the buffers.
      settled = now - lastAdvanced > 0.4
  case (if settled then phase else -1) of
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
      apply desktop ShowDisplaySettings
      apply desktop ToggleLanguage
      advance
      pure False
    10 | elapsed > 11.4 -> do
      screenshot (folder </> "pictures-settings.png")
      checkFontRestart window desktop >>= record "font replacement followed by a failed library can restart and draw"
      checkLateSelections window desktop folder >>= mapM_ (uncurry record)
      record "all eight rest thumbnails are preloaded" (Rest.thumbnailsReady (restAssets desktop))
      state <- readIORef (sessionRef desktop)
      record "rest starts with procedural water" (restScene state == TidalLight)
      apply desktop ShowRestSettings
      advance
      pure False
    11 | elapsed > 11.9 -> do
      screenshot (folder </> "rest-settings.png")
      gallery <- readIORef (sessionRef desktop)
      let cardsWork (width, height) = all (\(scene, Rectangle x y rw rh) -> View.hitTest width height gallery (x + rw / 2, y + rh / 2) == Just (View.Intent (ChooseRestScene scene))) (View.galleryCards width height)
      record "all eight gallery thumbnails select their scene at compact and desktop sizes" (all cardsWork [(760, 500), (1920, 1080), (2562, 1442)])
      record "gallery preview advances without entering rest" (viewingMode gallery == Working && restSeconds gallery > 0)
      apply desktop ToggleWindow
      modifyIORef' (sessionRef desktop) (\s -> s {idleSeconds = 10})
      state <- readIORef (sessionRef desktop)
      w <- R.getScreenWidth
      h <- R.getScreenHeight
      let firstClick = View.hitTest w h state (fromIntegral (w - 640) / 2 + 40, fromIntegral h - 110)
      record "a first click cannot activate a hidden transport control" (firstClick == Just (View.Intent ToggleRest))
      forM_ firstClick $ \action -> case action of View.Intent event -> apply desktop event; View.Quit -> pure ()
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    12 | elapsed > 12.4 -> do
      screenshot (folder </> "rest-entry-motion-a.png")
      apply desktop Activity
      state <- readIORef (sessionRef desktop)
      record "rest hides controls even after mouse activity" (viewingMode state == Resting && not (controlsVisible state))
      writeIORef pausePosition (Audio.position sound)
      advance
      pure False
    13 | elapsed > 13.4 -> do
      screenshot (folder </> "rest-entry-motion-b.png")
      before <- readIORef pausePosition
      record "music clock continues through rest and motion" (Audio.audible sound && Audio.position sound > before + 0.3)
      beforeRest <- readIORef (sessionRef desktop)
      beforePrefs <- readIORef (preferencesRef desktop)
      mapM_ (apply desktop) ([TogglePlayback, NextTrack, ChangeVolume 0.2, ToggleWindow, NextScreen, ToggleSettings, ShowRestSettings, ShowDisplaySettings, OpenMusic, UsePresets, OpenPicture, BrowsePicture NextPicture, RemovePicture, CyclePictureShade, ToggleLanguage, ToggleRestMotion, CycleRestShade] ++ map ChooseRestScene [minBound .. maxBound])
      afterRest <- readIORef (sessionRef desktop)
      afterPrefs <- readIORef (preferencesRef desktop)
      record "rest rejects settings, transport, volume, file and scene actions" (beforeRest == afterRest && beforePrefs == afterPrefs)
      apply desktop LeaveRest
      apply desktop ToggleRestMotion
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    14 | elapsed > 13.9 -> do
      screenshot (folder </> "rest-entry-still-a.png")
      advance
      pure False
    15 | elapsed > 14.5 -> do
      screenshot (folder </> "rest-entry-still-b.png")
      apply desktop LeaveRest
      apply desktop (ChooseRestScene Shallows)
      apply desktop ToggleRestMotion
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    16 | elapsed > 15.1 -> do
      screenshot (folder </> "rest-alternate-a.png")
      advance
      pure False
    17 | elapsed > 16.1 -> do
      screenshot (folder </> "rest-alternate-b.png")
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "rest preferences persist independently of the working picture" (warning == Nothing && reloaded == prefs && savedRestScene reloaded == Shallows && savedRestMotion reloaded == Gentle && savedBackground reloaded == Picture && savedPictureShade reloaded == Soft)
      apply desktop Escape
      state <- readIORef (sessionRef desktop)
      record "Esc restores the working background without resizing" (viewingMode state == Working && windowMode state == Fullscreen && background state == Picture && pictureShade state == Soft)
      apply desktop ToggleRest
      resting <- readIORef (sessionRef desktop)
      let returnClick = View.hitTest 760 500 resting (400, 200)
      record "click anywhere leaves rest" (returnClick == Just (View.Intent LeaveRest))
      -- The same event is used by the native focused -> unfocused edge.
      apply desktop LeaveRest
      returned <- readIORef (sessionRef desktop)
      record "LeaveRest used on focus loss preserves playback and display mode" (viewingMode returned == Working && transport returned == Playing && windowMode returned == Fullscreen)
      advance
      pure False
    18 | elapsed > 16.7 -> do
      screenshot (folder </> "rest-return.png")
      record "texture-free procedural shader compiled" (Rest.proceduralReady (restAssets desktop))
      apply desktop (ChooseRestScene TidalLight)
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    19 | elapsed > 17.3 -> do
      screenshot (folder </> "procedural-water-a.png")
      writeIORef pausePosition (Audio.position sound)
      advance
      pure False
    20 | elapsed > 18.3 -> do
      screenshot (folder </> "procedural-water-b.png")
      before <- readIORef pausePosition
      record "music advances during procedural rendering" (Audio.audible sound && Audio.position sound > before + 0.3)
      apply desktop LeaveRest
      apply desktop ToggleRestMotion
      apply desktop ToggleRest
      state <- readIORef (sessionRef desktop)
      writeIORef pausePosition (restSeconds state)
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    21 | elapsed > 18.9 -> do
      screenshot (folder </> "procedural-still-a.png")
      -- The frozen procedural frame must be identical over a different working
      -- background; this catches accidental transparency/image dependence.
      apply desktop LeaveRest
      apply desktop (ChooseBackground Black)
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    22 | elapsed > 19.5 -> do
      screenshot (folder </> "procedural-still-b.png")
      before <- readIORef pausePosition
      state <- readIORef (sessionRef desktop)
      record "procedural still mode holds scene time while music plays" (restSeconds state == before && transport state == Playing)
      apply desktop LeaveRest
      apply desktop (ChooseBackground Picture)
      apply desktop (ChooseRestScene LightVeil)
      apply desktop ToggleRestMotion
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    23 | elapsed > 20.2 -> do
      screenshot (folder </> "procedural-veil-a.png")
      advance
      pure False
    24 | elapsed > 21.2 -> do
      screenshot (folder </> "procedural-veil-b.png")
      apply desktop LeaveRest
      apply desktop (ChooseRestScene StarryNight)
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    25 | elapsed > 21.8 -> do
      screenshot (folder </> "procedural-stars-a.png")
      advance
      pure False
    26 | elapsed > 22.8 -> do
      screenshot (folder </> "procedural-stars-b.png")
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "procedural scene preference survives settings reload" (warning == Nothing && reloaded == prefs && savedRestScene reloaded == StarryNight && savedBackground reloaded == Picture)
      apply desktop Escape
      advance
      pure False
    p | p >= 27 && p <= 41 -> do
      let index = (p - 27) `div` 3
          (choice, name) = [(AmberSea, "amber"), (Shallows, "shallows"), (CloudSea, "clouds"), (Aurora, "aurora"), (InkFlow, "ink")] !! index
      if (p - 27) `mod` 3 == 0
        then do
          apply desktop LeaveRest
          apply desktop ShowRestSettings
          state <- readIORef (sessionRef desktop)
          w <- R.getScreenWidth
          h <- R.getScreenHeight
          forM_ (lookup choice (View.galleryCards w h)) $ \(Rectangle x y rw rh) ->
            forM_ (View.hitTest w h state (x + rw / 2, y + rh / 2)) $ \action -> case action of View.Intent event -> apply desktop event; View.Quit -> pure ()
          selected <- readIORef (sessionRef desktop)
          record ("gallery selection works: " ++ name) (restScene selected == choice)
          apply desktop ToggleRest
          modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
        else screenshot (folder </> ("procedural-" ++ name ++ (if (p - 27) `mod` 3 == 1 then "-a.png" else "-b.png")))
      advance
      pure False
    42 -> do
      apply desktop LeaveRest
      apply desktop ShowRestSettings
      advance
      pure False
    43 -> do
      screenshot (folder </> "gallery-fullscreen.png")
      apply desktop ToggleWindow
      apply desktop ShowRestSettings
      advance
      pure False
    44 -> do
      screenshot (folder </> "gallery-compact.png")
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "new scene preference persists and startup returns to work" (warning == Nothing && reloaded == prefs && savedRestScene reloaded == InkFlow && viewingMode (restoreSession reloaded) == Working)
      advance
      pure False
    45 -> do
      results <- reverse <$> readIORef checks
      frameTimes <- readIORef (checkFrameTimes desktop)
      let timings scene =
            let samples = sort [ms | (choice, ms) <- frameTimes, choice == scene]
                percentile fraction = case drop (floor (fraction * fromIntegral (max 0 (length samples - 1)))) samples of
                  value : _ -> Just value
                  [] -> Nothing
             in object
                  [ "scene" .= show scene,
                    "samples" .= length samples,
                    "medianMs" .= percentile (0.5 :: Double),
                    "p95Ms" .= percentile (0.95 :: Double)
                  ]
      BL.writeFile
        (folder </> "native-check.json")
        ( encode
            ( object
                [ "passed" .= all snd results,
                  "checks" .= [object ["name" .= name, "passed" .= ok] | (name, ok) <- results],
                  "renderTimingsIncludingCaptures" .= map timings [TidalLight, LightVeil, StarryNight, AmberSea, Shallows, CloudSea, Aurora, InkFlow]
                ]
            )
        )
      advance
      pure True
    _ -> pure False

-- Exercise font ownership across replacement and failed-library recovery.
checkFontRestart :: WindowResources -> Desktop -> IO Bool
checkFontRestart window desktop = do
  failed <- tryJust (\failure -> case failure of Audio.NoPlayableTracks -> Just (); _ -> Nothing) $
    withSessionFont window ["Before"] $ \ref -> do
      replacement <- View.loadTypeface window ["After 置換"]
      previousFont <- atomicModifyIORef' ref (\old -> (replacement, old))
      View.unloadTypeface window previousFont
      throwIO Audio.NoPlayableTracks
  case failed of
    Right () -> pure False
    Left () -> withSessionFont window ["Fallback 復帰"] $ \ref -> do
      font <- readIORef ref
      w <- R.getScreenWidth
      h <- R.getScreenHeight
      R.beginDrawing
      View.render font (restAssets desktop) w h ((initialSession defaultVolume) {windowMode = Compact}) Nothing "" "Fallback 復帰" Nothing (-1, -1)
      R.endDrawing
      pure True
