{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Desktop (runDesktop) where

import Control.Concurrent (forkOS, threadDelay)
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
import Raylib.Core.Textures (exportImageToMemory, loadImageFromScreen)
import Raylib.Internal (WindowResources)
import Raylib.Types
import Stillroom.Domain
import Stillroom.Runtime.Audio qualified as Audio
import Stillroom.Runtime.Native qualified as Native
import Stillroom.Runtime.Render qualified as View
import Stillroom.Runtime.Storage
import Stillroom.Session
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data Desktop = Desktop
  { sessionRef :: !(IORef Session),
    preferencesRef :: !(IORef Preferences),
    noticeRef :: !(IORef (Maybe String)),
    pickerBusy :: !(IORef Bool),
    pickerResult :: !(MVar (Either String (Maybe FilePath))),
    fontRef :: !(IORef Font),
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
  R.setConfigFlags ([WindowAlwaysRun, WindowResizable] ++ [WindowHidden | isJust checkDirectory])
  bracket (R.initWindow 760 500 "Stillroom") (R.closeWindow . Just) $ \window -> do
    R.setExitKey KeyNull
    R.setWindowMinSize 700 490
    -- A normal native title bar is available in compact mode; the full-screen
    -- mode uses an undecorated, non-topmost window on exactly one display.
    font <- View.loadTypeface window (map trackTitle (NE.toList tracks))
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
              <*> pure audio
              <*> pure statePath
              <*> pure (isJust checkDirectory)
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
        Fullscreen -> (Native.monitorWidth chosen, Native.monitorHeight chosen, Native.monitorX chosen, Native.monitorY chosen)
        Compact ->
          ( 760,
            500,
            Native.monitorX chosen + (Native.monitorWidth chosen - 760) `div` 2,
            Native.monitorY chosen + (Native.monitorHeight chosen - 500) `div` 2
          )
  if windowMode s == Fullscreen then R.setWindowState [WindowUndecorated] else R.clearWindowState [WindowUndecorated]
  R.setWindowSize w h
  R.setWindowPosition x y
  handle <- R.getWindowHandle
  when (windowMode s == Fullscreen) (Native.positionWindow handle x y w h)
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
      modifyIORef' (preferencesRef desktop) (\p -> p {savedFolder = Nothing})
      writeIORef (noticeRef desktop) Nothing
    PickMusicFolder -> do
      busy <- readIORef (pickerBusy desktop)
      unless busy $ do
        writeIORef (pickerBusy desktop) True
        handle <- R.getWindowHandle
        _ <- forkOS $ do
          result <- Native.pickFolder handle
          putMVar (pickerResult desktop) result
        pure ()

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
                  newFont <- View.loadTypeface window (map trackTitle (NE.toList tracks))
                  oldFont <- atomicModifyIORef' (fontRef desktop) (\old -> (newFont, old))
                  unloadFont oldFont window
                  modifyIORef' (preferencesRef desktop) (\p -> p {savedFolder = Just folder})
                  writeIORef (noticeRef desktop) Nothing
                  save desktop
          apply desktop Activity
  Audio.pump (engine desktop)
  finished <- case checkDirectory of
    Nothing -> pure False
    Just directory -> checkStep desktop directory checkPhase checks pausePosition (now - began)
  state <- readIORef (sessionRef desktop)
  sound <- Audio.snapshot (engine desktop)
  font <- readIORef (fontRef desktop)
  notice <- readIORef (noticeRef desktop)
  let shown = controlsVisible state
  focused <- R.isWindowFocused
  if shown || not focused || not onScreen then R.showCursor else R.hideCursor
  R.beginDrawing
  View.render font w h state (Audio.title sound) (notice `orElse` Audio.audioWarning sound) (mx, my)
  R.endDrawing
  closed <- R.windowShouldClose
  if closed || click == Just View.Quit || finished
    then do
      save desktop
      R.showCursor
      Native.setPower Paused Compact
      results <- readIORef checks
      pure (all snd results)
    else do
      -- There is no animation. Thirty small polls per second keep audio
      -- streaming and controls responsive without a busy render loop.
      spent <- (subtract now) <$> R.getTime
      threadDelay (max 0 (round ((1 / 30 - spent) * 1000000)))
      loop window desktop checkDirectory checkPhase checks pausePosition began now (frame + 1)
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
      apply desktop (ChooseBackground Lake)
      screenshot (folder </> "controls.png")
      advance
      pure False
    1 | elapsed > 1.2 -> do
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
      Audio.seekForCheck (engine desktop) (Audio.duration sound - 4)
      advance
      pure False
    4 | elapsed > 3.9 -> do
      record "two decks crossfade" (Audio.crossfading sound)
      advance
      pure False
    5 | elapsed > 8 -> do
      record "automatic next track" (Audio.transitionCount sound >= 1)
      apply desktop ToggleWindow
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
