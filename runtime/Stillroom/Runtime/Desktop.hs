{-# LANGUAGE OverloadedStrings #-}

module Stillroom.Runtime.Desktop (runDesktop, runLocalReview) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.MVar
import Control.Exception hiding (handle)
import Control.Monad (foldM, forM_, unless, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.List (find, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word32, Word64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import GameIO.Audio qualified as GameAudio
import GameIO.Graphics qualified as GPU
import GameIO.Input qualified as Input
import GameIO.Platform qualified as Platform
import GameIO.Platform.Icon (withWindowIcon)

import Stillroom.Domain
import Stillroom.Interface qualified as UI
import Stillroom.Journey.Ride qualified as Ride
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.Assets (assetPath)
import Stillroom.Runtime.Audio.Remote qualified as Audio
import Stillroom.Runtime.Capture (screenshot)
import Stillroom.Runtime.Journey qualified as Journey
import Stillroom.Runtime.JourneyStorage qualified as JourneyStorage
import Stillroom.Runtime.Native qualified as Native
import Stillroom.Runtime.Pictures qualified as Pictures
import Stillroom.Runtime.Render qualified as View
import Stillroom.Runtime.Rest qualified as Rest
import Stillroom.Runtime.Storage
import Stillroom.Session
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Random (randomIO)

data Desktop = Desktop
  { desktopWindow :: !Platform.Window,
    desktopRenderer :: !GPU.Renderer,
    applicationInstance :: !(Maybe Native.ApplicationInstance),
    sessionRef :: !(IORef Session),
    preferencesRef :: !(IORef Preferences),
    noticeRef :: !(IORef (Maybe String)),
    pickerBusy :: !(IORef Bool),
    pickerGeneration :: !(IORef Integer),
    pickerResult :: !(MVar (Integer, Either String (Maybe FilePath))),
    fontRef :: !(IORef View.Typeface),
    fontTitlesRef :: !(IORef [String]),
    picturesRef :: !(IORef [Pictures.PictureEntry]),
    pictureRef :: !(IORef (Maybe (Pictures.PictureEntry, Pictures.PictureTexture))),
    pictureBusy :: !(IORef Bool),
    pictureGeneration :: !(IORef Integer),
    pictureResult :: !(MVar (Integer, Either Pictures.PictureError (Maybe (Pictures.PictureEntry, Pictures.PreparedPicture)))),
    restAssets :: !Rest.RestAssets,
    controlFocusRef :: !(IORef (Maybe UI.UIAction)),
    checkFrameTimes :: !(IORef [(RestScene, Double)]),
    checkSwitchStart :: !(IORef (Maybe (RestScene, Double))),
    checkSwitchTimes :: !(IORef [(RestScene, Double)]),
    checkAudioPhases :: !(IORef [(Int, Word64)]),
    engine :: !Audio.Engine,
    preferencesPath :: !FilePath,
    mutedCheck :: !Bool
  }

runDesktop :: Maybe Native.ApplicationInstance -> FilePath -> Maybe FilePath -> Maybe FilePath -> IO Bool
runDesktop instanceValue = runDesktopStarting instanceValue Playing "Stillroom"

runLocalReview :: FilePath -> IO Bool
runLocalReview statePath = runDesktopStarting Nothing Paused "Stillroom · Haskell版" statePath Nothing Nothing

runDesktopStarting :: Maybe Native.ApplicationInstance -> Transport -> String -> FilePath -> Maybe FilePath -> Maybe FilePath -> IO Bool
runDesktopStarting instanceValue initialTransport title statePath checkDirectory overrideFolder = do
  startupBegan <- getMonotonicTimeNSec
  languageValue <- Native.systemLanguage
  (loaded, initialNotice) <- readPreferences statePath languageValue
  let preferences = loaded {savedFolder = overrideFolder <|> savedFolder loaded}
      prefs = if isJust checkDirectory then preferences {savedVolume = 0, savedWindow = Compact, savedLanguage = English} else preferences
  let builtins = presetTrack (savedMusicPreset prefs)
  tracksResult <- maybe (pure (Right builtins)) scanFolder (savedFolder prefs)
  let tracks = either (const builtins) id tracksResult
      notice = either (const (Just "Music folder unavailable; built-in music is playing.")) (const initialNotice) tracksResult
  Platform.withWindowVisibility (not (isJust checkDirectory)) (Platform.WindowConfig title 760 500) $ \window ->
    flip finally (Native.setPower Paused Compact) $ do
      Platform.setWindowMinimumSize window 700 500
      icon <- assetPath ("assets" </> "icon" </> "stillroom.ico")
      hasIcon <- doesFileExist icon
      let iconScope = if hasIcon then withWindowIcon window icon else id
      iconScope $ GPU.withRenderer window $ \renderer -> do
        rendererReady <- getMonotonicTimeNSec
        pictures <- Pictures.discoverPictures (takeDirectory statePath </> "backgrounds")
        Rest.withRestAssets window renderer $ \rests -> do
          scenesReady <- getMonotonicTimeNSec
          JourneyStorage.readJourney (takeDirectory statePath </> "journey.json") >>= Journey.restoreModel (Rest.journey rests)
          seed <- round . (* 1000000) <$> getPOSIXTime
          let start chosen startupNotice =
                withSessionFont renderer (map trackTitle (NE.toList chosen) ++ map Pictures.pictureName pictures) $ \liveFont -> do
                  fontReady <- getMonotonicTimeNSec
                  bracket (newIORef Nothing) (releasePicture renderer) $ \livePicture ->
                    Audio.withEngine initialTransport chosen seed (loudness (restoreSession prefs)) $ \audio -> do
                      workerReady <- getMonotonicTimeNSec
                      desktop <-
                        Desktop window renderer instanceValue
                          <$> newIORef ((restoreSession prefs) {transport = initialTransport, builtInMusic = all isBuiltIn (NE.toList chosen)})
                          <*> newIORef prefs
                          <*> newIORef startupNotice
                          <*> newIORef False
                          <*> newIORef 0
                          <*> newEmptyMVar
                          <*> pure liveFont
                          <*> newIORef (map trackTitle (NE.toList chosen))
                          <*> newIORef pictures
                          <*> pure livePicture
                          <*> newIORef False
                          <*> newIORef 0
                          <*> newEmptyMVar
                          <*> pure rests
                          <*> newIORef Nothing
                          <*> newIORef []
                          <*> newIORef Nothing
                          <*> newIORef []
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
                          Right image -> mask_ $ do
                            texture <- Pictures.uploadPicture renderer image
                            writeIORef livePicture (Just (entry, texture))
                            modifyIORef' (preferencesRef desktop) (\p -> p {savedPicture = Just (Pictures.pictureKey entry)})
                      place desktop False
                      now <- monotonicSeconds
                      phase <- newIORef (0 :: Int, now)
                      checks <- newIORef ([] :: [(String, Bool)])
                      pausePosition <- newIORef 0
                      forM_ checkDirectory $ \folder -> do
                        createDirectoryIfMissing True folder
                        startupFinished <- getMonotonicTimeNSec
                        initialSound <- Audio.snapshot audio
                        let sinceStart time = fromIntegral (time - startupBegan) / 1e6 :: Double
                        BL.writeFile (folder </> "startup.json") $ encode $ object
                          [ "rendererReadyMs" .= sinceStart rendererReady,
                            "scenesReadyMs" .= sinceStart scenesReady,
                            "fontReadyMs" .= sinceStart fontReady,
                            "audioWorkerReadyMs" .= sinceStart workerReady,
                            "readyBeforeFirstLoopMs" .= sinceStart startupFinished,
                            "audioClockBeforeFirstLoopSeconds" .= Audio.position initialSound,
                            "scope" .= ("App entry to ready loop, with Vulkan resources, font and audio worker. The check is muted." :: String)
                          ]
                      loop desktop checkDirectory phase checks pausePosition Input.initialInput now now
          result <- tryJust onlyLibraryFailure (start tracks notice)
          case result of
            Right success -> pure success
            Left _ -> start builtins (Just "Music files could not be decoded; built-in music is playing.")
  where
    isBuiltIn track = case trackSource track of LocalFile _ -> False; _ -> True
    onlyLibraryFailure Audio.NoPlayableTracks = Just ()
    onlyLibraryFailure _ = Nothing
    releasePicture renderer ref = readIORef ref >>= mapM_ (Pictures.unloadPicture renderer . snd)

withSessionFont :: GPU.Renderer -> [String] -> (IORef View.Typeface -> IO a) -> IO a
withSessionFont renderer titles =
  bracket
    (View.loadTypeface renderer titles >>= newIORef)
    (\ref -> readIORef ref >>= View.unloadTypeface renderer)

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
  Native.placeWindow (desktopWindow desktop) chosen (windowMode s)
  writeIORef (preferencesRef desktop) prefs {savedScreen = Native.monitorName chosen}
  Native.setPower (transport s) (windowMode s)
  where
    safeFirst [] = Nothing
    safeFirst (x : _) = Just x


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

saveJourney :: Desktop -> IO ()
saveJourney desktop = do
  result <- try $ Journey.model (Rest.journey (restAssets desktop)) >>= JourneyStorage.writeJourney (takeDirectory (preferencesPath desktop) </> "journey.json")
  case result of
    Left (_ :: IOException) -> writeIORef (noticeRef desktop) (Just "Journey could not be saved.")
    Right () -> pure ()

apply :: Desktop -> Event -> IO ()
apply desktop event = do
  old <- readIORef (sessionRef desktop)
  let (updated, effects) = step event old
  writeIORef (sessionRef desktop) updated
  when (viewingMode old == Resting && viewingMode updated == Working && restScene old == RailJourney) $ do
    Journey.suspend (Rest.journey (restAssets desktop))
    saveJourney desktop
  when
    (viewingMode old == Working && viewingMode updated == Resting && restScene updated == RailJourney)
    (Journey.enter (Rest.journey (restAssets desktop)))
  when
    (restScene old == RailJourney && restScene updated /= RailJourney)
    (Journey.unload (Rest.journey (restAssets desktop)))
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
    StartNewJourney -> do
      randomIO >>= Journey.replace (Rest.journey (restAssets desktop)) . World.seed
      saveJourney desktop
    SelectPresets -> do
      seed <- round . (* 1000000) <$> getPOSIXTime
      Audio.replaceLibrary (engine desktop) (presetTrack (musicPreset updated)) seed
      modifyIORef' (preferencesRef desktop) useBuiltInMusic
      writeIORef (noticeRef desktop) Nothing
    PickMusicFolder -> do
      busy <- (||) <$> readIORef (pickerBusy desktop) <*> readIORef (pictureBusy desktop)
      unless busy $ do
        writeIORef (pickerBusy desktop) True
        handle <- Platform.nativeWindowHandle (desktopWindow desktop)
        prefs <- readIORef (preferencesRef desktop)
        generation <- readIORef (pickerGeneration desktop)
        _ <- forkOS $ do
          result <- Native.pickFolder handle (savedFolder prefs <|> previousFolder prefs)
          putMVar (pickerResult desktop) (generation, result)
        pure ()
    PickPicture -> do
      busy <- readIORef (pickerBusy desktop)
      unless busy $ do
        handle <- Platform.nativeWindowHandle (desktopWindow desktop)
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

receivePicture :: Desktop -> IO ()
receivePicture desktop = do
  completed <- tryTakeMVar (pictureResult desktop)
  forM_ completed $ \(generation, result) -> do
    writeIORef (pictureBusy desktop) False
    current <- readIORef (pictureGeneration desktop)
    when (generation == current) (accept result)
  where
    accept result = do
      state <- readIORef (sessionRef desktop)
      case result of
        Left issue -> writeIORef (noticeRef desktop) (Just (Pictures.pictureErrorText (language state) issue))
        Right Nothing -> pure ()
        Right (Just (entry, prepared)) -> do
          loaded <- try (Pictures.uploadPicture (desktopRenderer desktop) prepared)
          case loaded of
            Left issue -> writeIORef (noticeRef desktop) (Just (Pictures.pictureErrorText (language state) issue))
            Right texture -> do
              previous <- atomicModifyIORef' (pictureRef desktop) (\old -> (Just (entry, texture), old))
              forM_ previous (\(_, oldTexture) -> Pictures.unloadPicture (desktopRenderer desktop) oldTexture)
              entries <- Pictures.discoverPictures (pictureDirectory desktop)
              oldEntries <- readIORef (picturesRef desktop)
              writeIORef (picturesRef desktop) entries
              when (entries /= oldEntries) $ do
                titles <- readIORef (fontTitlesRef desktop)
                newFont <- View.loadTypeface (desktopRenderer desktop) (titles ++ map Pictures.pictureName entries)
                oldFont <- atomicModifyIORef' (fontRef desktop) (\old -> (newFont, old))
                View.unloadTypeface (desktopRenderer desktop) oldFont
              modifyIORef' (preferencesRef desktop) (\p -> p {savedPicture = Just (Pictures.pictureKey entry)})
              writeIORef (noticeRef desktop) Nothing
              apply desktop (ChooseBackground Picture)
      apply desktop Activity

receiveMusic :: Desktop -> IO ()
receiveMusic desktop = do
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
                newFont <- View.loadTypeface (desktopRenderer desktop) (titles ++ map Pictures.pictureName entries)
                oldFont <- atomicModifyIORef' (fontRef desktop) (\old -> (newFont, old))
                View.unloadTypeface (desktopRenderer desktop) oldFont
                modifyIORef' (preferencesRef desktop) (useMusicFolder folder)
                modifyIORef' (sessionRef desktop) (\s -> s {builtInMusic = False})
                writeIORef (noticeRef desktop) Nothing
                save desktop
        apply desktop Activity
  where
    synchronous e = case fromException e :: Maybe AsyncException of Just _ -> Nothing; Nothing -> Just e

desktopKey :: Session -> Bool -> Word32 -> Maybe Event
desktopKey s focused key
  | viewingMode s == Resting = if key == 0x1b || key == 0x52 then Just LeaveRest else Nothing
  | otherwise = lookup key bindings
  where
    restControls = settingsOpen s && settingsPage s == RestSettings
    bindings =
      [(0x20, TogglePlayback) | not focused]
        ++ [ (0x52, ToggleRest),
             (0x4e, NextTrack),
             (0x7a, ToggleWindow),
             (0x1b, Escape),
             (0x4d, NextScreen),
             (0x4f, OpenMusic),
             (0x50, UsePresets),
             (0x53, ToggleSettings),
             (0x49, OpenPicture),
             (0xdb, BrowsePicture PreviousPicture),
             (0xdd, BrowsePicture NextPicture),
             (0x42, if restControls then CycleRestShade else CyclePictureShade),
             (0xbb, ChangeVolume 0.025),
             (0xbd, ChangeVolume (-0.025))
           ]
        ++ [(0x41, ToggleRestMotion) | restControls]

-- Apply Windows edges in sequence. A click resolves against the UI visible
-- before polling, even when a movement in the same poll wakes the controls.
desktopInput :: Desktop -> (Int, Int) -> Session -> Input.InputState -> [Input.TimedWindowEvent] -> IO (Input.InputState, [Input.InputChange], Bool)
desktopInput desktop (width, height) visible = \input events -> do
  (end, reversed, quit) <- foldM consume (input, [], False) events
  pure (end, reverse reversed, quit)
  where
    activate (UI.Intent event) = apply desktop event >> pure False
    activate UI.Quit = pure True
    consume (input, reversed, quit) event = do
      let (end, changes) = Input.reduceInput input [event]
      closed <- foldM (\already (_, change) -> if already then pure True else handleChange input end change) quit changes
      pure (end, foldl (\acc (_, change) -> change : acc) reversed changes, closed)
    handleChange _ _ Input.InputClose = pure True
    handleChange _ _ (Input.InputFocus False) = do
      apply desktop LeaveRest
      writeIORef (controlFocusRef desktop) Nothing
      pure False
    handleChange old _ (Input.InputPointer x y) = do
      when (inside x y && Input.pointerPosition old /= Just (x, y)) $ do
        apply desktop Activity
        writeIORef (controlFocusRef desktop) Nothing
      pure False
    handleChange _ _ (Input.InputButton Input.PointerLeft True x y) = do
      writeIORef (controlFocusRef desktop) Nothing
      if inside x y
        then case UI.hitTest width height visible (fromIntegral x, fromIntegral y) of
          Just action -> activate action
          Nothing -> apply desktop Activity >> pure False
        else pure False
    handleChange _ input (Input.InputKey Input.KeyPressed message) = do
      state <- readIORef (sessionRef desktop)
      let key = Input.keyVirtual message
      if key == 0x09 && viewingMode state == Working
        then do
          apply desktop Activity
          awake <- readIORef (sessionRef desktop)
          let backwards = any (Input.virtualKeyHeld input) [0x10, 0xa0, 0xa1]
          modifyIORef' (controlFocusRef desktop) (UI.nextFocus backwards (UI.controls width height awake))
          pure False
        else do
          focused <- readIORef (controlFocusRef desktop)
          let active = focused >>= \action -> if any ((== action) . UI.action) (UI.controls width height state) then Just action else Nothing
          mapM_ (apply desktop) (desktopKey state (isJust active) key)
          case active of
            Just action | key == 0x0d || key == 0x20 -> activate action
            _ -> pure False
    handleChange _ _ _ = pure False
    inside x y = x >= 0 && x < width && y >= 0 && y < height

monotonicSeconds :: IO Double
monotonicSeconds = (/ 1e9) . fromIntegral <$> getMonotonicTimeNSec

loop ::
  Desktop ->
  Maybe FilePath ->
  IORef (Int, Double) ->
  IORef [(String, Bool)] ->
  IORef Double ->
  Input.InputState ->
  Double ->
  Double ->
  IO Bool
loop desktop checkDirectory checkPhase checks pausePosition input began previous = do
  now <- monotonicSeconds
  let window = desktopWindow desktop
      elapsed = either (const zeroSeconds) id (seconds (max 0 (now - previous)))
  when (mutedCheck desktop) $ do
    previousState <- readIORef (sessionRef desktop)
    when (viewingMode previousState == Resting && restMotion previousState == Gentle) $
      modifyIORef' (checkFrameTimes desktop) ((restScene previousState, secondsValue elapsed * 1000) :)
  apply desktop (Elapse elapsed)
  forM_ (applicationInstance desktop) $ \instanceValue -> do
    requested <- Native.activationRequested instanceValue
    when requested (Native.activateWindow window >> apply desktop Activity)
  events <- Platform.pollWindowEventsStamped window
  before <- readIORef (sessionRef desktop)
  extent <- Platform.windowFramebufferSize window
  (currentInput, changes, closed) <- desktopInput desktop extent before input events
  receiveMusic desktop
  receivePicture desktop
  finished <- case checkDirectory of
    Nothing -> pure False
    Just directory -> checkStep desktop directory checkPhase checks pausePosition (now - began)
  if closed || finished
    then do
      save desktop
      Journey.suspend (Rest.journey (restAssets desktop))
      saveJourney desktop
      Platform.setPointerVisible window True
      all snd <$> readIORef checks
    else do
      state <- readIORef (sessionRef desktop)
      let journey = Rest.journey (restAssets desktop)
          journeyRest = viewingMode state == Resting && restScene state == RailJourney
          focused = Input.hasFocus currentInput
          clicked = any isClick changes
          (dx, dy) = foldl pointerDelta (0, 0) changes
      when (viewingMode state == Resting) (writeIORef (controlFocusRef desktop) Nothing)
      when (viewingMode before == Resting && journeyRest && (focused || mutedCheck desktop)) $
        Journey.update journey (secondsValue elapsed) clicked (fromIntegral dx, fromIntegral dy) currentInput changes
      ambience <- if journeyRest && journeySound state then Just <$> Journey.soundscape journey else pure Nothing
      Audio.setAmbience (engine desktop) ambience
      pointer <- Native.clientPointer window
      captured <- Journey.isCaptured journey
      let hide = not (controlsVisible state) && not journeyRest && focused && isJust pointer
      unless captured (Platform.setPointerVisible window (not hide))
      let mouse = maybe (-1, -1) (\(x, y) -> (fromIntegral x, fromIntegral y)) pointer
      (status, _) <- renderDesktop desktop False mouse
      when (status == GPU.FramePresented && mutedCheck desktop) $ do
        pending <- atomicModifyIORef' (checkSwitchStart desktop) (\value -> (Nothing, value))
        forM_ pending $ \(scene, startedAt) -> do
          presentedAt <- monotonicSeconds
          modifyIORef' (checkSwitchTimes desktop) ((scene, (presentedAt - startedAt) * 1000) :)
      let preview = settingsOpen state && settingsPage state == RestSettings && sceneCollection (restScene state) /= Atmospheres
          interval = if status == GPU.FrameMinimized || viewingMode state == Resting || preview then 1 / 30 else 1 / 60
      done <- monotonicSeconds
      threadDelay (max 0 (ceiling ((now + interval - done) * 1e6)))
      loop desktop checkDirectory checkPhase checks pausePosition currentInput began now
  where
    isClick (Input.InputButton Input.PointerLeft True _ _) = True
    isClick _ = False
    pointerDelta :: (Int, Int) -> Input.InputChange -> (Int, Int)
    pointerDelta (x, y) (Input.InputDelta dx dy) = (x + dx, y + dy)
    pointerDelta position _ = position

renderDesktop :: Desktop -> Bool -> (Float, Float) -> IO (GPU.FrameStatus, Maybe GPU.FramePixels)
renderDesktop desktop readback mouse = do
  (width, height) <- Platform.windowFramebufferSize (desktopWindow desktop)
  if width == 0 || height == 0 then pure (GPU.FrameMinimized, Nothing) else do
    state <- readIORef (sessionRef desktop)
    focus <- readIORef (controlFocusRef desktop)
    sound <- Audio.snapshot (engine desktop)
    font <- readIORef (fontRef desktop)
    notice <- readIORef (noticeRef desktop)
    picture <- readIORef (pictureRef desktop)
    entries <- readIORef (picturesRef desktop)

    let caption = case picture of
          Nothing -> if language state == Japanese then "画像を追加してください" else "Add a background image"
          Just (entry, _) ->
            let position = length (takeWhile ((/= Pictures.pictureKey entry) . Pictures.pictureKey) entries) + 1
                name = case Pictures.pictureKey entry of
                  "bundled:open-water.png" -> if language state == Japanese then "ひらけた湖" else "Open water"
                  "bundled:first-light.png" -> if language state == Japanese then "朝霧の森" else "First light"
                  "bundled:honey-mist.png" -> if language state == Japanese then "暖かな水面" else "Honey mist"
                  "bundled:blue-distance.png" -> if language state == Japanese then "青い稜線" else "Blue distance"
                  "bundled:01-mist-lake.png" -> if language state == Japanese then "霧の湖" else "Mist lake"
                  "bundled:02-quiet-forest.png" -> if language state == Japanese then "静かな森" else "Quiet forest"
                  "bundled:03-soft-dunes.png" -> if language state == Japanese then "砂丘" else "Soft dunes"
                  "bundled:04-night-field.png" -> if language state == Japanese then "夜の抽象画" else "Night field"
                  "bundled:05-afternoon-lake.png" -> if language state == Japanese then "午後の湖" else "Afternoon lake"
                  "bundled:06-woodland-pond.png" -> if language state == Japanese then "森の池" else "Woodland pond"
                  _ -> Pictures.pictureName entry
             in show position ++ " / " ++ show (length entries) ++ "   " ++ name
    View.renderWithFocus (desktopRenderer desktop) readback font (restAssets desktop) width height state
      (snd <$> picture) caption
      (if builtInMusic state then presetLabel (language state) (musicPreset state) else Audio.title sound)
      (notice <|> Audio.audioWarning sound) mouse focus


-- Inject completed work deterministically after a newer selection. This uses
-- the real completion boundary without relying on filesystem/decode timing.
checkLateSelections :: Desktop -> FilePath -> IO [(String, Bool)]
checkLateSelections desktop folder = do
  entries <- readIORef (picturesRef desktop)
  selectedPicture <- readIORef (pictureRef desktop)
  let snapshot = do
        state <- readIORef (sessionRef desktop)
        prefs <- readIORef (preferencesRef desktop)
        picture <- readIORef (pictureRef desktop)
        notice <- readIORef (noticeRef desktop)
        pure (state, prefs, fmap fst picture, notice)
      latePicture event = case filter (\entry -> Just entry /= fmap fst selectedPicture) entries of
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
              receivePicture desktop
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
  receiveMusic desktop
  after <- snapshot
  busy <- readIORef (pickerBusy desktop)
  apply desktop LeaveRest
  pure [("late image completion cannot overwrite a newer background", backgroundHeld), ("late image completion cannot change rest or its working picture", restHeld), ("late music picker completion cannot change rest", before == after && not busy)]

checkStep :: Desktop -> FilePath -> IORef (Int, Double) -> IORef [(String, Bool)] -> IORef Double -> Double -> IO Bool
checkStep desktop folder phaseRef checks pausePosition elapsed = do
  (phase, lastAdvanced) <- readIORef phaseRef
  now <- monotonicSeconds
  sound <- Audio.snapshot (engine desktop)
  let record name success = modifyIORef' checks ((name, success) :)
      capture file = screenshot file (renderDesktop desktop True (-1, -1))
      window = desktopWindow desktop
      advance = do
        (_, stats) <- Audio.deviceForCheck (engine desktop)
        modifyIORef' (checkAudioPhases desktop) ((phase, GameAudio.streamStarvedFrames stats) :)
        finishedAt <- monotonicSeconds
        writeIORef phaseRef (phase + 1, finishedAt)
      -- PNG encoding can take longer than an absolute phase interval. Wait
      -- after it finishes so at least several fresh frames reach the buffers.
      settled = now - lastAdvanced > 0.4
  case (if settled then phase else -1) of
    0 | elapsed > 0.7 -> do
      record "autoplay clock advances" (Audio.audible sound && Audio.position sound > 0.1)
      entries <- readIORef (picturesRef desktop)
      record "ten bundled pictures are available" (length (filter ((== Pictures.Bundled) . Pictures.pictureOrigin) entries) == 10)
      current <- readIORef (pictureRef desktop)
      record "startup uploads a valid picture texture" (maybe False (\(_, Pictures.PictureTexture width height _) -> width > 0 && height > 0) current)
      forM_ current $ \(entry, _) -> do
        let source = folder </> "登録の検査.PNG"
        BS.readFile (Pictures.picturePath entry) >>= BS.writeFile source
        requestPicture desktop (fmap Just <$> Pictures.importPicture (pictureDirectory desktop) source)
      advance
      pure False
    1 | elapsed > 1.5 -> do
      current <- readIORef (pictureRef desktop)
      record "Unicode image import copies and displays the image" (maybe False (\(entry, _) -> Pictures.pictureOrigin entry == Pictures.Imported && Pictures.picturePath entry /= folder </> "登録の検査.PNG") current)
      capture (folder </> "controls.png")
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
      -- Continuous music has no pretend end. Exercise the finite playlist
      -- contract with explicitly finite fixtures, then return to built-ins.
      Audio.replaceLibrary (engine desktop)
        (NE.map (\p -> Track (TrackId (fromEnum p)) (presetName p) (Procedural p)) allPresets) 413091
      Audio.seekForCheck (engine desktop) 176
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
      apply desktop UsePresets
      apply desktop ToggleWindow
      prefs <- readIORef (preferencesRef desktop)
      screens <- Native.monitors
      matches <- maybe (pure False) (Native.borderlessMatches window) (find ((== savedScreen prefs) . Native.monitorName) screens)
      record "borderless surface clips exactly to the chosen display" matches
      modifyIORef' (sessionRef desktop) (\s -> s {idleSeconds = 10})
      state <- readIORef (sessionRef desktop)
      record "fullscreen hides idle controls" (not (controlsVisible state))
      advance
      pure False
    6 | elapsed > 8.6 -> do
      capture (folder </> "quiet.png")
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
      Native.isUnclipped window >>= record "compact mode removes the fullscreen clip"
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
      record "next image uploads the next bundled picture" (maybe False ((== "bundled:first-light.png") . Pictures.pictureKey . fst) current)
      apply desktop (ChoosePictureShade Original)
      apply desktop CyclePictureShade
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "image choice and brightness survive settings reload" (warning == Nothing && reloaded == prefs && savedBackground reloaded == Picture && savedPictureShade reloaded == Soft)
      apply desktop ShowDisplaySettings
      apply desktop ToggleLanguage
      advance
      pure False
    10 | elapsed > 11.4 -> do
      capture (folder </> "pictures-settings.png")
      checkFontRestart desktop folder >>= record "font replacement followed by a failed library can restart and draw"
      checkLateSelections desktop folder >>= mapM_ (uncurry record)
      record "all twelve rest thumbnails are preloaded" (Rest.thumbnailsReady (restAssets desktop))
      state <- readIORef (sessionRef desktop)
      record "rest starts with procedural water" (restScene state == TidalLight)
      apply desktop ShowRestSettings
      advance
      pure False
    11 | elapsed > 11.9 -> do
      capture (folder </> "rest-settings.png")
      gallery <- readIORef (sessionRef desktop)
      let cardsWork collection (width, height) = all (\(scene, UI.Bounds x y rw rh) -> UI.hitTest width height gallery {restCollection = collection} (x + rw / 2, y + rh / 2) == Just (UI.Intent (ChooseRestScene scene))) (UI.galleryCards width height collection)
      record "all twelve gallery thumbnails select their scene at compact and desktop sizes" (and [cardsWork collection size | collection <- [Journeys, Landscapes, Atmospheres], size <- [(700, 500), (1920, 1080), (2562, 1442)]])
      record "gallery preview advances without entering rest" (viewingMode gallery == Working && restSeconds gallery > 0)
      apply desktop ToggleWindow
      modifyIORef' (sessionRef desktop) (\s -> s {idleSeconds = 10})
      state <- readIORef (sessionRef desktop)
      (w, h) <- Platform.windowFramebufferSize (desktopWindow desktop)
      let firstClick = UI.hitTest w h state (fromIntegral (w - 640) / 2 + 40, fromIntegral h - 110)
      record "a first click cannot activate a hidden transport control" (firstClick == Just (UI.Intent ToggleRest))
      forM_ firstClick $ \action -> case action of UI.Intent event -> apply desktop event; UI.Quit -> pure ()
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    12 | elapsed > 12.4 -> do
      capture (folder </> "rest-entry-motion-a.png")
      apply desktop Activity
      state <- readIORef (sessionRef desktop)
      record "rest hides controls even after mouse activity" (viewingMode state == Resting && not (controlsVisible state))
      writeIORef pausePosition (Audio.position sound)
      advance
      pure False
    13 | elapsed > 13.4 -> do
      capture (folder </> "rest-entry-motion-b.png")
      before <- readIORef pausePosition
      record "music clock continues through rest and motion" (Audio.audible sound && Audio.position sound > before + 0.3)
      beforeRest <- readIORef (sessionRef desktop)
      beforePrefs <- readIORef (preferencesRef desktop)
      mapM_ (apply desktop) ([TogglePlayback, NextTrack, ChangeVolume 0.2, ToggleWindow, NextScreen, ToggleSettings, ShowRestSettings, ShowDisplaySettings, OpenMusic, UsePresets, ChooseMusicPreset Night, ToggleJourneySound, OpenPicture, BrowsePicture NextPicture, RemovePicture, CyclePictureShade, ToggleLanguage, ToggleRestMotion, CycleRestShade] ++ map ChooseRestScene [minBound .. maxBound])
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
      capture (folder </> "rest-entry-still-a.png")
      advance
      pure False
    15 | elapsed > 14.5 -> do
      capture (folder </> "rest-entry-still-b.png")
      apply desktop LeaveRest
      apply desktop (ChooseRestScene Shallows)
      apply desktop ToggleRestMotion
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    16 | elapsed > 15.1 -> do
      capture (folder </> "rest-alternate-a.png")
      advance
      pure False
    17 | elapsed > 16.1 -> do
      capture (folder </> "rest-alternate-b.png")
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "rest preferences persist independently of the working picture" (warning == Nothing && reloaded == prefs && savedRestScene reloaded == Shallows && savedRestMotion reloaded == Gentle && savedBackground reloaded == Picture && savedPictureShade reloaded == Soft)
      apply desktop Escape
      state <- readIORef (sessionRef desktop)
      record "Esc restores the working background without resizing" (viewingMode state == Working && windowMode state == Fullscreen && background state == Picture && pictureShade state == Soft)
      apply desktop ToggleRest
      resting <- readIORef (sessionRef desktop)
      let returnClick = UI.hitTest 760 500 resting (400, 200)
      record "click anywhere leaves rest" (returnClick == Just (UI.Intent LeaveRest))
      -- Drive the same reducer and dispatcher as the native focus callback.
      let focusedEvent = Input.TimedWindowEvent 1 1 (Input.WindowFocusChanged True)
          lostEvent = Input.TimedWindowEvent 2 2 (Input.WindowFocusChanged False)
          (active, _) = Input.reduceInput Input.initialInput [focusedEvent]
      _ <- desktopInput desktop (760, 500) resting active [lostEvent]
      returned <- readIORef (sessionRef desktop)
      record "LeaveRest used on focus loss preserves playback and display mode" (viewingMode returned == Working && transport returned == Playing && windowMode returned == Fullscreen)
      advance
      pure False
    18 | elapsed > 16.7 -> do
      capture (folder </> "rest-return.png")
      ready <- Rest.proceduralReady (restAssets desktop)
      record "procedural shaders compiled" ready
      apply desktop (ChooseRestScene TidalLight)
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    19 | elapsed > 17.3 -> do
      capture (folder </> "procedural-water-a.png")
      writeIORef pausePosition (Audio.position sound)
      advance
      pure False
    20 | elapsed > 18.3 -> do
      capture (folder </> "procedural-water-b.png")
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
      capture (folder </> "procedural-still-a.png")
      -- The frozen procedural frame must be identical over a different working
      -- background; this catches accidental transparency/image dependence.
      apply desktop LeaveRest
      apply desktop (ChooseBackground Black)
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    22 | elapsed > 19.5 -> do
      capture (folder </> "procedural-still-b.png")
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
      capture (folder </> "procedural-veil-a.png")
      advance
      pure False
    24 | elapsed > 21.2 -> do
      capture (folder </> "procedural-veil-b.png")
      apply desktop LeaveRest
      apply desktop (ChooseRestScene StarryNight)
      apply desktop ToggleRest
      modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
      advance
      pure False
    25 | elapsed > 21.8 -> do
      capture (folder </> "procedural-stars-a.png")
      advance
      pure False
    26 | elapsed > 22.8 -> do
      capture (folder </> "procedural-stars-b.png")
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "procedural scene preference survives settings reload" (warning == Nothing && reloaded == prefs && savedRestScene reloaded == StarryNight && savedBackground reloaded == Picture)
      apply desktop Escape
      advance
      pure False
    p | p >= 27 && p <= 50 -> do
      let index = (p - 27) `div` 3
          (choice, name) = [(AmberSea, "amber"), (Shallows, "shallows"), (CloudSea, "clouds"), (Aurora, "aurora"), (InkFlow, "ink"), (BirchLake, "birch"), (AlpineLake, "alpine"), (MeadowStream, "stream")] !! index
      if (p - 27) `mod` 3 == 0
        then do
          apply desktop LeaveRest
          apply desktop ShowRestSettings
          apply desktop (ShowRestCollection (sceneCollection choice))
          writeIORef pausePosition (Audio.position sound)
          state <- readIORef (sessionRef desktop)
          (w, h) <- Platform.windowFramebufferSize (desktopWindow desktop)
          forM_ (lookup choice (UI.galleryCards w h (restCollection state))) $ \(UI.Bounds x y rw rh) ->
            forM_ (UI.hitTest w h state (x + rw / 2, y + rh / 2)) $ \action -> case action of UI.Intent event -> apply desktop event; UI.Quit -> pure ()
          selected <- readIORef (sessionRef desktop)
          record ("gallery selection works: " ++ name) (restScene selected == choice)
          when (sceneCollection choice == Landscapes) $ do
            selectedAt <- monotonicSeconds
            writeIORef (checkSwitchStart desktop) (Just (choice, selectedAt))
          apply desktop ToggleRest
          modifyIORef' (sessionRef desktop) (\s -> s {restHintSeconds = 0})
        else do
          capture (folder </> ("procedural-" ++ name ++ (if (p - 27) `mod` 3 == 1 then "-a.png" else "-b.png")))
          when (sceneCollection choice == Landscapes && (p - 27) `mod` 3 == 2) $ do
            before <- readIORef pausePosition
            ready <- Rest.proceduralReady (restAssets desktop)
            record ("music clock advances with a valid landscape: " ++ name) (ready && Audio.audible sound && Audio.position sound > before + 0.3)
      advance
      pure False
    51 -> do
      apply desktop LeaveRest
      apply desktop ShowRestSettings
      advance
      pure False
    52 -> do
      capture (folder </> "gallery-fullscreen.png")
      apply desktop ToggleWindow
      apply desktop ShowRestSettings
      advance
      pure False
    53 -> do
      capture (folder </> "gallery-compact.png")
      prefs <- readIORef (preferencesRef desktop)
      (reloaded, warning) <- readPreferences (preferencesPath desktop) English
      record "new scene preference persists and startup returns to work" (warning == Nothing && reloaded == prefs && savedRestScene reloaded == MeadowStream && restCollection (restoreSession reloaded) == Landscapes && viewingMode (restoreSession reloaded) == Working)
      advance
      pure False
    54 -> do
      apply desktop (ChooseRestScene RailJourney)
      writeIORef pausePosition (Audio.position sound)
      apply desktop ToggleRest
      advance
      pure False
    55 -> do
      issue <- Journey.status (Rest.journey (restAssets desktop)) English
      if issue == Nothing || now - lastAdvanced > 16
        then do
          state <- readIORef (sessionRef desktop)
          before <- readIORef pausePosition
          record "journey prepares incrementally while music continues" (issue == Nothing && Audio.audible sound && Audio.position sound > before + 0.3)
          record "journey input is isolated from music and settings" (UI.hitTest 760 500 state (380, 250) == Nothing && null (UI.controls 760 500 state))
          advance
          pure False
        else pure False
    56 -> do
      capture (folder </> "journey-ride.png")
      j <- Journey.model (Rest.journey (restAssets desktop))
      record "the integrated journey actually moves along its railway" (Ride.progress (Ride.car j) > 60)
      apply desktop LeaveRest
      saved <- Journey.model (Rest.journey (restAssets desktop))
      reloaded <- JourneyStorage.readJourney (takeDirectory (preferencesPath desktop) </> "journey.json")
      record "leaving rest saves the current place and seed" (JourneyStorage.bookmark reloaded == JourneyStorage.bookmark saved)
      writeIORef pausePosition (Ride.progress (Ride.car saved))
      advance
      pure False
    57 -> do
      j <- Journey.model (Rest.journey (restAssets desktop))
      position <- readIORef pausePosition
      captured <- Journey.isCaptured (Rest.journey (restAssets desktop))
      record "working freezes the journey and releases mouse capture" (Ride.progress (Ride.car j) == position && not captured)
      apply desktop ToggleRest
      advance
      pure False
    58 -> do
      j <- Journey.model (Rest.journey (restAssets desktop))
      position <- readIORef pausePosition
      record "re-entering continues the same journey without a clock jump" (Ride.progress (Ride.car j) >= position && Ride.progress (Ride.car j) < position + 2)
      apply desktop LeaveRest
      apply desktop NewJourney
      apply desktop ShowRestSettings
      advance
      pure False
    59 -> do
      issue <- Journey.status (Rest.journey (restAssets desktop)) English
      if issue == Nothing || now - lastAdvanced > 16
        then do
          j <- Journey.model (Rest.journey (restAssets desktop))
          record "a new landscape replaces the seed and renders inside settings" (issue == Nothing && Ride.worldSeed j /= World.firstSeed && Ride.progress (Ride.car j) == 60)
          capture (folder </> "journey-new-gallery.png")
          advance
          pure False
        else pure False
    60 -> do
      results <- reverse <$> readIORef checks
      frameTimes <- readIORef (checkFrameTimes desktop)
      switches <- reverse <$> readIORef (checkSwitchTimes desktop)
      landscapeDiagnostics <- Rest.landscapeInfo (restAssets desktop)
      journeyDiagnostics <- Journey.info (Rest.journey (restAssets desktop))
      (worker, audioStats) <- Audio.deviceForCheck (engine desktop)
      audioPhases <- reverse <$> readIORef (checkAudioPhases desktop)
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
                  "selectionToFirstPresentationMs" .= [object ["scene" .= show scene, "milliseconds" .= ms] | (scene, ms) <- switches],
                  "landscapeDiagnosticsJSON" .= landscapeDiagnostics,
                  "journeyDiagnosticsJSON" .= journeyDiagnostics,
                  "audioWorkerPid" .= worker,
                  "audioSourceWorkerPid" .= Audio.sourceWorkerPid (engine desktop),
                  "audioPackets" .= GameAudio.submittedPackets audioStats,
                  "emptyDeviceQueueEvents" .= GameAudio.emptyDeviceQueueEvents audioStats,
                  "streamStarvedFrames" .= GameAudio.streamStarvedFrames audioStats,
                  "streamStarvedFramesByPhase" .= [object ["phase" .= p, "total" .= total] | (p, total) <- audioPhases],
                  "exceededPaddingDeadline" .= GameAudio.exceededPaddingDeadline audioStats,
                  "exceededDevicePeriod" .= GameAudio.exceededDevicePeriod audioStats,

                  "renderTimingsIncludingCaptures" .= map timings [minBound .. maxBound]
                ]
            )
        )
      advance
      pure True
    _ -> pure False

-- Exercise font ownership across replacement and failed-library recovery.
checkFontRestart :: Desktop -> FilePath -> IO Bool
checkFontRestart desktop folder = do
  failed <- tryJust (\failure -> case failure of Audio.NoPlayableTracks -> Just (); _ -> Nothing) $
    withSessionFont renderer ["Before"] $ \ref -> do
      replacement <- View.loadTypeface renderer ["After 置換"]
      previousFont <- atomicModifyIORef' ref (\old -> (replacement, old))
      View.unloadTypeface renderer previousFont
      throwIO Audio.NoPlayableTracks
  case failed of
    Right () -> pure False
    Left () -> withSessionFont renderer ["Fallback 復帰 鉄道"] $ \ref -> do
      font <- readIORef ref
      (w, h) <- Platform.windowFramebufferSize (desktopWindow desktop)
      let state = (initialSession defaultVolume) {windowMode = Compact}
      -- The title uses imported glyphs as well as a character outside the
      -- preloaded catalogue. Both must survive the rebuilt atlas.
      screenshot (folder </> "font-replacement.png") $
        View.render renderer True font (restAssets desktop) w h state Nothing "" "Fallback 復帰 鉄道 🚂" Nothing (-1, -1)
      pure True
  where
    renderer = desktopRenderer desktop
