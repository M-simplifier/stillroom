-- | User intent and quiet-screen timing, independent of window handles and IO.
module Stillroom.Session
  ( Session (..),
    Event (..),
    Effect (..),
    initialSession,
    step,
    controlsVisible,
  )
where

import Stillroom.Domain

data Session = Session
  { transport :: !Transport,
    loudness :: !Volume,
    background :: !Background,
    pictureShade :: !PictureShade,
    windowMode :: !WindowMode,
    language :: !Language,
    settingsOpen :: !Bool,
    settingsPage :: !SettingsPage,
    viewingMode :: !ViewingMode,
    restScene :: !RestScene,
    restCollection :: !RestCollection,
    restMotion :: !RestMotion,
    restShade :: !PictureShade,
    restSeconds :: !Double,
    restHintSeconds :: !Double,
    idleSeconds :: !Double
  }
  deriving (Eq, Show)

data Event
  = TogglePlayback
  | ChangeVolume Double
  | SetVolume Volume
  | NextTrack
  | ChooseBackground Background
  | OpenPicture
  | BrowsePicture PictureDirection
  | RemovePicture
  | CyclePictureShade
  | ChoosePictureShade PictureShade
  | ToggleRest
  | LeaveRest
  | ChooseRestScene RestScene
  | ShowRestCollection RestCollection
  | ToggleRestMotion
  | ChooseRestMotion RestMotion
  | CycleRestShade
  | ChooseRestShade PictureShade
  | ToggleWindow
  | Escape
  | ToggleSettings
  | ShowRestSettings
  | ShowDisplaySettings
  | ShowMusicSettings
  | NextScreen
  | OpenMusic
  | UsePresets
  | ToggleLanguage
  | Activity
  | Elapse Seconds
  deriving (Eq, Show)

data Effect
  = ApplyTransport Transport
  | ApplyVolume Volume
  | SkipTrack
  | PlaceWindow
  | Persist
  | PickMusicFolder
  | SelectPresets
  | MoveScreen
  | PickPicture
  | SelectPicture PictureDirection
  | DeletePicture
  deriving (Eq, Show)

initialSession :: Volume -> Session
initialSession v = Session Playing v Forest Original Fullscreen English False RestSettings Working TidalLight Atmospheres Gentle Soft 0 0 0

step :: Event -> Session -> (Session, [Effect])
step event original
  | viewingMode original == Resting, not (allowedDuringRest event) = (original, [])
  | otherwise = stepAllowed event original

-- Rest is an exit-only view. Keep this boundary in the pure reducer as well as
-- in input routing: future controls cannot accidentally change music/settings.
allowedDuringRest :: Event -> Bool
allowedDuringRest (Elapse _) = True
allowedDuringRest ToggleRest = True
allowedDuringRest LeaveRest = True
allowedDuringRest Escape = True
allowedDuringRest _ = False

stepAllowed :: Event -> Session -> (Session, [Effect])
stepAllowed event original = case event of
  Elapse elapsed ->
    let dt = secondsValue elapsed
     in ( original
            { idleSeconds = idleSeconds original + dt,
              restSeconds =
                if not (viewingMode original == Resting || (settingsOpen original && settingsPage original == RestSettings))
                  then 0
                  else
                    if restMotion original == Gentle then restSeconds original + dt else restSeconds original,
              restHintSeconds = max 0 (restHintSeconds original - dt)
            },
          []
        )
  Activity -> (awake, [])
  ToggleRest
    | viewingMode original == Resting -> step LeaveRest original
    | otherwise -> (original {viewingMode = Resting, settingsOpen = False, restSeconds = 0, restHintSeconds = 3}, [])
  LeaveRest
    | viewingMode original == Working -> (original, [])
    | otherwise -> (original {viewingMode = Working, restSeconds = 0, restHintSeconds = 0, idleSeconds = 10}, [])
  ChooseRestScene scene -> (original {restScene = scene, restCollection = sceneCollection scene, restSeconds = 0, restHintSeconds = 2}, [Persist])
  ShowRestCollection collection -> (awake {restCollection = collection}, [])
  ToggleRestMotion -> (original {restMotion = if restMotion original == Still then Gentle else Still, restHintSeconds = 2}, [Persist])
  ChooseRestMotion motion -> (original {restMotion = motion}, [Persist])
  CycleRestShade -> (original {restShade = nextShade (restShade original), restHintSeconds = 2}, [Persist])
  ChooseRestShade shade -> (original {restShade = shade}, [Persist])
  TogglePlayback ->
    let p = if transport original == Playing then Paused else Playing
     in (awake {transport = p}, [ApplyTransport p])
  ChangeVolume d -> step (SetVolume (adjustVolume d (loudness original))) awake
  SetVolume v -> (awake {loudness = v}, [ApplyVolume v, Persist])
  NextTrack -> (awake, [SkipTrack])
  ChooseBackground b -> (awake {background = b}, [Persist])
  OpenPicture -> (awake, [PickPicture])
  BrowsePicture direction -> (awake, [SelectPicture direction])
  RemovePicture -> (awake, [DeletePicture])
  CyclePictureShade -> (awake {pictureShade = nextShade (pictureShade original)}, [Persist])
  ChoosePictureShade shade -> (awake {pictureShade = shade}, [Persist])
  ToggleWindow -> (awake {windowMode = opposite (windowMode original), settingsOpen = False, viewingMode = Working}, [PlaceWindow, Persist])
  Escape
    | viewingMode original == Resting -> step LeaveRest original
    | settingsOpen original -> (awake {settingsOpen = False}, [])
    | windowMode original == Fullscreen -> step ToggleWindow awake
    | otherwise -> (awake, [])
  ToggleSettings -> (awake {settingsOpen = not (settingsOpen original), viewingMode = Working}, [])
  ShowRestSettings -> (awake {settingsOpen = True, settingsPage = RestSettings, viewingMode = Working}, [])
  ShowDisplaySettings -> (awake {settingsOpen = True, settingsPage = DisplaySettings, viewingMode = Working}, [])
  ShowMusicSettings -> (awake {settingsOpen = True, settingsPage = MusicSettings, viewingMode = Working}, [])
  NextScreen -> (awake, [MoveScreen, Persist])
  OpenMusic -> (awake, [PickMusicFolder])
  UsePresets -> (awake, [SelectPresets, Persist])
  ToggleLanguage -> (awake {language = if language original == English then Japanese else English}, [Persist])
  where
    awake = original {idleSeconds = 0}
    opposite Fullscreen = Compact
    opposite Compact = Fullscreen

controlsVisible :: Session -> Bool
controlsVisible s = viewingMode s == Working && (windowMode s == Compact || settingsOpen s || idleSeconds s < 4.5)
