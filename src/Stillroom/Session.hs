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
  | ToggleWindow
  | Escape
  | ToggleSettings
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
initialSession v = Session Playing v Forest Original Fullscreen English False 0

step :: Event -> Session -> (Session, [Effect])
step event original = case event of
  Elapse elapsed -> (original {idleSeconds = idleSeconds original + secondsValue elapsed}, [])
  Activity -> (awake, [])
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
  ToggleWindow -> (awake {windowMode = opposite (windowMode original), settingsOpen = False}, [PlaceWindow, Persist])
  Escape
    | settingsOpen original -> (awake {settingsOpen = False}, [])
    | windowMode original == Fullscreen -> step ToggleWindow awake
    | otherwise -> (awake, [])
  ToggleSettings -> (awake {settingsOpen = not (settingsOpen original)}, [])
  NextScreen -> (awake, [MoveScreen, Persist])
  OpenMusic -> (awake, [PickMusicFolder])
  UsePresets -> (awake, [SelectPresets, Persist])
  ToggleLanguage -> (awake {language = if language original == English then Japanese else English}, [Persist])
  where
    awake = original {idleSeconds = 0}
    opposite Fullscreen = Compact
    opposite Compact = Fullscreen

controlsVisible :: Session -> Bool
controlsVisible s = windowMode s == Compact || settingsOpen s || idleSeconds s < 4.5
