{-# LANGUAGE OverloadedStrings #-}

module Stillroom.Runtime.Storage
  ( Preferences (..),
    defaults,
    readPreferences,
    writePreferences,
    fromSession,
    useBuiltInMusic,
    useMusicFolder,
    restoreSession,
    scanFolder,
    presetTracks,
    LibraryError (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (filterM, when)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Stillroom.Domain
import Stillroom.Session
import System.Directory
import System.FilePath
import Text.Read (readMaybe)

data Preferences = Preferences
  { savedVolume :: !Double,
    savedBackground :: !Background,
    savedWindow :: !WindowMode,
    savedLanguage :: !Language,
    savedScreen :: !String,
    savedFolder :: !(Maybe FilePath),
    previousFolder :: !(Maybe FilePath),
    savedPicture :: !(Maybe String),
    savedPictureShade :: !PictureShade,
    savedRestScene :: !RestScene,
    savedRestMotion :: !RestMotion,
    savedRestShade :: !PictureShade
  }
  deriving (Eq, Show)

instance ToJSON Preferences where
  toJSON p =
    object
      [ "version" .= (6 :: Int),
        "volume" .= savedVolume p,
        "background" .= show (savedBackground p),
        "window" .= show (savedWindow p),
        "language" .= show (savedLanguage p),
        "screen" .= savedScreen p,
        "musicFolder" .= savedFolder p,
        "lastMusicFolder" .= previousFolder p,
        "picture" .= savedPicture p,
        "pictureShade" .= show (savedPictureShade p),
        "restScene" .= show (savedRestScene p),
        "restMotion" .= show (savedRestMotion p),
        "restShade" .= show (savedRestShade p)
      ]

instance FromJSON Preferences where
  parseJSON = withObject "preferences" $ \o -> do
    version <- o .:? "version" .!= (1 :: Int)
    when (version `notElem` [1, 2, 3, 4, 5, 6]) (fail "unsupported preference version")
    v <- o .:? "volume" .!= 0.35
    b <- o .:? "background" .!= "Forest"
    w <- o .:? "window" .!= "Fullscreen"
    l <- o .:? "language" .!= "English"
    screen <- o .:? "screen" .!= ""
    folder <- o .:? "musicFolder"
    previous <- o .:? "lastMusicFolder"
    picture <- o .:? "picture"
    shade <- o .:? "pictureShade" .!= "Original"
    scene <- o .:? "restScene" .!= "TidalLight"
    motion <- o .:? "restMotion" .!= "Gentle"
    restBrightness <- o .:? "restShade" .!= "Soft"
    parsedVolume <- either (const (fail "invalid volume")) pure (volume v)
    case (readMaybe b, readMaybe w, readMaybe l, readMaybe shade, readRestScene scene, readMaybe motion, readMaybe restBrightness) of
      (Just backgroundValue, Just windowValue, Just languageValue, Just shadeValue, Just sceneValue, Just motionValue, Just restShadeValue) ->
        pure (Preferences (volumeValue parsedVolume) backgroundValue windowValue languageValue screen folder previous picture shadeValue sceneValue motionValue restShadeValue)
      _ -> fail "unknown preference value"

-- Retired local photo experiments migrate at the boundary, without retaining
-- image-specific scenes in the domain or discarding unrelated preferences.
readRestScene :: String -> Maybe RestScene
readRestScene "AfternoonLake" = Just TidalLight
readRestScene "WoodlandPond" = Just Shallows
readRestScene name = readMaybe name

defaults :: Language -> Preferences
defaults languageValue = Preferences 0.35 Forest Fullscreen languageValue "" Nothing Nothing Nothing Original TidalLight Gentle Soft

readPreferences :: FilePath -> Language -> IO (Preferences, Maybe String)
readPreferences file languageValue = do
  exists <- doesFileExist file
  if not exists
    then pure (defaults languageValue, Nothing)
    else do
      result <- try (BL.readFile file >>= \bytes -> pure $! eitherDecode bytes)
      pure $ case result of
        Right (Right p) -> (p, Nothing)
        Right (Left _) -> (defaults languageValue, Just "Settings could not be read; defaults are in use.")
        Left (_ :: IOException) -> (defaults languageValue, Just "Settings are unavailable; defaults are in use.")

writePreferences :: FilePath -> Preferences -> IO (Either IOException ())
writePreferences file prefs = try $ do
  createDirectoryIfMissing True (takeDirectory file)
  let temporary = file <.> "tmp"
  BL.writeFile temporary (encode prefs)
  renameFile temporary file

restoreSession :: Preferences -> Session
restoreSession p =
  (initialSession (either (const defaultVolume) id (volume (savedVolume p))))
    { background = savedBackground p,
      pictureShade = savedPictureShade p,
      restScene = savedRestScene p,
      restMotion = savedRestMotion p,
      restShade = savedRestShade p,
      windowMode = savedWindow p,
      language = savedLanguage p
    }

fromSession :: Session -> Preferences -> Preferences
fromSession s p =
  p
    { savedVolume = volumeValue (loudness s),
      savedBackground = background s,
      savedPictureShade = pictureShade s,
      savedRestScene = restScene s,
      savedRestMotion = restMotion s,
      savedRestShade = restShade s,
      savedWindow = windowMode s,
      savedLanguage = language s
    }

-- Choosing built-in music changes the source, not knowledge of the user's
-- library. This also migrates older settings with only musicFolder present.
useBuiltInMusic :: Preferences -> Preferences
useBuiltInMusic p = p {savedFolder = Nothing, previousFolder = savedFolder p <|> previousFolder p}

useMusicFolder :: FilePath -> Preferences -> Preferences
useMusicFolder folder p = p {savedFolder = Just folder, previousFolder = Just folder}

data LibraryError = FolderUnavailable | NoSupportedFiles | TooManyFiles deriving (Eq, Show)

scanFolder :: FilePath -> IO (Either LibraryError (NonEmpty Track))
scanFolder folder = do
  result <- try $ do
    names <- sort <$> listDirectory folder
    let supported = [folder </> n | n <- names, map toLower (takeExtension n) `elem` [".mp3", ".wav", ".ogg"]]
    files <- filterM doesFileExist (take 1001 supported)
    pure files
  pure $ case result of
    Left (_ :: IOException) -> Left FolderUnavailable
    Right [] -> Left NoSupportedFiles
    Right files
      | length files > 1000 -> Left TooManyFiles
      | otherwise -> case NE.nonEmpty (zipWith make [0 ..] files) of
          Just tracks -> Right tracks
          Nothing -> Left NoSupportedFiles
  where
    make i file = Track (TrackId i) (take 120 (takeBaseName file)) (LocalFile file)

presetTracks :: NonEmpty Track
presetTracks = NE.zipWith (\i p -> Track (TrackId i) (presetName p) (Procedural p)) (0 :| [1 ..]) allPresets
