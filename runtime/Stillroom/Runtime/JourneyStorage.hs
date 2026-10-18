{-# LANGUAGE OverloadedStrings #-}

-- | A versioned bookmark, independent of music and display preferences.
-- Transient velocities, route searches and GPU caches are never persisted.
module Stillroom.Runtime.JourneyStorage (Bookmark (..), bookmark, restore, readJourney, writeJourney) where

import Control.Exception (IOException, try)
import Control.Monad (guard, unless)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word32)
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

data Bookmark = Bookmark
  { seedNumber :: !Word32,
    railPosition :: !Double,
    aboard :: !Bool,
    travelling :: !Bool,
    pace :: !J.Pace,
    x :: !Double,
    z :: !Double,
    facing :: !Double,
    tilt :: !Double,
    strolling :: !Bool
  }
  deriving (Eq, Show)

instance ToJSON Bookmark where
  toJSON b =
    object
      [ "version" .= (1 :: Int),
        "seed" .= seedNumber b,
        "railPosition" .= railPosition b,
        "aboard" .= aboard b,
        "travelling" .= travelling b,
        "pace" .= show (pace b),
        "x" .= x b,
        "z" .= z b,
        "facing" .= facing b,
        "tilt" .= tilt b,
        "strolling" .= strolling b
      ]

instance FromJSON Bookmark where
  parseJSON = withObject "Journey bookmark" $ \o -> do
    version <- o .: "version"
    unless (version == (1 :: Int)) (fail "Unsupported journey version")
    paceText <- o .: "pace"
    speed <- maybe (fail "Unknown journey pace") pure (readMaybe paceText)
    Bookmark
      <$> o .: "seed"
      <*> o .: "railPosition"
      <*> o .: "aboard"
      <*> o .: "travelling"
      <*> pure speed
      <*> o .: "x"
      <*> o .: "z"
      <*> o .: "facing"
      <*> o .: "tilt"
      <*> o .: "strolling"

bookmark :: J.Journey -> Bookmark
bookmark j =
  let c = J.car j; w = J.person j
   in Bookmark
        (World.seedValue (J.worldSeed j))
        (J.progress c)
        (J.seat j == J.Aboard)
        (J.running c && not (J.exitRequested j))
        (J.pace c)
        (W.posX w)
        (W.posZ w)
        (canonicalAngle (if J.seat j == J.Aboard then W.gazeOffset w else W.yaw w))
        (W.pitch w)
        (W.walkingMode w == W.Strolling)

-- Looking around any number of times must still produce a readable bookmark.
canonicalAngle :: Double -> Double
canonicalAngle a = if abs a <= pi then a else atan2 (sin a) (cos a)

-- Validate before any native coordinate query. Corrupt/foreign bookmarks
-- cannot send NaN, out-of-range integer conversions or underwater positions
-- through the FFI. An unavailable bookmark starts a new first journey.
restore :: Bookmark -> Maybe J.Journey
restore b = do
  guard (all bounded [railPosition b, x b, z b, facing b, tilt b])
  guard (railPosition b >= 0 && railPosition b <= World.coordinateLimit && World.withinWorld (x b) (z b))
  guard (abs (facing b) <= 100000 && tilt b >= (-1.15) && tilt b <= 1.05)
  let seed = World.seed (seedNumber b)
      start = J.at seed (railPosition b)
      c = (J.car start) {J.running = aboard b && travelling b, J.pace = pace b}
  w <-
    if aboard b
      then Just (J.passenger seed c (facing b) (tilt b) 0)
      else do
        ground <- W.at seed (x b) (z b) (facing b)
        guard (J.clearOfCar seed c ground)
        pure ground {W.pitch = tilt b, W.walkingMode = if strolling b then W.Strolling else W.Manual}
  pure start {J.car = c, J.person = w, J.seat = if aboard b then J.Aboard else J.OnFoot}
  where
    bounded v = not (isNaN v || isInfinite v)

readJourney :: FilePath -> IO J.Journey
readJourney path = do
  result <- try (BS.readFile path)
  pure $ case result of
    Left (_ :: IOException) -> J.initial World.firstSeed
    Right bytes -> maybe (J.initial World.firstSeed) id (decodeStrict' bytes >>= restore)

writeJourney :: FilePath -> J.Journey -> IO ()
writeJourney path j = do
  createDirectoryIfMissing True (takeDirectory path)
  let temporary = path ++ ".tmp"
  BL.writeFile temporary (encode (bookmark j))
  renameFile temporary path
