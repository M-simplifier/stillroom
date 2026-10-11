-- | Validated values shared by the pure core. Units and states are explicit;
-- raw host numbers are admitted at this boundary only.
module Stillroom.Domain
  ( Volume,
    volume,
    volumeValue,
    adjustVolume,
    defaultVolume,
    Seconds,
    seconds,
    secondsValue,
    zeroSeconds,
    ValueError (..),
    Transport (..),
    Background (..),
    WindowMode (..),
    Language (..),
    Preset (..),
    allPresets,
    presetName,
    presetDuration,
    Source (..),
    Track (..),
    TrackId (..),
    Mix (..),
    mixAt,
    mixBetween,
    fadeIn,
  )
where

import Data.List.NonEmpty (NonEmpty (..))

newtype Volume = Volume Double deriving (Eq, Show)

newtype Seconds = Seconds Double deriving (Eq, Ord, Show)

data ValueError = NotFinite | OutsideUnitInterval | NegativeTime deriving (Eq, Show)

volume :: Double -> Either ValueError Volume
volume x
  | isNaN x || isInfinite x = Left NotFinite
  | x < 0 || x > 1 = Left OutsideUnitInterval
  | otherwise = Right (Volume x)

volumeValue :: Volume -> Double
volumeValue (Volume x) = x

defaultVolume :: Volume
defaultVolume = Volume 0.35

adjustVolume :: Double -> Volume -> Volume
adjustVolume delta original@(Volume x)
  | isNaN delta || isInfinite delta = original
  | otherwise = Volume (max 0 (min 1 (x + delta)))

seconds :: Double -> Either ValueError Seconds
seconds x
  | isNaN x || isInfinite x = Left NotFinite
  | x < 0 = Left NegativeTime
  | otherwise = Right (Seconds x)

secondsValue :: Seconds -> Double
secondsValue (Seconds x) = x

zeroSeconds :: Seconds
zeroSeconds = Seconds 0

data Transport = Playing | Paused deriving (Eq, Show)

data Background = Forest | Slate | Black | Lake deriving (Eq, Enum, Bounded, Show, Read)

data WindowMode = Fullscreen | Compact deriving (Eq, Show, Read)

data Language = English | Japanese deriving (Eq, Show, Read)

data Preset = Linen | Moss | Orbit | Night deriving (Eq, Enum, Bounded, Show, Read)

allPresets :: NonEmpty Preset
allPresets = Linen :| [Moss, Orbit, Night]

presetName :: Preset -> String
presetName = show

presetDuration :: Preset -> Seconds
presetDuration _ = Seconds 180

newtype TrackId = TrackId Int deriving (Eq, Ord, Show)

data Source = Procedural Preset | LocalFile FilePath deriving (Eq, Show)

data Track = Track {trackId :: !TrackId, trackTitle :: !String, trackSource :: !Source}
  deriving (Eq, Show)

data Mix
  = Solo
  | Crossfade {outgoingGain :: !Double, incomingGain :: !Double}
  | Advance
  deriving (Eq, Show)

-- | Equal-power curve. Short tracks use at most half their length.
-- At duration the
-- next deck becomes the only deck. Position comes from the audio clock.
mixAt :: Seconds -> Seconds -> Mix
mixAt duration = mixBetween duration duration

-- | Neither track can finish inside the overlap before ownership is handed over.
mixBetween :: Seconds -> Seconds -> Seconds -> Mix
mixBetween (Seconds duration) (Seconds incoming) (Seconds position)
  | duration <= 0 || position >= duration = Advance
  | position < duration - width = Solo
  | otherwise =
      let t = (position - (duration - width)) / width
       in Crossfade (cos (t * pi / 2)) (sin (t * pi / 2))
  where
    width = min 6 (min duration incoming / 2)

fadeIn :: Seconds -> Double
fadeIn (Seconds t) = min 1 (t / 1.6)
