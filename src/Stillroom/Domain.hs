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
    PictureShade (..),
    nextShade,
    shadeOpacity,
    PictureDirection (..),
    ViewingMode (..),
    RestScene (..),
    RestMotion (..),
    SettingsPage (..),
    restSceneName,
    restSceneDescription,
    restMotionName,
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

data Background = Forest | Slate | Black | Lake | Picture deriving (Eq, Enum, Bounded, Show, Read)

data PictureShade = Original | Soft | Dim deriving (Eq, Enum, Bounded, Show, Read)

nextShade :: PictureShade -> PictureShade
nextShade Original = Soft
nextShade Soft = Dim
nextShade Dim = Original

shadeOpacity :: PictureShade -> Int
shadeOpacity Original = 0
shadeOpacity Soft = 64
shadeOpacity Dim = 128

data PictureDirection = PreviousPicture | NextPicture deriving (Eq, Show)

data ViewingMode = Working | Resting deriving (Eq, Show)

data RestScene = TidalLight | LightVeil | StarryNight | AmberSea | Shallows | CloudSea | Aurora | InkFlow
  deriving (Eq, Enum, Bounded, Show, Read)

data RestMotion = Still | Gentle deriving (Eq, Enum, Bounded, Show, Read)

data SettingsPage = DisplaySettings | RestSettings deriving (Eq, Show)

restSceneName :: Language -> RestScene -> String
restSceneName Japanese TidalLight = "水と光"
restSceneName Japanese LightVeil = "光のヴェール"
restSceneName Japanese StarryNight = "星の夜"
restSceneName English TidalLight = "Water and light"
restSceneName English LightVeil = "Light veil"
restSceneName English StarryNight = "Starry night"
restSceneName Japanese AmberSea = "琥珀の海"
restSceneName Japanese Shallows = "浅瀬の光"
restSceneName Japanese CloudSea = "雲の海"
restSceneName Japanese Aurora = "極夜のオーロラ"
restSceneName Japanese InkFlow = "墨の流れ"
restSceneName English AmberSea = "Amber sea"
restSceneName English Shallows = "Sunlit shallows"
restSceneName English CloudSea = "Sea of clouds"
restSceneName English Aurora = "Northern lights"
restSceneName English InkFlow = "Flowing ink"

restSceneDescription :: Language -> RestScene -> String
restSceneDescription Japanese TidalLight = "青い水面。重なる波と空の反射。"
restSceneDescription Japanese LightVeil = "光の薄布。ゆっくりほどける色の層。"
restSceneDescription Japanese StarryNight = "動かない星々と、かすかな銀河の光。"
restSceneDescription Japanese AmberSea = "夕暮れの海。波に伸びる琥珀色の光。"
restSceneDescription Japanese Shallows = "澄んだ浅瀬。砂底に揺れる光の網。"
restSceneDescription Japanese CloudSea = "雲の上。やわらかな陰影と遠い空。"
restSceneDescription Japanese Aurora = "極夜の空。ゆらめく緑の光のカーテン。"
restSceneDescription Japanese InkFlow = "墨と青磁。ゆっくり溶け合う流れ。"
restSceneDescription English TidalLight = "Blue water, layered waves and reflected sky."
restSceneDescription English LightVeil = "Soft, slowly unfolding layers of colored light."
restSceneDescription English StarryNight = "Fixed stars and a faint galactic glow."
restSceneDescription English AmberSea = "Evening waves carrying a warm amber reflection."
restSceneDescription English Shallows = "Clear water and light moving across the sand."
restSceneDescription English CloudSea = "Above the clouds, with soft shadows and open sky."
restSceneDescription English Aurora = "Quiet curtains of light in the polar night."
restSceneDescription English InkFlow = "Slow currents of ink and celadon."

restMotionName :: Language -> RestMotion -> String
restMotionName Japanese Still = "静止"
restMotionName Japanese Gentle = "ゆっくり動く"
restMotionName English Still = "Still"
restMotionName English Gentle = "Gentle motion"

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
