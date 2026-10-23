{-# LANGUAGE BangPatterns #-}

-- | Resident environmental PCM, driven by the current surroundings. The bank
-- is prepared once; playback only reads samples and advances bounded state.
module Stillroom.Audio.Environment
  ( EnvironmentInput,
    environmentInput,
    EnvironmentState,
    initialEnvironment,
    EnvironmentBank,
    prepareEnvironmentBank,
    renderEnvironment,
  )
where

import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, xor)
import Data.List (group, sort)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import Data.Word (Word32)

data Travel = Aboard | OnFoot deriving (Eq, Show)

-- Constructors and record labels stay private: even record update must pass
-- through the sensor boundary below.
data EnvironmentInput = EnvironmentInput !Travel !Double !Double !Double
  deriving (Eq, Show)

-- | Aboard, speed in metres/second, forest weight, signed shore distance in
-- metres. Negative shore distance means nearby water. Invalid speed/forest/
-- shore sensors independently become stationary/bare/dry values.
environmentInput :: Bool -> Double -> Double -> Double -> EnvironmentInput
environmentInput aboard speed forest shore =
  EnvironmentInput
    (if aboard then Aboard else OnFoot)
    (finiteClamp 0 maximumSpeed 0 speed)
    (finiteClamp 0 1 0 forest)
    (finiteClamp 0 34 34 shore)

-- Samples are interleaved stereo at 44100 Hz. Frame count and finite PCM are
-- established at preparation, before the sample-reading hot path.
data Sample = Sample !(V.Vector Float) !Int

data EnvironmentBank = EnvironmentBank
  { windSample :: !Sample,
    leavesSample :: !Sample,
    waterSample :: !Sample,
    wheelSample :: !Sample,
    firstBodySample :: !Sample,
    secondBodySample :: !Sample,
    thirdBodySample :: !Sample
  }

-- | Require exactly the seven named samples, with no duplicate keys, empty or
-- odd-length buffers, non-finite PCM, or values outside [-1,1]. Preserve the
-- authored levels: neither preparation nor playback normalizes the samples.
prepareEnvironmentBank :: [(String, V.Vector Float)] -> Either String EnvironmentBank
prepareEnvironmentBank entries
  | any ((> 1) . length) (group (sort (map fst entries))) = Left "Duplicate environment sample key."
  | sort (map fst entries) /= sort sampleKeys = Left "Environment bank requires exactly wind, leaves, water, wheel, body-1, body-2, body-3."
  | otherwise =
      EnvironmentBank
        <$> prepare "wind"
        <*> prepare "leaves"
        <*> prepare "water"
        <*> prepare "wheel"
        <*> prepare "body-1"
        <*> prepare "body-2"
        <*> prepare "body-3"
  where
    prepare key = case lookup key entries of
      Nothing -> Left ("Missing environment sample: " ++ key)
      Just pcm
        | V.null pcm -> Left ("Environment sample is empty: " ++ key)
        | odd (V.length pcm) -> Left ("Environment sample is not interleaved stereo: " ++ key)
        | V.any (\value -> isNaN value || isInfinite value || abs value > 1) pcm ->
            Left ("Environment sample contains invalid PCM: " ++ key)
        | otherwise -> Right (Sample pcm (V.length pcm `div` 2))

sampleKeys :: [String]
sampleKeys = ["wind", "leaves", "water", "wheel", "body-1", "body-2", "body-3"]

data Controls = Controls
  { forestLevel :: !Double,
    waterLevel :: !Double,
    wheelLevel :: !Double,
    wheelRate :: !Double
  }
  deriving (Eq, Show)

data BodyKind = FirstBody | SecondBody | ThirdBody deriving (Eq, Show)

data BodyVoice = BodyVoice !BodyKind !Int !Double deriving (Eq, Show)

-- Four slots make the voice bound structural, including unusual long samples.
data BodyVoices = BodyVoices !(Maybe BodyVoice) !(Maybe BodyVoice) !(Maybe BodyVoice) !(Maybe BodyVoice)
  deriving (Eq, Show)

data EnvironmentState = EnvironmentState
  { windPosition :: !Double,
    leavesPosition :: !Double,
    waterPosition :: !Double,
    wheelPosition :: !Double,
    breezePhase29 :: !Double,
    breezePhase47 :: !Double,
    presence :: !Double,
    currentControls :: !(Maybe Controls),
    distanceToBody :: !Double,
    nextBodyKind :: !BodyKind,
    bodyRandom :: !Word32,
    bodyVoices :: !BodyVoices
  }
  deriving (Eq, Show)

-- | Decoupled loop offsets and the first seven-metre body event are taken
-- from the auditioned prototype. There is no growing playback-time history.
initialEnvironment :: EnvironmentState
initialEnvironment =
  EnvironmentState
    { windPosition = 2.1 * sampleRate,
      leavesPosition = 4.3 * sampleRate,
      waterPosition = 1.7 * sampleRate,
      wheelPosition = 3.1 * sampleRate,
      breezePhase29 = 0.3,
      breezePhase47 = 1.4,
      presence = 0,
      currentControls = Nothing,
      distanceToBody = 7,
      nextBodyKind = FirstBody,
      bodyRandom = 20261024,
      bodyVoices = noBodyVoices
    }

-- | Render stereo-interleaved PCM. A block is limited to one minute (the
-- runtime supplies 4096 frames), so an invalid request cannot overflow a
-- stereo length or demand unbounded allocation. Zero frames preserve state.
--
-- Nothing releases the scene over 1.6 seconds; once silent, every phase and
-- control freezes. Just enters over 1.1 seconds. Changing block boundaries
-- with the same input cannot change either the PCM or resulting state.
renderEnvironment :: EnvironmentBank -> Maybe EnvironmentInput -> EnvironmentState -> Int -> Either String (V.Vector Float, EnvironmentState)
renderEnvironment bank input state frames
  | frames < 0 = Left "Environment frame count must be nonnegative."
  | frames > maximumBlockFrames = Left "Environment block exceeds one minute."
  | frames == 0 = Right (V.empty, state)
  | input == Nothing && presence state == 0 = Right (V.replicate (2 * frames) 0, state)
  | otherwise = Right $ runST $ do
      pcm <- MV.new (2 * frames)
      let !targets = fmap controlTargets input
          renderFrame !index !previous
            | index == frames = pure previous
            | otherwise = do
                let ((!left, !right), !following) = advanceFrame bank input targets previous
                MV.write pcm (2 * index) (realToFrac left)
                MV.write pcm (2 * index + 1) (realToFrac right)
                renderFrame (index + 1) following
      finalState <- renderFrame 0 (fitLoopPositions bank state)
      result <- V.unsafeFreeze pcm
      pure (result, finalState)

type Stereo = (Double, Double)

advanceFrame :: EnvironmentBank -> Maybe EnvironmentInput -> Maybe Controls -> EnvironmentState -> (Stereo, EnvironmentState)
advanceFrame bank input targets previous
  | input == Nothing && presence previous == 0 = ((0, 0), previous)
  | otherwise = (faded, following)
  where
    -- One input sample follows admission -> controls -> distance event ->
    -- sample mix -> committed phase advance. Targets never depend on blocks.
    !controls = advanceControls targets (currentControls previous)
    (!remainingDistance, !followingKind, !followingRandom, !voicesToRead) =
      advanceDistance input previous
    (!body, !followingVoices) = advanceBody bank voicesToRead
    !breeze = 0.93 + 0.09 * sin (breezePhase29 previous) + 0.045 * sin (breezePhase47 previous)
    !wind = scaleStereo breeze (readLoop (windSample bank) (windPosition previous))
    !leaves = scaleStereo ((0.36 + 0.64 * forestLevel controls) * breeze) (readLoop (leavesSample bank) (leavesPosition previous))
    !water = scaleStereo (waterLevel controls) (readLoop (waterSample bank) (waterPosition previous))
    !wheel = scaleStereo (wheelLevel controls) (readLoop (wheelSample bank) (wheelPosition previous))
    !mixed = addStereo wind (addStereo leaves (addStereo water (addStereo wheel body)))
    -- This ceiling only protects callers supplying full-scale replacement
    -- banks. The authored bank has ample headroom and never reaches it.
    !faded = ceilingStereo (scaleStereo (smoothstep (presence previous)) mixed)
    !followingPresence = case input of
      Nothing -> max 0 (presence previous - 1 / (1.6 * sampleRate))
      Just _ -> min 1 (presence previous + 1 / (1.1 * sampleRate))
    !following =
      EnvironmentState
        { windPosition = advancePosition (windSample bank) 1 (windPosition previous),
          leavesPosition = advancePosition (leavesSample bank) 1 (leavesPosition previous),
          waterPosition = advancePosition (waterSample bank) 1 (waterPosition previous),
          wheelPosition = advancePosition (wheelSample bank) (wheelRate controls) (wheelPosition previous),
          breezePhase29 = advanceBreeze (2 * pi / (29 * sampleRate)) (breezePhase29 previous),
          breezePhase47 = advanceBreeze (2 * pi / (47 * sampleRate)) (breezePhase47 previous),
          presence = followingPresence,
          currentControls = Just controls,
          distanceToBody = remainingDistance,
          nextBodyKind = followingKind,
          bodyRandom = followingRandom,
          bodyVoices = if followingPresence == 0 then noBodyVoices else followingVoices
        }

advanceControls :: Maybe Controls -> Maybe Controls -> Controls
advanceControls targets previous = case (targets, previous) of
  (Nothing, Nothing) -> Controls 0 0 0 0.72
  (Nothing, Just controls) -> controls
  (Just desired, Nothing) -> desired
  (Just desired, Just controls) ->
    Controls
      (easeControl forestAlpha (forestLevel desired) (forestLevel controls))
      (easeControl waterAlpha (waterLevel desired) (waterLevel controls))
      (easeControl wheelAlpha (wheelLevel desired) (wheelLevel controls))
      (easeControl wheelAlpha (wheelRate desired) (wheelRate controls))

controlTargets :: EnvironmentInput -> Controls
controlTargets sensors@(EnvironmentInput _ _ forest shore) =
  Controls forest (1 - smoothstep ((shore - 2) / 32)) (carMix * speedRatio ** 0.82) (0.72 + 0.28 * speedRatio)
  where
    speedRatio = travelSpeed sensors / 4.6

travelSpeed :: EnvironmentInput -> Double
travelSpeed (EnvironmentInput Aboard speed _ _) = speed
travelSpeed (EnvironmentInput OnFoot _ _ _) = 0

easeControl :: Double -> Double -> Double -> Double
easeControl alpha target previous = target + alpha * (previous - target)

advanceDistance :: Maybe EnvironmentInput -> EnvironmentState -> (Double, BodyKind, Word32, BodyVoices)
advanceDistance input previous
  | distance >= distanceToBody previous && distance > 0 =
      let (!strengthUnit, !strengthRandom) = nextRandom (bodyRandom previous)
          (!intervalUnit, !intervalRandom) = nextRandom strengthRandom
          !strength = 0.65 + 0.30 * strengthUnit
          !gain = carMix * min 1.3 (speed / 4.6) ** 0.6 * strength
          !voice = BodyVoice (nextBodyKind previous) 0 gain
          !remaining = distanceToBody previous - distance + 10.5 + 7 * intervalUnit
       in (remaining, followingBodyKind (nextBodyKind previous), intervalRandom, pushBody voice (bodyVoices previous))
  | otherwise =
      (distanceToBody previous - distance, nextBodyKind previous, bodyRandom previous, bodyVoices previous)
  where
    speed = maybe 0 travelSpeed input
    distance = speed / sampleRate

followingBodyKind :: BodyKind -> BodyKind
followingBodyKind FirstBody = SecondBody
followingBodyKind SecondBody = ThirdBody
followingBodyKind ThirdBody = FirstBody

nextRandom :: Word32 -> (Double, Word32)
nextRandom previous = (fromIntegral result / 4294967296, result)
  where
    first = previous `xor` (previous `shiftL` 13)
    second = first `xor` (first `shiftR` 17)
    result = second `xor` (second `shiftL` 5)

noBodyVoices :: BodyVoices
noBodyVoices = BodyVoices Nothing Nothing Nothing Nothing

pushBody :: BodyVoice -> BodyVoices -> BodyVoices
pushBody voice (BodyVoices first second third _) = BodyVoices (Just voice) first second third

advanceBody :: EnvironmentBank -> BodyVoices -> (Stereo, BodyVoices)
advanceBody bank (BodyVoices first second third fourth) =
  (addStereo a (addStereo b (addStereo c d)), BodyVoices nextA nextB nextC nextD)
  where
    (!a, !nextA) = readBody bank first
    (!b, !nextB) = readBody bank second
    (!c, !nextC) = readBody bank third
    (!d, !nextD) = readBody bank fourth

readBody :: EnvironmentBank -> Maybe BodyVoice -> (Stereo, Maybe BodyVoice)
readBody _ Nothing = ((0, 0), Nothing)
readBody bank (Just (BodyVoice kind frame gain))
  | frame >= frames = ((0, 0), Nothing)
  | otherwise = (scaleStereo gain (readFrame sample frame), following)
  where
    sample@(Sample _ frames) = case kind of
      FirstBody -> firstBodySample bank
      SecondBody -> secondBodySample bank
      ThirdBody -> thirdBodySample bank
    following
      | frame + 1 < frames = Just (BodyVoice kind (frame + 1) gain)
      | otherwise = Nothing

readLoop :: Sample -> Double -> Stereo
readLoop sample@(Sample _ frames) position =
  addStereo (scaleStereo (1 - fraction) (readFrame sample frame)) (scaleStereo fraction (readFrame sample following))
  where
    frame = floor position
    fraction = position - fromIntegral frame
    following = if frame + 1 == frames then 0 else frame + 1

-- The private Sample and phase/voice transitions establish both indices. Use
-- checked indexing here so an internal invariant regression fails locally.
readFrame :: Sample -> Int -> Stereo
readFrame (Sample pcm _) frame = (realToFrac (pcm V.! (2 * frame)), realToFrac (pcm V.! (2 * frame + 1)))

advancePosition :: Sample -> Double -> Double -> Double
advancePosition (Sample _ frames) rate position =
  let following = position + rate
      lengthFrames = fromIntegral frames
      wrapped = following - lengthFrames
   in if following < lengthFrames
        then following
        else if wrapped < lengthFrames then wrapped else wrapped - lengthFrames

fitLoopPositions :: EnvironmentBank -> EnvironmentState -> EnvironmentState
fitLoopPositions bank state =
  state
    { windPosition = fit (windSample bank) (windPosition state),
      leavesPosition = fit (leavesSample bank) (leavesPosition state),
      waterPosition = fit (waterSample bank) (waterPosition state),
      wheelPosition = fit (wheelSample bank) (wheelPosition state)
    }
  where
    fit (Sample _ frames) position
      | position < fromIntegral frames = position
      | otherwise = position - fromIntegral (floor (position / fromIntegral frames) :: Int) * fromIntegral frames

advanceBreeze :: Double -> Double -> Double
advanceBreeze increment phase =
  let following = phase + increment
   in if following >= 2 * pi then following - 2 * pi else following

scaleStereo :: Double -> Stereo -> Stereo
scaleStereo gain (left, right) = (gain * left, gain * right)

addStereo :: Stereo -> Stereo -> Stereo
addStereo (left, right) (otherLeft, otherRight) = (left + otherLeft, right + otherRight)

ceilingStereo :: Stereo -> Stereo
ceilingStereo (left, right) = (max (-1) (min 1 left), max (-1) (min 1 right))

smoothstep :: Double -> Double
smoothstep value = let bounded = max 0 (min 1 value) in bounded * bounded * (3 - 2 * bounded)

finiteClamp :: Double -> Double -> Double -> Double -> Double
finiteClamp low high fallback value
  | isNaN value || isInfinite value = fallback
  | otherwise = max low (min high value)

sampleRate, maximumSpeed, carMix, forestAlpha, waterAlpha, wheelAlpha :: Double
sampleRate = 44100
maximumSpeed = 4.6 * 1.35
carMix = 0.90
forestAlpha = exp (-1 / (2 * sampleRate))
waterAlpha = exp (-1 / (2.4 * sampleRate))
wheelAlpha = exp (-1 / (0.35 * sampleRate))

maximumBlockFrames :: Int
maximumBlockFrames = 60 * 44100
