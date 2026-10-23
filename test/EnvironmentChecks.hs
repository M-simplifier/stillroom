module EnvironmentChecks (environmentChecks) where

import Data.Vector.Storable qualified as V
import Stillroom.Audio.Environment

-- Small resident banks keep these laws independent of installed assets,
-- decoders, native audio, and the prototype generator.
environmentChecks :: [(String, IO Bool)]
environmentChecks =
  [ ("environment PCM and state do not depend on block partition", pure partitionLaw),
    ("environment keeps sample phase when vehicle speed changes", pure speedPhaseOracle),
    ("environment fades to silence and freezes until re-entry", pure fadeAndFreeze),
    ("environment OnFoot stops car layers while nature continues", pure onFootLayers),
    ("environment rejects malformed banks and invalid frame counts", pure invalidBoundaries),
    ("environment sensors clamp with stationary and dry non-finite defaults", pure sensorBoundary)
  ]

sampleRate :: Int
sampleRate = 44100

keys :: [String]
keys = ["wind", "leaves", "water", "wheel", "body-1", "body-2", "body-3"]

emptyBankPcm :: [(String, V.Vector Float)]
emptyBankPcm = [(key, V.replicate 2 0) | key <- keys]

bankWith :: String -> V.Vector Float -> Either String EnvironmentBank
bankWith selected pcm = prepareEnvironmentBank [(key, if key == selected then pcm else quiet) | (key, quiet) <- emptyBankPcm]

tinyBank :: Either String EnvironmentBank
tinyBank = prepareEnvironmentBank [(key, V.fromList [0.01, -0.02, -0.01, 0.02, 0.005, -0.005]) | key <- keys]

moving :: Maybe EnvironmentInput
moving = Just (environmentInput True 4.6 0.7 4)

passed :: Either String Bool -> Bool
passed = either (const False) id

peak :: V.Vector Float -> Float
peak = V.foldl' (\largest value -> max largest (abs value)) 0

partitionLaw :: Bool
partitionLaw = passed $ do
  bank <- tinyBank
  let sizes = [1, 4096, 7, 11, 32768, 3, 44100, 9015]
      total = sum sizes
  (whole, wholeState) <- renderEnvironment bank moving initialEnvironment total
  (pieces, piecesState) <- chunks bank initialEnvironment sizes
  pure (whole == V.concat pieces && wholeState == piecesState)
  where
    chunks _ state [] = Right ([], state)
    chunks bank state (size : rest) = do
      (pcm, following) <- renderEnvironment bank moving state size
      (later, finalState) <- chunks bank following rest
      pure (pcm : later, finalState)

-- This oracle uses an integer starting position plus elapsed frame count.
-- A wrong implementation using absoluteTime * newRate predicts a different
-- sample, even if its own whole/chunk comparison still agrees.
speedPhaseOracle :: Bool
speedPhaseOracle = passed $ do
  let period = 1024
      tone = V.generate (2 * period) $ \index -> realToFrac (0.01 * sin (2 * pi * fromIntegral (index `div` 2) / fromIntegral period) :: Double)
  bank <- bankWith "wheel" tone
  (_, rolling) <- renderEnvironment bank moving initialEnvironment (2 * sampleRate)
  (changed, _) <- renderEnvironment bank (Just (environmentInput True 1.15 0.7 4)) rolling 2
  let alpha = exp (-1 / (0.35 * fromIntegral sampleRate))
      targetGain = 0.9 * 0.25 ** 0.82
      gain1 = targetGain + alpha * (0.9 - targetGain)
      gain2 = targetGain + alpha * (gain1 - targetGain)
      rate1 = 0.79 + alpha * (1 - 0.79)
      elapsedPosition = 136710 + 2 * sampleRate
      position = fromIntegral (elapsedPosition `mod` period)
      readTone at =
        let frame = floor at :: Int
            fraction = at - fromIntegral frame
            first = realToFrac (tone V.! (2 * (frame `mod` period))) :: Double
            next = realToFrac (tone V.! (2 * ((frame + 1) `mod` period))) :: Double
         in first * (1 - fraction) + next * fraction
      expected1 = realToFrac (gain1 * readTone position) :: Float
      expected2 = realToFrac (gain2 * readTone (position + rate1)) :: Float
      wrongAbsoluteTime = realToFrac (gain1 * readTone (fromIntegral elapsedPosition * rate1)) :: Float
  pure (abs (changed V.! 0 - expected1) < 2e-8 && abs (changed V.! 2 - expected2) < 2e-8 && abs (wrongAbsoluteTime - expected1) > 1e-4)

fadeAndFreeze :: Bool
fadeAndFreeze = passed $ do
  bank <- tinyBank
  (silent, initialFrozen) <- renderEnvironment bank Nothing initialEnvironment 4096
  (entry, entered) <- renderEnvironment bank moving initialEnvironment (2 * sampleRate)
  (release, released) <- renderEnvironment bank Nothing entered (2 * sampleRate)
  (laterSilence, laterState) <- renderEnvironment bank Nothing released 4096
  (resume, resumed) <- renderEnvironment bank moving released 4096
  (laterResume, laterResumed) <- renderEnvironment bank moving laterState 4096
  pure
    ( initialFrozen == initialEnvironment
        && V.all (== 0) silent
        && entry V.! 0 == 0
        && entry V.! 1 == 0
        && peak entry > 0
        && V.all (== 0) (V.drop (sampleRate * 7 `div` 2) release)
        && V.all (== 0) laterSilence
        && released == laterState
        && resume == laterResume
        && resumed == laterResumed
    )

onFootLayers :: Bool
onFootLayers = passed $ do
  wheelBank <- bankWith "wheel" (V.replicate 2 0.01)
  windBank <- bankWith "wind" (V.replicate 2 0.01)
  bodyBank <- prepareEnvironmentBank [(key, if take 5 key == "body-" then V.replicate 2 0.01 else quiet) | (key, quiet) <- emptyBankPcm]
  let foot = Just (environmentInput False 6.21 0.3 4)
  (_, rolling) <- renderEnvironment wheelBank moving initialEnvironment (2 * sampleRate)
  (stoppedWheel, _) <- renderEnvironment wheelBank foot rolling (5 * sampleRate)
  (standingNature, _) <- renderEnvironment windBank foot initialEnvironment (2 * sampleRate)
  (standingBody, _) <- renderEnvironment bodyBank foot initialEnvironment (2 * sampleRate)
  pure (peak (V.drop (8 * sampleRate) stoppedWheel) < 2e-7 && peak standingNature > 0.005 && V.all (== 0) standingBody)

invalidBoundaries :: Bool
invalidBoundaries = passed $ do
  bank <- tinyBank
  oneFrameBank <- prepareEnvironmentBank [(key, V.replicate 2 0.01) | key <- keys]
  (oneFrameSignal, _) <- renderEnvironment oneFrameBank (Just (environmentInput True 6.21 1 0)) initialEnvironment 4096
  (empty, unchanged) <- renderEnvironment bank moving initialEnvironment 0
  let malformed =
        [ [],
          drop 1 emptyBankPcm,
          ("wind", V.replicate 2 0) : emptyBankPcm,
          ("extra", V.replicate 2 0) : emptyBankPcm,
          ("wind", V.empty) : drop 1 emptyBankPcm,
          ("wind", V.replicate 3 0) : drop 1 emptyBankPcm,
          ("wind", V.fromList [0 / 0, 0]) : drop 1 emptyBankPcm,
          ("wind", V.fromList [1 / 0, 0]) : drop 1 emptyBankPcm,
          ("wind", V.fromList [-1 / 0, 0]) : drop 1 emptyBankPcm,
          ("wind", V.fromList [1.01, 0]) : drop 1 emptyBankPcm
        ]
  pure
    ( all (isLeft . prepareEnvironmentBank) malformed
        && all (isLeft . renderEnvironment bank moving initialEnvironment) [-1, maxBound, 60 * sampleRate + 1]
        && V.null empty
        && unchanged == initialEnvironment
        && V.all (\value -> not (isNaN value || isInfinite value)) oneFrameSignal
    )
  where
    isLeft (Left _) = True
    isLeft _ = False

sensorBoundary :: Bool
sensorBoundary = passed $ do
  bank <- tinyBank
  let observe sensors = renderEnvironment bank (Just sensors) initialEnvironment 4096
      safe = observe (environmentInput True 0 0 34)
      invalidSensors = [0 / 0, 1 / 0, -1 / 0]
  pure
    ( all (\invalid -> observe (environmentInput True invalid invalid invalid) == safe) invalidSensors
        && observe (environmentInput True 1e300 3 (-1e300)) == observe (environmentInput True (4.6 * 1.35) 1 0)
        && observe (environmentInput True (-1) 0.3 4) == observe (environmentInput True 0 0.3 4)
    )
