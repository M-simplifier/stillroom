module AudioChecks (audioChecks) where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Set qualified as Set
import Data.Vector.Storable qualified as V
import Data.Word (Word64)
import GHC.Float (castFloatToWord32)
import Stillroom.Audio.Composition (compositionEvents, requiredKeys)
import Stillroom.Audio.Mix
import Stillroom.Domain (Preset (..))
import Stillroom.Runtime.Audio.Bank
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Timeout (timeout)

audioChecks :: [(String, IO Bool)]
audioChecks =
  [ ("sample mixer matches independent stereo/start/end/addition oracle", pure mixOracle)
  , ("sample mixer explicitly rejects invalid values and missing keys", pure invalidMix)
  , ("PCM16 input matches known RIFF bytes and rejects malformed format", pure wavOracle)
  , ("sample loader resolves only requested keys and reports absence", selectedLoad)
  , ("composition references and packaged music assets satisfy requiredKeys", keysComplete)
  , ("composition PCM is invariant across representative block partitions", pure compositionPartitions)
  , ("composition queries remain bounded at far future Integer epochs", boundedEpochs)
  , ("composition seed changes PCM while repeated queries remain identical", pure seedVariation)
  ]

mixOracle :: Bool
mixOracle = either (const False) id $ do
  bank <- mkBank [("a",V.fromList [1,0.5,-0.5,0.25]),("b",V.fromList [0.25,-0.75])]
  a <- mkEvent 2 "a" 1 (-1)
  b <- mkEvent 2 "b" 1 1
  doubled <- mkEvent 2 "a" 2 (-1)
  centered <- mkEvent 2 "a" (sqrt 2) 0
  result <- renderMix bank [a,b] 0 6
  unclipped <- renderMix bank [a,doubled] 2 1
  center <- renderMix bank [centered] 2 2
  partial <- renderMix bank [a,b] 3 2
  let far = 10 ^ (100 :: Int)
  late <- mkEvent far "b" 1 1
  farResult <- renderMix bank [late] far 2
  pure $ result == V.fromList [0,0,0,0,1,-0.75,-0.5,0,0,0,0,0]
    && unclipped == V.fromList [3,0]
    && center == V.fromList [1,0.5,-0.5,0.25]
    && partial == V.fromList [-0.5,0,0,0]
    && farResult == V.fromList [0,-0.75,0,0]
    && bankTailFrames bank == 2

invalidMix :: Bool
invalidMix = and
  [ rejects (\e -> case e of InvalidSampleValue {} -> True; _ -> False) (mkBank [("x",V.fromList [0/0,0])])
  , rejects (== OddSampleValueCount "x" 1) (mkBank [("x",V.singleton 0)])
  , rejects (== EmptySample "x") (mkBank [("x",V.empty)])
  , rejects (== InvalidSampleKey "../x") (mkEvent 0 "../x" 1 0)
  , rejects (== NegativeEventStart (-1)) (mkEvent (-1) "x" 1 0)
  , rejects (== InvalidEventGain (-1)) (mkEvent 0 "x" (-1) 0)
  , rejects (== InvalidEventPan 2) (mkEvent 0 "x" 1 2)
  , either (const False) id $ do
      bank <- mkBank []
      missing <- mkEvent 100 "absent" 0 0
      pure $ rejects (== MissingSample "absent") (renderMix bank [missing] 0 0)
        && rejects (== NegativeRenderStart (-1)) (renderMix bank [] (-1) 1)
        && rejects (== InvalidRenderFrameCount (-1)) (renderMix bank [] 0 (-1))
  , either (const False) id $ do
      bank <- mkBank [("x",V.fromList [1,0])]
      huge <- mkEvent 0 "x" 1e300 (-1)
      pure (rejects (== NonFiniteMixValue 0) (renderMix bank [huge] 0 1))
  ]

rejects :: (e -> Bool) -> Either e a -> Bool
rejects matches (Left failure) = matches failure
rejects _ (Right _) = False

-- Independent hand-authored mono PCM16LE RIFF: -32768 and +32767 at44100 Hz.
-- These bytes do not come from an encoder paired with the decoder under test.
monoGolden :: BS.ByteString
monoGolden = BS.pack
  [82,73,70,70,40,0,0,0,87,65,86,69,102,109,116,32,16,0,0,0
  ,1,0,1,0,68,172,0,0,136,88,1,0,2,0,16,0,100,97,116,97,4,0,0,0,0,128,255,127]

wavOracle :: Bool
wavOracle =
  decodePcm16Wav monoGolden == Right (V.fromList [-1,-1,32767/32768,32767/32768])
  && rejects (== NotRiffWave) (decodePcm16Wav (replaceByte 0 0 monoGolden))
  && rejects (\e -> case e of UnsupportedFormat {} -> True; _ -> False) (decodePcm16Wav (replaceByte 20 3 monoGolden))
  && rejects (\e -> case e of InconsistentFormat {} -> True; _ -> False) (decodePcm16Wav (replaceByte 28 0 monoGolden))
  && rejects (\e -> case e of RiffSizeMismatch {} -> True; _ -> False) (decodePcm16Wav (BS.take 45 monoGolden))
  where replaceByte index value bytes = BS.take index bytes <> BS.singleton value <> BS.drop (index + 1) bytes

selectedLoad :: IO Bool
selectedLoad = do
  visited <- newIORef []
  let resolve key = modifyIORef' visited (++ [key]) >> pure (musicPath key)
  result <- loadSamples resolve ["kick-0"]
  names <- readIORef visited
  duplicate <- loadSamples (\_ -> fail "duplicate key unexpectedly resolved") ["x","x"]
  missing <- readPcm16Wav (musicPath "__absent-audio-check__")
  pure $ names == ["kick-0"]
    && either (const False) (\samples -> case samples of [("kick-0",pcm)] -> not (V.null pcm); _ -> False) result
    && rejects (== BankValidationFailure (DuplicateSampleKey "x")) duplicate
    && rejects (\e -> case e of SampleReadFailure {} -> True; _ -> False) missing

musicPath :: String -> FilePath
musicPath key = "assets" </> "audio" </> "music" </> (key ++ ".wav")

presets :: [Preset]
presets = [Linen,Night,Moss,Orbit]

queryCases :: [(Preset,Word64,Integer)]
queryCases = [(preset,seed,offset) | preset <- presets, seed <- [0,17,999], offset <- [0,10^ (12::Int),10^ (30::Int),10^ (100::Int)]]

keysComplete :: IO Bool
keysComplete = do
  present <- traverse (doesFileExist . musicPath) requiredKeys
  let keys = Set.fromList requiredKeys
      referenced = [eventKey event | (preset,seed,offset) <- queryCases, event <- compositionEvents preset seed offset 4096]
  pure $ not (null requiredKeys) && length requiredKeys == Set.size keys && and present
    && not (any ("glass-" `isPrefixOf`) requiredKeys)
    && all (`Set.member` keys) referenced

-- Air lasts almost24 seconds, so truncating the lookback to shorter voices
-- would lose a still-sounding note. Other samples are deliberately short.
-- The separate mixer oracle checks actual channel/key interpretation.
compositionPartitions :: Bool
compositionPartitions = either (const False) id $ do
  bank <- mkBank [(key,if "air-" `isPrefixOf` key then longTail else short) | key <- requiredKeys]
  comparisons <- sequence
    [ do
        let query at count = renderMix bank (compositionEvents preset 17 at count) at count
        whole <- query offset 8193
        first <- query offset 17
        second <- query (offset + 17) 4079
        third <- query (offset + 4096) 4097
        pure (sameBits whole (first <> second <> third))
    | preset <- presets, offset <- [0,16 * 44100 - 11,20 * 44100 - 11,96 * 44100 + 7,10^ (100::Int)] ]
  -- The authored opening Air note is still sounding at20 seconds. This
  -- observable prevents equal-but-incorrect truncated queries from passing
  -- the partition relation alone.
  let openingTail = any (\event -> eventStart event == 0 && "air-" `isPrefixOf` eventKey event)
        (compositionEvents Linen 17 (20 * 44100) 4096)
  pure (openingTail && and comparisons)
  where
    longTail = V.generate (2 * (24 * 44100 - 1)) (\i -> if even i then 0.125 else -0.0625)
    short = V.fromList [0.25,-0.5,0.125,-0.25]

sameBits :: V.Vector Float -> V.Vector Float -> Bool
sameBits left right = V.map castFloatToWord32 left == V.map castFloatToWord32 right

boundedEpochs :: IO Bool
boundedEpochs = do
  result <- timeout 2000000 (evaluate (all bounded queryCases))
  pure (result == Just True)
  where
    bounded (preset,seed,offset) =
      let events = compositionEvents preset seed offset 4096
          beginning = max 0 (offset - 24 * 44100)
      in length events <= 512
        && all (\event -> eventStart event >= beginning && eventStart event < offset + 4096) events

seedVariation :: Bool
seedVariation = either (const False) id $ do
  bank <- mkBank [(key,V.fromList [0.25,-0.5,0.125,-0.25]) | key <- requiredKeys]
  let render seed = renderMix bank (compositionEvents Linen seed 0 8) 0 8
  original <- render 0
  repeated <- render 0
  changed <- render 1
  pure (sameBits original repeated && not (sameBits original changed))
