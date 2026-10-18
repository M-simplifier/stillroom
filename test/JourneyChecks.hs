{-# LANGUAGE ForeignFunctionInterface #-}

module JourneyChecks (journeyChecks) where

import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Data.List (foldl', nub)
import Data.Maybe (isJust, listToMaybe)
import Data.Word (Word32)
import Foreign.C (CDouble (..), CUInt (..))
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.JourneyStorage qualified as S
import Test.QuickCheck

-- An IO view prevents common-subexpression elimination in the interleaving
-- test. It exercises the exact public query used by the pure FFI wrapper.
foreign import ccall unsafe "sr_car_track_y" queryY :: CUInt -> CDouble -> IO CDouble

advance :: Int -> J.Input -> J.Journey -> J.Journey
advance count input initial = foldl' (\s _ -> J.step input s) initial [1 .. count]

journeyChecks :: IO Bool
journeyChecks = do
  checks <-
    sequence
      [ law "journey bookmarks round-trip across independently seeded worlds" propBookmark,
        law "native field cache is keyed by the supplied seed" propSeedCache,
        law "different seeds change terrain and track, without changing the first world" propVariety,
        law "fixed-tick journey semantics do not depend on frame partition" propPartition,
        law "corrupt journey bookmarks are rejected before native queries" (once propInvalid),
        law "long journeys stop at a restorable coordinate boundary" propBoundary,
        law "ride catches up edges once and preserves total mouse motion" (once propInput),
        law "automatic walkers stay on navigable ground across world seeds" (once (ioProperty automaticChecks)),
        law "continuous ride and safe rail-to-foot transfers" (once (ioProperty transferChecks))
      ]
  pure (all isSuccess checks)
  where
    law title p = putStrLn title >> quickCheckWithResult stdArgs {maxSuccess = 120} p

propBookmark :: Word32 -> NonNegative Int -> Bool
propBookmark n (NonNegative raw) =
  let j = J.at (World.seed n) (fromIntegral (raw `mod` 100000))
      saved = S.bookmark j
   in decode (encode saved) == Just saved && (S.bookmark <$> S.restore saved) == Just saved

propSeedCache :: Word32 -> Word32 -> Small Int -> Property
propSeedCache a b (Small index) = ioProperty $ do
  let s = fromIntegral index * 32
  y <- queryY (fromIntegral a) s
  _ <- queryY (fromIntegral b) s
  restored <- queryY (fromIntegral a) s
  pure (y == restored && not (isNaN (realToFrac y :: Double)))

propVariety :: Word32 -> Bool
propVariety a =
  let values n = [(J.trackX (World.seed n) s, W.terrainHeight (World.seed n) 22 (-s)) | s <- [0, 256, 512]]
   in values a /= values (a + 1)

propPartition :: Word32 -> Positive Int -> Bool
propPartition n (Positive raw) =
  let size = 1 + raw `mod` 120
      initial = J.initial (World.seed n)
      (_, whole) = J.simulate size J.neutral initial initial
      (_, pieces) = foldl' (\(p, c) count -> J.simulate count J.neutral p c) (initial, initial) [size `div` 2, size - size `div` 2]
   in whole == pieces

propInvalid :: Bool
propInvalid =
  let b = S.bookmark (J.initial World.firstSeed)
      invalid = [b {S.railPosition = 0 / 0}, b {S.x = 1 / 0}, b {S.railPosition = (-1)}, b {S.railPosition = 1e8}, b {S.tilt = 9}, b {S.aboard = False, S.x = 1e8}]
   in all ((== Nothing) . S.restore) invalid

propInput :: Bool
propInput =
  let initial = J.initial World.firstSeed
      (_, stopped) = J.simulate 6 (J.neutral {J.walking = W.neutral {W.toggleStroll = True}}) initial initial
      (_, looked) = J.simulate 6 (J.neutral {J.walking = W.neutral {W.lookX = 100}}) initial initial
   in not (J.running (J.car stopped)) && abs (W.gazeOffset (J.person looked) - 0.21) < 1e-9

propBoundary :: Word32 -> Bool
propBoundary n =
  let seed = World.seed n
      start = J.at seed (World.coordinateLimit - 0.001)
      moving = start {J.car = (J.car start) {J.speed = 6.2}, J.person = (J.person start) {W.gazeOffset = 1e6}}
      stopped = advance 8 J.neutral moving
      saved = S.bookmark stopped
   in J.progress (J.car stopped) == World.coordinateLimit
        && J.speed (J.car stopped) == 0
        && not (J.running (J.car stopped))
        && (S.bookmark <$> S.restore saved) == Just saved
        && not (W.canStand seed (World.coordinateLimit + 1) 0)
        && not (W.canStand seed 0 (-World.coordinateLimit - 1))

automaticChecks :: IO Bool
automaticChecks = and <$> mapM one [World.firstSeed, World.seed 0, World.seed 1, World.seed 3735928559]
  where
    one seed = do
      start <-
        maybe (ioError (userError "No safe automatic-walk start")) pure $
          listToMaybe [w | s <- [80, 82 .. 1000], Just w <- [J.landing seed (J.car (J.at seed s))]]
      let walking = W.step seed (W.neutral {W.toggleStroll = True}) start
          states = take 901 (iterate (W.step seed W.neutral) walking)
      pure
        ( all (\w -> W.canStand seed (W.posX w) (W.posZ w)) states
            && any ((/= W.position start) . W.position) states
        )

transferChecks :: IO Bool
transferChecks = do
  let seed = World.firstSeed
      ride = take 36001 (iterate (J.step J.neutral) (J.initial seed))
      final = advance 36000 J.neutral (J.initial seed)
      habitats = nub [W.habitatAt seed (W.posX (J.person j)) (W.posZ (J.person j)) | (i, j) <- zip [0 :: Int ..] ride, i `mod` 60 == 0]
  assert "ten minute ride crosses at least three habitats" (J.progress (J.car final) > 2500 && length habitats >= 3)
  assert "passenger follows generated track" (all (\j -> abs (W.eyeY (J.person j) - J.trackY seed (J.progress (J.car j)) - 2) < 1e-8) ride)
  assert "gentle speed and acceleration" (all (\j -> J.speed (J.car j) >= 0 && J.speed (J.car j) <= 6.2 && abs (J.acceleration (J.car j)) <= 0.5) ride)
  place <- maybe (ioError (userError "No test landing")) pure $ listToMaybe [s | s <- [80, 82 .. 1000], isJust (J.landing seed (J.car (J.at seed s)))]
  let parked = J.pause (J.at seed place)
      outside = advance 45 J.neutral (J.step (J.neutral {J.access = True}) parked)
      walking = advance 45 (J.neutral {J.walking = W.neutral {W.forward = 1}}) outside
      returned = advance 45 (J.neutral {J.walking = W.neutral {W.forward = (-1)}}) walking
      boarded = advance 45 J.neutral (J.step (J.neutral {J.access = True}) returned)
      w = J.person outside
  assert "alighting lands on dry navigable ground" (J.seat outside == J.OnFoot && W.canStand seed (W.posX w) (W.posZ w))
  assert "walk and return to the same parked car" (J.progress (J.car returned) == place && W.position (J.person walking) /= W.position w && J.seat boarded == J.Aboard)
  assert "walking bookmark restores a valid ground position" (isJust (S.restore (S.bookmark outside)))
  assert "parked car blocks walking through it" (all (\j -> J.clearOfCar seed (J.car j) (J.person j)) (take 301 (iterate (J.step (J.neutral {J.walking = W.neutral {W.forward = 1}})) outside)))
  bridge <- maybe (ioError (userError "No test bridge")) pure $ listToMaybe [s | s <- [100, 102 .. 3000], not (isJust (J.landing seed (J.car (J.at seed s))))]
  let refused = advance 45 J.neutral (J.step (J.neutral {J.access = True}) (J.pause (J.at seed bridge)))
  assert "unsafe landing stays aboard and can resume" (J.seat refused == J.Aboard && J.running (J.car (J.step (J.neutral {J.walking = W.neutral {W.toggleStroll = True}}) refused)))
  pure True
  where
    assert name ok = unless ok (ioError (userError name))
