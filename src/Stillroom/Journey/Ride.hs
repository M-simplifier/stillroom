{-# LANGUAGE ForeignFunctionInterface #-}

-- Fixed-tick passenger/vehicle semantics. Native queries are stateless and
-- share the exact track field with geometry; GPU ownership stays in the runtime.
module Stillroom.Journey.Ride where

import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Foreign.C (CDouble (..), CUInt (..))
import Stillroom.Journey.Navigation (clamp, emptyRoute)
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World (Seed, coordinateLimit, seedValue)

foreign import ccall unsafe "sr_car_track_x" trackXC :: CUInt -> CDouble -> CDouble

foreign import ccall unsafe "sr_car_track_y" trackYC :: CUInt -> CDouble -> CDouble

trackX, trackY, trackHeading, trackMetric :: Seed -> Double -> Double
trackX seed = realToFrac . trackXC (fromIntegral (seedValue seed)) . realToFrac
trackY seed = realToFrac . trackYC (fromIntegral (seedValue seed)) . realToFrac
trackHeading seed s = atan2 ((trackX seed (s + 0.1) - trackX seed (s - 0.1)) / 0.2) 1
trackMetric seed s = sqrt (1 + dx * dx + dy * dy)
  where
    dx = (trackX seed (s + 0.1) - trackX seed (s - 0.1)) / 0.2
    dy = (trackY seed (s + 0.1) - trackY seed (s - 0.1)) / 0.2

data Seat = Aboard | OnFoot deriving (Eq, Show)

data Pace = Slow | Easy | Brisk deriving (Eq, Show, Read, Enum, Bounded)

paceSpeed :: Pace -> Double
paceSpeed Slow = 2.8
paceSpeed Easy = 4.6
paceSpeed Brisk = 6.2

shiftPace :: Int -> Pace -> Pace
shiftPace amount old = toEnum (max 0 (min 2 (fromEnum old + max (-1) (min 1 amount))))

data Railcar = Railcar {progress :: !Double, speed :: !Double, acceleration :: !Double, running :: !Bool, pace :: !Pace} deriving (Eq, Show)

data Journey = Journey
  { worldSeed :: !Seed,
    person :: !W.Walker,
    car :: !Railcar,
    seat :: !Seat,
    exitRequested :: !Bool,
    transferTicks :: !Int,
    transferTarget :: !(Maybe (Seat, W.Walker)),
    noticeTicks :: !Int
  }
  deriving (Eq, Show)

data Input = Input {walking :: !W.Input, access :: !Bool, changePace :: !Int} deriving (Eq, Show)

neutral :: Input
neutral = Input W.neutral False 0

at :: Seed -> Double -> Journey
at seed s = Journey seed (passenger seed c 0 (-0.035) 0) c Aboard False 0 Nothing 0
  where
    c = Railcar (clamp 0 coordinateLimit s) 0 0 (s < coordinateLimit) Easy

initial :: Seed -> Journey
initial seed = at seed 60

passenger :: Seed -> Railcar -> Double -> Double -> Int -> W.Walker
passenger seed c offset tilt time =
  let s = progress c; heading = trackHeading seed s
   in W.Walker (trackX seed s) (-s) (trackY seed s + 2.0) (heading + offset) tilt 0 0 W.Manual offset time heading emptyRoute [] 0

view :: Journey -> W.Walker
view = person

fade :: Journey -> Double
fade j = if transferTicks j == 0 then 0 else 1 - abs (fromIntegral (transferTicks j) - 18) / 18

-- Steps are only to nearby dry ground, never into water or down a bridge.
-- The parked vehicle remains exactly where it stopped.
landing :: Seed -> Railcar -> Maybe W.Walker
landing seed c =
  listToMaybe
    [ w {W.walkingMode = W.Manual}
    | side <- [1, -1 :: Double],
      longitudinal <- [0, -0.6, 0.6],
      let s = progress c; h = trackHeading seed s; x = trackX seed s + side * 2.65 * cos h + longitudinal * sin h; z = -s + side * 2.65 * sin h - longitudinal * cos h,
      abs (W.terrainHeight seed x z - (trackY seed s - 0.36)) < 0.8,
      all (\(dx, dz) -> W.canStand seed (x + dx) (z + dz)) [(0, 0), (0.24, 0), (-0.24, 0), (0, 0.24), (0, -0.24)],
      Just w <- [W.at seed x z (h - side * pi / 2)]
    ]

nearCar :: Journey -> Bool
nearCar j =
  let seed = worldSeed j; w = person j; c = car j; dx = W.posX w - trackX seed (progress c); dz = W.posZ w + progress c
   in dx * dx + dz * dz < 4.3 * 4.3 && abs (W.eyeY w - (trackY seed (progress c) + 1.68)) < 1.8

clearOfCar :: Seed -> Railcar -> W.Walker -> Bool
clearOfCar seed c w =
  let s = progress c
      h = trackHeading seed s
      dx = W.posX w - trackX seed s
      dz = W.posZ w + s
      across = dx * cos h + dz * sin h
      along = dx * sin h - dz * cos h
      feet = W.eyeY w - 1.68
   in abs across >= 1.38 || abs along >= 2.87 || feet > trackY seed s + 3.1 || W.eyeY w < trackY seed s + 0.15

beginTransfer :: Seat -> W.Walker -> Journey -> Journey
beginTransfer target w j = j {transferTicks = 36, transferTarget = Just (target, w), exitRequested = False, car = (car j) {running = False, speed = 0, acceleration = 0}, noticeTicks = 240}

pause :: Journey -> Journey
pause j = j {car = (car j) {running = False, speed = 0, acceleration = 0}, exitRequested = False, person = (person j) {W.velocityX = 0, W.velocityZ = 0, W.walkingMode = W.Manual}}

step :: Input -> Journey -> Journey
step input old
  | transferTicks old > 0 =
      let n = transferTicks old - 1
          switched = case transferTarget old of
            Just (target, destination) | n == 18 -> old {seat = target, person = destination {W.ticks = W.ticks (person old)}}
            _ -> old
       in switched {transferTicks = n, transferTarget = if n == 0 then Nothing else transferTarget old, person = (person switched) {W.ticks = W.ticks (person old) + 1}}
  | otherwise = interactWithCar advanced
  where
    seed = worldSeed old
    wi = walking input
    c0 = car old
    requested = if access input && seat old == Aboard then not (exitRequested old) else exitRequested old
    go = progress c0 < coordinateLimit && seat old == Aboard && not requested && (if W.toggleStroll wi then not (running c0) else running c0)
    selectedPace = shiftPace (changePace input) (pace c0)
    targetSpeed = if go then min (paceSpeed selectedPace) (sqrt (0.8 * max 0 (coordinateLimit - progress c0))) else 0
    desiredA = clamp (-0.5) 0.32 ((targetSpeed - speed c0) * 0.7)
    a = acceleration c0 + clamp (-0.45 * W.tickSeconds) (0.45 * W.tickSeconds) (desiredA - acceleration c0)
    v = clamp 0 6.2 (speed c0 + a * W.tickSeconds)
    stopped = targetSpeed == 0 && v < 0.018
    v1 = if stopped then 0 else v
    mid = progress c0 + v1 * W.tickSeconds / (2 * trackMetric seed (progress c0))
    nextProgress = min coordinateLimit (progress c0 + v1 * W.tickSeconds / trackMetric seed mid)
    atEnd = nextProgress >= coordinateLimit
    c = Railcar nextProgress (if atEnd then 0 else v1) (if stopped || atEnd then 0 else a) (go && not atEnd) selectedPace
    w0 = person old
    walked = W.step seed wi w0
    w
      | seat old == Aboard = passenger seed c (W.gazeOffset w0 + W.lookX wi * 0.0021) (clamp (-1.12) 1.05 (W.pitch w0 - W.lookY wi * 0.0019)) (W.ticks w0 + 1)
      | clearOfCar seed c walked = walked
      | otherwise = walked {W.posX = W.posX w0, W.posZ = W.posZ w0, W.eyeY = W.eyeY w0, W.velocityX = 0, W.velocityZ = 0, W.walkingMode = W.Manual}
    advanced = old {car = c, person = w, exitRequested = requested, noticeTicks = max 0 (noticeTicks old - 1)}
    interactWithCar j
      | seat j == OnFoot && access input && nearCar j = beginTransfer Aboard (passenger seed c 0 (-0.035) (W.ticks w)) j
      | seat j == Aboard && requested && speed c == 0 = case landing seed c of
          Just ground -> beginTransfer OnFoot ground j
          Nothing -> j {exitRequested = False, noticeTicks = 360}
      | otherwise = j

interpolate :: Double -> Journey -> Journey -> Journey
interpolate amount a b
  | seat a /= seat b = b
  | otherwise = b {person = W.interpolate amount (person a) (person b), car = (car b) {progress = progress (car a) + (progress (car b) - progress (car a)) * clamp 0 1 amount}}

simulate :: Int -> Input -> Journey -> Journey -> (Journey, Journey)
simulate count input previous current
  | count <= 0 = (previous, current)
  | otherwise = foldl' one (previous, current) [0 .. count - 1]
  where
    wi = walking input
    one (_, s) i = let once = input {walking = wi {W.lookX = W.lookX wi / fromIntegral count, W.lookY = W.lookY wi / fromIntegral count, W.toggleStroll = W.toggleStroll wi && i == 0}, access = access input && i == 0, changePace = if i == 0 then changePace input else 0}; next = step once s in (s, next)
