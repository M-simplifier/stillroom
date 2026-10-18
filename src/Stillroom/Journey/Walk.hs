{-# LANGUAGE ForeignFunctionInterface #-}

module Stillroom.Journey.Walk
  ( Mode (..),
    Walker (..),
    Input (..),
    neutral,
    at,
    step,
    tickSeconds,
    terrainHeight,
    canStand,
    interpolate,
    basis,
    position,
    field,
    shoreDistance,
    simulate,
    Habitat (..),
    habitatAt,
  )
where

import Data.List (foldl')
import Foreign.C (CDouble (..), CInt (..), CUInt (..))
import Stillroom.Journey.Navigation
import Stillroom.Journey.World (Seed, seedValue, withinWorld)

data Habitat = Meadow | Birchwood | Pines | Heath | Wetland | Dunes deriving (Eq, Ord, Show, Enum, Bounded)

foreign import ccall unsafe "sr_journey_biome" cBiome :: CUInt -> CDouble -> CDouble -> CInt

habitatAt :: Seed -> Double -> Double -> Habitat
habitatAt seed x z = case cBiome (fromIntegral (seedValue seed)) (realToFrac x) (realToFrac z) of
  1 -> Birchwood
  2 -> Pines
  3 -> Heath
  4 -> Wetland
  5 -> Dunes
  _ -> Meadow

-- Pure, stateless field queries, also used by the native geometry generator.
foreign import ccall unsafe "sr_journey_ground" cGround :: CUInt -> CDouble -> CDouble -> CDouble

foreign import ccall unsafe "sr_journey_stand" cStand :: CUInt -> CDouble -> CDouble -> CInt

foreign import ccall unsafe "sr_journey_interest" cInterest :: CUInt -> CDouble -> CDouble -> CDouble

foreign import ccall unsafe "sr_journey_shore" cShore :: CUInt -> CDouble -> CDouble -> CDouble

terrainHeight :: Seed -> Double -> Double -> Double
terrainHeight seed x z = realToFrac (cGround (fromIntegral (seedValue seed)) (realToFrac x) (realToFrac z))

canStand :: Seed -> Double -> Double -> Bool
canStand seed x z = withinWorld x z && cStand (fromIntegral (seedValue seed)) (realToFrac x) (realToFrac z) == 1

shoreDistance :: Seed -> Double -> Double -> Double
shoreDistance seed x z = realToFrac (cShore (fromIntegral (seedValue seed)) (realToFrac x) (realToFrac z))

field :: Seed -> Field
field seed = Field (\(Point x z) -> canStand seed x z) (\(Point x z) -> terrainHeight seed x z) (\(Point x z) -> realToFrac (cInterest (fromIntegral (seedValue seed)) (realToFrac x) (realToFrac z)))

data Mode = Manual | Strolling deriving (Eq, Show)

data Walker = Walker
  { posX :: !Double,
    posZ :: !Double,
    eyeY :: !Double,
    yaw :: !Double,
    pitch :: !Double,
    velocityX :: !Double,
    velocityZ :: !Double,
    walkingMode :: !Mode,
    gazeOffset :: !Double,
    ticks :: !Int,
    bodyHeading :: !Double,
    route :: !Route,
    visited :: ![Point],
    blockedTicks :: !Int
  }
  deriving (Eq, Show)

data Input = Input {forward :: !Double, sideways :: !Double, lookX :: !Double, lookY :: !Double, toggleStroll :: !Bool} deriving (Eq, Show)

neutral :: Input
neutral = Input 0 0 0 0 False

tickSeconds :: Double
tickSeconds = 1 / 60

position :: Walker -> Point
position s = Point (posX s) (posZ s)

at :: Seed -> Double -> Double -> Double -> Maybe Walker
at seed x z facing
  | not (canStand seed x z) = Nothing
  | otherwise = Just (Walker x z (terrainHeight seed x z + 1.68) facing (-0.025) 0 0 Manual 0 0 facing emptyRoute [] 0)

basis :: Double -> ((Double, Double), (Double, Double))
basis a = ((sin a, -cos a), (cos a, sin a))

step :: Seed -> Input -> Walker -> Walker
step seed input old =
  let moving = abs (forward input) + abs (sideways input) > 0.01
      toggled = if toggleStroll input then (if walkingMode old == Manual then Strolling else Manual) else walkingMode old
      mode = if moving then Manual else toggled
      entered = mode == Strolling && walkingMode old == Manual
      p = position old
      heading0 = if entered then yaw old else bodyHeading old
      seen = remember p (visited old)
      oldRoute = if entered then emptyRoute {generation = generation (route old)} else followRoute p (route old)
      arrived = not (null (waypoints (route old))) && null (waypoints oldRoute)
      approaching = intent oldRoute == Exploring && (case waypoints oldRoute of [q] -> distance p q < 3.0; _ -> False)
      needsRoute = approaching || (null (waypoints oldRoute) && (entered || arrived || generation oldRoute == 0 || ticks old `mod` 30 == 0)) || blockedTicks old > 45
      newRoute = if mode == Strolling && needsRoute then planRoute (field seed) p heading0 seen (generation oldRoute) else oldRoute
      desired = case waypoints newRoute of q : _ -> direction p q; [] -> heading0
      body = if mode == Strolling then maxTurn (0.48 * tickSeconds) desired heading0 else yaw old + lookX input * 0.0021
      oldOffset = if entered then 0 else gazeOffset old
      lookDelta = lookX input * 0.0021
      offset = oldOffset + lookDelta
      facing = if mode == Strolling then maxTurn (0.32 * tickSeconds) (body + oldOffset) (yaw old) + lookDelta else yaw old + lookDelta
      tilt = clamp (-1.15) 1.05 (pitch old - lookY input * 0.0019)
      ((fx, fz), (rx, rz)) = basis facing
      turn = abs (angleDifference desired body)
      pace = if null (waypoints newRoute) then 0 else 1.28 * clamp 0 1 (1 - turn / 0.85)
      (targetX, targetZ) =
        if mode == Strolling
          then (sin body * pace, -cos body * pace)
          else let mx = fx * forward input + rx * sideways input; mz = fz * forward input + rz * sideways input; m = max 1 (sqrt (mx * mx + mz * mz)) in (mx / m * 1.55, mz / m * 1.55)
      vx = velocityX old + (targetX - velocityX old) * 0.13
      vz = velocityZ old + (targetZ - velocityZ old) * 0.13
      candidateX = posX old + vx * tickSeconds
      candidateZ = posZ old + vz * tickSeconds
      (x, z) =
        if canStand seed candidateX candidateZ
          then (candidateX, candidateZ)
          else
            if canStand seed candidateX (posZ old)
              then (candidateX, posZ old)
              else
                if canStand seed (posX old) candidateZ
                  then (posX old, candidateZ)
                  else (posX old, posZ old)
      blocked = if needsRoute then 0 else if mode == Strolling && pace > 0.15 && distance p (Point x z) < pace * tickSeconds * 0.35 then blockedTicks old + 1 else max 0 (blockedTicks old - 2)
      y = eyeY old + (terrainHeight seed x z + 1.68 - eyeY old) * 0.14
   in Walker x z y facing tilt vx vz mode offset (ticks old + 1) body newRoute seen blocked

interpolate :: Double -> Walker -> Walker -> Walker
interpolate amount a b =
  let t = clamp 0 1 amount; mix x y = x + (y - x) * t
   in b {posX = mix (posX a) (posX b), posZ = mix (posZ a) (posZ b), eyeY = mix (eyeY a) (eyeY b), yaw = yaw a + angleDifference (yaw b) (yaw a) * t, pitch = mix (pitch a) (pitch b)}

-- One observed frame may execute several semantic ticks. Relative mouse
-- movement is distributed, while a mode-switch edge is consumed exactly once.
simulate :: Seed -> Int -> Input -> Walker -> Walker -> (Walker, Walker)
simulate seed count input previous current
  | count <= 0 = (previous, current)
  | otherwise = foldl' one (previous, current) [0 .. count - 1]
  where
    one (_, s) i = let next = step seed (input {lookX = lookX input / fromIntegral count, lookY = lookY input / fromIntegral count, toggleStroll = toggleStroll input && i == 0}) s in (s, next)
