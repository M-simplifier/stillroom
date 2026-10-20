-- | Stillroom's frame policy: one geometry description, observed by the
-- passenger, the sun, and the reflected eye. These three choices belong to
-- the journey, not to the reusable geometry/observation core.
module Stillroom.Journey.Frame
  ( module Stillroom.Graphics.Space,
    FrameInput (..), Targets (..), ImageSize (..), PlanError (..),
    GeometryContext, referenceEye, renderOrigin, semanticTime,
    FramePlan, geometry, targets, mainPass, shadowPass, reflectionPass,
    carTransform, sunDirection, Pass (..), planFrame, originOf, referenceOf,
    clipPoint, objectPoint, shadowTexel, shadowGrid, shadowDepthSpan,
    shadowBaseBias, shadowSlopeBias,
  )
where

import Stillroom.Graphics.Space
import Stillroom.Graphics.Observation

data Targets = Targets
  { rasterWidth :: !Int, rasterHeight :: !Int,
    shadowResolution :: !Int, reflectionWidth :: !Int, reflectionHeight :: !Int
  } deriving (Eq, Show)

data FrameInput = FrameInput
  { eye :: !(Point World), yaw :: !Double, pitch :: !Double,
    seconds :: !Double, carPosition :: !(Point World),
    carHeading :: !Double, carGrade :: !Double
  } deriving (Eq, Show)

data GeometryContext = GeometryContext
  { geometryReference :: !(Point World), geometryOrigin :: !(Point World),
    geometryTime :: !Double
  } deriving (Eq, Show)

data Pass = Main | Shadow | Reflection deriving (Eq, Show, Enum, Bounded)
data FramePlan = FramePlan
  { frameGeometry :: !GeometryContext, frameTargets :: !Targets,
    frameMain :: !(Observation ImageSize), frameShadow :: !(Observation ImageSize), frameReflection :: !(Observation ImageSize),
    frameSun :: !(Direction Render),
    frameCar :: !(Transform Object Render),
    frameShadowTexel :: !Double, frameShadowGrid :: !(Integer, Integer)
  } deriving (Eq, Show)


data ImageSize = ImageSize !Int !Int deriving (Eq, Show)

data PlanError
  = NonFiniteInput | InvalidTargets | OutsideWorld | OutsideVerticalRange
  | DegenerateCamera | ObservationFailure !ObservationError
  deriving (Eq, Show)

originOf, referenceOf :: FramePlan -> Point World
originOf = geometryOrigin . frameGeometry
referenceOf = geometryReference . frameGeometry

planFrame :: Targets -> FrameInput -> Either PlanError FramePlan
planFrame ts input
  | not (all finite values) = Left NonFiniteInput
  | any (\n -> n < 1 || n > 16384) dimensions = Left InvalidTargets
  | maximum (map abs (pointList (eye input) ++ pointList (carPosition input))) > 1100000 =
      Left OutsideWorld
  | maximum (map abs [ey,carY]) > 1024 = Left OutsideVerticalRange
  | abs (pitch input) >= pi / 2 = Left DegenerateCamera
  | otherwise = do
      mainView <- either (Left . ObservationFailure) Right main
      lightView <- either (Left . ObservationFailure) Right light
      mirrorView <- either (Left . ObservationFailure) Right mirror
      pure (FramePlan context ts mainView lightView mirrorView sun car texel (gx, gy))
  where
    values = pointList (eye input) ++ pointList (carPosition input)
      ++ [yaw input, pitch input, seconds input, carHeading input, carGrade input]
    dimensions = [rasterWidth ts, rasterHeight ts, shadowResolution ts, reflectionWidth ts, reflectionHeight ts]
    Point ex ey ez = eye input
    Point _ carY _ = carPosition input
    origin = Point (cell ex) 0 (cell ez)
    cell n = fromInteger (floor (n / 256) :: Integer) * 256
    context = GeometryContext (eye input) origin (seconds input)
    localEye = relative origin (eye input)
    heading = yaw input
    tilt = pitch input
    forward = Direction (sin heading * cos tilt) (sin tilt) (-cos heading * cos tilt)
    main = projection (ImageSize (rasterWidth ts) (rasterHeight ts)) localEye forward
    Point lx ly lz = localEye
    Direction fx fy fz = forward
    mirror = projection (ImageSize (reflectionWidth ts) (reflectionHeight ts))
      (Point lx (-ly) lz) (Direction fx (-fy) fz)
    -- Quantize the fixed LIGHT basis, including vertical movement. Grid
    -- indices describe a world-anchored lattice, independent of render origin.
    sun = unit (Direction (-0.55) 0.57 0.38)
    right = unit (cross (Direction 0 1 0) sun)
    up = cross sun right
    texel = 96 / fromIntegral (shadowResolution ts)
    focus = Direction ex (ey - 1.68) ez
    gx = floor (dot right focus / texel + 0.5)
    gy = floor (dot up focus / texel + 0.5)
    center = plus (plus (scale (fromInteger gx * texel) right)
                         (scale (fromInteger gy * texel) up))
                  (scale (dot sun focus) sun)
    lightWorld = pointOf (plus center (scale 192 sun))
    light = observe (ImageSize (shadowResolution ts) (shadowResolution ts))
      (relative origin lightWorld) (scale (-1) sun) (ortho 48 0.1 384)
    car = objectTransform (relative origin (carPosition input)) (carHeading input) (carGrade input)

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

projection :: ImageSize -> Point Render -> Direction Render -> Either ObservationError (Observation ImageSize)
projection size@(ImageSize width height) p f =
  observe size p f (perspective (62 * pi / 180) (fromIntegral width / fromIntegral height) 0.15 1200)

projectionFor :: Pass -> FramePlan -> Observation ImageSize
projectionFor Main = frameMain
projectionFor Shadow = frameShadow
projectionFor Reflection = frameReflection

clipPoint :: FramePlan -> Pass -> Point Render -> (Double, Double, Double, Double)
clipPoint frame pass p =
  let pr = projectionFor pass frame
   in transform (clipFromRender pr) p

objectPoint :: FramePlan -> Point Object -> Point Render
objectPoint frame p = let (x,y,z,w) = transform (frameCar frame) p in Point (x/w) (y/w) (z/w)


-- Physical world lengths; the encoding boundary converts bias to the shader's
-- convention. Keep the depth range and its span in the same product policy.
shadowDepthSpan, shadowBaseBias, shadowSlopeBias :: Double
shadowDepthSpan = 384 - 0.1
shadowBaseBias = 0.004
shadowSlopeBias = 0.025


-- Read-only queries preserve the validated plan. These are not record labels:
-- clients cannot replace a context, observation, or placement after planning.
referenceEye, renderOrigin :: GeometryContext -> Point World
referenceEye = geometryReference
renderOrigin = geometryOrigin
semanticTime :: GeometryContext -> Double
semanticTime = geometryTime
geometry :: FramePlan -> GeometryContext
geometry = frameGeometry
targets :: FramePlan -> Targets
targets = frameTargets
mainPass, shadowPass, reflectionPass :: FramePlan -> Observation ImageSize
mainPass = frameMain
shadowPass = frameShadow
reflectionPass = frameReflection
sunDirection :: FramePlan -> Direction Render
sunDirection = frameSun
carTransform :: FramePlan -> Transform Object Render
carTransform = frameCar
shadowTexel :: FramePlan -> Double
shadowTexel = frameShadowTexel
shadowGrid :: FramePlan -> (Integer,Integer)
shadowGrid = frameShadowGrid
