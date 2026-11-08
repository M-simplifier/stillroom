{-# LANGUAGE RankNTypes #-}

-- The eight Stillroom atmospheres from assets/rest/procedural.fs. Each scene
-- has its own pipeline. Its original bottom-left screen coordinate is derived
-- from Vulkan's top-left fragment coordinate before evaluating the scene.
module Stillroom.Runtime.Graphics.Atmosphere (atmosphereShader, atmosphereParameters) where

import GameIO.Graphics.Plan (Vec2, Vec3, Vec4(..))
import GameIO.Graphics.Shader
import Stillroom.Domain (RestScene(..))
import Stillroom.Runtime.Graphics.Shader

atmosphereParameters :: Float -> Float -> Double -> [Vec4]
atmosphereParameters width height seconds =
  let cycles = seconds / 240
      phase = 2 * pi * (cycles - fromIntegral (floor cycles :: Integer))
  in [Vec4 width height (realToFrac phase) 0]

atmosphereShader :: RestScene -> Maybe FragmentShader
atmosphereShader scene = case scene of
  TidalLight -> atmosphere (waterScene Tidal)
  LightVeil -> atmosphere veilScene
  StarryNight -> atmosphere nightScene
  AmberSea -> atmosphere (waterScene Amber)
  Shallows -> atmosphere (waterScene Shallow)
  CloudSea -> atmosphere cloudScene
  Aurora -> atmosphere auroraScene
  InkFlow -> atmosphere inkScene
  _ -> Nothing

atmosphere :: (forall local. F local Vec4 -> F local Vec2 -> F local Vec3) -> Maybe FragmentShader
atmosphere paint = do
  parameters <- uniformSlot 0
  origin <- uniformSlot 1
  pure $ FragmentShader $ build $ do
    p <- share (Uniform parameters)
    offset <- share (Uniform origin)
    fragment <- share FragmentCoordinates
    uv <- share (v2 ((x4 fragment - x4 offset) / x4 p) (1 - (y4 fragment - y4 offset) / y4 p))
    edge <- share (scale2 (sub2 uv (splat2 0.5)) 2)
    let vignette = 1 - 0.10 * Dot Vector2 edge edge
    pure (rgba (clamp3 (scale3 (paint p uv) vignette) 0 1) 1)

type S local = F local Float

tau :: S local
tau = 6.28318530718

aspectPoint :: F local Vec4 -> F local Vec2 -> F local Vec2
aspectPoint p uv = mul2 (sub2 uv (splat2 0.5)) (v2 (x4 p / y4 p) 1)

hash2 :: F local Vec2 -> S local
hash2 p = build $ do
  uv <- share p
  q <- share (fract3 (mul3 (v3 (x2 uv) (y2 uv) (x2 uv)) (v3 0.1031 0.1030 0.0973)))
  shifted <- share (add3 q (splat3 (Dot Vector3 q (add3 (v3 (y3 q) (z3 q) (x3 q)) (splat3 19.19)))))
  pure (fractF ((x3 shifted + y3 shifted) * z3 shifted))

noise2 :: F local Vec2 -> S local
noise2 point = build $ do
  p <- share point
  cell <- share (floor2 p)
  frac <- share (fract2 p)
  blend <- share (mul2 (mul2 frac frac) (sub2 (splat2 3) (scale2 frac 2)))
  pure (cellNoise cell (x2 blend) (y2 blend))

cellNoise :: F local Vec2 -> S local -> S local -> S local
cellNoise cell u v =
  mixF (mixF (hash2 cell) (hash2 (add2 cell (v2 1 0))) u)
       (mixF (hash2 (add2 cell (v2 0 1))) (hash2 (add2 cell (v2 1 1))) u) v

mist :: F local Vec2 -> S local
mist point = letExpr point $ \p -> z4 $
  repeatN Float4 5 (v4 (x2 p) (y2 p) 0 0.5) $ \_ state -> build $ do
    position <- share (v2 (x4 state) (y4 state))
    next <- share (add2 (scale2 (rotate2 0.80 0.60 position) 2.03) (v2 7.2 4.1))
    pure (v4 (x2 next) (y2 next) (z4 state + w4 state * noise2 position) (w4 state * 0.5))

sunDirection :: Bool -> F local Vec3
sunDirection evening = Normalize Vector3 (if evening then v3 (-0.34) 0.10 1 else v3 (-0.46) 0.42 1)

daylight :: Bool -> F local Vec3 -> S local -> F local Vec3
daylight evening ray blur = build $ do
  direction <- share ray
  softness <- share blur
  elevation <- share (maxF (y3 direction) 0)
  facing <- share (maxF (Dot Vector3 direction (sunDirection evening)) 0)
  let horizon = if evening then v3 0.79 0.40 0.22 else v3 0.60 0.73 0.79
  let zenith = if evening then v3 0.14 0.21 0.34 else v3 0.13 0.32 0.52
  let glow = if evening then v3 0.60 0.29 0.10 else v3 0.26 0.24 0.19
  let cloudTint = if evening then v3 0.78 0.60 0.48 else v3 0.86 0.88 0.87
  let sunTint = if evening then v3 1.7 1 0.46 else v3 1 0.85 0.58
  let color = add3 (mix3 horizon zenith (powF (unit (elevation * 1.7)) 0.45)) (scale3 glow (powF facing 12))
  let cloudUV = scale2 (xz direction) (1.3 / (0.25 + absF (y3 direction)))
  let cloud = mist (add2 (mul2 cloudUV (v2 0.7 2.2)) (v2 11 5))
  let wisps = smooth 0.43 0.78 cloud * smooth 0 0.16 elevation
  let disk = smooth (0.9995 - softness * 0.002) 0.9999 facing
  pure (add3 (mix3 color cloudTint (wisps * (0.65 - softness * 0.4))) (scale3 sunTint disk))

-- The spatial frequency is part of the loop state. The amplitude is the
-- geometric sequence 0.105 * 0.56^i, evaluated at the original wave index.
waves :: S local -> F local Vec2 -> S local -> Bool -> F local Vec3
waves phase point footprint detail = build $ do
  p <- share point
  spanPixels <- share footprint
  pure $ rgb $ repeatN Float4 (if detail then 13 else 7) (v4 0 0 0 0.62) $ \i state -> build $ do
    angle <- share (i * 2.39996 + 0.4 * sinF (i * 1.7))
    direction <- share (v2 (cosF angle) (sinF angle))
    k <- share (w4 state)
    theta <- share (Dot Vector2 p direction * k + phase * floorF (8 * sqrtF k + 0.5) + i * 2.83)
    let attenuation = if detail then expF (-k * k * spanPixels * spanPixels * 0.5) else 1
    let amplitude = 0.105 * powF 0.56 i
    let height = sinF theta - 0.18 * cosF (2 * theta)
    slope <- share (k * (cosF theta + 0.36 * sinF (2 * theta)))
    let contribution = scale3 (v3 height (x2 direction * slope) (y2 direction * slope)) (amplitude * attenuation)
    pure (rgba (add3 (rgb state) contribution) (k * 1.63))

neighbor :: S local -> F local Vec2
neighbor i = letExpr (floorF (i / 3)) (\row -> v2 (i - 3 * row - 1) (row - 1))

caustics :: S local -> F local Vec2 -> S local
caustics phase point = build $ do
  p <- share point
  let flow = scale2 (v2 (sinF (phase * 3)) (cosF (phase * 2))) 0.25
  source <- share (scale2 p 0.6)
  q <- share (add2 p (add2 flow (scale2 (v2 (noise2 source) (noise2 (add2 source (splat2 8)))) 0.9)))
  cell <- share (floor2 q)
  frac <- share (fract2 q)
  closest <- share $ repeatN Float2 9 (splat2 10) $ \i distances -> build $ do
    offset <- share (neighbor i)
    cellID <- share (add2 cell offset)
    let shifted h = 0.5 + 0.35 * sinF (h * tau + phase * 2)
    let center = v2 (shifted (hash2 cellID)) (shifted (hash2 (add2 cellID (splat2 12.1))))
    distance <- share (Length Vector2 (sub2 (add2 offset center) frac))
    pure (If Float2 (less distance (x2 distances)) (v2 distance (x2 distances))
      (v2 (x2 distances) (minF (y2 distances) distance)))
  pure (expF (-28 * (y2 closest - x2 closest)) * 0.7)

data Water = Tidal | Amber | Shallow deriving (Eq)

waterScene :: Water -> F local Vec4 -> F local Vec2 -> F local Vec3
waterScene kind p uv = build $ do
  screen <- share (aspectPoint p uv)
  let evening = kind == Amber
  let shallow = kind == Shallow
  let phase = z4 p
  let eyeHeight = if shallow then 3.6 else 1.55
  let eye = v3 0 eyeHeight 0
  ray <- share (Normalize Vector3 (v3 (x2 screen * 0.95)
    (if shallow then y2 uv * 0.7 - 1.05 else (y2 uv - 0.65) * 0.88) 1))
  let clearSky = sub3 (splat3 1) (exp3 (scale3 (daylight evening ray 0) (-1.25)))
  pure $ If Float3 (greater (y3 ray) (-0.001)) clearSky $ build $ do
    rayDown <- share (negate (y3 ray))
    let along distance = add3 eye (scale3 ray distance)
    let field distance = x3 (waves phase (xz (along distance)) 0 False)
    lo <- share ((eyeHeight - 0.32) / rayDown)
    hi <- share (minF ((eyeHeight + 0.32) / rayDown) 1600)
    let initial = v4 lo hi (0.32 - field lo) (eyeHeight - rayDown * hi - field hi)
    let estimate bracket = mixF (x4 bracket) (y4 bracket)
          (clampF (z4 bracket / maxF (z4 bracket - w4 bracket) 0.00001) 0.1 0.9)
    bracket <- share $ repeatN Float4 6 initial $ \_ bounds -> build $ do
      distance <- share (estimate bounds)
      f <- share (eyeHeight - rayDown * distance - field distance)
      pure (If Float4 (greater f 0) (v4 distance (y4 bounds) f (w4 bounds)) (v4 (x4 bounds) distance (z4 bounds) f))
    travel <- share (estimate bracket)
    point <- share (along travel)
    footprint <- share (travel / y4 p / maxF rayDown 0.035)
    wave <- share (waves phase (xz point) footprint True)
    normal <- share (Normalize Vector3 (v3 (negate (y3 wave)) 1 (negate (z3 wave))))
    facing <- share (maxF (Dot Vector3 normal (scale3 ray (-1))) 0)
    let fresnel = 0.0204 + 0.9796 * powF (1 - facing) 5
    let reflection = daylight evening (reflect3 ray normal) (unit (footprint * 0.5))
    let baseWater = if evening then v3 0.018 0.075 0.095 else v3 0.018 0.15 0.19
    let volume = if shallow then shallowColor phase (xz point) ray normal
          else add3 baseWater (scale3 (v3 0.02 0.08 0.07) (0.5 + 0.5 * x3 wave / 0.25))
    let halfVector = Normalize Vector3 (sub3 (sunDirection evening) ray)
    nh <- share (maxF (Dot Vector3 normal halfVector) 0)
    rough <- share (0.055 + minF 0.09 (footprint * 0.14))
    a2 <- share (rough * rough)
    denominator <- share (nh * nh * (a2 - 1) + 1)
    let specular = minF 1.3 (a2 / (3.14159 * denominator * denominator) * 0.018)
    let specularTint = if evening then v3 1 0.57 0.24 else v3 0.88 0.86 0.73
    let color = add3 (mix3 volume reflection fresnel) (scale3 specularTint specular)
    let haze = 1 - expF (-travel * 0.004)
    let horizon = daylight evening (Normalize Vector3 (v3 (x3 ray) 0 (z3 ray))) 1
    let exposed = sub3 (splat3 1) (exp3 (scale3 (mix3 color horizon haze) (-1.55)))
    pure (pow3 exposed 0.88)

shallowColor :: S local -> F local Vec2 -> F local Vec3 -> F local Vec3 -> F local Vec3
shallowColor phase point ray normal = build $ do
  let eta = 1 / 1.333
  d <- share (Dot Vector3 normal ray)
  let k = 1 - eta * eta * (1 - d * d)
  underRay <- share (sub3 (scale3 ray eta) (scale3 normal (eta * d + sqrtF k)))
  distance <- share ((2.1 + 0.25 * noise2 (scale2 point 0.32)) / maxF (negate (y3 underRay)) 0.1)
  floorPoint <- share (add2 point (scale2 (xz underRay) distance))
  let grain = noise2 (scale2 floorPoint 28) * 0.035
  let ridges = 0.5 + 0.5 * sinF (x2 floorPoint * 6 + mist (scale2 floorPoint 0.7) * 4)
  let sand = add3 (scale3 (v3 0.55 0.49 0.32) (0.82 + ridges * 0.1 + grain))
        (scale3 (v3 0.40 0.44 0.31) (caustics phase (scale2 floorPoint 1.45)))
  transmission <- share (exp3 (scale3 (v3 (-0.30) (-0.10) (-0.055)) distance))
  pure (add3 (mul3 sand transmission) (mul3 (v3 0.02 0.26 0.27) (sub3 (splat3 1) transmission)))

veilScene :: F local Vec4 -> F local Vec2 -> F local Vec3
veilScene p uv = build $ do
  screen <- share (aspectPoint p uv)
  glowPosition <- share (sub2 screen (v2 (-0.25) 0.1))
  let phase = z4 p
  let gradient = mix3 (v3 0.018 0.035 0.065) (v3 0.038 0.085 0.11) (y2 uv)
  let glow = expF (-1.7 * Dot Vector2 glowPosition glowPosition)
  let initial = add3 gradient (scale3 (v3 0.015 0.035 0.045) glow)
  pure $ repeatN Float3 4 initial $ \n color -> build $ do
    let x = x2 screen
    let center = -0.18 + n * 0.105 + 0.13 * sinF (x * 2.1 + phase * 2 + n * 0.72)
          + 0.055 * sinF (x * 4 - phase * 3 + n * 1.33)
    width <- share (0.085 + 0.025 * sinF (x * 1.9 + phase + n))
    offset <- share (y2 screen - center)
    d <- share (offset / width)
    foldDistance <- share ((offset + width * 0.32) / (width * 0.18))
    let body = expF (-d * d * 0.70)
    let fold = expF (-foldDistance * foldDistance)
    let along = 0.6 + 0.4 * cosF (x * 1.4 + n * 0.7)
    let tint = mix3 (v3 0.12 0.40 0.41) (v3 0.42 0.29 0.38) (n / 3)
    pure (add3 color (scale3 tint ((body * 0.42 + fold * 0.25) * along)))

stars :: F local Vec4 -> F local Vec2 -> S local -> S local -> F local Vec3
stars p point frequency seed = build $ do
  scaled <- share (scale2 point frequency)
  cell <- share (floor2 scaled)
  frac <- share (fract2 scaled)
  pure $ repeatN Float3 9 (splat3 0) $ \i light -> build $ do
    offset <- share (neighbor i)
    cellID <- share (add2 (add2 cell offset) (splat2 seed))
    let hashed salt = hash2 (add2 cellID (splat2 salt))
    chance <- share (hash2 cellID)
    let center = add2 (scale2 (v2 (hashed 5.3) (hashed 17.1)) 0.8) (splat2 0.1)
    delta <- share (scale2 (sub2 (add2 offset center) frac) (1 / frequency))
    radius <- share (maxF (mixF 0.00045 0.00105 (powF (hashed 29.7) 3)) (0.58 / y4 p))
    distance <- share (Dot Vector2 delta delta / (radius * radius))
    let core = expF (negate distance)
    let halo = expF (-distance / 9) * 0.085
    let breathe = 0.94 + 0.06 * sinF (z4 p * (5 + floorF (chance * 5)) + chance * tau)
    let intensity = mixF 0.18 0.70 (powF (hashed 43.3) 4)
    let tint = mix3 (v3 0.58 0.72 0.90) (v3 0.96 0.81 0.64) (hashed 31.9)
    pure (add3 light (scale3 tint ((core + halo) * intensity * breathe * stepF 0.72 chance)))

nightScene :: F local Vec4 -> F local Vec2 -> F local Vec3
nightScene p uv = build $ do
  screen <- share (aspectPoint p uv)
  galactic <- share (rotate2 0.85 0.53 screen)
  bandDistance <- share ((y2 galactic - 0.04) / 0.19)
  let band = expF (-bandDistance * bandDistance)
  let drift = scale2 (v2 (sinF (z4 p)) (cosF (z4 p))) 0.022
  dust <- share (mist (add2 (add2 (mul2 galactic (v2 3.4 7.8)) (v2 13 3)) drift))
  let fine = mist (add2 (scale2 galactic 17) (v2 23 8))
  let glow = scale3 (mix3 (v3 0.055 0.095 0.115) (v3 0.12 0.075 0.12) dust)
        (band * (0.25 + 0.8 * dust) * (0.5 + fine) * 1.8)
  let background = mix3 (v3 0.012 0.022 0.045) (v3 0.029 0.048 0.085) (y2 uv)
  let lights = scale3 (add3 (stars p screen 35 0) (scale3 (stars p screen 62 87) 0.52)) 1.35
  pure (add3 (add3 background glow) lights)

volumeNoise :: F local Vec3 -> S local
volumeNoise point = build $ do
  p <- share point
  whole <- share (floor3 p)
  frac <- share (fract3 p)
  blend <- share (mul3 (mul3 frac frac) (sub3 (splat3 3) (scale3 frac 2)))
  cell <- share (add2 (xy whole) (scale2 (v2 37 61) (z3 whole)))
  pure (mixF (cellNoise cell (x3 blend) (y3 blend))
    (cellNoise (add2 cell (v2 37 61)) (x3 blend) (y3 blend)) (z3 blend))

cloudDensity :: F local Vec3 -> F local Vec3 -> S local
cloudDensity drift point = build $ do
  p <- share point
  q <- share (add3 (mul3 p (v3 0.52 0.65 0.52)) drift)
  xzPoint <- share (xz q)
  cell <- share (floor2 xzPoint)
  frac <- share (fract2 xzPoint)
  shape <- share $ repeatN Float1 9 10 $ \i old -> build $ do
    offset <- share (neighbor i)
    cellID <- share (add2 cell offset)
    let shifted salt = hash2 (add2 cellID (splat2 salt))
    let center = add2 (splat2 0.12) (scale2 (v2 (hash2 cellID) (shifted 18)) 0.76)
    let altitude = 0.25 + shifted 29 * 0.40
    delta <- share (sub2 (sub2 frac offset) center)
    let radius = 0.50 + shifted 43 * 0.28
    distance <- share (Length Vector3 (v3 (x2 delta) (y2 delta) ((y3 q - altitude) * 0.90)) - radius)
    blend <- share (maxF (0.22 - absF (old - distance)) 0 / 0.22)
    pure (minF old distance - blend * blend * 0.055)
  let erosion = volumeNoise (add3 (scale3 p 2.6) drift) * 0.105 + volumeNoise (scale3 p 5.2) * 0.035
  pure (unit ((-shape - erosion) * 6) * smooth 0 0.25 (y3 p))

cloudScene :: F local Vec4 -> F local Vec2 -> F local Vec3
cloudScene p uv = build $ do
  ray <- share (Normalize Vector3 (v3 (x2 (aspectPoint p uv)) (y2 uv - 0.68) 1))
  sky <- share (mix3 (v3 0.66 0.73 0.76) (v3 0.15 0.33 0.51) (smooth 0 0.45 (y3 ray)))
  pure $ If Float3 (less (y3 ray) (-0.01)) (build $ do
    downward <- share (negate (y3 ray))
    start <- share ((3 - 2.4) / downward)
    stride <- share (minF (0.0375 / downward) 0.55)
    drift <- share (scale3 (v3 (sinF (z4 p)) 0 (cosF (z4 p))) 0.22)
    sun <- share (Normalize Vector3 (v3 (-0.8) 0.45 0.35))
    result <- share $ repeatN Float4 64 (v4 0 0 0 1) $ \i state ->
      -- Stop evaluating density and lighting once the original march's
      -- transmittance threshold is reached.
      If Float4 (less (w4 state) 0.015) state $ build $ do
        travel <- share (start + i * stride)
        point <- share (add3 (v3 0 3 0) (scale3 ray travel))
        density <- share (cloudDensity drift point)
        pure $ If Float4 (greater density 0.001) (build $ do
          let shadeAt distance = cloudDensity drift (add3 point (scale3 sun distance))
          let shade = shadeAt 0.22 * 0.3 + shadeAt 0.60 * 0.55 + shadeAt 1.2 * 0.8
          let light = expF (-shade * 2.1)
          let tint = mix3 (mix3 (v3 0.27 0.38 0.51) (v3 0.93 0.90 0.80) light)
                sky (1 - expF (-travel * 0.012))
          alpha <- share (1 - expF (-density * stride * 2.6))
          pure (rgba (add3 (rgb state) (scale3 tint (w4 state * alpha))) (w4 state * (1 - alpha)))) state
    pure (add3 (rgb result) (scale3 sky (w4 result)))) sky

auroraScene :: F local Vec4 -> F local Vec2 -> F local Vec3
auroraScene p uv = build $ do
  screen <- share (aspectPoint p uv)
  let phase = z4 p
  let initial = add3 (mix3 (v3 0.016 0.034 0.055) (v3 0.008 0.018 0.042) (y2 uv))
        (scale3 (stars p screen 42 16) 0.65)
  let curtains = repeatN Float3 12 initial $ \i color -> build $ do
        z <- share (i / 11)
        x <- share (x2 screen * (1 + z * 0.15) + z * 0.12)
        let hem = -0.10 + 0.14 * sinF (x * 2.5 + phase * 2 + z * 0.4)
              + 0.055 * sinF (x * 5.7 - phase + z * 0.7)
        height <- share (y2 screen - hem + z * 0.06)
        let body = smooth (-0.025) 0.025 height * expF (-maxF height 0 * 7.5)
        let fibers = 0.45 + 0.55 * noise2 (v2 (x * 34 + sinF phase) (z * 7))
        let tint = mix3 (v3 0.12 0.68 0.38) (v3 0.30 0.20 0.55) (unit (height * 3.5))
        pure (add3 color (scale3 tint (body * fibers * 0.18)))
  ridge <- share (-0.37 + 0.028 * noise2 (v2 (x2 screen * 6) 3))
  pure (mix3 (v3 0.012 0.026 0.033) curtains (smooth ridge (ridge + 0.004) (y2 screen)))

inkScene :: F local Vec4 -> F local Vec2 -> F local Vec3
inkScene p uv = build $ do
  screen <- share (scale2 (aspectPoint p uv) 2)
  drift <- share (scale2 (v2 (sinF (z4 p)) (cosF (z4 p))) 0.24)
  moved <- share (add2 screen drift)
  flow <- share (v2 (mist (add2 moved (splat2 4))) (mist (add2 moved (v2 8 1))))
  let folded = v2 (mist (add2 (add2 screen (scale2 flow 2.5)) (v2 5 9)))
        (mist (add2 (sub2 screen (scale2 flow 2)) (splat2 14)))
  pigment <- share (mist (add2 (add2 screen (scale2 folded 3.2)) (scale2 drift 0.3)))
  let color = mix3 (v3 0.055 0.12 0.14) (v3 0.64 0.72 0.64) (smooth 0.22 0.77 pigment)
  let contour = expF (-absF (pigment - 0.48) * 75)
  pure (add3 color (scale3 (v3 0.17 0.13 0.065) contour))
