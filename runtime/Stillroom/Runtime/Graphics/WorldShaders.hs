{-# LANGUAGE DataKinds #-}

-- Equations ported from assets/rest/{nature,journey}. Camera and shadow
-- projection enter in Vulkan clip space, so z is never remapped a second time.
module Stillroom.Runtime.Graphics.WorldShaders
  ( WorldStyle(..), Surface(..), WorldParameters(..), worldParameters
  , landVertex, landFragment, shadowFragment, waterVertex, waterFragment
  , skyShader, postShader
  ) where

import Data.Word (Word32)
import GameIO.Graphics.Plan (Mat4, Vec2, Vec3(..), Vec4(..), mat4Columns)
import GameIO.Graphics.Shader
import Stillroom.Landscape.Grass qualified as Grass
import Stillroom.Runtime.Graphics.Shader

data WorldStyle = FixedWorld | JourneyWorld deriving (Eq, Show)
data Surface = Terrain | Bark | Leaves | Blades | Carriage deriving (Eq, Ord, Show, Enum, Bounded)

data WorldParameters = WorldParameters
  { worldClock :: !Float
  , worldQuality :: !Int
  , worldLandscape :: !Int
  , worldClip :: !Bool
  , worldEye :: !Vec3
  , worldFar :: !Bool
  , worldSun :: !Vec3
  , worldRasterHeight :: !Int
  , worldAnchor :: !Vec3
  , worldBoneRows :: !Int
  , worldOffset :: !Vec3
  , worldReference :: !Vec3
  , worldShadowUnits :: !Vec4
  , worldShadowWidth :: !Int
  , worldReflectionWidth :: !Int
  , worldReflectionHeight :: !Int
  , worldHeightRect :: !Vec4
  , worldLightMatrix :: !Mat4
  , worldReflectionMatrix :: !Mat4
  , worldObjectMatrix :: !Mat4
  }

data WorldSlot = Clock | Eye | Sun | Anchor | Offset | Reference | ShadowUnits | ImageSizes | HeightRect
  deriving (Enum)

worldParameters :: WorldParameters -> [Vec4]
worldParameters p =
  [Vec4 (worldClock p) (fromIntegral (worldQuality p)) (fromIntegral (worldLandscape p)) (flag (worldClip p))
  , attach (worldEye p) (flag (worldFar p))
  , attach (worldSun p) (fromIntegral (worldRasterHeight p))
  , attach (worldAnchor p) (fromIntegral (worldBoneRows p))
  , attach (worldOffset p) 0
  , attach (worldReference p) 0
  , worldShadowUnits p
  , Vec4 (fromIntegral (worldShadowWidth p)) (fromIntegral (worldReflectionWidth p))
      (fromIntegral (worldReflectionHeight p)) 0
  , worldHeightRect p] ++ concatMap mat4Columns [worldLightMatrix p, worldReflectionMatrix p, worldObjectMatrix p]
  where
    attach (Vec3 x y z) w = Vec4 x y z w
    flag yes = if yes then 1 else 0

parameter :: Word32 -> V4 local stage
parameter index = case uniformSlot index of
  Just slot -> Uniform slot
  Nothing -> error "Stillroom shader parameter exceeds the uniform block"

worldParameter :: WorldSlot -> V4 local stage
worldParameter = parameter . fromIntegral . fromEnum

lightMatrix, reflectionMatrix, objectMatrix :: Expr local stage Mat4
lightMatrix = matrix (fromIntegral (fromEnum HeightRect + 1))
reflectionMatrix = matrix (fromIntegral (fromEnum HeightRect + 5))
objectMatrix = matrix (fromIntegral (fromEnum HeightRect + 9))

matrix :: Word32 -> Expr local stage Mat4
matrix base = MakeMat4 (parameter base) (parameter (base+1)) (parameter (base+2)) (parameter (base+3))

type S local stage = Scalar local stage
type SF local = F local Float

quality, sceneNumber, time :: S local stage
quality = y4 (worldParameter Clock)
sceneNumber = z4 (worldParameter Clock)
time = x4 (worldParameter Clock)

same :: S local stage -> S local stage -> Expr local stage Bool
same = Compare EqualF

both, eitherTrue :: Expr local stage Bool -> Expr local stage Bool -> Expr local stage Bool
both = BoolBinary Both
eitherTrue = BoolBinary EitherTrue

oneOf :: [Expr local stage Bool] -> Expr local stage Bool
oneOf = foldr eitherTrue (BoolLiteral False)

square :: S local stage -> S local stage
square x = letExpr x (\n -> n*n)

wind :: V2 local stage -> S local stage -> S local stage
wind point clock = letExpr point $ \p -> 0.47
  + 0.24 * sinF (Dot Vector2 p (v2 0.082 0.047) - clock * 0.58)
  + 0.13 * sinF (Dot Vector2 p (v2 0.21 (-0.13)) - clock * 1.13)
  + 0.10 * sinF (clock * 0.23)

hash12 :: V2 local stage -> S local stage
hash12 point = build $ do
  p <- share point
  q <- share (fract3 (scale3 (v3 (x2 p) (y2 p) (x2 p)) 0.1031))
  shifted <- share (add3 q (splat3 (Dot Vector3 q (add3 (v3 (y3 q) (z3 q) (x3 q)) (splat3 33.33)))))
  pure (fractF ((x3 shifted + y3 shifted) * z3 shifted))

noise :: V2 local stage -> S local stage
noise point = build $ do
  p <- share point
  cell <- share (floor2 p)
  frac <- share (fract2 p)
  f <- share (mul2 (mul2 frac frac) (sub2 (splat2 3) (scale2 frac 2)))
  pure (mixF (mixF (hash12 cell) (hash12 (add2 cell (v2 1 0))) (x2 f))
    (mixF (hash12 (add2 cell (v2 0 1))) (hash12 (add2 cell (v2 1 1))) (x2 f)) (y2 f))

fbm :: V2 local stage -> S local stage
fbm point = letExpr point $ \p -> z4 $
  repeatN Float4 5 (v4 (x2 p) (y2 p) 0 0.5) $ \_ state -> build $ do
    position <- share (v2 (x4 state) (y4 state))
    next <- share (add2 (rotate2 1.6 (-1.2) position) (splat2 17.1))
    pure (v4 (x2 next) (y2 next) (z4 state + w4 state * noise position) (w4 state * 0.5))

clearSky :: V3 local stage -> V3 local stage -> V3 local stage
clearSky sun direction = build $ do
  d <- share direction
  mu <- share (clampF (Dot Vector3 d sun) (-1) 1)
  air <- share (1 / (maxF (y3 d) 0 + 0.20))
  trans <- share (exp3 (scale3 (v3 (-0.075) (-0.145) (-0.265)) air))
  let rayleigh = scale3 (v3 0.22 0.43 0.76) ((1 + mu*mu) * (0.40 + 0.26*air))
  let mie = 0.055 / powF (maxF 0.14 (1 + 0.72*0.72 - 1.44*mu)) 1.3
  pure (add3 (add3 (mul3 rayleigh trans) (scale3 (mul3 (v3 0.76 0.76 0.66) (sub3 (splat3 1) trans)) 0.59))
    (add3 (scale3 (v3 1 0.78 0.48) mie) (scale3 (v3 5 3.8 2.35) (smooth 0.99965 0.99992 mu))))

skyColor :: V3 local stage -> V3 local stage -> V3 local stage
skyColor sun direction = build $ do
  d <- share direction
  base <- share (clearSky sun d)
  pure $ If Float3 (greater (y3 d) 0.015) (build $ do
    position <- share (add2 (scale2 (xz d) (3 / (y3 d + 0.20))) (v2 3.2 7.8))
    clouds <- share (fbm position)
    let lit = clampF ((clouds - fbm (add2 position (scale2 (Normalize Vector2 (xz sun)) 0.28))) * 0.95 + 0.66) 0.25 1
    let shade = add3 (mix3 (v3 0.54 0.62 0.66) (v3 1.42 1.37 1.21) lit)
          (scale3 (v3 0.58 0.39 0.18) (powF (maxF (Dot Vector3 d sun) 0) 12 * 0.24))
    pure (mix3 base shade (smooth 0.48 0.70 clouds * smooth 0.01 0.18 (y3 d) * 0.85))) base

fogged :: V3 local stage -> V3 local stage -> V3 local stage -> V3 local stage -> V3 local stage
fogged sun eye point color = build $ do
  ray <- share (sub3 point eye)
  distance <- share (Length Vector3 ray)
  direction <- share (Normalize Vector3 ray)
  dh <- share (y3 ray * 0.024)
  let integrated = If Float1 (less (absF dh) 0.001) 1 ((1 - expF (negate dh)) / dh)
  let optical = maxF (distance * 0.0017 * expF (-maxF (y3 eye) 0 * 0.024) * integrated) 0
  transmission <- share (exp3 (scale3 (v3 (-0.76) (-1) (-1.32)) optical))
  let aerial = add3 (mul3 color transmission) (mul3 (clearSky sun direction) (sub3 (splat3 1) transmission))
  pure (If Float3 (greater distance 380) (mix3 aerial (skyColor sun direction) (smooth 380 500 distance)) aerial)

skyShader :: FragmentShader
skyShader = FragmentShader $ build $ do
  forward <- share (parameter 0)
  right <- share (parameter 1)
  up <- share (parameter 2)
  sun <- share (rgb (parameter 3))
  uv <- share (scale2 (sub2 InterpolatedUV (splat2 0.5)) 2)
  let ray = Normalize Vector3 (add3 (rgb forward)
        (add3 (scale3 (rgb right) (x2 uv * w4 right * w4 forward)) (scale3 (rgb up) (-y2 uv * w4 forward))))
  pure (rgba (skyColor sun ray) 1)

landVertex :: WorldStyle -> Surface -> Bool -> VertexShader
landVertex style surface instanced = SurfaceVertexShader $ build $ do
  let journey = style == JourneyWorld
  anchor <- shareVertex (worldParameter Anchor)
  offset <- shareVertex (rgb (worldParameter Offset))
  let absolutePosition local = add3 local (rgb anchor)
  (initial, normal, uv, extra, tint) <- if instanced
    then do
      root <- shareVertex InstanceRoot
      shape <- shareVertex InstanceParameters
      corner <- shareVertex VertexPosition
      let definition = Grass.BladeInputs (x4 root, y4 root, z4 root) (w4 root) (x4 shape) (y4 shape, z4 shape) (w4 shape)
      let row = Grass.CornerInputs (x3 corner) (y3 corner) (z3 corner) (if journey then y4 root else z3 corner)
      let blade = Grass.bladeCorner definition row
      let vec3 (x,y,z) = v3 x y z
      let vec2 (x,y) = v2 x y
      pure (vec3 (Grass.position blade), vec3 (Grass.normal blade), vec2 (Grass.texCoord blade),
        add2 (vec2 (Grass.extra blade)) (xz (rgb anchor)), InstanceColor)
    else pure (VertexPosition, VertexNormal, VertexUV, VertexExtra, VertexColor)
  source <- shareVertex initial
  baseNormal <- shareVertex normal
  coords <- shareVertex uv
  detail <- shareVertex extra
  (point, direction) <- case surface of
    Terrain | journey -> do
      let eligible = both (less (x2 coords) 0.5) (less (w4 (worldParameter Eye)) 0.5)
      let distance = Length Vector2 (sub2 (xz (add3 source offset)) (xz (rgb (worldParameter Reference))))
      let height = If Float1 eligible (mixF (y3 source) (x2 detail) (smooth 55 85 distance)) (y3 source)
      pure (v3 (x3 source) height (z3 source), baseNormal)
    Bark -> bendBones source baseNormal (x2 detail) anchor
    Leaves -> do
      flutter <- shareVertex (sinF (time * 3.8 + y2 detail) * (if journey then 0.014 else 0.035) * x2 coords)
      let point = add3 source (scale3 baseNormal flutter)
      let direction = Normalize Vector3 (add3 baseNormal (v3 (flutter*2.8) 0 (negate flutter)))
      bendBones point direction (x2 detail) anchor
    Blades -> do
      let distance = Length Vector2 (sub2 (xz (add3 source offset)) (xz (rgb (worldParameter Reference))))
      fade <- shareVertex (if journey then 1 - smooth 27 46 distance else 1)
      let root = sub2 detail (xz (rgb anchor))
      let stem = if journey then v3 (mixF (x2 root) (x3 source) fade) (mixF (y2 coords) (y3 source) fade)
            (mixF (y2 root) (z3 source) fade) else source
      bending <- shareVertex ((0.28 + wind detail time * 0.65 + 0.16 * sinF (time * 2.1 + y2 detail)) * square (x2 coords) * fade)
      pure (add3 stem (v3 (bending*0.32) (-square bending*0.055) (-bending*0.085)),
        Normalize Vector3 (add3 baseNormal (v3 (bending*0.45) 0.1 0)))
    Carriage -> pure (rgb (MatrixVector objectMatrix (Homogeneous source)), rgb (MatrixVector objectMatrix (rgba baseNormal 0)))
    _ -> pure (source, baseNormal)
  world <- shareVertex (if surface == Carriage then point else add3 point offset)
  pure (VertexOutputs (Transform (Homogeneous world)) coords tint world direction
    (if surface == Carriage then source else absolutePosition source) baseNormal detail)

bendBones :: V3 local 'VertexStage -> V3 local 'VertexStage -> S local 'VertexStage -> V4 local 'VertexStage
          -> Build (VertexBody local) (V3 local 'VertexStage, V3 local 'VertexStage)
bendBones position normal first anchor = do
  i <- shareVertex (truncateF (first + 0.5))
  chain (5 :: Int) position normal i
  where
    truncateF value = letExpr value (\n -> If Float1 (less n 0) (-floorF (-n)) (floorF n))
    bone index = letExpr (floorF (index / 256)) $ \row -> SampleAt Texture2
      (v2 ((index - row*256 + 0.5) / 256) ((row + 0.5) / w4 anchor))
    axis = Normalize Vector3 (v3 (-0.26) 0 (-0.966))
    rotate direction angle = add3 (add3 (scale3 direction (cosF angle)) (scale3 (Cross axis direction) (sinF angle)))
      (scale3 axis (Dot Vector3 axis direction * (1 - cosF angle)))
    chain 0 p n _ = pure (p, n)
    chain depth p n index = do
      valid <- shareVertex (lessEqual 0 index)
      b <- shareVertex (If Float4 valid (bone (index*2)) (Constant4 (Vec4 0 0 0 0)))
      q <- shareVertex (If Float4 valid (bone (index*2+1)) (Constant4 (Vec4 0 0 0 0)))
      origin <- shareVertex (sub3 (rgb b) (rgb anchor))
      gust <- shareVertex (wind (xz (rgb b)) time)
      angle <- shareVertex ((0.35 + gust) * sinF (time*0.66 + y4 q) * x4 q + gust*x4 q*0.65)
      p' <- shareVertex (If Float3 valid (add3 origin (rotate (sub3 p origin) angle)) p)
      n' <- shareVertex (If Float3 valid (rotate n angle) n)
      next <- shareVertex (w4 b - 1)
      chain (depth - 1) p' n' next

waterVertex :: VertexShader
waterVertex = SurfaceVertexShader $ build $ do
  field <- shareVertex (add3 VertexPosition (rgb (worldParameter Anchor)))
  let wave = 0.012 * sinF (Dot Vector2 (xz field) (v2 0.6 0.18) - time*0.9)
        + 0.008 * sinF (Dot Vector2 (xz field) (v2 (-0.23) 0.73) - time*0.67)
  point <- shareVertex (add3 (v3 (x3 VertexPosition) wave (z3 VertexPosition)) (rgb (worldParameter Offset)))
  pure (VertexOutputs (Transform (Homogeneous point)) VertexUV VertexColor point VertexNormal field VertexNormal VertexExtra)

shadowFragment :: FragmentShader
shadowFragment = FragmentShader (Constant4 (Vec4 1 1 1 1))

clipWaterline :: F local Bool
clipWaterline = both (greater (w4 (worldParameter Clock)) 0.5) (less (y3 InterpolatedPosition) 0.025)

farPass :: F local Bool
farPass = greater (w4 (worldParameter Eye)) 0.5

landFragment :: WorldStyle -> Surface -> FragmentShader
landFragment style surface = MaskedFragmentShader (landMask style surface) (landColor style surface)

landMask :: WorldStyle -> Surface -> F local Bool
landMask FixedWorld _ = clipWaterline
landMask JourneyWorld surface = build $ do
  eye <- share (rgb (worldParameter Eye))
  world <- share InterpolatedPosition
  let range = Length Vector2 (sub2 (xz world) (xz eye))
  let distantGrass = greater (Length Vector3 (sub3 world eye)) 47
  let split = ifCondition farPass (less range 90) (greaterEqual range 90)
  let glY = w4 (worldParameter Sun) - y4 FragmentCoordinates
  let screen = floor2 (v2 (x4 FragmentCoordinates) glY)
  let threshold = fractF (Dot Vector2 screen (v2 0.754877666 0.569840296))
  let lod = smooth 77 102 range
  let screenDoor = ifCondition farPass (greaterEqual threshold lod) (less threshold lod)
  let leaves = noise (add2 (mul2 InterpolatedUV (v2 18 27)) (scale2 (xz InterpolatedSourcePosition) 0.07))
  let outline = If Float1 (greater (y2 InterpolatedExtra) 0.5)
        (absF (x2 InterpolatedUV) / (1.04 - (y2 InterpolatedUV*0.5+0.5)*0.96)) (Length Vector2 InterpolatedUV)
  let farLeaves = both farPass (eitherTrue (greater outline (1 - 0.19*leaves)) (greater leaves 0.79))
  let surfaceMask = case surface of
        Terrain -> ifCondition (less (x2 InterpolatedUV) 0.5) split screenDoor
        Bark -> screenDoor
        Leaves -> eitherTrue screenDoor farLeaves
        Blades -> distantGrass
        Carriage -> BoolLiteral False
  pure (eitherTrue clipWaterline surfaceMask)

greaterEqual :: S local stage -> S local stage -> Expr local stage Bool
greaterEqual = flip lessEqual

ifCondition :: Expr local stage Bool -> Expr local stage Bool -> Expr local stage Bool -> Expr local stage Bool
ifCondition condition yes no = eitherTrue (both condition yes) (both (BoolNot condition) no)

footprint2 :: F local Vec2 -> SF local
footprint2 point = letExpr point $ \p -> maxF (Length Vector2 (Derivative ScreenX Float2 p))
  (Length Vector2 (Derivative ScreenY Float2 p))

landColor :: WorldStyle -> Surface -> F local Vec4
landColor style surface = build $ do
  let journey = style == JourneyWorld
  let thin = surface == Leaves || surface == Blades
  point <- share InterpolatedPosition
  original <- share InterpolatedSourcePosition
  coords <- share InterpolatedUV
  tint <- share InterpolatedColor
  eye <- share (rgb (worldParameter Eye))
  sun <- share (rgb (worldParameter Sun))
  initial <- share (Normalize Vector3 InterpolatedNormal)
  normal <- share (If Float3 FrontFacing initial (scale3 initial (-1)))
  view <- share (Normalize Vector3 (sub3 eye point))
  let base = rgb tint
  let h = y3 original
  let u = x2 coords
  let track = case surface of Carriage -> BoolLiteral True; Terrain -> greater u 1.5; _ -> BoolLiteral False
  let occlusion = case surface of Leaves -> mixF 0.58 1 (w4 tint); Blades -> mixF 0.47 1 u; _ -> 1
  albedo <- share $ case surface of
    Terrain -> terrainColor style point original normal coords base
    Bark -> let grain = noise (v2 (u*8) (h*2.5))
                marks = stepF 0.70 (noise (v2 (u*20) (h*11))) * 0.55
                cutoff = if journey then smooth 0.32 0.50 (x4 tint) else 1
            in scale3 base (0.64 + 0.45*grain - marks*cutoff)
    Leaves -> scale3 base (0.79 + 0.22*u + 0.12*(1-absF (y2 coords)))
    Blades -> let c = scale3 base (mixF 0.46 1.15 (sqrtF u))
              in if journey then c else mix3 (v3 0.115 0.18 0.035) c (expF (-Length Vector3 (sub3 eye point)*0.010))
    Carriage -> base
  let railBase = build $ do
        grainCoordinates <- share (mul2 (xz original) (v2 4 45))
        let pixel = footprint2 grainCoordinates
        let grain = mixF 0.5 (noise grainCoordinates) (expF (-2 * square pixel))
        pure (scale3 base (0.93 + 0.10*grain))
  -- Footprints must be evaluated before a per-fragment rail/terrain branch.
  trackColor <- share (if journey && surface `elem` [Terrain,Carriage] then railBase else albedo)
  c <- share (If Float3 track trackColor albedo)
  lit <- share $ If Float3 (greater quality 0)
    (build $ do
      let soil = less u 1.5
      let wet = 1 - smooth 0.05 0.65 h
      n <- share $ if surface == Terrain then build $ do
        bumped <- share (terrainNormal point original normal)
        pure (If Float3 soil bumped normal)
        else normal
      let wetColor = if surface == Terrain then If Float3 soil (scale3 c (mixF 1 0.64 wet)) c else c
      let startRoughness = case surface of
            Terrain -> If Float1 soil (mixF 0.86 0.24 wet) 0.84
            Bark -> 0.78
            Leaves -> 0.52
            Blades -> 0.52
            Carriage -> 0.43
      rough <- share (If Float1 track (mixF 0.43 0.67 (smooth 0.08 0.24 (x4 tint-z4 tint))) startRoughness)
      grain <- share (if surface `elem` [Terrain,Carriage] then woodGrain style original normal tint else 1)
      material <- share (If Float3 track (scale3 wetColor grain) wetColor)
      shade <- share (shadow style sun n point)
      nl <- share (maxF (Dot Vector3 n sun) 0)
      nv <- share (maxF (Dot Vector3 n view) 0.03)
      let ambient = scale3 (mix3 (v3 0.16 0.19 0.13) (v3 0.36 0.47 0.59) (unit (y3 n*0.5+0.5))) occlusion
      let sunlight = v3 1.85 1.53 1.08
      let diffuse = mul3 material (add3 ambient (scale3 sunlight (nl*shade)))
      halfway <- share (Normalize Vector3 (add3 view sun))
      let nh = maxF (Dot Vector3 n halfway) 0
      let vh = maxF (Dot Vector3 view halfway) 0
      a2 <- share (square (square rough))
      let distribution = a2 / (3.14159*square (nh*nh*(a2-1)+1)+0.0001)
      let visibility = ggxVisibility nv nl (square (rough+1)/8) 0.0001
      let fresnel = 0.035 + 0.965*powF (1-vh) 5
      let specular = scale3 sunlight (distribution*visibility*fresnel/(4*nv+0.0001)*shade)
      let thinness = if surface == Leaves then mixF 0.55 1 (w4 tint) else mixF 0.35 0.85 u
      let forward = powF (maxF (Dot Vector3 view (scale3 sun (-1))) 0) 4
      let transmitted = scale3 (mul3 material (v3 1.30 1.14 0.54))
            ((forward*0.72 + maxF (-Dot Vector3 n sun) 0*0.34)*thinness*shade)
      pure (if thin then add3 (add3 diffuse specular) transmitted else add3 diffuse specular))
    (build $ do
      shade <- share (shadow style sun normal point)
      facing <- share (Dot Vector3 normal sun)
      let up = 0.5 + 0.5*y3 normal
      let ambient = scale3 (mix3 (v3 0.20 0.22 0.13) (v3 0.40 0.51 0.63) up) occlusion
      let direct = scale3 (v3 1.42 1.25 0.91) (maxF facing 0*shade)
      let diffuse = mul3 c (add3 ambient direct)
      let nh = maxF (Dot Vector3 normal (Normalize Vector3 (add3 sun view))) 0
      let forward = powF (maxF (Dot Vector3 view (scale3 sun (-1))) 0) 3
      let leaf = scale3 (mul3 c (v3 0.85 1.02 0.36)) ((forward*0.65 + maxF (-facing) 0*0.30)*shade)
      let glint = scale3 (v3 0.42 0.46 0.24) (powF nh 34*shade*0.14)
      let rail = scale3 (v3 0.20 0.18 0.12) (powF nh 48*shade)
      pure (if thin then add3 (add3 diffuse leaf) glint
        else if journey then If Float3 track (add3 diffuse rail) diffuse else diffuse))
  pure (rgba (fogged sun eye point lit) 1)

ggxVisibility :: SF local -> SF local -> SF local -> SF local -> SF local
ggxVisibility nv nl k epsilon = letExpr k $ \factor ->
  nv / (nv*(1-factor)+factor) * nl / (nl*(1-factor)+factor+epsilon)

terrainColor :: WorldStyle -> F local Vec3 -> F local Vec3 -> F local Vec3 -> F local Vec2 -> F local Vec3 -> F local Vec3
terrainColor style point original normal uv tint = build $ do
  ground <- share (xz original)
  patches <- share (fbm (scale2 ground 0.28))
  grain <- share (mixF 0.5 (noise (scale2 ground 7.5)) (expF (-footprint2 ground*10)))
  let h = y3 original
  let alp = same sceneNumber 1
  let greens = if style == JourneyWorld then scale3 tint (0.65 + 0.55*patches)
        else mix3 (v3 0.060 0.105 0.023) (v3 0.17 0.24 0.057) patches
  let initial = mix3 (v3 0.22 0.19 0.13) (scale3 greens (0.85 + 0.28*grain)) (smooth (-0.12) 0.50 h)
  let slope = 1 - y3 normal
  let ordinaryStone = smooth 0.38 0.70 slope * smooth 3 10 h
  stone <- share (If Float1 alp (maxF ordinaryStone (smooth 5 21 h)) ordinaryStone)
  let rock = mix3 initial (scale3 (v3 0.19 0.205 0.20) (0.75+0.38*patches)) stone
  let highlands = if style == FixedWorld then rock else
        mix3 (mix3 rock (scale3 (v3 0.24 0.25 0.245) (0.75+0.38*patches)) (smooth 40 64 h * smooth 0.12 0.50 slope))
          (v3 0.69 0.75 0.78) (smooth 69 78 (h+patches*4) * smooth 0.65 0.91 (y3 normal))
  high <- share highlands
  mountain <- share $ If Float3 alp (build $ do
    let strata = noise (v2 (x3 original*0.24) (h*0.82+z3 original*0.13))
    let stones = mix3 (v3 0.12 0.14 0.15) (v3 0.30 0.30 0.27) (strata*0.6+patches*0.4)
    let snowLine = h+patches*8+noise (scale2 ground 0.11)*5
    pure (mix3 (mix3 high stones stone) (v3 0.73 0.79 0.83) (smooth 34 40 snowLine*smooth 0.40 0.83 (y3 normal)))) high
  let stream = scale3 (v3 0.20 0.215 0.19) ((0.65+0.5*patches)*mixF 0.45 1 (smooth 0 0.4 (y3 point)))
  color <- share (scale3 (If Float3 (greater (x2 uv) 0.5) stream mountain) (mixF 0.65 1 (smooth 0 0.65 h)))
  pure (if style == JourneyWorld then If Float3 (less (x2 uv) 0.5)
    (mix3 color (scale3 (v3 0.24 0.205 0.13) (0.78+0.30*grain)) (y2 uv*0.87)) color else color)

terrainNormal :: F local Vec3 -> F local Vec3 -> F local Vec3 -> F local Vec3
terrainNormal point original normal = build $ do
  ground <- share (xz original)
  dx <- share (Derivative ScreenX Float3 point)
  dy <- share (Derivative ScreenY Float3 point)
  rx <- share (Cross dy normal)
  ry <- share (Cross normal dx)
  det <- share (Dot Vector3 dx rx)
  -- Treat the already filtered pixel footprint as constant across the
  -- finite-difference stencil, avoiding a second derivative of it.
  gain <- share (expF (-footprint2 ground*6)*0.028)
  high <- share (noise (scale2 ground 5.5))
  low <- share (noise (scale2 ground 1.7))
  let gradient axis = Derivative axis Float1 high*gain + Derivative axis Float1 low*0.035
  tangent <- share (add3 (scale3 rx (gradient ScreenX)) (scale3 ry (gradient ScreenY)))
  let sign = If Float1 (less det 0) (-1) 1
  pure (If Float3 (greater (absF det) 0.0000001)
    (Normalize Vector3 (sub3 (scale3 normal (absF det)) (scale3 tangent sign))) normal)

woodGrain :: WorldStyle -> F local Vec3 -> F local Vec3 -> F local Vec4 -> SF local
woodGrain style original normal tint = build $ do
  let localNormal = if style == JourneyWorld then InterpolatedSourceNormal else normal
  plane <- share (If Float2 (greater (absF (y3 localNormal)) 0.5) (xz original) (v2 (x3 original + z3 original) (y3 original)))
  warp <- share (noise (mul2 plane (v2 2 0.8)))
  coordinates <- share (v2 (x2 plane*2.5) (y2 plane*55+warp*6))
  let pixels = if style == JourneyWorld then footprint2 coordinates else footprint2 plane
  let fade = if style == JourneyWorld then expF (-2*square pixels) else expF (-pixels*22)
  let fine = (noise coordinates-0.5)*fade
  pure (1 + smooth 0.08 0.24 (x4 tint-z4 tint)*(fine*0.22+(warp-0.5)*0.055))

shadow :: WorldStyle -> F local Vec3 -> F local Vec3 -> F local Vec3 -> SF local
shadow style sun normal world = build $ do
  clip <- share (MatrixVector lightMatrix (rgba world 1))
  p <- share (v3 (x4 clip / w4 clip*0.5+0.5) (y4 clip / w4 clip*0.5+0.5) (z4 clip / w4 clip))
  size <- share (x4 (worldParameter ImageSizes))
  let outside = oneOf [less (x3 p) 0, greater (x3 p) 1, less (y3 p) 0, greater (y3 p) 1, greater (z3 p) 1,
        if style == JourneyWorld then less (z3 p) 0 else BoolLiteral False]
  edge <- share (minF (minF (x3 p) (1-x3 p)) (minF (y3 p) (1-y3 p)))
  result <- share (case style of
    FixedWorld -> If Float1 outside 1 (fixedShadow sun normal p size edge)
    JourneyWorld -> journeyShadow sun normal p size edge outside)
  pure result

fixedShadow :: F local Vec3 -> F local Vec3 -> F local Vec3 -> SF local -> SF local -> SF local
fixedShadow sun normal p size edge = build $ do
  bias <- share (maxF 0.00023 (0.0011*(1-Dot Vector3 normal sun)))
  let receiver = z3 p-bias
  let depth offset = x4 (SampleAt Texture0 (add2 (xy p) (scale2 offset (1/size))))
  let circle i = letExpr (i*2.399963) (\angle -> v2 (cosF angle) (-sinF angle))
  let squareOffset i = letExpr (floorF (i/3)) (\j -> v2 (j-1) (i-3*j-1))
  let average = repeatN Float1 9 0 (\i accumulated -> accumulated + stepF receiver (depth (squareOffset i))) / 9
  let soft = build $ do
        blockers <- share $ repeatN Float2 6 (v2 0 0) $ \i totals -> build $ do
          z <- share (depth (scale2 (circle i) (sqrtF (i+0.5)*2)))
          pure (If Float2 (less z receiver) (add2 totals (v2 z 1)) totals)
        penumbra <- share (If Float1 (greater (y2 blockers) 0)
          (clampF ((z3 p-x2 blockers/y2 blockers)*750) 1.2 5.5) 1.2)
        let sumVisibility = repeatN Float1 16 0 $ \i total -> total
              + stepF receiver (depth (scale2 (circle i) (sqrtF ((i+0.5)/16)*penumbra)))
        pure (0.10 + 0.90*sumVisibility/16)
  pure (mixF 1 (If Float1 (greater quality 1) soft (0.18+0.82*average)) (smooth 0.01 0.10 edge))

journeyShadow :: F local Vec3 -> F local Vec3 -> F local Vec3 -> SF local -> SF local -> F local Bool -> SF local
journeyShadow sun normal p size edge outside = build $ do
  dx <- share (Derivative ScreenX Float3 p)
  dy <- share (Derivative ScreenY Float3 p)
  scale <- share (maxF (maxF (absF (x3 dx)) (absF (y3 dx))) (maxF (absF (x3 dy)) (absF (y3 dy))))
  x <- share (scale3 dx (If Float1 (greater scale 0) (1/scale) 1))
  y <- share (scale3 dy (If Float1 (greater scale 0) (1/scale) 1))
  det <- share (x3 x*y3 y-y3 x*x3 y)
  gradient <- share (If Float2 (greater (absF det) 0.000001)
    (scale2 (v2 (y3 y*z3 x-y3 x*z3 y) (x3 x*z3 y-x3 y*z3 x)) (1/det)) (v2 0 0))
  units <- share (worldParameter ShadowUnits)
  let bias = maxF (z4 units) (w4 units*(1-Dot Vector3 normal sun)) / y4 units
  let compareCell cell = build $ do
        uv <- share (scale2 (add2 cell (splat2 0.5)) (1/size))
        let out = oneOf [less (x2 cell) 0, less (y2 cell) 0, greaterEqual (x2 cell) size, greaterEqual (y2 cell) size]
        let receiver = z3 p + Dot Vector2 gradient (sub2 uv (xy p)) - bias
        pure (If Float1 out 1 (stepF receiver (x4 (SampleAt Texture0 uv))))
  let filtered uv = build $ do
        texel <- share (sub2 (scale2 uv size) (splat2 0.5))
        cell <- share (floor2 texel)
        f <- share (fract2 texel)
        pure (mixF (mixF (compareCell cell) (compareCell (add2 cell (v2 1 0))) (x2 f))
          (mixF (compareCell (add2 cell (v2 0 1))) (compareCell (add2 cell (v2 1 1))) (x2 f)) (y2 f))
  let base = xy p
  let tent = repeatN Float1 9 0 $ \i total -> build $ do
        xOff <- share (floorF (i/3)-1)
        yOff <- share (i-3*(xOff+1)-1)
        let weight n = If Float1 (same n 0) 2 1
        let radius = clampF (0.055/x4 units) 0.75 2.5
        let at = add2 base (scale2 (v2 xOff yOff) (radius/size))
        pure (total + weight xOff*weight yOff*filtered at)
  pure (If Float1 outside 1 (mixF 1 (0.14+0.86*If Float1 (same quality 0) (filtered base) (tent/16)) (smooth 0.01 0.10 edge)))

waterFragment :: WorldStyle -> FragmentShader
waterFragment style = MaskedFragmentShader (waterMask style) waterColor

waterDepth :: SF local
waterDepth = build $ do
  bounds <- share (worldParameter HeightRect)
  position <- share InterpolatedPosition
  let uv = v2 ((x3 position-x4 bounds)/z4 bounds) ((z3 position-y4 bounds)/w4 bounds)
  pure (-x4 (SampleAt Texture3 uv))

waterMask :: WorldStyle -> F local Bool
waterMask FixedWorld = less waterDepth 0.01
waterMask JourneyWorld =
  let range = Length Vector2 (sub2 (xz InterpolatedPosition) (xz (rgb (worldParameter Eye))))
  in eitherTrue (ifCondition farPass (less range 90) (greaterEqual range 90)) (less waterDepth 0.01)

waterColor :: F local Vec4
waterColor = build $ do
  depth <- share waterDepth
  position <- share InterpolatedPosition
  field <- share (xz InterpolatedSourcePosition)
  sun <- share (rgb (worldParameter Sun))
  eye <- share (rgb (worldParameter Eye))
  pixel <- share (footprint2 (xz position))
  gust <- share (wind field time)
  current <- share (add2 field (v2 0 (If Float1 (same sceneNumber 2) (time*0.23) 0)))
  warped <- share $ If Float2 (greater quality 0) (build $ do
    p <- share (scale2 field 0.075)
    pure (add2 current (scale2 (v2 (noise p) (noise (add2 p (splat2 37)))) 0.55))) current
  waves <- share $ repeatN Float3 7 (v3 0 0 0.008) $ \i state -> build $ do
    k <- share (1.2*powF 1.65 i)
    angle <- share (0.42+i*1.8)
    direction <- share (v2 (cosF angle) (sinF angle))
    let phase = Dot Vector2 warped direction*k-time*sqrtF (9.81*k)*0.31
    let attenuate = expF (-0.25*square (k*pixel))
    contribution <- share (scale2 direction (cosF phase*z3 state*k*attenuate*(0.7+0.35*gust)))
    pure (v3 (x3 state+x2 contribution) (y3 state+y2 contribution) (z3 state*0.60))
  n <- share (Normalize Vector3 (v3 (-x3 waves) 1 (-y3 waves)))
  view <- share (Normalize Vector3 (sub3 eye position))
  facing <- share (maxF (Dot Vector3 n view) 0)
  fresnel <- share (0.025+0.975*powF (1-facing) 5)
  clip <- share (MatrixVector reflectionMatrix (rgba position 1))
  uv <- share (add2 (v2 (x4 clip/w4 clip*0.5+0.5) (y4 clip/w4 clip*0.5+0.5))
    (scale2 (v2 (x3 waves*0.012) (-y3 waves*0.021)) (unit depth)))
  let reflectAt coord = rgb (SampleAt Texture1 (FloatTernary Float2 Clamp coord (splat2 0.002) (splat2 0.998)))
  reflected <- share (reflectAt uv)
  halfVector <- share (Normalize Vector3 (add3 view sun))
  let nh = maxF (Dot Vector3 n halfVector) 0
  let nl = maxF (Dot Vector3 n sun) 0.001
  let sunFresnel = 0.02+0.98*powF (1-maxF (Dot Vector3 view halfVector) 0) 5
  let glint nv rough a2 tint = letExpr a2 $ \a ->
        let distribution = a / (3.14159*square (nh*nh*(a-1)+1))
        in scale3 tint (distribution*ggxVisibility nv nl (rough*0.5) 0*sunFresnel/(4*nv))
  color <- share $ If Float3 (greater quality 0)
    (build $ do
      sizes <- share (worldParameter ImageSizes)
      spread <- share (scale2 (v2 (1/y4 sizes) (1/z4 sizes)) (0.65+1.3*unit (1-y3 view)))
      let horizontal = v2 (x2 spread) 0
      let vertical = v2 0 (y2 spread)
      let surrounding = foldr1 add3 [reflectAt (add2 uv horizontal), reflectAt (sub2 uv horizontal),
            reflectAt (add2 uv vertical), reflectAt (sub2 uv vertical)]
      let softened = add3 (scale3 reflected 0.40) (scale3 surrounding 0.15)
      path <- share (depth/maxF (y3 view) 0.22)
      let pebbles = noise (scale2 field 4.5)
      let fine = mixF 0.5 (noise (scale2 field 19)) (expF (-pixel*25))
      bed <- share (scale3 (mix3 (v3 0.16 0.185 0.12) (v3 0.37 0.34 0.23) pebbles) (0.84+0.24*fine))
      absorption <- share (exp3 (scale3 (v3 (-0.43) (-0.16) (-0.10)) path))
      let cx = sinF (x2 field*2.3+sinF (y2 field*1.7+time*0.22))
      let cz = sinF (y2 field*2.5-sinF (x2 field*1.3-time*0.19))
      let caustic = scale3 (v3 0.12 0.14 0.07) (powF (maxF 0 (cx*cz)) 5*expF (-path*0.8)*expF (-pixel*8))
      let submerged = add3 (add3 (scale3 (mul3 bed absorption) 0.82)
            (mul3 (v3 0.018 0.105 0.116) (sub3 (splat3 1) absorption))) caustic
      let liquid = mix3 submerged softened (unit (fresnel+0.035))
      dx <- share (Derivative ScreenX Float3 n)
      dy <- share (Derivative ScreenY Float3 n)
      rough <- share (0.15+0.025*gust)
      let a2 = rough*rough + minF 0.035 (Dot Vector3 dx dx+Dot Vector3 dy dy)
      let lit = add3 liquid (glint (maxF facing 0.025) rough a2 (v3 1.85 1.53 1.08))
      pure (mix3 (scale3 bed 0.42) lit (smooth 0.01 0.12 depth)))
    (build $ do
      bed <- share (mix3 (v3 0.22 0.22 0.105) (v3 0.40 0.39 0.20) (noise (scale2 field 5)))
      let absorption = exp3 (scale3 (v3 (-0.40) (-0.18) (-0.12)) depth)
      let under = FloatTernary Float3 Mix (v3 0.022 0.105 0.105) (scale3 bed 0.75) absorption
      let focus = 0.5+0.5*sinF (x2 field*5+sinF (y2 field*3+time*0.4)+time*0.7)
      let caustic = scale3 (v3 0.08 0.10 0.05) (powF focus 7*expF (-depth*1.4))
      let liquid = mix3 (add3 under caustic) reflected (unit (fresnel+0.12))
      rough <- share (0.12+0.035*gust)
      let lit = add3 liquid (glint (maxF facing 0.001) rough (rough*rough) (v3 1.3 1.12 0.70))
      pure (mix3 (scale3 bed 0.35) lit (smooth 0.01 0.16 depth)))
  pure (rgba (fogged sun eye position color) 1)

postShader :: FragmentShader
postShader = FragmentShader $ build $ do
  p <- share (parameter 0)
  uv <- share InterpolatedUV
  let tier = x4 (parameter 1)
  let displayed coord = build $ do
        linear <- share (scale3 (rgb (SampleTexture coord)) 0.88)
        let aces = divide3 (mul3 linear (add3 (scale3 linear 2.51) (splat3 0.03)))
              (add3 (mul3 linear (add3 (scale3 linear 2.43) (splat3 0.59))) (splat3 0.14))
        pure (pow3 (clamp3 aces 0 1) (1/2.2))
  let taps radius = map (displayed . add2 uv)
        [radius, scale2 radius (-1), v2 (x2 radius) (-y2 radius), v2 (-x2 radius) (y2 radius)]
  let supersample = letExpr (v2 (0.28/maxF (z4 p) 1) (0.28/maxF (w4 p) 1)) $ \r ->
        scale3 (foldr1 add3 (taps r)) 0.25
  let detail = build $ do
        center <- share (displayed uv)
        let x = v2 (x4 p) 0
        let y = v2 0 (y4 p)
        east <- share (displayed (add2 uv x))
        west <- share (displayed (sub2 uv x))
        north <- share (displayed (sub2 uv y))
        south <- share (displayed (add2 uv y))
        let brightness c = Dot Vector3 c (v3 0.299 0.587 0.114)
        lc <- share (brightness center)
        neighborLights <- traverse (share . brightness) [east, west, north, south]
        high <- share (foldr maxF lc neighborLights)
        low <- share (foldr minF lc neighborLights)
        let brightnessRange = high-low
        let contrast = absF (lc-sum neighborLights*0.25)/brightnessRange
        let filterColor = scale3 (foldr1 add3 ([center,east,west,north,south] ++ taps (v2 (x4 p) (y4 p)))) (1/9)
        pure (If Float3 (less brightnessRange (maxF 0.035 (high*0.10))) center
          (mix3 center filterColor (clampF ((contrast-0.12)*1.35) 0 0.70)))
  delta <- share (sub2 uv (splat2 0.5))
  pure (rgba (scale3 (If Float3 (same tier 3) supersample detail) (1-0.105*Dot Vector2 delta delta)) 1)
