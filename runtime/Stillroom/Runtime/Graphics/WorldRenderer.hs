module Stillroom.Runtime.Graphics.WorldRenderer
  ( WorldRenderer, openWorldRenderer, closeWorldRenderer
  , WorldTargets, targets, selectedTier, targetRevision, prepareTargets, currentTargets, setAllocationLimitsForCheck
  , ChunkDraw(..), WorldFrame(..), worldDraw, projectClip, objectTransform, gpuPoint, gpuDirection
  ) where

import Control.Exception (bracketOnError, finally, mask_, throwIO, tryJust)
import Control.Monad (when)
import Data.IORef
import Data.Map.Strict qualified as Map
import GameIO.Graphics qualified as GPU
import GameIO.Graphics.Shader
import Stillroom.Graphics qualified as G
import Stillroom.Graphics.Observation qualified as O
import Stillroom.Journey.Frame qualified as F
import Stillroom.Landscape.Geometry qualified as Land
import Stillroom.Runtime.Graphics.Scene
import Stillroom.Runtime.Graphics.WorldGeometry qualified as Geometry
import Stillroom.Runtime.Graphics.WorldShaders

data WorldRenderer = WorldRenderer
  { renderer :: !GPU.Renderer
  , style :: !WorldStyle
  , meshPrograms :: !(Map.Map Surface (GPU.Material, GPU.Material))
  , grassProgram :: !GPU.Material
  , waterProgram :: !GPU.Material
  , skyProgram :: !GPU.Material
  , postProgram :: !GPU.Material
  , template :: !GPU.InstanceTemplate
  , worldTargets :: !(IORef (Maybe WorldTargets))
  , allocationLimits :: !(IORef (Int, Int))
  , scope :: !Scope
  }

openWorldRenderer :: GPU.Renderer -> WorldStyle -> IO WorldRenderer
openWorldRenderer gpu styleValue = bracketOnError newScope closeScope $ \resources -> do
  let material mode vertex fragment = own resources (GPU.createMaterial gpu (GPU.MaterialConfig mode vertex fragment)) (GPU.destroyMaterial gpu)
  land <- traverse (\surface -> do
    let vertex = landVertex styleValue surface False
    lit <- material GPU.WorldDepth vertex (landFragment styleValue surface)
    shadow <- material GPU.WorldDepth vertex shadowFragment
    pure (surface, (lit, shadow))) [minBound .. maxBound]
  grass <- material GPU.WorldDepth (landVertex styleValue Blades True) (landFragment styleValue Blades)
  water <- material GPU.WorldDepth waterVertex (waterFragment styleValue)
  sky <- material GPU.OverlayReplace defaultVertexShader skyShader
  post <- material GPU.OverlayReplace defaultVertexShader postShader
  blade <- Geometry.grassTemplate gpu resources
  cache <- newIORef Nothing
  limits <- newIORef (0,0)
  pure (WorldRenderer gpu styleValue (Map.fromList land) grass water sky post blade cache limits resources)

closeWorldRenderer :: WorldRenderer -> IO ()
closeWorldRenderer world = mask_ $ do
  old <- atomicModifyIORef' (worldTargets world) (\a -> (Nothing, a))
  closeScope (scope world) `finally` mapM_ (closeScope . imagesScope) old

data WorldTargets = WorldTargets
  { requestKey :: !(G.Tier, Bool, Int, Int)
  , targets :: !F.Targets
  , selectedTier :: !G.Tier
  , shadowMap :: !GPU.RenderTarget
  , reflectionImage :: !GPU.RenderTarget
  , hdrImage :: !GPU.RenderTarget
  , targetRevision :: !Int
  , imagesScope :: !Scope
  }

currentTargets :: WorldRenderer -> IO (Maybe WorldTargets)
currentTargets = readIORef . worldTargets

setAllocationLimitsForCheck :: WorldRenderer -> Int -> Int -> IO ()
setAllocationLimitsForCheck world color depth = writeIORef (allocationLimits world) (color, depth)

prepareTargets :: WorldRenderer -> G.Tier -> Bool -> Int -> Int -> IO (Maybe WorldTargets)
prepareTargets world tier reference width height
  | width < 1 || height < 1 = pure Nothing
  | otherwise = mask_ $ do
      old <- currentTargets world
      case old of
        Just available | requestKey available == key -> pure old
        _ -> do
          let revision = maybe 0 ((+1) . targetRevision) old
          fresh <- allocate tier revision
          case fresh of
            Just next -> do
              writeIORef (worldTargets world) fresh
              mapM_ (closeScope . imagesScope) old
              pure (Just next)
            Nothing | not reference -> do
              let retained = fmap (\image -> image {requestKey = key}) old
              writeIORef (worldTargets world) retained
              pure retained
            Nothing -> pure Nothing
  where
    key = (tier, reference, width, height)
    gpu = renderer world
    allocate candidate revision = do
      built <- tryJust imageAllocationError $ bracketOnError newScope closeScope $ \resources -> do
        (colorLimit, depthLimit) <- readIORef (allocationLimits world)
        let sizes = targetSizes candidate reference width height
        let make limit format w h = do
              when (limit > 0 && max w h > limit) (throwIO (GPU.InvalidTargetSize w h))
              own resources (GPU.createRenderTarget gpu format w h) (GPU.destroyRenderTarget gpu)
        let shadowSize = F.shadowResolution sizes
        shadow <- make depthLimit GPU.RGBA8 shadowSize shadowSize
        reflection <- make colorLimit GPU.RGBA16Float (F.reflectionWidth sizes) (F.reflectionHeight sizes)
        hdr <- make colorLimit GPU.RGBA16Float (F.rasterWidth sizes) (F.rasterHeight sizes)
        pure (WorldTargets key sizes candidate shadow reflection hdr revision resources)
      case built of
        Right images -> pure (Just images)
        Left () | not reference && candidate > G.Economy -> allocate (pred candidate) revision
        Left () -> pure Nothing

targetSizes :: G.Tier -> Bool -> Int -> Int -> F.Targets
targetSizes quality reference width height = F.Targets w h shadow rw rh
  where
    (widthLimit, heightLimit, shadow, reflectionScale) = case quality of
      G.Economy -> (1120, 630, 1024, 0.5)
      G.Balanced -> (1600, 900, 2048, 0.5)
      G.Detailed -> (2560, 1440, 3072, 0.75)
      G.Supersampled -> (3840, 2160, 4096, 1)
    wf = fromIntegral width :: Float
    hf = fromIntegral height :: Float
    ratio = if reference then 1 else min (if quality == G.Supersampled then 1.5 else 1) (min (widthLimit/wf) (heightLimit/hf))
    w = max 1 (floor (wf*ratio))
    h = max 1 (floor (hf*ratio))
    scale = if reference then 1 else reflectionScale :: Float
    rw = ceiling (fromIntegral w*scale)
    rh = ceiling (fromIntegral h*scale)

data ChunkDraw = ChunkDraw
  { geometry :: !Geometry.Geometry
  , distant :: !Bool
  , showLand :: !Bool
  , castShadow :: !Bool
  , showGrass :: !Bool
  }

data WorldFrame = WorldFrame
  { sceneIndex :: !Int
  , sceneTime :: !Double
  , sunDirection :: !(F.Direction F.Render)
  , worldOrigin :: !(F.Point F.World)
  , referencePoint :: !(F.Point F.Render)
  , sunObservation :: !(O.Observation F.ImageSize)
  , reflectionObservation :: !(O.Observation F.ImageSize)
  , mainObservation :: !(O.Observation F.ImageSize)
  , shadowUnits :: !GPU.Vec4
  , chunks :: ![ChunkDraw]
  , car :: !(Maybe (GPU.Mesh, F.Transform F.Object F.Render))
  }

worldDraw :: WorldRenderer -> WorldTargets -> WorldFrame -> SceneRect -> SceneDraw
worldDraw world images frame rect@(SceneRect _ _ outputWidth outputHeight) = SceneDraw passes [display] (const (pure ()))
  where
    shadeMap = GPU.renderTargetDepth (shadowMap images)
    mirror = GPU.renderTargetTexture (reflectionImage images)
    tier = fromEnum (selectedTier images)
    sizes = targets images
    F.Point ox oy oz = worldOrigin frame
    light = projectClip (O.clipFromRender (sunObservation frame))
    reflection = projectClip (O.clipFromRender (reflectionObservation frame))
    time = realToFrac (sceneTime frame)
    blank = GPU.Color 0 0 0 1
    sun = gpuDirection (sunDirection frame)
    parameters observation clip far anchor bones heightRect isWater object = WorldParameters
      time tier (if isWater && style world == JourneyWorld then 2 else sceneIndex frame) clip
      (gpuPoint (O.passEye observation)) far sun (imageHeight observation)
      (Geometry.absoluteAnchor anchor) bones (Geometry.relativeTo anchor (ox, oy, oz)) (gpuPoint (referencePoint frame))
      (shadowUnits frame) (F.shadowResolution sizes) (F.reflectionWidth sizes) (F.reflectionHeight sizes)
      heightRect light reflection object
    imageHeight observation = case O.imageTarget observation of F.ImageSize _ height -> height
    land observation isShadow clip chunk =
      let g = geometry chunk
          matrix = projectClip (O.clipFromRender observation)
          uniform = worldParameters (parameters observation clip (distant chunk) (Geometry.landAnchor g)
            (Geometry.boneHeight g) (GPU.Vec4 0 0 0 0) False GPU.identityMat4)
          textures = (Texture2, Geometry.boneTexture g) : [(Texture0, shadeMap) | not isShadow]
          inputs = GPU.ShaderInputs textures uniform
          kind material = case material of
            Land.Terrain -> Terrain
            Land.Wood -> Bark
            Land.Foliage -> Leaves
            Land.Grass -> Blades
          surface = [GPU.WorldDrawWith mesh (if isShadow then shadowProgram else program) inputs matrix
            | (material, mesh) <- Geometry.landMeshes g
            , material /= Land.Grass || (showGrass chunk && not isShadow)
            , Just (program, shadowProgram) <- [Map.lookup (kind material) (meshPrograms world)]]
          grass = [GPU.WorldInstances (template world) instanceData (grassProgram world) inputs matrix
            | showGrass chunk && not isShadow, Just instanceData <- [Geometry.grassInstances g]]
      in surface ++ grass
    carDraw observation isShadow clip =
      [GPU.WorldDrawWith mesh (if isShadow then shadowProgram else program)
        (GPU.ShaderInputs [(Texture0, shadeMap) | not isShadow] uniforms) (projectClip (O.clipFromRender observation))
      | Just (mesh, transform) <- [car frame]
      , let F.Point cx cy cz = O.passEye observation
      , let (x, y, z, _) = F.transform transform (F.Point 0 0 0)
      , let squared a = a*a
      , squared (cx-x) + squared (cy-y) + squared (cz-z) <= 540*540
      , Just (program, shadowProgram) <- [Map.lookup Carriage (meshPrograms world)]
      , let uniforms = worldParameters (parameters observation clip False (Geometry.Anchor 0 0) 1
              (GPU.Vec4 0 0 0 0) False (objectTransform transform))]
    water observation =
      [GPU.WorldDrawWith mesh (waterProgram world) (GPU.ShaderInputs [(Texture1, mirror), (Texture3, height)] uniforms)
        (projectClip (O.clipFromRender observation))
      | chunk <- chunks frame
      , let g = geometry chunk
      , Just (mesh, height) <- [Geometry.waterMesh g]
      , let anchor@(Geometry.Anchor ax az) = Geometry.landAnchor g
      , let extent = if distant chunk then 128 else 32
      , let texel = if distant chunk then 8 else 1
      , let range = if style world == FixedWorld then GPU.Vec4 (-130) (-200) 260 224 else
              GPU.Vec4 (realToFrac (ax-ox-texel/2)) (realToFrac (az-oz-texel/2)) (realToFrac (extent+texel)) (realToFrac (extent+texel))
      , let uniforms = worldParameters (parameters observation False (distant chunk) anchor
              (Geometry.boneHeight g) range True GPU.identityMat4)]
    shaded observation clip = concatMap (land observation False clip) (filter showLand (chunks frame)) ++ carDraw observation False clip
    shadows = concatMap (land (sunObservation frame) True False) (filter castShadow (chunks frame))
      ++ carDraw (sunObservation frame) True False
    sky observation = GPU.OverlayBatchWith Nothing (skyProgram world) (GPU.ShaderInputs [] (skyParameters observation sun))
      (rectangleVertices (wholeSurface w h) (GPU.Vec4 0 0 1 1) (GPU.Color 1 1 1 1))
      where F.ImageSize w h = O.imageTarget observation
    reflected = GPU.FrameWithBackground blank [sky (reflectionObservation frame)] (shaded (reflectionObservation frame) True) []
    opaque = GPU.FrameWithBackground blank [sky (mainObservation frame)]
      (shaded (mainObservation frame) False ++ water (mainObservation frame)) []
    passes = [GPU.RenderPass (shadowMap images) (GPU.Frame (GPU.Color 1 1 1 1) shadows []),
      GPU.RenderPass (reflectionImage images) reflected, GPU.RenderPass (hdrImage images) opaque]
    postParameters = [GPU.Vec4 (1/fromIntegral (F.rasterWidth sizes)) (1/fromIntegral (F.rasterHeight sizes))
      outputWidth outputHeight, GPU.Vec4 (fromIntegral tier) 0 0 0]
    display = GPU.OverlayBatchWith Nothing (postProgram world)
      (GPU.ShaderInputs [(Texture0, GPU.renderTargetTexture (hdrImage images))] postParameters)
      (rectangleVertices rect (GPU.Vec4 0 0 1 1) (GPU.Color 1 1 1 1))

skyParameters :: O.Observation F.ImageSize -> GPU.Vec3 -> [GPU.Vec4]
skyParameters observation (GPU.Vec3 sx sy sz) =
  [GPU.Vec4 (cvt (-c)) (cvt (-g)) (cvt (-k)) (cvt (1/fov)),
   GPU.Vec4 (cvt a) (cvt e) (cvt i) (fromIntegral width / fromIntegral height),
   GPU.Vec4 (cvt b) (cvt f) (cvt j) 0, GPU.Vec4 sx sy sz 0]
  where
    ((a,b,c,_),(e,f,g,_),(i,j,k,_),_) = F.matrixElements (O.viewMatrix observation)
    (_,(_,fov,_,_),_,_) = F.matrixElements (O.projectionMatrix observation)
    F.ImageSize width height = O.imageTarget observation
    cvt = realToFrac

projectClip :: F.Transform a F.Clip -> GPU.Mat4
projectClip matrix = case F.matrixElements matrix of
  (a,b,c,d) -> GPU.mat4FromColumns (column a) (column b) (column c) (column d)
  where
    column (x,y,z,w) = GPU.Vec4 (realToFrac x) (realToFrac (negate y)) (realToFrac ((z+w)*0.5)) (realToFrac w)

objectTransform :: F.Transform F.Object F.Render -> GPU.Mat4
objectTransform matrix = case F.matrixElements matrix of
  (a,b,c,d) -> GPU.mat4FromColumns (column a) (column b) (column c) (column d)
  where
    column (x,y,z,w) = GPU.Vec4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w)

gpuPoint :: F.Point space -> GPU.Vec3
gpuPoint (F.Point x y z) = GPU.Vec3 (realToFrac x) (realToFrac y) (realToFrac z)

gpuDirection :: F.Direction space -> GPU.Vec3
gpuDirection (F.Direction x y z) = GPU.Vec3 (realToFrac x) (realToFrac y) (realToFrac z)
