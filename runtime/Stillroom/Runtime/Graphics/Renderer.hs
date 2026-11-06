{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Graphics.Renderer
  ( Renderer (..),
    loadRenderer,
    releaseRenderer,
    healthy,
    failRenderer,
    setQuality,
    setShaderQuality,
    prepareWorld,
    drawGeometry,
    drawWorld,
    fixedSky,
    plannedSky,
    composite,
  )
where

import Control.Exception (bracketOnError)
import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.IORef
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Textures (drawTexturePro)
import Raylib.Types
import Raylib.Util.Math
import Stillroom.Graphics.Observation qualified as O
import Stillroom.Journey.Frame qualified as F
import Stillroom.Runtime.Graphics.Resource
import Stillroom.Runtime.Graphics.Target qualified as Target

data Renderer = Renderer
  { landProgram :: !Program,
    waterProgram :: !Program,
    skyProgram :: !Program,
    postProgram :: !Program,
    material :: !MaterialHandle,
    targets :: !Target.TargetSet,
    issue :: !(IORef (Maybe String)),
    rendererScope :: !Scope
  }

loadRenderer :: [BS.ByteString] -> IO Renderer
loadRenderer sources = bracketOnError newScope closeScope $ \scope -> case sources of
  [common, landVS, landFS, waterVS, waterFS, skyFS, postFS] -> do
    land <- loadProgram scope common (Just landVS) landFS "uTime"
    water <- loadProgram scope common (Just waterVS) waterFS "uReflection"
    skyShader <- loadProgram scope common Nothing skyFS "uForward"
    post <- loadProgram scope common Nothing postFS "texture0"
    mat <- loadMaterial scope
    ts <- own scope Target.newTargets Target.releaseTargets
    problem <- newIORef Nothing
    let renderer = Renderer land water skyShader post mat ts problem scope
    _ <- setQuality renderer 1
    pure renderer
  _ -> ioError (userError "Landscape shader source count")

releaseRenderer :: Renderer -> IO ()
releaseRenderer = closeScope . rendererScope

healthy :: Renderer -> IO Bool
healthy renderer = (== Nothing) <$> readIORef (issue renderer)

failRenderer :: Renderer -> String -> IO ()
failRenderer renderer = writeIORef (issue renderer) . Just

setQuality :: Renderer -> Int -> IO Bool
setQuality renderer desired = do
  ok <- healthy renderer
  if not ok
    then pure False
    else do
      selected <- Target.selectQuality (targets renderer) desired
      when (not selected) (failRenderer renderer "Landscape shadow allocation failed")
      pure selected

setShaderQuality :: Renderer -> Int -> IO ()
setShaderQuality r q = mapM_ (\p -> int p "uQuality" q) [landProgram r, waterProgram r, skyProgram r, postProgram r]

drawWorld :: Renderer -> GpuGeometry -> Int -> Vector3 -> Matrix -> Texture -> Float -> Int -> Bool -> Bool -> Vector3 -> IO ()
drawWorld renderer geometry scene sun light shadow time mask shadowPass clip eye = do
  prepareWorld renderer scene sun light shadow shadowPass clip eye
  drawGeometry renderer geometry time mask shadowPass

prepareWorld :: Renderer -> Int -> Vector3 -> Matrix -> Texture -> Bool -> Bool -> Vector3 -> IO ()
prepareWorld renderer scene sun light shadow shadowPass clip eye = do
  let p = landProgram renderer
  int p "uLandscape" scene
  useMaterial (material renderer) p
  int p "uShadowPass" (if shadowPass then 1 else 0)
  int p "uClip" (if clip then 1 else 0)
  vec3 p "uEye" eye
  vec3 p "uSun" sun
  matrix p "uLightVP" light
  texture p "uShadow" shadow 9

drawGeometry :: Renderer -> GpuGeometry -> Float -> Int -> Bool -> IO ()
drawGeometry renderer geometry time mask shadowPass = do
  let p = landProgram renderer
  texture p "uBones" (gpuBones geometry) 8
  doubleSided (material renderer) $
    forM_ (zip [0 ..] (gpuMaterials geometry)) $ \(kind, mesh) ->
      when (not (kind == 3 && (shadowPass || mask .&. 4 == 0))) $ forM_ mesh $ \m -> do
        int p "uKind" kind
        float p "uTime" (if mask .&. (if kind == 3 then 1 else 2) /= 0 then time else 0)
        drawMesh (material renderer) m

fixedSky :: Renderer -> Int -> Vector3 -> Camera3D -> Int -> Int -> IO ()
fixedSky renderer scene sun camera width height = do
  let forward = vectorNormalize (camera3D'target camera |-| camera3D'position camera)
      right = vectorNormalize (vector3CrossProduct forward (camera3D'up camera))
      up = vector3CrossProduct right forward
  sky renderer scene sun forward right up (tan (camera3D'fovy camera * pi / 180 * 0.5)) width height

plannedSky :: Renderer -> Int -> Vector3 -> O.Observation F.ImageSize -> IO ()
plannedSky renderer scene sun observation = do
  view <- nativeTransform (O.viewMatrix observation)
  projection <- nativeTransform (O.projectionMatrix observation)
  let right = Vector3 (matrix'm0 view) (matrix'm4 view) (matrix'm8 view)
      up = Vector3 (matrix'm1 view) (matrix'm5 view) (matrix'm9 view)
      forward = Vector3 (-matrix'm2 view) (-matrix'm6 view) (-matrix'm10 view)
      F.ImageSize width height = O.imageTarget observation
  sky renderer scene sun forward right up (1 / matrix'm5 projection) width height

sky :: Renderer -> Int -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> Float -> Int -> Int -> IO ()
sky renderer scene sun forward right up fov width height = do
  let p = skyProgram renderer
  int p "uLandscape" scene
  vec2 p "uResolution" (Vector2 (fromIntegral width) (fromIntegral height))
  vec3 p "uForward" forward
  vec3 p "uRight" right
  vec3 p "uUp" up
  vec3 p "uSun" sun
  float p "uTanFov" fov
  shaderMode p (drawRectangle 0 0 width height white)

composite :: Renderer -> RenderTexture -> Rectangle -> IO ()
composite renderer image rectangle@(Rectangle _ _ width height) = do
  let p = postProgram renderer
      t = renderTexture'texture image
  vec2 p "uOutputResolution" (Vector2 width height)
  shaderMode p (drawTexturePro t (Rectangle 0 0 (fromIntegral (texture'width t)) (-fromIntegral (texture'height t))) rectangle (Vector2 0 0) 0 white)
