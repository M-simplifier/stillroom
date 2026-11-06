{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Graphics.Landscape (Landscape, load, release, available, quality, reference, draw, info) where

import Control.Exception
import Control.Monad (forM_, when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.IntMap.Strict qualified as Map
import Raylib.Core qualified as R
import Raylib.Types
import Raylib.Util.Math
import Raylib.Util.RLGL qualified as RL
import Stillroom.Landscape.Geometry qualified as G
import Stillroom.Runtime.Graphics.GL qualified as GL
import Stillroom.Runtime.Graphics.Renderer
import Stillroom.Runtime.Graphics.Resource
import Stillroom.Runtime.Graphics.Target qualified as Target

data Landscape = Landscape
  { renderer :: !Renderer,
    cache :: !(IORef (Map.IntMap GpuGeometry)),
    generation :: !(IORef (Map.IntMap Double)),
    activeScene :: !(IORef Int),
    cachedFrame :: !(IORef (Maybe (Int, Integer, Int))),
    referenceMode :: !(IORef Bool),
    draws :: !(IORef Int)
  }

load :: [BS.ByteString] -> IO Landscape
load sources = bracketOnError (loadRenderer sources) releaseRenderer $ \r -> do
  geometries <- newIORef Map.empty
  timings <- newIORef Map.empty
  scene <- newIORef (-1)
  frame <- newIORef Nothing
  fullRaster <- newIORef False
  count <- newIORef 0
  pure
    Landscape
      { renderer = r,
        cache = geometries,
        generation = timings,
        activeScene = scene,
        cachedFrame = frame,
        referenceMode = fullRaster,
        draws = count
      }

release :: Landscape -> IO ()
release landscape = do
  old <- atomicModifyIORef' (cache landscape) (\m -> (Map.empty, m))
  mapM_ releaseGeometry old `finally` releaseRenderer (renderer landscape)

available :: Landscape -> IO Bool
available = healthy . renderer

quality :: Landscape -> Int -> IO ()
quality landscape q = setQuality (renderer landscape) q >> pure ()

reference :: Landscape -> Bool -> IO ()
reference landscape enabled = do
  old <- readIORef (referenceMode landscape)
  when (old /= enabled) $ do
    writeIORef (referenceMode landscape) enabled
    Target.invalidateTargets (targets (renderer landscape))

geometryFor :: Landscape -> Int -> IO GpuGeometry
geometryFor landscape scene = mask_ $ do
  existing <- readIORef (cache landscape)
  case Map.lookup scene existing of
    Just geometry -> pure geometry
    Nothing -> do
      began <- R.getTime
      let choice = case scene of
            0 -> G.BirchLake
            1 -> G.AlpineLake
            _ -> G.MeadowStream
      geometry <-
        either
          (throwIO . GeometryUnavailable . show)
          (uploadGeometry (grassTemplate (renderer landscape)))
          (G.fixedSceneGeometry choice)
      ended <- R.getTime
      modifyIORef' (cache landscape) (Map.insert scene geometry)
      modifyIORef' (generation landscape) (Map.insert scene ((ended - began) * 1000))
      pure geometry

draw :: Landscape -> Int -> Double -> Rectangle -> IO Bool
draw landscape scene seconds rectangle@(Rectangle _ _ width height)
  | scene < 0 || scene > 2 || any (\x -> isNaN x || isInfinite x) [seconds, realToFrac width, realToFrac height] || width < 1 || height < 1 = pure False
  | otherwise = do
      ok <- available landscape
      if not ok
        then pure False
        else do
          result <- try $ bracket saveParent restoreParent $ \_ -> do
            geometry <- geometryFor landscape scene
            writeIORef (activeScene landscape) scene
            ref <- readIORef (referenceMode landscape)
            allocated <- Target.budgetTargets (targets r) width height ref
            if not allocated
              then failRenderer r "Landscape framebuffer unavailable" >> pure Nothing
              else do
                ts <- Target.readTargets (targets r)
                setShaderQuality r (Target.currentQuality ts)
                let tick = floor (max 0 seconds * 30) :: Integer
                    key = (scene, tick, Target.targetRevision ts)
                previous <- readIORef (cachedFrame landscape)
                case (Target.shadowTarget ts, Target.reflectionTarget ts, Target.hdrTarget ts) of
                  (Just shadow, Just reflection, Just hdr) -> do
                    when (previous /= Just key) $ do
                      render landscape geometry scene (realToFrac (fromIntegral tick / 30 :: Double)) shadow reflection hdr
                      writeIORef (cachedFrame landscape) (Just key)
                      modifyIORef' (draws landscape) (+ 1)
                    pure (Just hdr)
                  _ -> failRenderer r "Landscape targets unavailable" >> pure Nothing
          case result of
            Left (e :: GraphicsError) -> failRenderer r (show e)
            Right image -> forM_ image (\target -> composite r target rectangle)
          e <- GL.getError
          when (e /= 0) (failRenderer r ("Landscape OpenGL error " ++ show e))
          available landscape
  where
    r = renderer landscape

render :: Landscape -> GpuGeometry -> Int -> Float -> RenderTexture -> RenderTexture -> RenderTexture -> IO ()
render landscape geometry scene time shadow reflection hdr = do
  let r = renderer landscape
      sun = vectorNormalize (Vector3 (-0.58) 0.68 0.45)
      camera = case scene of
        1 -> Camera3D (Vector3 1 3 13) (Vector3 0 3.5 (-34)) (Vector3 0 1 0) 49 CameraPerspective
        2 -> Camera3D (Vector3 0 2.5 10) (Vector3 3 1.6 (-35)) (Vector3 0 1 0) 52 CameraPerspective
        _ -> Camera3D (Vector3 0.7 2.5 8.3) (Vector3 1.3 1.55 (-27)) (Vector3 0 1 0) 49 CameraPerspective
      light = Camera3D (Vector3 0 0 (-25) |+| (sun |* 95)) (Vector3 0 0 (-25)) (Vector3 0 1 0) 105 CameraOrthographic
      mirror = camera {camera3D'position = mirrorY (camera3D'position camera), camera3D'target = mirrorY (camera3D'target camera)}
      mirrorY (Vector3 x y z) = Vector3 x (-y) z
      world lightVP shadowPass clip cam = drawWorld r geometry scene sun lightVP (renderTexture'depth shadow) time 7 shadowPass clip (camera3D'position cam)
  lightVP <- textureMode shadow $ do
    R.clearBackground white
    mode3D light $ do
      vp <- matrixMultiply <$> RL.rlGetMatrixModelview <*> RL.rlGetMatrixProjection
      world vp True False light
      pure vp
  reflectionVP <- textureMode reflection $ do
    R.clearBackground black
    let image = renderTexture'texture reflection
    fixedSky r scene sun mirror (texture'width image) (texture'height image)
    mode3D mirror $ do
      vp <- matrixMultiply <$> RL.rlGetMatrixModelview <*> RL.rlGetMatrixProjection
      world lightVP False True mirror
      pure vp
  textureMode hdr $ do
    R.clearBackground black
    let image = renderTexture'texture hdr
    fixedSky r scene sun camera (texture'width image) (texture'height image)
    mode3D camera $ do
      world lightVP False False camera
      let p = waterProgram r
      useMaterial (material r) p
      vec3 p "uEye" (camera3D'position camera)
      vec3 p "uSun" sun
      float p "uTime" time
      int p "uLandscape" scene
      matrix p "uReflectionVP" reflectionVP
      texture p "uReflection" (renderTexture'texture reflection) 10
      forM_ (gpuHeight geometry) (\heightImage -> texture p "uHeight" heightImage 11)
      RL.rlDisableBackfaceCulling
      forM_ (gpuWater geometry) (drawMesh (material r)) `finally` RL.rlEnableBackfaceCulling

info :: Landscape -> IO String
info landscape = do
  scene <- readIORef (activeScene landscape)
  geometry <- Map.lookup scene <$> readIORef (cache landscape)
  ts <- Target.readTargets (targets (renderer landscape))
  timings <- readIORef (generation landscape)
  count <- readIORef (draws landscape)
  name <- GL.rendererName
  pure $
    BL.unpack $
      encode $
        object
          [ "quality" .= Target.currentQuality ts,
            "shadowWidth" .= maybe 0 (texture'width . renderTexture'depth) (Target.shadowTarget ts),
            "reflectionWidth" .= maybe 0 (texture'width . renderTexture'texture) (Target.reflectionTarget ts),
            "scene" .= scene,
            "trees" .= maybe 0 gpuTrees geometry,
            "blades" .= maybe 0 gpuGrass geometry,
            "grassRenderingPath" .= grassRenderingPath (grassTemplate (renderer landscape)),
            "grassInstances" .= maybe 0 grassInstanceCount geometry,
            "grassInstanceBytes" .= (40*maybe 0 grassInstanceCount geometry),
            "grassSharedCornerBytes" .= (180 :: Int),
            "triangles" .= maybe 0 geometryTriangles geometry,
            "draws" .= count,
            "width" .= Target.rasterWidth ts,
            "height" .= Target.rasterHeight ts,
            "generationMs" .= map (\i -> Map.findWithDefault 0 i timings) [0, 1, 2],
            "renderer" .= name
          ]
