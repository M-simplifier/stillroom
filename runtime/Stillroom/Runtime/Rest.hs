{-# LANGUAGE PatternSynonyms #-}

-- | Texture-free procedural scenes. Rendering changes
-- neither the music nor the normal picture. All resources belong to the window.
module Stillroom.Runtime.Rest
  ( RestAssets,
    loadRestAssets,
    renderRest,
    renderPreview,
    renderThumbnail,
    restIssue,
    thumbnailsReady,
    proceduralReady,
  )
where

import Control.Exception (IOException, bracket_, catch)
import Control.Monad (forM)
import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (isJust)
import Raylib.Core qualified as R
import Raylib.Core.Shapes (drawRectangleRec)
import Raylib.Core.Textures (drawTexturePro, loadRenderTexture, setTextureFilter)
import Raylib.Internal (WindowResources, managed)
import Raylib.Types
import Stillroom.Domain
import Stillroom.Runtime.Pictures qualified as Pictures
import Stillroom.Session
import System.FilePath ((</>))

data ProceduralShader = ProceduralShader !Shader !Int !Int !Int !Int

data RestAssets = RestAssets (Maybe ProceduralShader) [(RestScene, Texture)] RenderTexture

proceduralIndex :: RestScene -> Int
proceduralIndex TidalLight = 0
proceduralIndex LightVeil = 1
proceduralIndex StarryNight = 2
proceduralIndex AmberSea = 3
proceduralIndex Shallows = 4
proceduralIndex CloudSea = 5
proceduralIndex Aurora = 6
proceduralIndex InkFlow = 7

loadProgram :: WindowResources -> FilePath -> IO (Maybe Shader)
loadProgram window name = do
  source <-
    (Just . BSC.unpack <$> (Pictures.assetPath ("assets" </> "rest" </> name) >>= BSC.readFile))
      `catch` (\(_ :: IOException) -> pure Nothing)
  traverse (\code -> managed window (R.loadShaderFromMemory Nothing (Just code))) source

loadRestAssets :: WindowResources -> IO RestAssets
loadRestAssets window = do
  program <- loadProgram window "procedural.fs"
  procedural <- case program of
    Nothing -> pure Nothing
    Just loaded -> do
      time <- R.getShaderLocation loaded "phase" window
      size <- R.getShaderLocation loaded "resolution" window
      choice <- R.getShaderLocation loaded "scene" window
      origin <- R.getShaderLocation loaded "origin" window
      -- A failed compilation can return raylib's valid default shader.
      -- Required uniforms distinguish that fallback from this program.
      pure (if all (>= 0) [time, size, choice, origin] then Just (ProceduralShader loaded time size choice origin) else Nothing)
  -- Bound fill cost at large desktop resolutions. Preview and full-screen use
  -- the identical scene; only the raster size changes. No history accumulation.
  surface <- managed window (loadRenderTexture 1920 1200)
  _ <- setTextureFilter (renderTexture'texture surface) TextureFilterBilinear
  let assets = RestAssets procedural [] surface
  thumbs <- forM [minBound .. maxBound] $ \scene -> do
    target <- managed window (loadRenderTexture 320 180)
    _ <- setTextureFilter (renderTexture'texture target) TextureFilterBilinear
    bracket_ (R.beginTextureMode target) R.endTextureMode $
      renderPreview assets 180 (Rectangle 0 0 320 180) ((initialSession defaultVolume) {restScene = scene, restShade = Original})
    pure (scene, renderTexture'texture target)
  pure (RestAssets procedural thumbs surface)

thumbnailsReady :: RestAssets -> Bool
thumbnailsReady (RestAssets _ thumbs _) = all ready [minBound .. maxBound]
  where
    ready scene = maybe False ((> 0) . texture'id) (lookup scene thumbs)

proceduralReady :: RestAssets -> Bool
proceduralReady (RestAssets shader _ _) = isJust shader

restIssue :: RestAssets -> Session -> Maybe String
restIssue (RestAssets procedural _ _) s
  | not (isJust procedural) = Just (local "Rest rendering unavailable. Click or Esc to return." "休息の景色を描画できません。クリックかEscで戻ります。")
  | otherwise = Nothing
  where
    local en ja = if language s == Japanese then ja else en

renderRest :: RestAssets -> Int -> Int -> Session -> IO ()
renderRest assets@(RestAssets _ _ target) w h s = do
  let limit = if restScene s == CloudSea then 1120 else 1920 :: Float
      ratio = min 1 (min (limit / fromIntegral w) (1200 / fromIntegral h))
      rw = max 1 (floor (fromIntegral w * ratio)) :: Int
      rh = max 1 (floor (fromIntegral h * ratio)) :: Int
  bracket_ (R.beginTextureMode target) R.endTextureMode $
    renderPreview assets 1200 (Rectangle 0 0 (fromIntegral rw) (fromIntegral rh)) s {restShade = Original}
  drawTexturePro
    (renderTexture'texture target)
    -- Sample inside the used subrectangle's texel centers. Bilinear reads
    -- at its outer boundary otherwise blend with unused transparent rows.
    (Rectangle 0.5 (1200 - fromIntegral rh + 0.5) (fromIntegral rw - 1) (1 - fromIntegral rh))
    (Rectangle 0 0 (fromIntegral w) (fromIntegral h))
    (Vector2 0 0)
    0
    (Color 255 255 255 255)
  -- Apply shade after the opaque texture is composited. Applying standard
  -- alpha blending inside an RGBA target would lower its stored alpha and
  -- leak the working wallpaper through the subsequent full-screen draw.
  drawRectangleRec (Rectangle 0 0 (fromIntegral w) (fromIntegral h)) (Color 0 0 0 (fromIntegral (shadeOpacity (restShade s))))

renderThumbnail :: RestAssets -> RestScene -> Rectangle -> IO ()
renderThumbnail (RestAssets _ thumbs _) scene rectangle =
  forM (lookup scene thumbs) (\texture -> drawTexturePro texture (Rectangle 0 0 320 (-180)) rectangle (Vector2 0 0) 0 (Color 255 255 255 255)) >> pure ()

renderPreview :: RestAssets -> Int -> Rectangle -> Session -> IO ()
renderPreview (RestAssets procedural _ _) viewportHeight rectangle@(Rectangle x y wf hf) s = do
  drawRectangleRec rectangle (Color 5 13 18 255)
  case procedural of
    Nothing -> pure ()
    Just (ProceduralShader program time size choice origin) -> do
      R.nativeSetShaderValue program time (ShaderUniformFloat phase)
      R.nativeSetShaderValue program size (ShaderUniformVec2 (Vector2 wf hf))
      R.nativeSetShaderValue program origin (ShaderUniformVec2 (Vector2 x (fromIntegral viewportHeight - y - hf)))
      R.nativeSetShaderValue program choice (ShaderUniformInt (proceduralIndex (restScene s)))
      bracket_ (R.beginShaderMode program) R.endShaderMode $
        drawRectangleRec rectangle (Color 255 255 255 255)
      drawRectangleRec rectangle (Color 0 0 0 (fromIntegral (shadeOpacity (restShade s))))
  where
    phase =
      let cycles = restSeconds s / 240
       in realToFrac (2 * pi * (cycles - fromIntegral (floor cycles :: Integer)))
