{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Texture-free procedural scenes. Rendering changes
-- neither the music nor the normal picture. All resources belong to the window.
module Stillroom.Runtime.Rest
  ( RestAssets,
    withRestAssets,
    renderRest,
    renderPreview,
    renderThumbnail,
    restIssue,
    thumbnailsReady,
    proceduralReady,
    landscapeInfo,
    landscapeReference,
    graphicsInfo,
    journey,
  )
where

import Control.Exception (IOException, bracket, bracket_, mask_, catch, throwIO)
import Control.Monad (forM, when)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Maybe (isJust)
import Foreign (Ptr, alloca, peek, with)
import Foreign.C (CInt (..))
import Raylib.Core qualified as R
import Raylib.Core.Shapes (drawRectangleRec)
import Raylib.Core.Textures (drawTexturePro, loadRenderTexture, c'unloadRenderTexture, setTextureFilter)
import Raylib.Internal (WindowResources, managed)
import Raylib.Types
import Stillroom.Graphics qualified as G
import Stillroom.Runtime.Graphics qualified as Graphics
import Stillroom.Domain
import Stillroom.Runtime.Journey qualified as Journey
import Stillroom.Runtime.Nature qualified as Nature
import Stillroom.Runtime.Pictures qualified as Pictures
import Stillroom.Session
import System.FilePath ((</>))

data ProceduralShader = ProceduralShader !Shader !Int !Int !Int !Int

-- This lazy surface is explicitly bracketed, outside raylib's managed list.
releaseSurface :: RenderTexture -> IO ()
releaseSurface surface = with surface c'unloadRenderTexture

foreign import ccall safe "sr_graphics_surface" nativeSurface :: CInt -> CInt -> Ptr RenderTexture -> IO CInt

surfaceFor :: IORef (Maybe (RenderTexture, (Int, Int))) -> IO () -> Int -> Int -> IO (Maybe RenderTexture)
surfaceFor ref invalidate width height = mask_ $ do
  old <- readIORef ref
  case old of
    Just (target, requested) | requested == (width, height) -> pure (Just target)
    _ -> do
      fresh <- attempt width height
      case fresh of
        Just target -> do
          writeIORef ref (Just (target, (width, height)))
          invalidate
          mapM_ (releaseSurface . fst) old
          pure (Just target)
        Nothing -> do
          writeIORef ref (fmap (\(target, _) -> (target, (width, height))) old)
          pure (fst <$> old)
  where
    attempt w h = do
      result <- alloca $ \out -> do
        ok <- nativeSurface (fromIntegral w) (fromIntegral h) out
        if ok == 1 then Just <$> peek out else pure Nothing
      case result of
        Just _ -> pure result
        Nothing | w > 320 && h > 180 -> attempt (max 1 (w * 3 `div` 4)) (max 1 (h * 3 `div` 4))
        Nothing -> pure Nothing

data RestAssets = RestAssets Journey.JourneyRuntime Nature.Nature (Maybe ProceduralShader) [(RestScene, Texture)] (IORef (Maybe (RenderTexture, (Int, Int)))) (IORef (Maybe (RestScene, Int, Int, Double, G.Tier))) Graphics.Graphics (IORef G.Tier)

proceduralIndex :: RestScene -> Int
proceduralIndex RailJourney = 0
proceduralIndex TidalLight = 0
proceduralIndex LightVeil = 1
proceduralIndex StarryNight = 2
proceduralIndex AmberSea = 3
proceduralIndex Shallows = 4
proceduralIndex CloudSea = 5
proceduralIndex Aurora = 6
proceduralIndex InkFlow = 7
proceduralIndex BirchLake = 0
proceduralIndex AlpineLake = 0
proceduralIndex MeadowStream = 0

loadProgram :: WindowResources -> FilePath -> IO (Maybe Shader)
loadProgram window name = do
  source <-
    (Just . BSC.unpack <$> (Pictures.assetPath ("assets" </> "rest" </> name) >>= BSC.readFile))
      `catch` (\(_ :: IOException) -> pure Nothing)
  traverse (\code -> managed window (R.loadShaderFromMemory Nothing (Just code))) source

withRestAssets :: WindowResources -> (RestAssets -> IO a) -> IO a
withRestAssets window action = Graphics.withGraphics $ \graphics -> Journey.withJourney $ \travel -> Nature.withNature $ \native ->
  bracket (newIORef Nothing) (\ref -> readIORef ref >>= mapM_ (releaseSurface . fst)) $ \surface ->
    loadRestAssets window travel native graphics surface >>= action

journey :: RestAssets -> Journey.JourneyRuntime
journey (RestAssets travel _ _ _ _ _ _ _) = travel

loadRestAssets :: WindowResources -> Journey.JourneyRuntime -> Nature.Nature -> Graphics.Graphics -> IORef (Maybe (RenderTexture, (Int, Int))) -> IO RestAssets
loadRestAssets window travel native graphics surface = do
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
  resolved <- newIORef G.Balanced
  rendered <- newIORef Nothing
  let assets = RestAssets travel native procedural [] surface rendered graphics resolved
  thumbs <- forM [scene | scene <- [minBound .. maxBound], scene /= RailJourney] $ \scene -> do
    target <- managed window (loadRenderTexture 320 180)
    _ <- setTextureFilter (renderTexture'texture target) TextureFilterBilinear
    bracket_ (R.beginTextureMode target) R.endTextureMode $
      renderPreview assets 180 (Rectangle 0 0 320 180) ((initialSession defaultVolume) {restScene = scene, restShade = Original})
    pure (scene, renderTexture'texture target)
  posterPath <- Pictures.assetPath ("assets" </> "rest" </> "journey" </> "poster.png")
  prepared <- Pictures.preparePicture (Pictures.PictureEntry "journey-poster" posterPath "Journey" Pictures.Bundled) >>= either throwIO pure
  poster <- Pictures.uploadPicture window prepared
  pure (RestAssets travel native procedural ((RailJourney, poster) : thumbs) surface rendered graphics resolved)

thumbnailsReady :: RestAssets -> Bool
thumbnailsReady (RestAssets _ _ _ thumbs _ _ _ _) = all ready [minBound .. maxBound]
  where
    ready scene = maybe False ((> 0) . texture'id) (lookup scene thumbs)

proceduralReady :: RestAssets -> IO Bool
proceduralReady (RestAssets _ native shader _ _ _ _ _) = (isJust shader &&) <$> Nature.available native

landscapeInfo :: RestAssets -> IO String
landscapeInfo (RestAssets _ native _ _ _ _ _ _) = Nature.info native

landscapeReference :: RestAssets -> Bool -> IO ()
landscapeReference (RestAssets _ native _ _ _ _ _ _) = Nature.reference native

restIssue :: RestAssets -> Session -> IO (Maybe String)
restIssue (RestAssets travel _ _ _ _ _ _ _) s | restScene s == RailJourney = Journey.status travel (language s)
restIssue (RestAssets _ native procedural _ surface _ _ _) s = do
  hasSurface <- isJust <$> readIORef surface
  ready <- if sceneCollection (restScene s) == Landscapes then Nature.available native else pure (isJust procedural && (viewingMode s /= Resting || hasSurface))
  pure (if ready then Nothing else Just (local "Rest rendering unavailable. Click or Esc to return." "休息の景色を描画できません。クリックかEscで戻ります。"))
  where
    local en ja = if language s == Japanese then ja else en

graphicsInfo :: RestAssets -> IO (Bool, G.Tier, Int, Double)
graphicsInfo (RestAssets _ _ _ _ _ _ graphics _) = Graphics.diagnostics graphics

configure :: RestAssets -> G.Tier -> Session -> IO ()
configure (RestAssets travel native _ _ _ _ _ resolved) tier s = do
  writeIORef resolved tier
  if restScene s == RailJourney then Journey.quality travel tier
    else when (sceneCollection (restScene s) == Landscapes) (Nature.quality native tier)

renderRest :: RestAssets -> Int -> Int -> Session -> IO ()
renderRest assets@(RestAssets _ _ _ _ _ _ graphics _) w h s = do
  let moving = restMotion s == Gentle || restScene s == RailJourney
      adaptive = moving && graphicsQuality s == Automatic
  tier <- Graphics.choose graphics (graphicsQuality s) (restScene s) w h adaptive
  configure assets tier s
  Graphics.measured graphics adaptive $
    if sceneCollection (restScene s) /= Atmospheres
      then renderRaw assets h (Rectangle 0 0 (fromIntegral w) (fromIntegral h)) s
      else renderProceduralRest assets w h s

renderProceduralRest :: RestAssets -> Int -> Int -> Session -> IO ()
renderProceduralRest assets@(RestAssets _ _ _ _ surface rendered _ resolved) w h s = do
  tier <- readIORef resolved
  let (limit, heightLimit) = case tier of
        G.Economy -> (1120, 630)
        G.Balanced -> (1920, 1200)
        G.Detailed -> (2560, 1440)
        G.Supersampled -> (3840, 2160)
      cloudLimit = if restScene s == CloudSea then limit * 0.6 else limit :: Float
      ratio = min (if tier == G.Supersampled then 1.5 else 1) (min (cloudLimit / fromIntegral w) (heightLimit / fromIntegral h))
      rw = max 1 (floor (fromIntegral w * ratio)) :: Int
      rh = max 1 (floor (fromIntegral h * ratio)) :: Int
  available <- surfaceFor surface (writeIORef rendered Nothing) rw rh
  case available of
    Nothing -> drawRectangleRec (Rectangle 0 0 (fromIntegral w) (fromIntegral h)) (Color 5 13 18 255)
    Just target -> renderSurface assets w h s tier target

renderSurface :: RestAssets -> Int -> Int -> Session -> G.Tier -> RenderTexture -> IO ()
renderSurface assets@(RestAssets _ _ _ _ _ rendered _ _) w h s tier target = do
  let rw = texture'width (renderTexture'texture target)
      rh = texture'height (renderTexture'texture target)
      signature = (restScene s, rw, rh, restSeconds s, tier)
  previous <- readIORef rendered
  -- Frozen means no new scene, not the same expensive shader run every frame.
  -- Dimensions and scene are part of the key; shade is composited afterward.
  when (previous /= Just signature) $ do
    bracket_ (R.beginTextureMode target) R.endTextureMode $
      renderRaw assets rh (Rectangle 0 0 (fromIntegral rw) (fromIntegral rh)) s {restShade = Original}
    writeIORef rendered (Just signature)
  drawTexturePro
    (renderTexture'texture target)
    -- Sample inside the used subrectangle's texel centers. Bilinear reads
    -- at its outer boundary otherwise blend with unused transparent rows.
    (Rectangle 0.5 0.5 (fromIntegral rw - 1) (1 - fromIntegral rh))
    (Rectangle 0 0 (fromIntegral w) (fromIntegral h))
    (Vector2 0 0)
    0
    (Color 255 255 255 255)
  -- Apply shade after the opaque texture is composited. Applying standard
  -- alpha blending inside an RGBA target would lower its stored alpha and
  -- leak the working wallpaper through the subsequent full-screen draw.
  drawRectangleRec (Rectangle 0 0 (fromIntegral w) (fromIntegral h)) (Color 0 0 0 (fromIntegral (shadeOpacity (restShade s))))

renderThumbnail :: RestAssets -> RestScene -> Rectangle -> IO ()
renderThumbnail (RestAssets _ _ _ thumbs _ _ _ _) scene rectangle@(Rectangle _ _ w h) =
  forM (lookup scene thumbs) (\texture -> drawTexturePro texture source rectangle (Vector2 0 0) 0 (Color 255 255 255 255)) >> pure ()
  where
    scale = max (w / 320) (h / 180)
    sw = w / scale
    sh = h / scale
    source = Rectangle ((320 - sw) / 2) ((180 - sh) / 2) sw (if scene == RailJourney then sh else -sh)

renderPreview :: RestAssets -> Int -> Rectangle -> Session -> IO ()
renderPreview assets viewportHeight rectangle s = do
  configure assets (maybe G.Balanced id (G.fixedTier (graphicsQuality s))) s
  renderRaw assets viewportHeight rectangle s

renderRaw :: RestAssets -> Int -> Rectangle -> Session -> IO ()
renderRaw (RestAssets travel native procedural _ _ _ _ _) viewportHeight rectangle@(Rectangle x y wf hf) s = do
  drawRectangleRec rectangle (Color 5 13 18 255)
  if restScene s == RailJourney
    then Journey.draw travel rectangle
    else
      if sceneCollection (restScene s) == Landscapes
        then do
          _ <- Nature.draw native (restScene s) (restSeconds s) x y wf hf
          pure ()
        else case procedural of
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
