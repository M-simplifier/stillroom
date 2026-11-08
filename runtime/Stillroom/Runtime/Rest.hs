-- Scene construction borrows the host renderer. The host appends UI overlays
-- in the same submission, after the ordered offscreen passes.
module Stillroom.Runtime.Rest
  ( RestAssets, SceneRect(..), SceneDraw(..), withRestAssets, renderRest, renderPreview
  , renderThumbnail, restIssue, thumbnailsReady, proceduralReady
  , landscapeInfo, landscapeReference, graphicsInfo, journey
  , setAllocationLimitsForCheck
  ) where

import Control.Exception (bracket, mask_, throwIO, tryJust)
import Control.Monad (forM, forM_, when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Maybe (isJust)
import GameIO.Graphics qualified as GPU
import GameIO.Graphics.Shader (defaultVertexShader)
import GameIO.Image qualified as Image
import GameIO.Platform (Window)
import Stillroom.Domain
import Stillroom.Graphics qualified as G
import Stillroom.Runtime.Assets (assetPath)
import Stillroom.Runtime.Graphics qualified as Graphics
import Stillroom.Runtime.Graphics.Atmosphere
import Stillroom.Runtime.Graphics.Scene
import Stillroom.Runtime.Journey qualified as Journey
import Stillroom.Runtime.Nature qualified as Nature
import Stillroom.Session
import System.FilePath ((</>))

data Surface = Surface !GPU.RenderTarget !Int !Int !(Int, Int)

data RestAssets = RestAssets
  { gpu :: !GPU.Renderer
  , journey :: !Journey.JourneyRuntime
  , nature :: !Nature.Nature
  , atmospheres :: ![(RestScene, GPU.Material)]
  , thumbnails :: ![(RestScene, (GPU.Texture, Int, Int))]
  , currentSurface :: !(IORef (Maybe Surface))
  , rendered :: !(IORef (Maybe (RestScene, Int, Int, Double, G.Tier)))
  , graphics :: !Graphics.Graphics
  , colorLimitForCheck :: !(IORef Int)
  }

withRestAssets :: Window -> GPU.Renderer -> (RestAssets -> IO a) -> IO a
withRestAssets window renderer use = Graphics.withGraphics renderer $ \governor ->
  Journey.withJourney window renderer $ \travel -> Nature.withNature renderer $ \landscape ->
    withScope $ \scope -> bracket (newIORef Nothing) release $ \surface -> do
      cache <- newIORef Nothing
      limit <- newIORef 0
      programs <- forM [(scene, shader) | scene <- scenes, Just shader <- [atmosphereShader scene]] $
        \(scene, shader) -> do
          material <- own scope (GPU.createMaterial renderer (GPU.MaterialConfig GPU.OverlayReplace defaultVertexShader shader))
            (GPU.destroyMaterial renderer)
          pure (scene, material)
      let assets = RestAssets renderer travel landscape programs [] surface cache governor limit
      cards <- loadThumbnails scope assets
      use assets {thumbnails = cards}
  where
    release ref = readIORef ref >>= mapM_ (\(Surface target _ _ _) -> GPU.destroyRenderTarget renderer target)

scenes :: [RestScene]
scenes = [minBound .. maxBound]

loadThumbnails :: Scope -> RestAssets -> IO [(RestScene, (GPU.Texture, Int, Int))]
loadThumbnails scope assets = do
  thumbs <- forM (filter (/= RailJourney) scenes) $ \scene -> do
    target <- own scope (GPU.createRenderTarget renderer GPU.RGBA8 320 180) (GPU.destroyRenderTarget renderer)
    let state = (initialSession defaultVolume) {restScene = scene, restShade = Original}
    draw <- renderPreview assets 180 (wholeSurface 320 180) state
    GPU.renderOffscreen renderer (scenePasses draw ++ [GPU.RenderPass target (GPU.Frame clearColor [] (sceneOverlay draw))])
    pure (scene, (GPU.renderTargetTexture target, 320, 180))
  posterBytes <- assetPath ("assets" </> "rest" </> "journey" </> "poster.png") >>= BS.readFile
  poster <- either (throwIO . userError . show) pure (Image.decodeImage posterBytes)
  let (w,h) = Image.imageSize poster
  posterTexture <- own scope (GPU.createTextureRGBA8With renderer GPU.Linear w h (Image.imageRGBA poster))
    (GPU.destroyTexture renderer)
  pure ((RailJourney, (posterTexture,w,h)) : thumbs)
  where
    renderer = gpu assets

thumbnailsReady :: RestAssets -> Bool
thumbnailsReady assets = all (\scene -> isJust (lookup scene (thumbnails assets))) scenes

proceduralReady :: RestAssets -> IO Bool
proceduralReady assets = do
  landscapeReady <- Nature.available (nature assets)
  pure (landscapeReady && all (\scene -> sceneCollection scene /= Atmospheres || isJust (lookup scene (atmospheres assets))) scenes)

landscapeInfo :: RestAssets -> IO String
landscapeInfo = Nature.info . nature

landscapeReference :: RestAssets -> Bool -> IO ()
landscapeReference = Nature.reference . nature

restIssue :: RestAssets -> Session -> IO (Maybe String)
restIssue assets s | restScene s == RailJourney = Journey.status (journey assets) (language s)
restIssue assets s = do
  ready <- case sceneCollection (restScene s) of
    Atmospheres -> do
      allocated <- isJust <$> readIORef (currentSurface assets)
      pure (isJust (lookup (restScene s) (atmospheres assets)) && (viewingMode s /= Resting || allocated))
    _ -> Nature.available (nature assets)
  let message = if language s == Japanese then "休息の景色を描画できません。クリックかEscで戻ります。"
        else "Rest rendering unavailable. Click or Esc to return."
  pure (if ready then Nothing else Just message)

graphicsInfo :: RestAssets -> IO (Bool, G.Tier, Int, Double)
graphicsInfo = Graphics.diagnostics . graphics

setAllocationLimitsForCheck :: RestAssets -> Int -> Int -> IO ()
setAllocationLimitsForCheck assets color depth = do
  writeIORef (colorLimitForCheck assets) color
  Nature.setAllocationLimitsForCheck (nature assets) color depth
  Journey.setAllocationLimitsForCheck (journey assets) color depth

configure :: RestAssets -> G.Tier -> RestScene -> IO ()
configure assets tier scene
  | scene == RailJourney = Journey.quality (journey assets) tier
  | sceneCollection scene == Landscapes = Nature.quality (nature assets) tier
  | otherwise = pure ()

renderRest :: RestAssets -> Int -> Int -> Session -> IO SceneDraw
renderRest assets w h s
  | w < 1 || h < 1 = pure mempty
  | otherwise = do
    let scene = restScene s
    let adaptive = (restMotion s == Gentle || scene == RailJourney) && graphicsQuality s == Automatic
    tier <- Graphics.choose (graphics assets) (graphicsQuality s) scene w h adaptive
    configure assets tier scene
    draw <- if sceneCollection scene == Atmospheres then renderAtmosphereSurface assets tier w h s
      else renderRaw assets (wholeSurface w h) s
    pure draw {scenePresented = \status ->
      scenePresented draw status >> Graphics.notePresented (graphics assets) adaptive status}

renderPreview :: RestAssets -> Int -> SceneRect -> Session -> IO SceneDraw
renderPreview assets _ rectangle s = do
  configure assets (maybe G.Balanced id (G.fixedTier (graphicsQuality s))) (restScene s)
  renderRaw assets rectangle s

renderThumbnail :: RestAssets -> RestScene -> SceneRect -> IO SceneDraw
renderThumbnail assets scene area@(SceneRect _ _ w h)
  | w <= 0 || h <= 0 = pure mempty
  | otherwise = pure $ case lookup scene (thumbnails assets) of
      Nothing -> mempty
      Just (texture, width, height) ->
        let scale = max (w/fromIntegral width) (h/fromIntegral height)
            u = w/(scale*fromIntegral width)
            v = h/(scale*fromIntegral height)
            bounds = GPU.Vec4 ((1-u)/2) ((1-v)/2) ((1+u)/2) ((1+v)/2)
        in mempty {sceneOverlay = [textured texture area bounds]}

clearColor :: GPU.Color
clearColor = GPU.Color (5/255) (13/255) (18/255) 1

shade :: SceneRect -> Session -> SceneDraw
shade rect s = mempty {sceneOverlay = [solid rect (GPU.Color 0 0 0 (fromIntegral (shadeOpacity (restShade s))/255))]}

renderRaw :: RestAssets -> SceneRect -> Session -> IO SceneDraw
renderRaw assets rect s = do
  let clear = mempty {sceneOverlay = [solid rect clearColor]}
  let scene = restScene s
  frame <- if scene == RailJourney then Journey.draw (journey assets) rect else
    if sceneCollection scene == Landscapes then Nature.draw (nature assets) scene (restSeconds s) rect
    else pure (atmosphere assets rect scene (restSeconds s))
  pure (clear <> frame <> shade rect s)

atmosphere :: RestAssets -> SceneRect -> RestScene -> Double -> SceneDraw
atmosphere assets rect@(SceneRect x y width height) scene elapsed = case lookup scene (atmospheres assets) of
  Just material | width > 0 && height > 0 ->
    let parameters = atmosphereParameters width height elapsed ++ [GPU.Vec4 x y 0 0]
        vertices = rectangleVertices rect (GPU.Vec4 0 0 1 1) (GPU.Color 1 1 1 1)
    in mempty {sceneOverlay = [GPU.OverlayBatchWith Nothing material (GPU.ShaderInputs [] parameters) vertices]}
  _ -> mempty

renderAtmosphereSurface :: RestAssets -> G.Tier -> Int -> Int -> Session -> IO SceneDraw
renderAtmosphereSurface assets tier w h s = do
  let scene = restScene s
  let (limit, heightLimit) = case tier of
        G.Economy -> (1120, 630)
        G.Balanced -> (1920, 1200)
        G.Detailed -> (2560, 1440)
        G.Supersampled -> (3840, 2160)
  let cloudLimit = if scene == CloudSea then limit*0.6 else limit
  let ratio = min (if tier == G.Supersampled then 1.5 else 1)
        (min (cloudLimit/fromIntegral w) (heightLimit/fromIntegral h)) :: Float
  let rw = max 1 (floor (fromIntegral w*ratio))
  let rh = max 1 (floor (fromIntegral h*ratio))
  let destination = wholeSurface w h
  available <- surfaceFor assets rw rh
  case available of
    Nothing -> pure mempty {sceneOverlay = [solid destination clearColor]}
    Just (Surface target actualW actualH _) -> do
      let key = (scene, actualW, actualH, restSeconds s, tier)
      previous <- readIORef (rendered assets)
      let draw = atmosphere assets (wholeSurface actualW actualH) scene (restSeconds s)
      let passes = [GPU.RenderPass target (GPU.Frame clearColor [] (sceneOverlay draw)) | previous /= Just key]
      let u = 0.5/fromIntegral actualW
      let v = 0.5/fromIntegral actualH
      let output = textured (GPU.renderTargetTexture target) destination (GPU.Vec4 u v (1-u) (1-v))
      let committed status = when (status == GPU.FramePresented) (writeIORef (rendered assets) (Just key))
      pure (SceneDraw passes [output] committed <> shade destination s)

surfaceFor :: RestAssets -> Int -> Int -> IO (Maybe Surface)
surfaceFor assets width height = mask_ $ do
  old <- readIORef (currentSurface assets)
  case old of
    Just (Surface _ _ _ requested) | requested == (width,height) -> pure old
    _ -> do
      fresh <- attempt width height
      case fresh of
        Nothing -> do
          let keep (Surface target w h _) = Surface target w h (width,height)
          let retained = fmap keep old
          writeIORef (currentSurface assets) retained
          pure retained
        Just next -> do
          writeIORef (currentSurface assets) fresh
          writeIORef (rendered assets) Nothing
          forM_ old (\(Surface target _ _ _) -> GPU.destroyRenderTarget (gpu assets) target)
          pure (Just next)
  where
    attempt w h = do
      limit <- readIORef (colorLimitForCheck assets)
      result <- if limit > 0 && max w h > limit then pure (Left ())
        else tryJust imageAllocationError (GPU.createRenderTarget (gpu assets) GPU.RGBA8 w h)
      case result of
        Right target -> pure (Just (Surface target w h (width,height)))
        Left () | w > 320 && h > 180 -> attempt (w*3 `div` 4) (h*3 `div` 4)
        Left () -> pure Nothing
