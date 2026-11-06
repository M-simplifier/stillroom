{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.Graphics.Target
  ( TargetSet,
    TargetState (..),
    newTargets,
    releaseTargets,
    selectQuality,
    budgetTargets,
    readTargets,
    invalidateTargets,
    allocationLimitsForCheck,
    createSurface,
    releaseSurface,
  )
where

import Control.Exception (bracket, finally, mask_)
import Control.Monad (when)
import Data.IORef
import Foreign
import Raylib.Core.Textures qualified as T
import Raylib.Types
import Raylib.Util.RLGL qualified as RL
import Stillroom.Runtime.Graphics.GL qualified as GL
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE allocationLimits #-}
allocationLimits :: IORef (Int, Int)
allocationLimits = unsafePerformIO (newIORef (0, 0))

allocationLimitsForCheck :: Int -> Int -> IO ()
allocationLimitsForCheck color shadow = writeIORef allocationLimits (color, shadow)

allowed :: Bool -> Int -> Int -> IO Bool
allowed color width height = do
  (a, b) <- readIORef allocationLimits
  let limit = if color then a else b
  pure (limit <= 0 || (width <= limit && height <= limit))

data TargetState = TargetState
  { shadowTarget :: !(Maybe RenderTexture),
    reflectionTarget :: !(Maybe RenderTexture),
    hdrTarget :: !(Maybe RenderTexture),
    currentQuality :: !Int,
    qualityCeiling :: !Int,
    rasterWidth :: !Int,
    rasterHeight :: !Int,
    blockedSize :: !(Maybe (Int, Int)),
    targetRevision :: !Int
  }

newtype TargetSet = TargetSet (IORef TargetState)

newTargets :: IO TargetSet
newTargets = TargetSet <$> newIORef (TargetState Nothing Nothing Nothing (-1) 3 0 0 Nothing 0)

readTargets :: TargetSet -> IO TargetState
readTargets (TargetSet ref) = readIORef ref

invalidateTargets :: TargetSet -> IO ()
invalidateTargets (TargetSet ref) = modifyIORef' ref (\s -> s {rasterWidth = 0, targetRevision = targetRevision s + 1})

releaseTargets :: TargetSet -> IO ()
releaseTargets (TargetSet ref) = mask_ $ do
  old <- atomicModifyIORef' ref (\s -> (s {shadowTarget = Nothing, reflectionTarget = Nothing, hdrTarget = Nothing}, s))
  mapM_ releaseTarget (shadowTarget old)
    `finally` (mapM_ releaseTarget (reflectionTarget old) `finally` mapM_ releaseTarget (hdrTarget old))

releaseTarget :: RenderTexture -> IO ()
releaseTarget target = mask_ $ do
  parent <- RL.rlGetActiveFramebuffer
  when (texture'id (renderTexture'texture target) /= 0) (RL.rlUnloadTexture (texture'id (renderTexture'texture target)))
  -- rlUnloadFramebuffer owns its attached depth texture.
  when (renderTexture'id target /= 0) (RL.rlUnloadFramebuffer (renderTexture'id target))
  RL.rlEnableFramebuffer (if parent == renderTexture'id target then 0 else parent)

newTarget :: Bool -> Int -> Int -> IO (Maybe RenderTexture)
newTarget color width height = mask_ $ do
  permitted <- allowed color width height
  if not permitted
    then pure Nothing
    else bracket RL.rlGetActiveFramebuffer RL.rlEnableFramebuffer $ \_ -> do
      framebuffer <- RL.rlLoadFramebuffer
      if framebuffer == 0
        then GL.clearErrors >> pure Nothing
        else do
          RL.rlEnableFramebuffer framebuffer
          image <- if color then fromIntegral <$> RL.c'rlLoadTexture nullPtr (fromIntegral width) (fromIntegral height) 13 1 else pure 0
          when color (RL.rlFramebufferAttach framebuffer image RLAttachmentColorChannel0 RLAttachmentTexture2D 0)
          depth <- RL.rlLoadTextureDepth width height False
          RL.rlFramebufferAttach framebuffer depth RLAttachmentDepth RLAttachmentTexture2D 0
          let make n format = Texture n width height 1 (toEnum format)
              target = RenderTexture framebuffer (make image 13) (make depth 19)
          complete <- RL.rlFramebufferComplete framebuffer
          if framebuffer == 0 || depth == 0 || (color && image == 0) || not complete
            then releaseTarget target >> GL.clearErrors >> pure Nothing
            else do
              when color $ do
                _ <- T.setTextureFilter (renderTexture'texture target) TextureFilterBilinear
                _ <- T.setTextureWrap (renderTexture'texture target) TextureWrapClamp
                pure ()
              _ <- T.setTextureWrap (renderTexture'depth target) TextureWrapClamp
              pure (Just target)

shadowSize :: Int -> Int
shadowSize q = case q of
  3 -> 4096
  2 -> 3072
  1 -> 2048
  _ -> 1024

reflectionScale :: Int -> Float
reflectionScale q = case q of
  3 -> 1
  2 -> 0.75
  _ -> 0.5

rasterRatio :: Int -> Float -> Float -> Float
rasterRatio q w h = min (if q == 3 then 1.5 else 1) (min (mw / w) (mh / h))
  where
    (mw, mh) = case q of
      3 -> (3840, 2160)
      2 -> (2560, 1440)
      1 -> (1600, 900)
      _ -> (1120, 630)

selectQuality :: TargetSet -> Int -> IO Bool
selectQuality (TargetSet ref) desired
  | desired < 0 || desired > 3 = pure False
  | otherwise = mask_ $ do
      state <- readIORef ref
      attempt (min desired (qualityCeiling state))
  where
    attempt candidate = do
      state <- readIORef ref
      if candidate == currentQuality state
        then pure True
        else do
          fresh <- newTarget False (shadowSize candidate) (shadowSize candidate)
          case fresh of
            Just shadow -> do
              writeIORef
                ref
                state
                  { shadowTarget = Just shadow,
                    currentQuality = candidate,
                    rasterWidth = 0,
                    blockedSize = Nothing,
                    targetRevision = targetRevision state + 1
                  }
              mapM_ releaseTarget (shadowTarget state)
              pure True
            Nothing | candidate > 0 -> do
              GL.clearErrors
              modifyIORef' ref (\s -> s {qualityCeiling = candidate - 1})
              attempt (candidate - 1)
            Nothing -> case shadowTarget state of
              Nothing -> pure False
              Just _ -> do
                GL.clearErrors
                writeIORef ref state {currentQuality = 0, qualityCeiling = 0, rasterWidth = 0, targetRevision = targetRevision state + 1}
                pure True

budgetTargets :: TargetSet -> Float -> Float -> Bool -> IO Bool
budgetTargets targets@(TargetSet ref) width height reference = mask_ attempt
  where
    attempt = do
      state <- readIORef ref
      let q = currentQuality state
          ratio = if reference then 1 else rasterRatio q width height
          w = max 1 (floor (width * ratio))
          h = max 1 (floor (height * ratio))
          scale = if reference then 1 else reflectionScale q
          keep = hdrTarget state /= Nothing && ((w == rasterWidth state && h == rasterHeight state) || blockedSize state == Just (w, h))
      if keep
        then pure True
        else do
          hdr <- newTarget True w h
          reflection <- case hdr of
            Nothing -> pure Nothing
            Just _ -> newTarget True (ceiling (fromIntegral w * scale)) (ceiling (fromIntegral h * scale))
          case (hdr, reflection) of
            (Just a, Just b) -> do
              writeIORef ref state {hdrTarget = Just a, reflectionTarget = Just b, rasterWidth = w, rasterHeight = h, targetRevision = targetRevision state + 1}
              mapM_ releaseTarget (hdrTarget state)
              mapM_ releaseTarget (reflectionTarget state)
              pure True
            _ -> do
              mapM_ releaseTarget hdr
              mapM_ releaseTarget reflection
              GL.clearErrors
              if q == 0 || reference
                then case (hdrTarget state, reflectionTarget state) of
                  (Just old, Just _) | not reference -> do
                    let image = renderTexture'texture old
                    writeIORef ref state {rasterWidth = texture'width image, rasterHeight = texture'height image, blockedSize = Just (w, h)}
                    pure True
                  _ -> pure False
                else do
                  modifyIORef' ref (\s -> s {qualityCeiling = q - 1})
                  selected <- selectQuality targets (q - 1)
                  if selected then attempt else pure False

createSurface :: Int -> Int -> IO (Maybe RenderTexture)
createSurface width height = mask_ $ do
  permitted <- allowed True width height
  if not permitted
    then pure Nothing
    else bracket RL.rlGetActiveFramebuffer RL.rlEnableFramebuffer $ \_ -> do
      target <- T.loadRenderTexture width height
      RL.rlEnableFramebuffer (renderTexture'id target)
      complete <- RL.rlFramebufferComplete (renderTexture'id target)
      if renderTexture'id target == 0 || texture'id (renderTexture'texture target) == 0 || texture'id (renderTexture'depth target) == 0 || not complete
        then releaseSurface target >> GL.clearErrors >> pure Nothing
        else T.setTextureFilter (renderTexture'texture target) TextureFilterBilinear >> pure (Just target)

releaseSurface :: RenderTexture -> IO ()
releaseSurface target = mask_ $ do
  parent <- RL.rlGetActiveFramebuffer
  with target T.c'unloadRenderTexture
  RL.rlEnableFramebuffer (if parent == renderTexture'id target then 0 else parent)
