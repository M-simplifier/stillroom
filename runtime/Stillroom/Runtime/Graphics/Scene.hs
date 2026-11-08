module Stillroom.Runtime.Graphics.Scene
  ( SceneRect(..), SceneDraw(..), solid, textured, rectangleVertices, wholeSurface
  , Scope, newScope, own, closeScope, withScope
  , imageAllocationError
  ) where

import Control.Exception (SomeException, bracket, finally, fromException, mask_)
import Data.IORef
import GameIO.Graphics qualified as GPU

data SceneRect = SceneRect !Float !Float !Float !Float deriving (Eq, Show)

-- The submitted callback belongs to these exact passes. A minimized or
-- deferred frame leaves cached image content uncommitted.
data SceneDraw = SceneDraw
  { scenePasses :: ![GPU.RenderPass]
  , sceneOverlay :: ![GPU.OverlayBatch]
  , scenePresented :: GPU.FrameStatus -> IO ()
  }

instance Semigroup SceneDraw where
  SceneDraw passes overlays submitted <> SceneDraw morePasses moreOverlays moreSubmitted =
    SceneDraw (passes ++ morePasses) (overlays ++ moreOverlays) (\status -> submitted status >> moreSubmitted status)

instance Monoid SceneDraw where
  mempty = SceneDraw [] [] (const (pure ()))

wholeSurface :: Int -> Int -> SceneRect
wholeSurface width height = SceneRect 0 0 (fromIntegral width) (fromIntegral height)

solid :: SceneRect -> GPU.Color -> GPU.OverlayBatch
solid area tint = GPU.OverlayBatch Nothing (rectangleVertices area (GPU.Vec4 0 0 1 1) tint)

textured :: GPU.Texture -> SceneRect -> GPU.Vec4 -> GPU.OverlayBatch
textured texture area uv = GPU.OverlayBatch (Just texture) (rectangleVertices area uv (GPU.Color 1 1 1 1))

rectangleVertices :: SceneRect -> GPU.Vec4 -> GPU.Color -> [GPU.Vertex]
rectangleVertices (SceneRect left top width height) (GPU.Vec4 u0 v0 u1 v1) tint = [tl, tr, br, tl, br, bl]
  where
    vertex x y u v = GPU.Vertex x y 0 u v tint
    tl = vertex left top u0 v0
    tr = vertex (left + width) top u1 v0
    br = vertex (left + width) (top + height) u1 v1
    bl = vertex left (top + height) u0 v1

newtype Scope = Scope (IORef [IO ()])

newScope :: IO Scope
newScope = Scope <$> newIORef []

own :: Scope -> IO a -> (a -> IO ()) -> IO a
own (Scope releases) acquire release = mask_ $ do
  resource <- acquire
  modifyIORef' releases (release resource :)
  pure resource

closeScope :: Scope -> IO ()
closeScope (Scope releases) = mask_ $ do
  actions <- atomicModifyIORef' releases (\old -> ([], old))
  foldr finally (pure ()) actions

withScope :: (Scope -> IO a) -> IO a
withScope = bracket newScope closeScope

imageAllocationError :: SomeException -> Maybe ()
imageAllocationError problem = case fromException problem of
  Just GPU.InvalidTargetSize{} -> Just ()
  Just GPU.TargetFormatUnavailable{} -> Just ()
  _ -> case fromException problem of
    Just (GPU.VulkanCallFailed _ result) | result `elem` imageAllocationFailures -> Just ()
    _ -> Nothing
  where
    -- VK_ERROR_OUT_OF_HOST_MEMORY, VK_ERROR_OUT_OF_DEVICE_MEMORY and
    -- VK_ERROR_FORMAT_NOT_SUPPORTED in Vulkan-Headers vk.xml.
    imageAllocationFailures = [-1, -2, -11]
