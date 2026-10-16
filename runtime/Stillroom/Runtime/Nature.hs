{-# LANGUAGE ForeignFunctionInterface #-}

-- | A bracketed native renderer. Shader contents cross the UTF-8 boundary once;
-- frames pass only a scene, clock and rectangle. No finalizer runs after GL closes.
module Stillroom.Runtime.Nature (Nature, withNature, available, draw, info, synchronize, reference, landscapeIndex) where

import Control.Exception (IOException, bracket, try)
import Data.ByteString qualified as BS
import Foreign (Ptr, nullPtr, withArray, withMany)
import Foreign.C (CDouble (..), CFloat (..), CInt (..), CString, peekCString)
import Stillroom.Domain
import Stillroom.Runtime.Pictures qualified as Pictures
import System.FilePath ((</>))

data NativeNature

data Nature = Missing | Loaded !(Ptr NativeNature)

foreign import ccall safe "sr_nature_load" nativeLoad :: Ptr CString -> IO (Ptr NativeNature)

foreign import ccall safe "sr_nature_free" nativeFree :: Ptr NativeNature -> IO ()

foreign import ccall unsafe "sr_nature_ready" nativeReady :: Ptr NativeNature -> IO CInt

foreign import ccall safe "sr_nature_draw" nativeDraw :: Ptr NativeNature -> CInt -> CDouble -> CFloat -> CFloat -> CFloat -> CFloat -> IO CInt

foreign import ccall unsafe "sr_nature_info" nativeInfo :: Ptr NativeNature -> IO CString

foreign import ccall safe "sr_nature_sync" synchronize :: IO ()

foreign import ccall safe "sr_nature_reference" nativeReference :: Ptr NativeNature -> CInt -> IO ()

-- | Benchmark-only quality comparison. Normal application code leaves this off.
reference :: Nature -> Bool -> IO ()
reference Missing _ = pure ()
reference (Loaded pointer) enabled = nativeReference pointer (if enabled then 1 else 0)

withNature :: (Nature -> IO a) -> IO a
withNature = bracket acquire release
  where
    acquire = do
      sources <-
        try $
          mapM
            (\name -> Pictures.assetPath ("assets" </> "rest" </> "nature" </> name) >>= BS.readFile)
            ["common.glsl", "land.vs", "land.fs", "water.vs", "water.fs", "sky.fs", "post.fs"]
      case sources of
        Left (_ :: IOException) -> pure Missing
        Right files -> do
          pointer <- withMany BS.useAsCString files (\strings -> withArray strings nativeLoad)
          pure (if pointer == nullPtr then Missing else Loaded pointer)
    release Missing = pure ()
    release (Loaded pointer) = nativeFree pointer

available :: Nature -> IO Bool
available Missing = pure False
available (Loaded pointer) = (== 1) <$> nativeReady pointer

landscapeIndex :: RestScene -> Maybe Int
landscapeIndex BirchLake = Just 0
landscapeIndex AlpineLake = Just 1
landscapeIndex MeadowStream = Just 2
landscapeIndex _ = Nothing

draw :: Nature -> RestScene -> Double -> Float -> Float -> Float -> Float -> IO Bool
draw Missing _ _ _ _ _ _ = pure False
draw (Loaded pointer) scene time x y width height = case landscapeIndex scene of
  Nothing -> pure False
  Just index ->
    (== 1)
      <$> nativeDraw
        pointer
        (fromIntegral index)
        (realToFrac time)
        (realToFrac x)
        (realToFrac y)
        (realToFrac width)
        (realToFrac height)

info :: Nature -> IO String
info Missing = pure "{}"
info (Loaded pointer) = nativeInfo pointer >>= peekCString
