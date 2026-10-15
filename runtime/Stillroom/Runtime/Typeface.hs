{-# LANGUAGE PatternSynonyms #-}

-- | Keep the native font's glyphs resident instead of marshalling the complete
-- glyph bitmap list on every DrawTextEx/MeasureTextEx call in h-raylib 5.6.0.0.
-- The window owns the GPU atlas; the ForeignPtr owns only CPU allocations.
-- Never run GPU deletion in a GC finalizer. Reaudit on a binding upgrade.
module Stillroom.Runtime.Typeface (Typeface, load, unload, draw, measure) where

import Control.Exception (finally, mask_, onException, throwIO)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Map.Strict qualified as Map
import Foreign
import Foreign.C (withCString)
import Foreign.Concurrent qualified as FC
import Raylib.Core.Text qualified as T
import Raylib.Core.Textures (setTextureFilter, unloadTexture)
import Raylib.Internal (WindowResources, managed)
import Raylib.Internal.Foreign (c'free, pop)
import Raylib.Types

data Typeface = Typeface !(ForeignPtr Font) !Texture !(IORef (Map.Map (String, Float, Float) Vector2))

load :: WindowResources -> FilePath -> [Int] -> IO Typeface
load window file glyphs = do
  bytes <- BS.readFile file
  mask_ $ do
    raw <- BS.useAsCStringLen bytes $ \(buffer, size) ->
      withCString ".otf" $ \extension -> withArray (map fromIntegral glyphs) $ \points ->
        T.c'loadFontFromMemory extension (castPtr buffer) (fromIntegral size) 32 points (fromIntegral (length glyphs))
    when (raw == nullPtr) (throwIO (userError "The bundled font could not be loaded."))
    baseSize <- peek (p'font'baseSize raw)
    -- raylib's default font is borrowed. Free only the wrapper on fallback.
    when (baseSize /= 32) (free raw >> throwIO (userError "The bundled font could not be loaded."))
    texture <- peek (p'font'texture raw)
    owned <- FC.newForeignPtr raw (releaseCPU raw)
      `onException` (T.c'unloadFont raw `finally` free raw)
    _ <- managed window (pure texture)
    _ <- setTextureFilter texture TextureFilterBilinear
    Typeface owned texture <$> newIORef Map.empty
  where
    releaseCPU raw = do
      count <- peek (p'font'glyphCount raw)
      glyphData <- peek (p'font'glyphs raw)
      rectangles <- peek (p'font'recs raw)
      T.c'unloadFontData glyphData count
        `finally` (c'free (castPtr rectangles) `finally` free raw)

unload :: WindowResources -> Typeface -> IO ()
unload window (Typeface owned texture _) =
  finalizeForeignPtr owned `finally` unloadTexture texture window

draw :: Typeface -> String -> Vector2 -> Float -> Float -> Color -> IO ()
draw (Typeface owned _ _) caption position size spacing tint =
  withForeignPtr owned $ \font -> withCString caption $ \text ->
    with position $ \point -> with tint $ \color ->
      T.c'drawTextEx font text point (realToFrac size) (realToFrac spacing) color

measure :: Typeface -> String -> Float -> Float -> IO Vector2
measure (Typeface owned _ cacheRef) caption size spacing = do
  cache <- readIORef cacheRef
  let key = (caption, size, spacing)
  case Map.lookup key cache of
    Just dimensions -> pure dimensions
    Nothing -> do
      dimensions <- withForeignPtr owned $ \font -> withCString caption $ \text ->
        T.c'measureTextEx font text (realToFrac size) (realToFrac spacing) >>= pop
      -- A font replacement gets a fresh cache. Bound long-running library and
      -- resize churn; captions and their metrics must not accumulate forever.
      let retained = if Map.size cache >= 1024 then Map.empty else cache
      writeIORef cacheRef (Map.insert key dimensions retained)
      pure dimensions
