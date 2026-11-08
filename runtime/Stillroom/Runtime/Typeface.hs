-- | Immutable glyph texture, owned by the renderer's thread.
module Stillroom.Runtime.Typeface (Typeface, load, unload, draw, measure) where

import Control.Exception (bracketOnError, throwIO)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GameIO.Graphics (Color, OverlayBatch, Renderer, Vec2 (..))
import GameIO.Text qualified as Text
import GameIO.Text.Atlas qualified as Atlas
import GameIO.Text.Render qualified as Render

data Typeface = Typeface
  !Render.AtlasTexture
  !(Set.Set Char)
  !(IORef (Map.Map (String, Float, Float) Vec2))

load :: Renderer -> FilePath -> String -> IO Typeface
load renderer file candidates = do
  font <- BS.readFile file >>= checked . Text.decodeFontFace 0
  size <- maybe (throwIO (userError "Invalid glyph atlas size.")) pure (Text.pixelSize 32)
  available <- traverse (\c -> (c,) <$> checked (Text.lookupGlyph font c)) candidates
  let characters = [c | (c, Just _) <- available]
  atlas <- checked $ case Atlas.buildAtlas 1024 font size characters of
    Left (Atlas.AtlasFull _ _) -> Atlas.buildAtlas 2048 font size characters
    result -> result
  bracketOnError (Render.createAtlasTexture renderer atlas) (Render.destroyAtlasTexture renderer) $ \texture ->
    Typeface texture (Set.fromList characters) <$> newIORef Map.empty

unload :: Renderer -> Typeface -> IO ()
unload renderer (Typeface texture _ _) = Render.destroyAtlasTexture renderer texture

draw :: Typeface -> String -> Vec2 -> Float -> Float -> Color -> IO OverlayBatch
draw (Typeface texture characters _) caption (Vec2 x y) size spacing tint = do
  let (ascent, descent, _) = Atlas.atlasLineMetrics (Render.textureAtlas texture)
      scale = realToFrac size / (ascent - descent)
      baseline = realToFrac y + ascent * scale
  snd <$> checked (Render.atlasTextBatch texture (realToFrac x) baseline scale
    (realToFrac spacing / scale) tint (supported characters caption))

measure :: Typeface -> String -> Float -> Float -> IO Vec2
measure (Typeface texture characters cacheRef) caption size spacing = do
  cache <- readIORef cacheRef
  let key = (caption, size, spacing)
  case Map.lookup key cache of
    Just dimensions -> pure dimensions
    Nothing -> do
      let atlas = Render.textureAtlas texture
          (ascent, descent, _) = Atlas.atlasLineMetrics atlas
          scale = realToFrac size / (ascent - descent)
      run <- checked (Atlas.atlasLine atlas (realToFrac spacing / scale) (supported characters caption))
      let dimensions = Vec2 (realToFrac (scale * Text.runAdvance run)) size
          retained = if Map.size cache >= 1024 then Map.empty else cache
      writeIORef cacheRef (Map.insert key dimensions retained)
      pure dimensions

-- Titles from the selected library and picture catalogue are added to the
-- atlas on replacement. A character outside the font or the budget uses '?'.
supported :: Set.Set Char -> String -> String
supported characters = map (\character -> if Set.member character characters then character else '?')

checked :: Show error => Either error value -> IO value
checked = either (throwIO . userError . ("Typeface: " ++) . show) pure
