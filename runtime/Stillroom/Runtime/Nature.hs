module Stillroom.Runtime.Nature (Nature, withNature, available, draw, info, synchronize, reference, quality, landscapeIndex) where

import Control.Exception (IOException, bracket, try, catch)
import Data.ByteString qualified as BS
import Raylib.Types (Rectangle (..))
import Stillroom.Graphics (Tier)
import Stillroom.Domain
import Stillroom.Runtime.Graphics.GL qualified as GL
import Stillroom.Runtime.Graphics.Landscape qualified as L
import Stillroom.Runtime.Graphics.Resource (GraphicsError)
import Stillroom.Runtime.Pictures qualified as Pictures
import System.FilePath ((</>))

data Nature = Missing | Loaded !L.Landscape
withNature :: (Nature -> IO a) -> IO a
withNature = bracket acquire release
  where
    acquire = do
      sources <- try $ mapM (\name -> Pictures.assetPath ("assets" </> "rest" </> "nature" </> name) >>= BS.readFile)
        ["common.glsl","land.vs","land.fs","water.vs","water.fs","sky.fs","post.fs"]
      case sources of
        Left (_ :: IOException) -> pure Missing
        Right files -> (Loaded <$> L.load files) `catch` (\(_ :: GraphicsError) -> pure Missing)
    release Missing = pure ()
    release (Loaded landscape) = L.release landscape
available :: Nature -> IO Bool
available Missing = pure False
available (Loaded landscape) = L.available landscape
quality :: Nature -> Tier -> IO ()
quality Missing _ = pure ()
quality (Loaded landscape) tier = L.quality landscape (fromEnum tier)
reference :: Nature -> Bool -> IO ()
reference Missing _ = pure ()
reference (Loaded landscape) enabled = L.reference landscape enabled
synchronize :: IO ()
synchronize = GL.finish
landscapeIndex :: RestScene -> Maybe Int
landscapeIndex BirchLake = Just 0
landscapeIndex AlpineLake = Just 1
landscapeIndex MeadowStream = Just 2
landscapeIndex _ = Nothing
draw :: Nature -> RestScene -> Double -> Float -> Float -> Float -> Float -> IO Bool
draw Missing _ _ _ _ _ _ = pure False
draw (Loaded landscape) scene time x y width height = case landscapeIndex scene of
  Nothing -> pure False
  Just index -> L.draw landscape index time (Rectangle x y width height)
info :: Nature -> IO String
info Missing = pure "{}"
info (Loaded landscape) = L.info landscape
