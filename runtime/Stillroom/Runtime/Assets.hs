module Stillroom.Runtime.Assets (assetPath) where

import Paths_stillroom (getDataFileName)
import System.Directory (doesPathExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)

assetPath :: FilePath -> IO FilePath
assetPath relative = do
  executable <- getExecutablePath
  let beside = takeDirectory executable </> relative
  exists <- doesPathExist beside
  if exists then pure beside else getDataFileName relative
