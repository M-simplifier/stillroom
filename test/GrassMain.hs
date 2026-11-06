module Main (main) where

import Control.Monad (unless)
import GrassChecks (grassChecks)
import Stillroom.Landscape.Grass (grassShapeGLSL)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  passed <- grassChecks
  args <- getArgs
  case args of
    [kernelPath] -> writeFile kernelPath grassShapeGLSL
    [] -> pure ()
    _ -> fail "Supply at most one generated GLSL output path"
  unless passed exitFailure
