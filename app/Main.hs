module Main (main) where

import Control.Exception
import GHC.IO.Encoding (setForeignEncoding, utf8)
import Stillroom.Runtime.Benchmark (runBenchmark)
import Stillroom.Runtime.Capture (runCapture)
import Stillroom.Runtime.Desktop (runDesktop)
import Stillroom.Runtime.Native
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  setForeignEncoding utf8
  args <- getArgs
  result <- try (dispatch args)
  case result of
    Right True -> pure ()
    Right False -> exitFailure
    Left (problem :: SomeException) -> case fromException problem :: Maybe AsyncException of
      Just _ -> throwIO problem
      Nothing -> do
        if take 1 args `elem` [["--check"], ["--benchmark"], ["--capture-ui"]]
          then hPutStrLn stderr (displayException problem)
          else showProblem (displayException problem)
        exitFailure

dispatch :: [String] -> IO Bool
dispatch ["--benchmark", directory] = runBenchmark directory
dispatch ["--capture-ui", directory] = runCapture directory
dispatch ["--check", directory] = runDesktop (directory </> "settings.json") (Just directory) Nothing
dispatch ["--check", directory, "--music", folder] = runDesktop (directory </> "settings.json") (Just directory) (Just folder)
dispatch [] = do
  entry <- beginInstance
  case entry of
    Left problem -> showProblem problem >> pure False
    Right False -> pure True
    Right True -> flip finally endInstance $ do
      config <- getXdgDirectory XdgConfig "Stillroom"
      createDirectoryIfMissing True config
      runDesktop (config </> "settings.json") Nothing Nothing
dispatch _ = showProblem "Start Stillroom without arguments. Diagnostics: --capture-ui OUTPUT; --check OUTPUT [--music FOLDER]; --benchmark OUTPUT" >> pure False
