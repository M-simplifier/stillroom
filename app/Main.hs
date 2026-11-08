module Main (main) where

import Control.Concurrent (runInBoundThread)
import Control.Exception
import GHC.IO.Encoding (setForeignEncoding, utf8)
import Stillroom.Runtime.Audio.Remote (runAudioService)
import Stillroom.Runtime.GraphicsCheck (runGraphicsCheck)
import Stillroom.Runtime.Benchmark (runBenchmark)
import Stillroom.Runtime.Capture (runCapture)
import Stillroom.Runtime.Desktop (runDesktop, runLocalReview)
import Stillroom.Runtime.JourneyCheck (runJourneyCheck)
import Stillroom.Runtime.GraphicsCapture (runQualityCapture, runQualityFilm)
import Stillroom.Graphics (Tier (..))
import Stillroom.Runtime.Native
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  setForeignEncoding utf8
  args <- getArgs
  result <- try (runInBoundThread (dispatch args))
  case result of
    Right True -> pure ()
    Right False -> exitFailure
    Left (problem :: SomeException) -> case fromException problem :: Maybe AsyncException of
      Just _ -> throwIO problem
      Nothing -> do
        if take 1 args `elem` [["--audio-service"], ["--check"], ["--benchmark"], ["--capture-ui"], ["--check-journey"], ["--quality-capture"], ["--quality-film"], ["--check-graphics"]]
          then hPutStrLn stderr (displayException problem)
          else showProblem (displayException problem)
        exitFailure

dispatch :: [String] -> IO Bool
dispatch ["--audio-service"] = runAudioService >> pure True
dispatch ["--check-graphics", directory] = runGraphicsCheck directory
dispatch ["--benchmark", directory] = runBenchmark directory
dispatch ["--capture-ui", directory] = runCapture directory
dispatch ["--check-journey", directory] = runJourneyCheck directory
dispatch ["--quality-capture", directory] = runQualityCapture directory
dispatch ["--quality-film", "standard"] = runQualityFilm Balanced
dispatch ["--quality-film", "detailed"] = runQualityFilm Detailed
dispatch ["--quality-film", "supersampled"] = runQualityFilm Supersampled
dispatch ["--check", directory] = runDesktop Nothing (directory </> "settings.json") (Just directory) Nothing
dispatch ["--check", directory, "--music", folder] = runDesktop Nothing (directory </> "settings.json") (Just directory) (Just folder)
dispatch [] = do
  executable <- getExecutablePath
  let directory = takeDirectory executable
      localState = directory </> "local-state"
  review <- doesFileExist (directory </> ".stillroom-local-review")
  if review
    then do
      createDirectoryIfMissing True localState
      runLocalReview (localState </> "settings.json")
    else launchInstalled

dispatch _ = showProblem "Start Stillroom without arguments. Diagnostics: --capture-ui OUTPUT; --check OUTPUT [--music FOLDER]; --check-journey OUTPUT; --benchmark OUTPUT" >> pure False

launchInstalled :: IO Bool
launchInstalled = withApplicationInstance $ \instanceValue -> do
  config <- getXdgDirectory XdgConfig "Stillroom"
  createDirectoryIfMissing True config
  runDesktop (Just instanceValue) (config </> "settings.json") Nothing Nothing
