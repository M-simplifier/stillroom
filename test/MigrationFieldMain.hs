module Main (main) where

import Control.Monad (unless)
import Data.List (foldl', intercalate)
import Data.Word (Word32)
import Stillroom.Journey.World qualified as W
import Stillroom.Landscape.Field qualified as F
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

data Fixture = Fixture !String !Word32 !Double !Double ![Double] !Bool !Int ![Double] !Double

split :: String -> [String]
split text = case break (== ',') text of
  (item, []) -> [item]
  (item, _ : rest) -> item : split rest

parse :: Int -> String -> Either String Fixture
parse line text = case split text of
  [group, seed, x, z, g, n, sh, rx, ry, forest, stand, dominant, a, b, c, d, e, f, interest] ->
    Fixture group <$> number seed <*> number x <*> number z
      <*> traverse number [g,n,sh,rx,ry,forest] <*> ((== (1 :: Int)) <$> number stand)
      <*> number dominant <*> traverse number [a,b,c,d,e,f] <*> number interest
  _ -> Left ("Wrong CSV width on line " ++ show line)
  where
    number :: Read a => String -> Either String a
    number value = maybe (Left ("Invalid number on line " ++ show line ++ ": " ++ value)) Right (readMaybe value)

close :: Double -> Double -> Bool
close expected actual = not (isNaN actual || isInfinite actual) &&
  abs (expected-actual) <= 1e-9 + 1e-11 * max (abs expected) (abs actual)

observe :: Fixture -> ([String], [Double])
observe (Fixture group rawSeed x z expected stand dominant weights expectedInterest) =
  (numericFailures ++ discreteFailures, zipWith (\a b -> abs (a-b)) numericExpected numericActual)
  where
    seed = W.seed rawSeed
    mix = F.habitat seed x z
    values = [F.ground seed x z,F.naturalGround seed x z,F.shoreDistance seed x z,
      F.railX seed z,F.railY seed z,F.forestDensity seed x z]
    actualWeights = map (F.habitatWeight mix) [minBound .. maxBound]
    names = ["ground","naturalGround","shoreDistance","railX","railY","forestDensity"] ++
      ["habitat/" ++ show h | h <- [minBound .. maxBound :: F.Habitat]] ++ ["interest"]
    numericExpected = expected ++ weights ++ [expectedInterest]
    numericActual = values ++ actualWeights ++ [F.interest seed x z]
    context = group ++ " seed=" ++ show rawSeed ++ " x=" ++ show x ++ " z=" ++ show z
    numericFailures = [context ++ " " ++ name ++ " expected=" ++ show a ++ " actual=" ++ show b |
      (name,a,b) <- zip3 names numericExpected numericActual, not (close a b)]
    discreteFailures = [context ++ " standable differs" | F.standable seed x z /= stand] ++
      [context ++ " dominant habitat differs" | fromEnum (F.dominantHabitat mix) /= dominant]

main :: IO ()
main = do
  args <- getArgs
  let path = case args of [] -> "test/fixtures/landscape-field-v0.11.0.csv"; file : _ -> file
  contents <- readFile path
  fixtures <- either fail pure (traverse (uncurry parse) (zip [2..] (drop 1 (lines contents))))
  unless (length fixtures >= 17000) (fail "Field fixture is incomplete")
  let results = map observe fixtures
      failures = concatMap fst results
      maxima = foldl' (zipWith max) (replicate 13 0) (map snd results)
      negativeRejected = case fixtures of
        Fixture group seed x z (g : rest) stand dominant weights interest : _ ->
          not (null (fst (observe (Fixture group seed x z (g+0.25 : rest) stand dominant weights interest))))
        _ -> False
  putStrLn ("Independent f72f242 field samples: " ++ show (length fixtures))
  putStrLn ("Maximum absolute numeric errors: " ++ intercalate ", " (map show maxima))
  putStrLn ("Perturbed ground negative control rejected: " ++ show negativeRejected)
  unless (null failures && negativeRejected) $ do
    mapM_ putStrLn (take 16 failures)
    putStrLn ("Total failed observations: " ++ show (length failures))
    exitFailure
