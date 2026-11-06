{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (foldM)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
  initial <- evaluate (J.initial World.firstSeed)
  performGC
  before <- getRTSStats
  began <- getMonotonicTimeNSec
  (final, simulationNs, habitatNs, habitatSum) <- foldM frame (initial,0,0,0 :: Int) [1..1800 :: Int]
  ended <- getMonotonicTimeNSec
  performGC
  after <- getRTSStats
  printf "{\"frames\":1800,\"ticksPerFrame\":20,\"wallMs\":%.6f,\"simulationMs\":%.6f,\"habitatMs\":%.6f,\"allocatedBytes\":%d,\"habitatChecksum\":%d,\"finalProgress\":%.12f,\"finalX\":%.12f,\"finalEyeY\":%.12f}\n"
    (fromIntegral (ended-began)/1e6 :: Double) (fromIntegral simulationNs/1e6 :: Double)
    (fromIntegral habitatNs/1e6 :: Double) (allocated_bytes after-allocated_bytes before)
    habitatSum (J.progress (J.car final)) (W.posX (J.person final)) (W.eyeY (J.person final))
  where
    frame (!previous,!simulation,!habitatTime,!checksum) _ = do
      began <- getMonotonicTimeNSec
      let (_,next) = J.simulate 20 J.neutral previous previous
      current <- evaluate next
      stepped <- getMonotonicTimeNSec
      let person=J.person current
      habitat <- evaluate (fromEnum (W.habitatAt (J.worldSeed current) (W.posX person) (W.posZ person)))
      ended <- getMonotonicTimeNSec
      pure (current,simulation+stepped-began,habitatTime+ended-stepped,checksum+habitat)
