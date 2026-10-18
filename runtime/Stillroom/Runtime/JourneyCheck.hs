{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Stillroom.Runtime.JourneyCheck (runJourneyCheck) where

import Control.Exception (bracket, bracket_, throwIO)
import Control.Monad (forM, forM_, replicateM_, unless)
import Data.Aeson (Value, decodeStrict', encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe)
import Raylib.Core qualified as R
import Raylib.Types
import Stillroom.Domain (Language (English))
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.Capture (screenshot)
import Stillroom.Runtime.Journey qualified as Runtime
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Accelerated semantic time, actual generated/uploaded/drawn production meshes.
-- This is a streaming correctness check, not a real-time FPS benchmark.
runJourneyCheck :: FilePath -> IO Bool
runJourneyCheck folder = do
  createDirectoryIfMissing True folder
  R.setTraceLogLevel LogWarning
  R.setConfigFlags [WindowHidden, WindowUndecorated, WindowAlwaysRun]
  bracket (R.initWindow 960 540 "Stillroom journey check") (R.closeWindow . Just) $ \_ -> do
    R.setTargetFPS 0
    Runtime.withJourney $ \runtime -> do
      began <- R.getTime
      let frame = bracket_ R.beginDrawing R.endDrawing $ Runtime.draw runtime (Rectangle 0 0 960 540)
          warm = replicateM_ 82 frame >> healthy runtime
      warm
      initial <- Runtime.model runtime
      let w = J.person initial; cx = floor (W.posX w / 32); cz = floor (W.posZ w / 32)
      (near, _) <- Runtime.fingerprintsForCheck runtime cx cz
      (_, far) <- Runtime.fingerprintsForCheck runtime (floor (W.posX w / 128)) (floor (W.posZ w / 128))
      habitats <- forM [1 .. 1800 :: Int] $ \i -> do
        Runtime.advanceForCheck runtime 20 J.neutral
        frame
        healthy runtime
        j <- Runtime.model runtime
        let person = J.person j
        if i `mod` 600 == 0 then screenshot (folder </> ("journey-" ++ show i ++ ".png")) else pure ()
        pure (W.habitatAt (J.worldSeed j) (W.posX person) (W.posZ person))
      end <- Runtime.model runtime
      metadata <- Runtime.info runtime
      (absentNear, _) <- Runtime.fingerprintsForCheck runtime cx cz
      (_, absentFar) <- Runtime.fingerprintsForCheck runtime (floor (W.posX w / 128)) (floor (W.posZ w / 128))
      -- Recreate from the original bookmark after a long journey: generation
      -- must be identical despite eviction, clock advancement and resource reload.
      Runtime.restoreModel runtime initial
      warm
      (regeneratedNear, _) <- Runtime.fingerprintsForCheck runtime cx cz
      (_, regeneratedFar) <- Runtime.fingerprintsForCheck runtime (floor (W.posX w / 128)) (floor (W.posZ w / 128))
      differences <- forM [0, 1, 0xffffffff] $ \number -> do
        let seed = World.seed number
        terrain <- Runtime.terrainMatchesForCheck seed
        Runtime.replace runtime seed
        warm
        j <- Runtime.model runtime
        let p = J.person j
        (signature, _) <- Runtime.fingerprintsForCheck runtime (floor (W.posX p / 32)) (floor (W.posZ p / 32))
        pure (terrain, signature)
      -- The same native renderer also follows the actual on-foot transfer.
      let seed = World.firstSeed
      stop <- maybe (throwIO (userError "No safe test stop")) pure $ listToMaybe [s | s <- [80, 82 .. 1000], isJust (J.landing seed (J.car (J.at seed s)))]
      Runtime.restoreModel runtime (J.pause (J.at seed stop))
      warm
      Runtime.advanceForCheck runtime 1 (J.neutral {J.access = True})
      forM_ [1 .. 45 :: Int] $ \_ -> Runtime.advanceForCheck runtime 1 J.neutral >> frame
      outside <- Runtime.model runtime
      Runtime.advanceForCheck runtime 60 (J.neutral {J.walking = W.neutral {W.forward = 1}})
      frame
      screenshot (folder </> "journey-on-foot.png")
      walked <- Runtime.model runtime
      Runtime.advanceForCheck runtime 60 (J.neutral {J.walking = W.neutral {W.forward = (-1)}})
      Runtime.advanceForCheck runtime 1 (J.neutral {J.access = True})
      Runtime.advanceForCheck runtime 45 J.neutral
      frame
      boarded <- Runtime.model runtime
      finished <- R.getTime
      let diagnostics = decodeStrict' (BC.pack metadata) :: Maybe Value
          bounded =
            diagnostics
              >>= parseMaybe
                ( withObject "Stream counters" $ \o -> do
                    resident <- o .: "resident"
                    farResident <- o .: "farResident"
                    missing <- o .: "nearMissing"
                    pure (resident == (49 :: Int) && farResident == (81 :: Int) && missing == (0 :: Int))
                )
          checks :: [(String, Bool)]
          checks =
            [ ("continuous rendered ten-minute ride across habitats", J.progress (J.car end) > 2500 && length (nub habitats) >= 3),
              ("near and far starting chunks evicted", near /= 0 && far /= 0 && absentNear == 0 && absentFar == 0),
              ("resident caches remain bounded and current terrain is present", bounded == Just True),
              ("regenerated near geometry is byte-identical", near == regeneratedNear),
              ("regenerated far geometry is byte-identical", far == regeneratedFar),
              ("navigation and renderer share each seed's terrain", all fst differences),
              ("new seeds produce distinct uploaded geometry", length (nub (map snd differences)) == 3 && all ((/= 0) . snd) differences),
              ("renderer follows safe exit, walking, and reboarding", J.seat outside == J.OnFoot && W.position (J.person outside) /= W.position (J.person walked) && J.seat boarded == J.Aboard)
            ]
      BL.writeFile
        (folder </> "journey-check.json")
        ( encode
            ( object
                [ "passed" .= all snd checks,
                  "checks" .= [object ["name" .= name, "passed" .= ok] | (name, ok) <- checks],
                  "semanticSeconds" .= (600 :: Int),
                  "renderedTravelFrames" .= (1800 :: Int),
                  "elapsedSeconds" .= (finished - began),
                  "habitats" .= map show (nub habitats),
                  "travelRenderer" .= diagnostics,
                  "audioOpened" .= False,
                  "personalSettingsRead" .= False
                ]
            )
        )
      pure (all snd checks)
  where
    healthy runtime = Runtime.status runtime English >>= \issue -> unless (issue == Nothing) (throwIO (userError (show issue)))
