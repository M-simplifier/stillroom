-- | Rendering preference and a bounded, conservative automatic policy.
-- Neither the scene clock nor the journey's semantic world depends on this.
module Stillroom.Graphics
  ( GraphicsQuality (..), Tier (..), choices, fixedTier,
    Governor, initialGovernor, tier, observe,
  ) where

import Data.List (sort)

data GraphicsQuality = Automatic | Lightweight | Standard | High | Ultra
  deriving (Eq, Enum, Bounded, Show, Read)

-- FFI codes are explicit; do not couple them to preference enumeration.
data Tier = Economy | Balanced | Detailed | Supersampled
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

choices :: [GraphicsQuality]
choices = [minBound .. maxBound]

fixedTier :: GraphicsQuality -> Maybe Tier
fixedTier Automatic = Nothing
fixedTier Lightweight = Just Economy
fixedTier Standard = Just Balanced
fixedTier High = Just Detailed
fixedTier Ultra = Just Supersampled

data Governor = Governor
  { tier :: !Tier, ceilingTier :: !Tier, warmup :: !Int, samples :: [Double] }
  deriving (Eq, Show)

initialGovernor :: Governor
initialGovernor = Governor Balanced Supersampled 90 []

-- Completed GPU queries only. Ignore invalid/missing observations, settle for
-- three seconds, then consider the 90th percentile of four seconds at 30 Hz.
-- A sustained overload lowers one step and caps later promotion for this
-- scene/viewport session. One isolated streaming hitch cannot force a change.
observe :: Double -> Governor -> Governor
observe ms g
  | isNaN ms || isInfinite ms || ms <= 0 = g
  | warmup g > 0 = g {warmup = warmup g - 1}
  | length batch < 120 = g {samples = batch}
  | otherwise = Governor next cap 90 []
  where
    batch = ms : samples g
    p90 = case drop 107 (sort batch) of x : _ -> x; [] -> ms
    down = p90 > 25 && tier g > Economy
    up = p90 < 14 && tier g < ceilingTier g
    next | down = pred (tier g)
         | up = succ (tier g)
         | otherwise = tier g
    cap = if down then next else ceilingTier g
