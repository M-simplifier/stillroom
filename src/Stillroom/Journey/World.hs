-- | A world is a value. Every field query receives this seed explicitly;
-- renderer instances and saved journeys cannot change another world's answers.
module Stillroom.Journey.World (Seed, seed, seedValue, firstSeed, coordinateLimit, withinWorld) where

import Data.Word (Word32)

newtype Seed = Seed {seedValue :: Word32} deriving (Eq, Ord, Show)

seed :: Word32 -> Seed
seed = Seed

-- The landscape used in the first public journey film.
firstSeed :: Seed
firstSeed = Seed 0x73419a21

-- Keep simulation, native integer conversions and bookmarks in the same
-- supported range. Streaming bounds memory; it does not extend float precision.
coordinateLimit :: Double
coordinateLimit = 1000000

withinWorld :: Double -> Double -> Bool
withinWorld x z = abs x <= coordinateLimit && abs z <= coordinateLimit
