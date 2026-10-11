-- | A finite shuffle bag over a non-empty collection. Every cycle is a
-- permutation. With distinct inputs, cycles never meet on the same item.
module Stillroom.Shuffle (Bag, newBag, draw, exclude) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import System.Random (StdGen, mkStdGen, randomR)

data Bag a = Bag !(NonEmpty a) ![a] !(Maybe a) !(Maybe a) !StdGen deriving (Show)

newBag :: Int -> NonEmpty a -> Bag a
newBag seed xs = Bag xs [] Nothing Nothing (mkStdGen seed)

-- | Remove a failed item from both the remaining cycle and all future cycles.
-- Returning Nothing makes an exhausted catalogue explicit.
exclude :: (Eq a) => a -> Bag a -> Maybe (Bag a)
exclude item (Bag original _ previous beforeDraw generator) = do
  remaining <- NE.nonEmpty (filter (/= item) (NE.toList original))
  let accepted = if previous == Just item then beforeDraw else previous
  -- A changed catalogue starts a fresh cycle. A failed draw must not replace
  -- the last accepted item used to prevent a repeat at that boundary.
  pure (Bag remaining [] accepted accepted generator)

draw :: (Eq a) => Bag a -> (a, Bag a)
draw (Bag original (x : xs) previous _ generator) = (x, Bag original xs (Just x) previous generator)
draw (Bag original [] previous _ generator) =
  let (shuffled, nextGenerator) = shuffle (NE.toList original) generator
      rotated = case shuffled of
        a : b : rest | Just a == previous -> b : a : rest
        _ -> shuffled
   in case rotated of
        x : xs -> (x, Bag original xs (Just x) previous nextGenerator)
        [] -> let x :| xs = original in (x, Bag original xs (Just x) previous nextGenerator)

-- Selection without replacement: O(n^2), appropriate for a bounded desktop
-- music catalogue; total even at the empty boundary. No unsafe array indexing.
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle [] g = ([], g)
shuffle xs g =
  let (index, g') = randomR (0, length xs - 1) g
      (before, after) = splitAt index xs
   in case after of
        [] -> (xs, g')
        chosen : rest ->
          let (tailItems, g'') = shuffle (before ++ rest) g'
           in (chosen : tailItems, g'')
