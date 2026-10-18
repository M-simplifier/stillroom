module Stillroom.Journey.Navigation
  ( Point (..),
    Field (..),
    Route (..),
    RouteIntent (..),
    emptyRoute,
    planRoute,
    distance,
    direction,
    angleDifference,
    segmentClear,
    followRoute,
    remember,
    clamp,
    maxTurn,
  )
where

import Data.Array (Array, array, (!))
import Data.IntMap.Strict qualified as Map
import Data.List (foldl', sortOn)
import Data.Set qualified as Set

data Point = Point !Double !Double deriving (Eq, Show)

data Field = Field
  { walkable :: Point -> Bool,
    elevation :: Point -> Double,
    attraction :: Point -> Double
  }

data RouteIntent = Exploring | Recovering deriving (Eq, Show)

data Route = Route {waypoints :: ![Point], destination :: !(Maybe Point), generation :: !Int, intent :: !RouteIntent}
  deriving (Eq, Show)

emptyRoute :: Route
emptyRoute = Route [] Nothing 0 Recovering

clamp :: Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi

distance :: Point -> Point -> Double
distance (Point x z) (Point a b) = sqrt ((x - a) ^ (2 :: Int) + (z - b) ^ (2 :: Int))

direction :: Point -> Point -> Double
direction (Point x z) (Point a b) = atan2 (a - x) (z - b)

angleDifference :: Double -> Double -> Double
angleDifference a b = atan2 (sin (a - b)) (cos (a - b))

maxTurn :: Double -> Double -> Double -> Double
maxTurn limit target current = current + clamp (-limit) limit (angleDifference target current)

offset :: Point -> Double -> Double -> Point
offset (Point x z) yaw d = Point (x + sin yaw * d) (z - cos yaw * d)

-- Samples a whole movement segment, including a small lateral margin. This
-- guards path smoothing and diagonal edges, not just endpoint occupancy.
segmentClear :: Field -> Point -> Point -> Bool
segmentClear field from@(Point x z) to@(Point a b) =
  let lengthOfSegment = distance from to
      count = max 1 (ceiling (lengthOfSegment / 0.30)) :: Int
      angle = direction from to
      sample t =
        let margin = 0.10 * min 1 (t * lengthOfSegment / 0.4)
            p = Point (x + (a - x) * t) (z + (b - z) * t)
         in walkable field p
              && walkable field (offset p (angle + pi / 2) margin)
              && walkable field (offset p (angle - pi / 2) margin)
      -- A manual handover may touch a thin steep rim. Probe the first few
      -- centimetres as well as the regular grid, so a route cannot skip it.
      early = [min 1 (d / max 0.0001 lengthOfSegment) | d <- [0.025, 0.05, 0.10, 0.20]]
   in all sample (early ++ [fromIntegral i / fromIntegral count | i <- [1 .. count]])

remember :: Point -> [Point] -> [Point]
remember p [] = [p]
remember p old@(lastPoint : _)
  | distance p lastPoint >= 7 = take 48 (p : old)
  | otherwise = old

-- Safety filters candidates before scenic interest is considered. A destination
-- is chosen only when a clear segment or an actual traversable path reaches it.
planRoute :: Field -> Point -> Double -> [Point] -> Int -> Route
planRoute field start heading visited serial =
  let Point x z = start
      wander = 0.42 * sin (x * 0.007 + z * 0.006 + fromIntegral serial * 0.53)
      candidates = [offset start (heading + wander + a) r | r <- [24, 34], a <- [0, 0.42, -0.42, 0.85, -0.85, 1.45, -1.45, 2.3, -2.3, pi]]
      penalty p = sum [exp (negate ((distance p q / 14) ^ (2 :: Int))) | q <- visited]
      score p = 2.4 * cos (angleDifference (direction start p) heading) + 0.85 * attraction field p - 1.3 * penalty p
      ranked = sortOn (negate . score) (filter (walkable field) candidates)
      routeTo p
        | segmentClear field start p = Just [p]
        | otherwise = gridPath field start p
      choose [] = []
      choose (p : ps) = case routeTo p of Just route -> route; Nothing -> choose ps
      mainRoute = choose (take 8 ranked)
      -- A close obstruction can leave no two-metre grid edge from the exact
      -- manual position. Small reachable steps recover without teleporting.
      escape = [offset start (heading + a) r | r <- [3, 1, 0.45], a <- [0, 0.6, -0.6, 1.2, -1.2, 1.9, -1.9, pi]]
      fallback = case filter (segmentClear field start) escape of p : _ -> [p]; [] -> []
      points = if null mainRoute then fallback else mainRoute
   in Route points (case reverse points of p : _ -> Just p; [] -> Nothing) (serial + 1) (if null mainRoute then Recovering else Exploring)

-- Bounded local A*. The grid is centered on the actual current position, so
-- enabling auto never snaps the player to a global grid or a prescribed route.
gridPath :: Field -> Point -> Point -> Maybe [Point]
gridPath field start@(Point x z) target = search (0 :: Int) initialQueue (Map.singleton origin 0) Map.empty Set.empty
  where
    radius = 19 :: Int
    side = 2 * radius + 1
    origin = radius * side + radius
    point i = let (j, k) = i `divMod` side in Point (x + fromIntegral (k - radius) * 2) (z + fromIntegral (j - radius) * 2)
    cells :: Array Int Bool
    cells = array (0, side * side - 1) [(i, i == origin || walkable field (point i)) | i <- [0 .. side * side - 1]]
    heuristic i = distance (point i) target
    initialQueue = Set.singleton (heuristic origin, origin)
    neighbors i =
      let (j, k) = i `divMod` side
       in [nj * side + nk | (dj, dk) <- [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)], let nj = j + dj, let nk = k + dk, nj >= 0, nj < side, nk >= 0, nk < side]
    reconstruct parents i
      | i == origin = []
      | otherwise = case Map.lookup i parents of Nothing -> []; Just previous -> reconstruct parents previous ++ [point i]
    simplify _ [] = []
    simplify p path = case dropWhile (not . segmentClear field p . snd) (reverse (zip [0 ..] path)) of
      (i, q) : _ -> q : simplify q (drop (i + 1) path)
      [] -> path
    search count queue scores parents closed
      | count >= 650 = Nothing
      | otherwise = case Set.minView queue of
          Nothing -> Nothing
          Just ((_, node), rest)
            | Set.member node closed -> search count rest scores parents closed
            | heuristic node < 2.9 && segmentClear field (point node) target -> Just (simplify start (reconstruct parents node ++ [target]))
            | otherwise ->
                let oldCost = Map.findWithDefault 1e20 node scores
                    visit (q, sc, pa) next
                      | Set.member next closed || not (cells ! next) || not (segmentClear field (point node) (point next)) = (q, sc, pa)
                      | otherwise =
                          let cost = oldCost + distance (point node) (point next) + abs (elevation field (point node) - elevation field (point next)) * 2.0
                           in if cost >= Map.findWithDefault 1e20 next sc
                                then (q, sc, pa)
                                else (Set.insert (cost + heuristic next, next) q, Map.insert next cost sc, Map.insert next node pa)
                    (nextQ, nextScores, nextParents) = foldl' visit (rest, scores, parents) (neighbors node)
                 in search (count + 1) nextQ nextScores nextParents (Set.insert node closed)

followRoute :: Point -> Route -> Route
followRoute p route = route {waypoints = dropWhile (\q -> distance p q < (if intent route == Recovering then 0.12 else 0.35)) (waypoints route)}
