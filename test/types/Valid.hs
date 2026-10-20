module Valid where

import Stillroom.Graphics.Observation
import Stillroom.Graphics.Space
import Stillroom.Journey.Frame

composed :: Transform Render Clip
composed = compose (perspective 1 1 0.15 1200) (look (Point 0 2 0) (Direction 0 0 (-1)))

readPlan :: FramePlan -> Double
readPlan = shadowTexel

sameObservation :: Observation target -> Observation target
sameObservation = mapTarget id
