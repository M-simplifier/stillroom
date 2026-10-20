module ReplacePlan where

import Stillroom.Journey.Frame

invalid :: FramePlan -> FramePlan
invalid frame = frame {shadowTexel = 0}
