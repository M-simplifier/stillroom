module ReplaceObservation where

import Stillroom.Graphics.Observation

invalid :: Observation target -> Observation target -> Observation target
invalid observation other = observation {clipFromRender = clipFromRender other}
