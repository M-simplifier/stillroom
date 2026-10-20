module CoerceSpace where

import Data.Coerce (coerce)
import Stillroom.Graphics.Space

invalid :: Point World -> Point Render
invalid = coerce
