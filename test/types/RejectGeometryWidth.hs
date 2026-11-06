module RejectGeometryWidth where

import Stillroom.Landscape.Geometry

invalid :: Geometry -> Geometry
invalid geometry = geometry { heightWidth = 0 }
