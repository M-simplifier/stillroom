module RejectMeshPositions where

import Data.Vector.Storable qualified as V
import Stillroom.Landscape.Geometry

invalid :: MeshData -> MeshData
invalid mesh = mesh { meshPositions = V.empty }
