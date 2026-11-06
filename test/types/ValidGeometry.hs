module ValidGeometry where

import Stillroom.Landscape.Geometry

readWidth :: Geometry -> Int
readWidth = heightWidth

readVertexCount :: MeshData -> Int
readVertexCount = meshVertexCount

readHeight :: Scene -> Either GenerationError Int
readHeight scene = heightWidth <$> fixedSceneGeometry scene
