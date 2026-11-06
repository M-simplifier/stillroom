module Stillroom.Landscape.Chunk
  ( seededChunkGeometry, chunkSpan, chunkCells, chunkSamplingStep, rasterGround ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Bits ((.|.))
import Data.IntMap.Strict qualified as Map
import Data.STRef (writeSTRef)
import Data.Vector.Storable qualified as V
import Stillroom.Journey.World (Seed, coordinateLimit)
import Stillroom.Landscape.Ecology
import Stillroom.Landscape.Field qualified as F
import Stillroom.Landscape.Internal
import Stillroom.Landscape.Math
import Stillroom.Landscape.Plants
import Stillroom.Landscape.Railway

chunkSpan, chunkCells :: ChunkDetail -> Int
chunkSpan NearChunk = 32
chunkSpan FarChunk = 128
chunkCells NearChunk = 32
chunkCells FarChunk = 16

chunkSamplingStep :: ChunkDetail -> Float
chunkSamplingStep detail = fromIntegral (chunkSpan detail)/fromIntegral (chunkCells detail)

-- | Rendering intentionally quantizes coordinates to Float before the Double
-- analytical query. Walking continues to query the unquantized field.
rasterGround :: Seed -> Double -> Double -> Float
rasterGround seed x z = realToFrac (F.ground seed (realToFrac (realToFrac x :: Float)) (realToFrac (realToFrac z :: Float)))

seededChunkGeometry :: Seed -> Int -> Int -> ChunkDetail -> Either GenerationError Geometry
seededChunkGeometry seed cx cz detail
  | abs (fromIntegral cx*fromIntegral extent :: Double)>coordinateLimit+1024 || abs (fromIntegral cz*fromIntegral extent :: Double)>coordinateLimit+1024 = Left (ChunkOutsideWorld cx cz)
  | otherwise = runST $ do
      let cells = chunkCells detail; stride = cells+1; stepSize = chunkSamplingStep detail
          ox = fromIntegral (cx*extent); oz = fromIntegral (cz*extent)
          -- Controls are bounded to this patch and shared by value, including
          -- the normal apron and nearest-track projection outside its edge.
          firstControl = floor (((realToFrac oz :: Double)-256)/256)
          lastControl = floor (((realToFrac oz :: Double)+fromIntegral extent+256)/256)
          controls = Map.fromList [(i,F.railControlY seed i) | i <- [firstControl-1..lastControl+2]]
          control i = Map.findWithDefault (F.railControlY seed i) i controls
          trackY z = let f = z/256; i = floor f in F.railBlend (control (i-1)) (control i) (control (i+1)) (control (i+2)) (f-fromIntegral i)
          height x z = realToFrac (F.groundWithRail seed trackY (realToFrac x) (realToFrac z))
          apronStride = cells+3
          apron = V.generate (apronStride*apronStride) $ \k -> height (ox+fromIntegral (k `mod` apronStride-1)*stepSize) (oz+fromIntegral (k `div` apronStride-1)*stepSize)
          heights = V.generate (stride*stride) $ \k -> apron V.! ((k `div` stride+1)*apronStride+k `mod` stride+1)
          grid = V.generate (stride*stride) $ \k ->
            let i = k `mod` stride; j = k `div` stride
                x = ox+fromIntegral i*stepSize; z = oz+fromIntegral j*stepSize
                center = (j+1)*apronStride+i+1
                normal = normalize3 (V3 (apron V.! (center-1)-apron V.! (center+1)) (2*stepSize) (apron V.! (center-apronStride)-apron V.! (center+apronStride)))
                y = heights V.! k
                morph = if detail==FarChunk then y else patchHeight heights (fromIntegral i) (fromIntegral j) 8
             in Vertex (V3 x y z) normal (V2 0 0) (V2 morph 0) (terrainTint seed x z)
      s <- newBuild (Journey seed) (F.worldHash seed cx cz .|. 1) height trackY
      forM_ [0..cells-1] $ \j ->
        forM_ [0..cells-1] $ \i -> do
          let k = j*stride+i; a = grid V.! k; b = grid V.! (k+1)
              d = grid V.! (k+stride); e = grid V.! (k+stride+1)
          tri s Terrain a d b; tri s Terrain b d e
      forM_ [cz*(extent `div` 8)..(cz+1)*(extent `div` 8)-1] $ \j ->
        forM_ [cx*(extent `div` 8)..(cx+1)*(extent `div` 8)-1] $ \i ->
          case F.treePlace seed i j of
            Nothing -> pure ()
            Just (x,z,h) -> do
              writeSTRef (buildRandom s) (F.worldHash seed i j .|. 1)
              let species = F.treeSpecies seed x z
              if detail==FarChunk then distantTree s seed (realToFrac x) (realToFrac z) (realToFrac h) species
              else if species==F.Pines then conifer s (realToFrac x) (realToFrac z) (realToFrac h)
              else tree s (realToFrac x) (realToFrac z) (realToFrac h) True
      writeSTRef (buildRandom s) (F.worldHash seed (cx+119) (cz-271) .|. 1)
      when (detail==NearChunk) $ do
        vegetation s seed ox oz heights
        forM_ [floor (oz/12)..floor ((oz+fromIntegral extent)/12)] $ \j ->
          forM_ [floor (ox/12)..floor ((ox+fromIntegral extent)/12)] $ \i ->
            case F.boulderPlace seed i j of
              Just (x,z,r) | x>=realToFrac ox && x<realToFrac (ox+fromIntegral extent) && z>=realToFrac oz && z<realToFrac (oz+fromIntegral extent) -> rock s (realToFrac x) (realToFrac z) (realToFrac r)
              _ -> pure ()
      railChunk s seed cx cz extent detail
      let water = if V.minimum heights<0 then plane (fromIntegral extent) (fromIntegral extent) 16 16 (ox+fromIntegral extent*0.5) (oz+fromIntegral extent*0.5) else emptyMesh
      finishGeometry s water heights stride stride
  where extent = chunkSpan detail
