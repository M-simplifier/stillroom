{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Exception (bracket, evaluate)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Vector.Storable qualified as V
import Data.Word (Word8)
import Foreign (Storable, castPtr, free, mallocBytes, pokeByteOff, sizeOf)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Stillroom.Journey.World qualified as W
import Stillroom.Landscape.Geometry qualified as G
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutBuf, withFile)
import System.Mem (performGC)

writeVector :: Storable a => FilePath -> V.Vector a -> IO ()
writeVector path values = case V.uncons values of
  Nothing -> BS.writeFile path BS.empty
  Just (value, _) -> V.unsafeWith values $ \pointer ->
    BS.packCStringLen (castPtr pointer, V.length values * sizeOf value) >>= BS.writeFile path

writeMesh :: FilePath -> G.MeshData -> IO ()
writeMesh path mesh = bracket (mallocBytes bytes) free $ \pointer -> do
  forM_ [0..count-1] $ \i -> do
    let scalar offset vector dimension component =
          pokeByteOff pointer (i*44+offset) (vector V.! (i*dimension+component))
    forM_ [0..2] $ \j -> scalar (j*4) (G.meshPositions mesh) 3 j
    forM_ [0..2] $ \j -> scalar (12+j*4) (G.meshNormals mesh) 3 j
    forM_ [0..1] $ \j -> scalar (24+j*4) (G.meshTexCoords mesh) 2 j
    forM_ [0..1] $ \j -> scalar (32+j*4) (G.meshExtra mesh) 2 j
    forM_ [0..3] $ \j -> pokeByteOff pointer (i*44+40+j) (G.meshColors mesh V.! (i*4+j) :: Word8)
  withFile path WriteMode $ \handle -> hPutBuf handle pointer bytes
  where
    count = G.meshVertexCount mesh
    bytes = count*44

writeGeometry :: FilePath -> String -> G.Geometry -> IO ()
writeGeometry folder name geometry = do
  writeVector (part "bones") (V.take (G.boneCount geometry*8) (G.bonePixels geometry))
  writeVector (part "height") (G.heightPixels geometry)
  forM_ (zip [0..] [minBound .. maxBound :: G.Material]) $ \(i, material) ->
    writeMesh (part ("material" ++ show (i :: Int))) (G.diagnosticMaterialMesh geometry material)
  where
    part suffix = folder </> name ++ "-" ++ suffix ++ ".bin"

main :: IO ()
main = do
  args <- getArgs
  folder <- case args of directory : _ -> pure directory; _ -> fail "Supply a fresh output folder"
  createDirectoryIfMissing True folder
  let fixed = [("fixed-" ++ show (fromEnum scene), G.fixedSceneGeometry scene) | scene <- [minBound .. maxBound]]
      chunks = [("chunk-" ++ show seed ++ "-" ++ show cx ++ "-" ++ show cz ++ "-" ++ show far,
          G.seededChunkGeometry (W.seed seed) cx cz detail) |
        seed <- [0x73419a21,1,0xffffffff], (cx,cz) <- [(-1,-2),(0,0),(1,-1),(-3,2)],
        (far,detail) <- [(0 :: Int,G.NearChunk),(1,G.FarChunk)]]
  enabled <- getRTSStatsEnabled
  unless enabled (fail "Run geometry acquisition with +RTS -T -RTS")
  putStrLn "name,bones,trees,grass,heightWidth,heightRows,water,terrain,wood,foliage,grassVertices,signature,cpuMs,wallMs,allocatedBytes,generationWallMs,fingerprintWallMs"
  forM_ (fixed ++ chunks) $ \(name, result) -> do
    performGC
    began <- getCPUTime
    wallBegan <- getMonotonicTimeNSec
    statsBefore <- getRTSStats
    geometry <- either (fail . show) evaluate result
    geometryReady <- getMonotonicTimeNSec
    signature <- evaluate (G.geometrySignature geometry)
    ended <- getCPUTime
    wallEnded <- getMonotonicTimeNSec
    performGC
    statsAfter <- getRTSStats
    let water = if G.meshVertexCount (G.waterMesh geometry)>0 then 1 else 0 :: Int
        counts = [G.boneCount geometry,G.treeCount geometry,G.grassCount geometry,
          G.heightWidth geometry,G.heightHeight geometry,water] ++
          [G.materialVertexCount geometry material | material <- [minBound .. maxBound]]
    putStrLn (intercalate "," (name : map show counts ++ [show signature,show (fromIntegral (ended-began)/1e9 :: Double),show (fromIntegral (wallEnded-wallBegan)/1e6 :: Double),show (allocated_bytes statsAfter-allocated_bytes statsBefore),show (fromIntegral (geometryReady-wallBegan)/1e6 :: Double),show (fromIntegral (wallEnded-geometryReady)/1e6 :: Double)]))
    unless (all (>=0) counts) (fail "Negative generation count")
    writeGeometry folder name geometry
