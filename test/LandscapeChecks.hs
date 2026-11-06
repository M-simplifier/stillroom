module LandscapeChecks (landscapeChecks) where

import Control.Monad (forM)
import Data.List (foldl')
import Data.Vector.Storable qualified as V
import Data.Word (Word32)
import Stillroom.Journey.Walk qualified as Walk
import Stillroom.Journey.World (Seed, firstSeed, seed)
import Stillroom.Landscape.Field qualified as Field
import Stillroom.Landscape.Geometry qualified as Geometry
import Test.QuickCheck

landscapeChecks :: IO Bool
landscapeChecks = do
  properties <- sequence
    [ check "habitat weights are a finite probability partition" propHabitat,
      check "world queries remain isolated across interleaved seeds" propSeedIsolation,
      check "walking retains analytical Double terrain semantics" propWalkingField,
      check "raster coordinates explicitly quantize before field sampling" propRasterSampling,
      check "triangulation preserves corners and planar fields" propTrianglePlane
    ]
  examples <- forM unitChecks $ \(title, passed) -> do
    putStrLn title
    putStrLn (if passed then "+++ OK, passed 1 test." else "*** Failed.")
    pure passed
  pure (all isSuccess properties && and examples)
  where
    check title law = putStrLn title >> quickCheckWithResult stdArgs {maxSuccess=100} law

coordinates :: Int -> Int -> (Double,Double)
coordinates x z = (fromIntegral (x `mod` 20001)-10000.375,fromIntegral (z `mod` 20001)-10000.625)

propHabitat :: Word32 -> Int -> Int -> Bool
propHabitat raw x0 z0 =
  let (x,z) = coordinates x0 z0
      mix = Field.habitat (seed raw) x z
      weights = map (Field.habitatWeight mix) [minBound..maxBound]
      totalWeight = foldl' (+) 0 weights
   in all (\w -> finite w && w>=0 && w<=1) weights && abs (totalWeight-1)<1e-12

propSeedIsolation :: Word32 -> Word32 -> Int -> Int -> Bool
propSeedIsolation a b x0 z0 =
  let (x,z) = coordinates x0 z0
      before = Field.ground (seed a) x z
      other = Field.ground (seed b) (x-127.25) (z+255.75)
      after = Field.ground (seed a) x z
   in other `seq` before==after

propWalkingField :: Word32 -> Int -> Int -> Bool
propWalkingField raw x0 z0 =
  let (x,z) = coordinates x0 z0; world = seed raw
   in Walk.terrainHeight world x z==Field.ground world x z

propRasterSampling :: Word32 -> Int -> Int -> Bool
propRasterSampling raw x0 z0 =
  let (x,z) = coordinates x0 z0; world = seed raw
      xf = realToFrac x :: Float; zf = realToFrac z :: Float
   in Geometry.rasterGround world x z==realToFrac (Field.ground world (realToFrac xf) (realToFrac zf))

propTrianglePlane :: Int -> Int -> Int -> NonNegative Int -> NonNegative Int -> Bool
propTrianglePlane a0 b0 c0 (NonNegative u0) (NonNegative v0) =
  let a = fromIntegral (a0 `mod` 20) :: Float
      b = fromIntegral (b0 `mod` 20); c = fromIntegral (c0 `mod` 20)
      d = b+c-a; u = fromIntegral (u0 `mod` 101)/100; v = fromIntegral (v0 `mod` 101)/100
   in abs (Geometry.triangleHeight a b c d u v-(a+(b-a)*u+(c-a)*v))<0.00001

unitChecks :: [(String,Bool)]
unitChecks =
  [ ("analytical and sampled surfaces have an independent counterexample", Geometry.triangleHeight 0 1 1 2 0.5 0.5==1 && (0.5 :: Double)**2+0.5**2==0.5),
    ("invalid terrain destinations are rejected before integer conversion", all (\(x,z) -> not (Field.standable firstSeed x z)) [(0/0,0),(1/0,0),(0,-1/0),(10000001,0)]),
    ("chunk construction rejects unsupported coordinates", case Geometry.seededChunkGeometry firstSeed maxBound minBound Geometry.NearChunk of Left _ -> True; Right _ -> False),
    ("near and far geometry carry bounded streams and valid bone ancestry", all validChunk referenceChunks),
    ("negative-coordinate terrain seams have equal positions and normals", negativeSeam),
    ("chunk generation order cannot change an immutable result", reverseOrder)
  ]

referenceChunks :: [(Seed,Int,Int,Geometry.ChunkDetail)]
referenceChunks = [(firstSeed,-1,-2,Geometry.NearChunk),(seed 1,0,0,Geometry.NearChunk),(firstSeed,-1,-2,Geometry.FarChunk),(seed 0xffffffff,-3,2,Geometry.FarChunk)]

validChunk :: (Seed,Int,Int,Geometry.ChunkDetail) -> Bool
validChunk (world,x,z,detail) = case Geometry.seededChunkGeometry world x z detail of
  Left _ -> False
  Right geometry ->
    let materialValid material =
          let mesh = Geometry.materialMesh geometry material; n = Geometry.meshVertexCount mesh
           in n<=8388608 && n `mod` 3==0 && V.length (Geometry.meshNormals mesh)==n*3
              && V.length (Geometry.meshTexCoords mesh)==n*2 && V.length (Geometry.meshExtra mesh)==n*2
              && V.length (Geometry.meshColors mesh)==n*4
              && V.all finite (Geometry.meshPositions mesh) && V.all finite (Geometry.meshNormals mesh)
        size = Geometry.chunkCells detail+1
        bones = Geometry.boneCount geometry
        boneData = Geometry.bonePixels geometry
        ancestryValid = all (\i -> let parent = boneData V.! (i*8+3) in parent>=0 && parent<=fromIntegral i && parent==fromInteger (floor parent)) [0..bones-1]
        boneIdsValid material =
          let ids = Geometry.meshExtra (Geometry.materialMesh geometry material)
           in and [let bone = ids V.! i in bone>=(-1) && bone<fromIntegral bones && bone==fromInteger (floor bone) | i <- [0,2..V.length ids-1]]
     in all materialValid [minBound..maxBound] && Geometry.boneCount geometry<=16000
        && ancestryValid && boneIdsValid Geometry.Wood && boneIdsValid Geometry.Foliage
        && V.length (Geometry.bonePixels geometry)==256*Geometry.boneTextureHeight geometry*4
        && V.length (Geometry.heightPixels geometry)==size*size
        && V.all finite (Geometry.bonePixels geometry) && V.all finite (Geometry.heightPixels geometry)

negativeSeam :: Bool
negativeSeam = case (Geometry.seededChunkGeometry firstSeed (-2) (-2) Geometry.NearChunk,Geometry.seededChunkGeometry firstSeed (-1) (-2) Geometry.NearChunk) of
  (Right a,Right b) ->
    let ma = Geometry.materialMesh a Geometry.Terrain; mb = Geometry.materialMesh b Geometry.Terrain
        edge mesh x =
          let positions = Geometry.meshPositions mesh; normals = Geometry.meshNormals mesh
           in [(positions V.! (i*3+2),(positions V.! (i*3+1),normals V.! (i*3),normals V.! (i*3+1),normals V.! (i*3+2))) | i <- [0..6143],positions V.! (i*3)==x]
        ea = edge ma (-32); eb = edge mb (-32)
     in not (null ea) && all (`elem` eb) ea && all (`elem` ea) eb
  _ -> False

reverseOrder :: Bool
reverseOrder =
  let buildSignature (world,x,z,detail) = Geometry.geometrySignature <$> Geometry.seededChunkGeometry world x z detail
      forward = map buildSignature referenceChunks
      backward = reverse (map buildSignature (reverse referenceChunks))
   in forward==backward

finite :: RealFloat a => a -> Bool
finite x = not (isNaN x || isInfinite x)
