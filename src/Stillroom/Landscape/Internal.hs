{-# LANGUAGE BangPatterns #-}

-- Internal constructors are kept out of the renderer-facing module. Mutable
-- buffers are scoped by runST and never retained by a Geometry.
module Stillroom.Landscape.Internal where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.STRef
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as M
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Storable
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Float (castFloatToWord32)
import Stillroom.Journey.World (Seed)
import Stillroom.Landscape.Math

data Material = Terrain | Wood | Foliage | Grass deriving (Eq, Ord, Show, Enum, Bounded)
data Scene = BirchLake | AlpineLake | MeadowStream deriving (Eq, Ord, Show, Enum, Bounded)
data ChunkDetail = NearChunk | FarChunk deriving (Eq, Ord, Show)
data GenerationError = VertexCapacityExceeded !Material | BoneCapacityExceeded | ChunkOutsideWorld !Int !Int
  deriving (Eq, Show)

data Vertex = Vertex !V3 !V3 !V2 !V2 !Colour deriving (Eq, Show)

instance Storable Vertex where
  sizeOf _ = 44
  alignment _ = 4
  peek p = do
    x <- peekByteOff p 0; y <- peekByteOff p 4; z <- peekByteOff p 8
    nx <- peekByteOff p 12; ny <- peekByteOff p 16; nz <- peekByteOff p 20
    u <- peekByteOff p 24; v <- peekByteOff p 28; b <- peekByteOff p 32; phase <- peekByteOff p 36
    r <- peekByteOff p 40; g <- peekByteOff p 41; bl <- peekByteOff p 42; a <- peekByteOff p 43
    pure (Vertex (V3 x y z) (V3 nx ny nz) (V2 u v) (V2 b phase) (Colour r g bl a))
  poke p (Vertex (V3 x y z) (V3 nx ny nz) (V2 u v) (V2 b phase) (Colour r g bl a)) = do
    pokeByteOff p 0 x; pokeByteOff p 4 y; pokeByteOff p 8 z
    pokeByteOff p 12 nx; pokeByteOff p 16 ny; pokeByteOff p 20 nz
    pokeByteOff p 24 u; pokeByteOff p 28 v; pokeByteOff p 32 b; pokeByteOff p 36 phase
    pokeByteOff p 40 r; pokeByteOff p 41 g; pokeByteOff p 42 bl; pokeByteOff p 43 a

data MeshData = MeshData
  { meshPositions :: !(V.Vector Float), meshNormals :: !(V.Vector Float),
    meshTexCoords :: !(V.Vector Float), meshExtra :: !(V.Vector Float),
    meshColors :: !(V.Vector Word8), meshIndices :: !(V.Vector Word16)
  } deriving (Eq, Show)

meshVertexCount :: MeshData -> Int
meshVertexCount = (`div` 3) . V.length . meshPositions

meshTriangleCount :: MeshData -> Int
meshTriangleCount m = if V.null (meshIndices m) then meshVertexCount m `div` 3 else V.length (meshIndices m) `div` 3

emptyMesh :: MeshData
emptyMesh = MeshData V.empty V.empty V.empty V.empty V.empty V.empty

data Geometry = Geometry
  { geometryTerrain :: !MeshData, geometryWood :: !MeshData,
    geometryFoliage :: !MeshData, geometryGrass :: !MeshData,
    waterMesh :: !MeshData, bonePixels :: !(V.Vector Float),
    boneTextureHeight :: !Int, heightPixels :: !(V.Vector Float),
    heightWidth :: !Int, heightHeight :: !Int,
    boneCount :: !Int, treeCount :: !Int, grassCount :: !Int,
    geometrySignature :: !Word64
  } deriving (Eq, Show)

materialMesh :: Geometry -> Material -> MeshData
materialMesh g Terrain = geometryTerrain g
materialMesh g Wood = geometryWood g
materialMesh g Foliage = geometryFoliage g
materialMesh g Grass = geometryGrass g

data Landscape = Fixed !Scene | Journey !Seed deriving (Eq, Show)

data Builder s = Builder !Material !(STRef s (M.MVector s Vertex)) !(STRef s Int)
data Build s = Build
  { terrainBuilder :: !(Builder s), woodBuilder :: !(Builder s),
    foliageBuilder :: !(Builder s), grassBuilder :: !(Builder s),
    buildBones :: !(M.MVector s Float), buildBoneCount :: !(STRef s Int),
    buildTreeCount :: !(STRef s Int), buildGrassCount :: !(STRef s Int),
    buildRandom :: !(STRef s Word32), buildFailure :: !(STRef s (Maybe GenerationError)),
    buildLandscape :: !Landscape, buildHeight :: !(Float -> Float -> Float),
    buildRailY :: !(Double -> Double)
  }

newBuild :: Landscape -> Word32 -> (Float -> Float -> Float) -> (Double -> Double) -> ST s (Build s)
newBuild landscape seed height trackY = do
  let builder material = do
        vs <- M.new 16384 >>= newSTRef
        n <- newSTRef 0
        pure (Builder material vs n)
  a <- builder Terrain; b <- builder Wood; c <- builder Foliage; d <- builder Grass
  bones <- M.replicate (16000*8) 0
  bc <- newSTRef 0; tc <- newSTRef 0; gc <- newSTRef 0; rng <- newSTRef seed; failure <- newSTRef Nothing
  pure (Build a b c d bones bc tc gc rng failure landscape height trackY)

builderFor :: Build s -> Material -> Builder s
builderFor s Terrain = terrainBuilder s
builderFor s Wood = woodBuilder s
builderFor s Foliage = foliageBuilder s
builderFor s Grass = grassBuilder s

addVertex :: Build s -> Material -> Vertex -> ST s ()
addVertex s material v = do
  let Builder _ storage size = builderFor s material
  n <- readSTRef size
  if n>=8388608 then writeSTRef (buildFailure s) (Just (VertexCapacityExceeded material)) else do
    current <- readSTRef storage
    grown <- if n<M.length current then pure current else do
      next <- M.grow current (M.length current)
      writeSTRef storage next
      pure next
    M.write grown n v
    writeSTRef size (n+1)

tri :: Build s -> Material -> Vertex -> Vertex -> Vertex -> ST s ()
tri s material a b c = do
  let Builder _ storage size = builderFor s material
  n <- readSTRef size
  if n+3>8388608 then writeSTRef (buildFailure s) (Just (VertexCapacityExceeded material)) else do
    current <- readSTRef storage
    grown <- if n+3<=M.length current then pure current else do
      next <- M.grow current (M.length current)
      writeSTRef storage next
      pure next
    M.write grown n a
    M.write grown (n+1) b
    M.write grown (n+2) c
    writeSTRef size (n+3)
{-# INLINE tri #-}

vert :: V3 -> V3 -> V2 -> Int -> Float -> Colour -> Vertex
vert p n uv bone phase c = Vertex p n uv (V2 (fromIntegral bone) phase) c

newBone :: Build s -> V3 -> Int -> Float -> Float -> ST s Int
newBone s (V3 x y z) parent stiffness phase = do
  n <- readSTRef (buildBoneCount s)
  if n>=16000 then writeSTRef (buildFailure s) (Just BoneCapacityExceeded) >> pure (-1) else do
    let values = [x,y,z,fromIntegral (parent+1),stiffness,phase,0,0]
    forM_ (zip [n*8..] values) $ \(i,v) -> M.write (buildBones s) i v
    writeSTRef (buildBoneCount s) (n+1)
    pure n

randf :: Build s -> ST s Float
randf s = do
  old <- readSTRef (buildRandom s)
  let a = old `xor` (old `shiftL` 13); b = a `xor` (a `shiftR` 17); c = b `xor` (b `shiftL` 5)
  writeSTRef (buildRandom s) c
  pure (fromIntegral (c `shiftR` 8)*(1/16777216))

range :: Build s -> Float -> Float -> ST s Float
range s a b = (\r -> a+(b-a)*r) <$> randf s

surface :: Build s -> Float -> Float -> V3
surface s x z = V3 x (buildHeight s x z) z

terrainNormal :: Build s -> Float -> Float -> V3
terrainNormal s x z =
  let d = 0.12; h = buildHeight s
   in normalize3 (V3 (h (x-d) z-h (x+d) z) (2*d) (h x (z-d)-h x (z+d)))

freezeBuilder :: Word64 -> Builder s -> ST s (MeshData,Word64)
freezeBuilder initialHash (Builder _ storage size) = do
  n <- readSTRef size
  current <- readSTRef storage
  positions <- M.new (n*3)
  normals <- M.new (n*3)
  texCoords <- M.new (n*2)
  extra <- M.new (n*2)
  colors <- M.new (n*4)
  let bytes = M.unsafeCast current
      hashBytes !j !end !signature
        | j>=end = pure signature
        | otherwise = do
            byte <- M.read bytes j
            hashBytes (j+1) end (fnvByte signature byte)
      pack !i !signature
        | i>=n = pure signature
        | otherwise = do
            vertex@(Vertex (V3 x y z) (V3 nx ny nz) (V2 u v) (V2 bone phase) (Colour r g b a)) <- M.read current i
            M.write positions (i*3) x; M.write positions (i*3+1) y; M.write positions (i*3+2) z
            M.write normals (i*3) nx; M.write normals (i*3+1) ny; M.write normals (i*3+2) nz
            M.write texCoords (i*2) u; M.write texCoords (i*2+1) v
            M.write extra (i*2) bone; M.write extra (i*2+1) phase
            M.write colors (i*4) r; M.write colors (i*4+1) g; M.write colors (i*4+2) b; M.write colors (i*4+3) a
            nextSignature <- case targetByteOrder of
              LittleEndian -> hashBytes (i*44) (i*44+44) signature
              BigEndian -> pure (fnvVertex signature vertex)
            pack (i+1) nextSignature
  signature <- pack 0 initialHash
  p <- V.unsafeFreeze positions; nrm <- V.unsafeFreeze normals
  uv <- V.unsafeFreeze texCoords; ex <- V.unsafeFreeze extra; color <- V.unsafeFreeze colors
  pure (MeshData p nrm uv ex color V.empty,signature)

fnvByte :: Word64 -> Word8 -> Word64
fnvByte h b = (h `xor` fromIntegral b)*1099511628211
{-# INLINE fnvByte #-}

fnvFloat :: Word64 -> Float -> Word64
fnvFloat h f =
  let w = castFloatToWord32 f
      byte n = fromIntegral (w `shiftR` n .&. 255)
   in fnvByte (fnvByte (fnvByte (fnvByte h (byte 0)) (byte 8)) (byte 16)) (byte 24)
{-# INLINE fnvFloat #-}

fnvVertex :: Word64 -> Vertex -> Word64
fnvVertex h (Vertex (V3 x y z) (V3 a b c) (V2 u v) (V2 bone phase) (Colour r g bl alpha)) =
  let h0 = fnvFloat (fnvFloat (fnvFloat h x) y) z
      h1 = fnvFloat (fnvFloat (fnvFloat h0 a) b) c
      h2 = fnvFloat (fnvFloat h1 u) v
      h3 = fnvFloat (fnvFloat h2 bone) phase
   in fnvByte (fnvByte (fnvByte (fnvByte h3 r) g) bl) alpha
{-# INLINE fnvVertex #-}

-- The compatibility signature is defined in little-endian scalar order.
-- Vertex has no padding; on little-endian hosts its stored bytes are already
-- that representation, so hashing need not reconstruct every Float's bits.
fnvFloats :: Word64 -> V.Vector Float -> Word64
fnvFloats h values = case targetByteOrder of
  LittleEndian -> V.foldl' fnvByte h (V.unsafeCast values)
  BigEndian -> V.foldl' fnvFloat h values

finishGeometry :: Build s -> MeshData -> V.Vector Float -> Int -> Int -> ST s (Either GenerationError Geometry)
finishGeometry s water heights width height = do
  failed <- readSTRef (buildFailure s)
  case failed of
    Just e -> pure (Left e)
    Nothing -> do
      bones <- readSTRef (buildBoneCount s); trees <- readSTRef (buildTreeCount s); blades <- readSTRef (buildGrassCount s)
      let bh = max 1 ((bones*2+255) `div` 256)
      payload <- M.replicate (256*bh*4) 0
      M.copy (M.slice 0 (bones*8) payload) (M.slice 0 (bones*8) (buildBones s))
      pixels <- V.unsafeFreeze payload
      let !signature0 = fnvFloats 1469598103934665603 (V.take (bones*8) pixels)
          !signature1 = fnvFloats signature0 heights
      (a,signature2) <- freezeBuilder signature1 (terrainBuilder s)
      (b,signature3) <- freezeBuilder signature2 (woodBuilder s)
      (c,signature4) <- freezeBuilder signature3 (foliageBuilder s)
      (d,signature) <- freezeBuilder signature4 (grassBuilder s)
      pure (Right (Geometry a b c d water pixels bh heights width height bones trees blades signature))

-- raylib GenMeshPlane's retained vertex/index topology, with world translation.
plane :: Float -> Float -> Int -> Int -> Float -> Float -> MeshData
plane width lengthZ resX resZ ox oz =
  let nx = resX+1; nz = resZ+1; n = nx*nz
      pos = V.generate (n*3) $ \i -> let k = i `div` 3; x = k `mod` nx; z = k `div` nx in case i `mod` 3 of
        0 -> (fromIntegral x/fromIntegral resX-0.5)*width+ox
        1 -> 0
        _ -> (fromIntegral z/fromIntegral resZ-0.5)*lengthZ+oz
      normals = V.generate (n*3) (\i -> if i `mod` 3==1 then 1 else 0)
      uv = V.generate (n*2) $ \i -> let k = i `div` 2 in if even i then fromIntegral (k `mod` nx)/fromIntegral resX else fromIntegral (k `div` nx)/fromIntegral resZ
      indices = V.generate (resX*resZ*6) $ \i ->
        let cell = i `div` 6; x = cell `mod` resX; z = cell `div` resX
            a = z*nx+x; b = a+1; c = (z+1)*nx+x; d = c+1
         in fromIntegral (case i `mod` 6 of 0 -> c; 1 -> b; 2 -> a; 3 -> c; 4 -> d; _ -> b)
   in MeshData pos normals uv V.empty V.empty indices
