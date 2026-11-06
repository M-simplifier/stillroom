module GrassChecks (grassChecks, Sample (..), authored, bladeFor, expanded) where

import Control.Monad (forM)
import Data.Vector.Storable qualified as V
import Data.Word (Word8, Word32)
import Foreign.Storable (peek, peekByteOff, sizeOf)
import GHC.Float (castFloatToWord32)
import Stillroom.Landscape.Grass
import Test.QuickCheck hiding (sample, scale)

data Sample = Sample (Float,Float,Float) Float Float Float Float (Word8,Word8,Word8,Word8)
  deriving Show

instance Arbitrary Sample where
  arbitrary = do
    x <- choose (-10000000,10000000); y <- choose (0.10,1000); z <- choose (-10000000,10000000)
    height <- choose (0.01,2.5); width <- choose (0.001,0.1)
    angle <- choose (0,2*piF); curve <- choose (-0.22,0.22); color <- arbitrary
    pure (Sample (x,y,z) height width angle curve color)

piF :: Float
piF = 3.14159265358979323846

bladeFor :: Word32 -> Sample -> GrassBlade
bladeFor offset (Sample root height width angle curve color) = grassBlade offset root height width angle curve color

expanded :: GrassUV -> GrassBlade -> [GrassVertex]
expanded mode blade = reverse (foldGrassVertices (flip (:)) [] mode blade)

-- Frozen authored emitBlade arithmetic/topology from fbf0300. This deliberately
-- does not call the shared kernel, its row factors or its canonical corners.
authored :: GrassUV -> Sample -> [GrassVertex]
authored mode (Sample root@(x,y,z) height width angle curve color) =
  let add (a,b,c) (d,e,f) = (a+d,b+e,c+f)
      scale (a,b,c) factor = (a*factor,b*factor,c*factor)
      side = (cos angle,0,sin angle)
      normal = (-sin angle,0.18,cos angle)
      vertex :: Int -> Float -> GrassVertex
      vertex i sideSign =
        let t = fromIntegral i/3
            w = width*(1-t)*(0.7+0.3*sin (t*piF))
            p = add root (curve*t*t,height*t,-curve*0.28*t*t)
            v = case mode of JourneyGrass -> y; FixedGrass -> sideSign
         in GrassVertex (add p (scale side (sideSign*w))) normal (t,v) (x,z) color
      row i = [vertex i (-1),vertex i 1,vertex (i+1) (-1)] ++
        if i<2 then [vertex i 1,vertex (i+1) 1,vertex (i+1) (-1)] else []
   in concatMap row [0..2]

vertexBits :: GrassVertex -> ([Word32],(Word8,Word8,Word8,Word8))
vertexBits (GrassVertex (x,y,z) (nx,ny,nz) (u,v) (a,b) color) =
  (map castFloatToWord32 [x,y,z,nx,ny,nz,u,v,a,b],color)

sameVertices :: [GrassVertex] -> [GrassVertex] -> Bool
sameVertices a b = map vertexBits a==map vertexBits b

propAuthored :: Sample -> Bool
propAuthored sample = all (\mode -> sameVertices (authored mode sample) (expanded mode (bladeFor 42 sample))) [JourneyGrass,FixedGrass]

grassChecks :: IO Bool
grassChecks = do
  putStrLn "compact grass preserves every authored Float bit and corner in both UV modes"
  parity <- quickCheckWithResult stdArgs {maxSuccess=1000} propAuthored
  storage <- packedLayout
  examples <- forM
    [ ("compact grass has fifteen canonical corners and five triangles", V.length grassCorners==15)
    , ("compact grass preserves signed zeros and authored endpoint cases", all propAuthored edgeCases)
    , ("grass oracle rejects changed curvature, corner order, UV rule and color", negativeOracles)
    , ("grass pre-wind GPU comparison threshold is finite and positive", all boundedTolerance edgeCases)
    , ("grass instance and canonical corner layouts round-trip at documented byte offsets", storage)
    ] $ \(title,passed) -> do
      putStrLn title
      putStrLn (if passed then "+++ OK, passed 1 test." else "*** Failed.")
      pure passed
  pure (isSuccess parity && and examples)

edgeCases :: [Sample]
edgeCases =
  [Sample (x,y,z) height width angle curve (0,127,255,255) |
    (x,y,z) <- [(0,0.1,0),(-0,0.1,-0),(-32,1,128),(-10000000,12,10000000)],
    (height,width) <- [(0,0),(0.01,0.001),(2.5,0.1)],
    angle <- [0,piF/2,piF,2*piF],curve <- [-0.22,-0,0.22]]

negativeOracles :: Bool
negativeOracles =
  let sample = Sample (3.125,0.7,-2.75) 0.43 0.022 0.91 0.19 (31,87,12,255)
      wrongCurve = Sample (3.125,0.7,-2.75) 0.43 0.022 0.91 (-0.19) (31,87,12,255)
      wrongColor = Sample (3.125,0.7,-2.75) 0.43 0.022 0.91 0.19 (32,87,12,255)
      correct = authored JourneyGrass sample
      compact = expanded JourneyGrass (bladeFor 0 sample)
      reordered = case compact of a:b:rest -> b:a:rest; _ -> []
   in sameVertices correct compact
      && not (sameVertices correct (expanded JourneyGrass (bladeFor 0 wrongCurve)))
      && not (sameVertices correct reordered)
      && not (sameVertices correct (expanded FixedGrass (bladeFor 0 sample)))
      && not (sameVertices correct (expanded JourneyGrass (bladeFor 0 wrongColor)))

boundedTolerance :: Sample -> Bool
boundedTolerance sample =
  let bound = grassPositionComparisonTolerance (bladeFor 0 sample)
   in bound>0 && not (isNaN bound || isInfinite bound)

packedLayout :: IO Bool
packedLayout = do
  let blade = grassBlade 0x10203040 (1.25,2.5,-3.75) 0.5 0.025 0.73 (-0.18) (17,34,51,68)
  bladePassed <- V.unsafeWith (V.singleton blade) $ \pointer -> do
    copied <- peek pointer
    rootX <- peekByteOff pointer 0; rootY <- peekByteOff pointer 4; rootZ <- peekByteOff pointer 8
    height <- peekByteOff pointer 12; width <- peekByteOff pointer 16
    sideX <- peekByteOff pointer 20; sideZ <- peekByteOff pointer 24; curve <- peekByteOff pointer 28
    color <- mapM (peekByteOff pointer) [32..35]
    offset <- peekByteOff pointer 36
    pure (copied==blade && sizeOf blade==40
      && (rootX,rootY,rootZ)==bladeRoot blade && height==bladeHeight blade && width==bladeWidth blade
      && (sideX,sideZ)==bladeSide blade && curve==bladeCurve blade
      && color==([17,34,51,68] :: [Word8]) && offset==bladeVertexOffset blade)
  cornersPassed <- and <$> V.unsafeWith grassCorners (\pointer -> forM [0..14] $ \i -> do
    copied <- peekByteOff pointer (i*12)
    pure (sizeOf copied==12 && copied==grassCorners V.! i))
  pure (bladePassed && cornersPassed)
