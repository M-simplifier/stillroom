module GraphicsChecks (graphicsChecks) where

import Data.List (tails)
import Data.Map.Strict qualified as Map
import Data.Vector.Storable qualified as V
import Stillroom.Journey.Frame qualified as F
import Stillroom.Graphics.Observation qualified as O
import Stillroom.Runtime.Journey.Packet qualified as Packet
import Stillroom.Graphics.Encoding.Mesh qualified as Mesh
import Stillroom.Graphics.BoxUnion qualified as S
import Stillroom.Journey.Railcar qualified as Railcar
import Test.QuickCheck

graphicsChecks :: IO Bool
graphicsChecks = and <$> sequence
  [ law "rectangle difference conserves exact area and disjointness" propDifference
  , law "solid boundary encloses the analytic union volume" propVolume
  , law "authored railcar exterior has one owner per positive-area face" (once propOwnership)
  , law "lowered railcar triangles have finite positions and matching winding" (once propWinding)
  , law "main projection preserves near/far and the viewing axis" propProjection
  , law "relative frame is invariant under large cell translations" propTranslation
  , law "shadow grid holds static vertices within a cell" propShadow
  , law "invalid frame inputs and targets are rejected" (once propInvalid)
  , law "reflection resolution preserves shared geometry and other observations" propReflectionIndependence
  ]
  where
    law title p = putStrLn title >> isSuccess <$> quickCheckWithResult stdArgs {maxSuccess=300} p

rect :: [Int] -> Maybe S.Rect
rect (a:b:c:d:_) =
  let x=fromIntegral (a `mod` 20-10); y=fromIntegral (b `mod` 20-10)
   in S.rectangle x (x+fromIntegral (1+c `mod` 10)) y (y+fromIntegral (1+d `mod` 10))
rect _ = Nothing

overlap :: S.Rect -> S.Rect -> Rational
overlap a b =
  let (x0,x1,y0,y1)=S.rectBounds a; (u0,u1,v0,v1)=S.rectBounds b
   in max 0 (min x1 u1-max x0 u0)*max 0 (min y1 v1-max y0 v0)

disjoint :: [S.Rect] -> Bool
disjoint rs = and [overlap a b == 0 | a:rest <- tails rs, b <- rest]

propDifference :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
propDifference a b c d e f g h = case (rect [a,b,c,d], rect [e,f,g,h]) of
  (Just r,Just s) ->
    let pieces=S.difference r s
     in sum (map S.rectArea pieces)+overlap r s == S.rectArea r
        && disjoint pieces && all (\p -> overlap p s==0 && overlap p r==S.rectArea p) pieces
  _ -> False

propVolume :: Int -> Int -> Int -> Bool
propVolume a b c =
  let x=fromIntegral (a `mod` 9-4); y=fromIntegral (b `mod` 9-4); z=fromIntegral (c `mod` 9-4)
      shared = max 0 (min 3 (x+2)-max 0 x)
             * max 0 (min 4 (y+3)-max 0 y) * max 0 (min 5 (z+4)-max 0 z)
   in case (S.cuboid (0,0,0) (3,4,5) Railcar.Wood,S.cuboid (x,y,z) (x+2,y+3,z+4) Railcar.Brass) of
        (Just p,Just q) ->
          let volume = sum [S.plane f*S.rectArea (S.region f) *
                   (if S.side f==S.Positive then 1 else -1) | f<-S.boundary [p,q]]/3
           in volume == 60+24-shared
        _ -> False

propOwnership :: Bool
propOwnership = case (Railcar.railcarBoxes,Railcar.railcarFaces) of
  (Right boxes,Right faces) ->
    length boxes==71 && not (null faces) &&
      all disjoint (Map.elems (Map.fromListWith (++)
        [((fromEnum (S.axis f),S.plane f),[S.region f]) | f<-faces]))
  _ -> False

propWinding :: Bool
propWinding = case Railcar.railcarMesh of
  Left _ -> False
  Right mesh -> case Mesh.lowerMesh mesh of
    Left _ -> False
    Right packet -> let values = Mesh.meshValues packet
      in all finite (V.toList values) && V.length values `mod` 30==0
         && all outward (chunks 30 (V.toList values))
  where
    finite x=not(isNaN x || isInfinite x)
    outward triangle = case chunks 10 triangle of
      [ax:ay:az:nx:ny:nz:_,bx:by:bz:_,cx:cy:cz:_] ->
        let ux=bx-ax; uy=by-ay; uz=bz-az; vx=cx-ax; vy=cy-ay; vz=cz-az
         in nx*(uy*vz-uz*vy)+ny*(uz*vx-ux*vz)+nz*(ux*vy-uy*vx)>0
      _ -> False
    chunks _ []=[]
    chunks n xs=take n xs:chunks n(drop n xs)

targets :: F.Targets
targets=F.Targets 1600 900 4096 1120 630

input :: Double -> Double -> F.FrameInput
input x z=F.FrameInput (F.Point x 12 z) 0.3 (-0.1) 60
  (F.Point (x+0.2) 10 (z-1.4)) (-0.3) 0.05

propProjection :: Int -> Bool
propProjection n =
  let raw=(input (fromIntegral(n `mod` 1000000)) (-901234.5)){F.yaw=0,F.pitch=0}
   in case F.planFrame targets raw of
    Left _ -> False
    Right f ->
      let F.Point ox _ oz=F.originOf f; F.Point x y z=F.eye raw
          project distance=let (a,b,c,w)=F.clipPoint f F.Main (F.Point (x-ox) y (z-oz-distance))
                            in (a/w,b/w,c/w)
          (nx,ny,nz)=project 0.15; (fx,fy,fz)=project 1200
       in maximum(map abs [nx,ny,nz+1,fx,fy,fz-1])<1e-8

propTranslation :: Int -> Int -> Bool
propTranslation ix iz =
  let dx=fromIntegral(ix `mod` 3500-1750)*256; dz=fromIntegral(iz `mod` 3500-1750)*256
      shifted=(input (17+dx) (91+dz))
      parts f=case Packet.lowerFrame f of
        Left _ -> Nothing
        Right encoded -> let p=Packet.packetValues encoded
          in Just (V.slice 16 16 p,V.slice 68 41 p)
   in case (F.planFrame targets (input 17 91),F.planFrame targets shifted) of
        (Right a,Right b) -> parts a/=Nothing && parts a==parts b
        _ -> False

propShadow :: Int -> Int -> Bool
propShadow ix iz =
  let x=fromIntegral(ix `mod` 1000000-500000)+0.321
      z=fromIntegral(iz `mod` 1000000-500000)+0.765
   in case (F.planFrame targets(input x z),F.planFrame targets(input (x+1e-7) (z-1e-7))) of
    (Right a,Right b) ->
      let uv f = let F.Point ox _ oz=F.originOf f
                     (clipU,clipV,_,w)=F.clipPoint f F.Shadow (F.Point (x+3-ox) 7 (z-8-oz))
                  in (clipU/w,clipV/w)
          (u,v)=uv a; (s,t)=uv b
       in F.shadowGrid a/=F.shadowGrid b || max(abs(u-s))(abs(v-t))<1e-9
    _ -> False

propInvalid :: Bool
propInvalid = encodingRejectsOverflow && all rejected
  [F.planFrame targets ((input 0 0){F.yaw=0/0}),
   F.planFrame targets ((input 0 0){F.seconds=1/0}),
   F.planFrame targets ((input 0 0){F.eye=F.Point 0 2048 0}),
   F.planFrame targets ((input 0 0){F.pitch=pi/2}),
   F.planFrame targets (input 1200000 0),
   F.planFrame (targets{F.shadowResolution=0}) (input 0 0)]
  where rejected (Left _)=True
        rejected _=False


-- Finite Double values may still be unrepresentable by a backend. Reject them
-- at the lowering boundary without conflating that failure with world input.
encodingRejectsOverflow :: Bool
encodingRejectsOverflow =
  case F.planFrame targets ((input 0 0){F.seconds=1e300}) of
    Left _ -> False
    Right frame -> case Packet.lowerFrame frame of
      Left Packet.NonFiniteEncoding -> True
      Right _ -> False

-- Changing one image's sampling resolution must not choose another geometry
-- or another clock for the other observations.
propReflectionIndependence :: Positive Int -> Positive Int -> Bool
propReflectionIndependence (Positive w) (Positive h) =
  let width=1+w `mod` 4096
      height=1+h `mod` 4096
      changed=targets{F.reflectionWidth=width,F.reflectionHeight=height}
   in case (F.planFrame targets (input 120 350),F.planFrame changed (input 120 350)) of
      (Right a,Right b) ->
        F.geometry a==F.geometry b && F.carTransform a==F.carTransform b &&
        F.mainPass a==F.mainPass b && F.shadowPass a==F.shadowPass b &&
        O.imageTarget (F.reflectionPass b)==F.ImageSize width height
      _ -> False
