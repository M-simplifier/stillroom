-- | Versioned native protocol for Stillroom's journey renderer.
-- The generic core has no knowledge of this packet or its fixed pass schedule.
-- Validate representability once, after Double-relative transforms are complete.
module Stillroom.Runtime.Journey.Packet
  ( FramePacket, FrameEncodingError (..), lowerFrame, packetValues, packetSize,
  )
where

import Data.Vector.Storable qualified as V
import Stillroom.Graphics.Observation
import Stillroom.Journey.Frame

newtype FramePacket = FramePacket (V.Vector Float)
data FrameEncodingError = NonFiniteEncoding deriving (Eq, Show)

-- v1: header16 + object/light/reflection48 + shadow units4
--     + three observations(41 each) + main clip16.
packetSize :: Int
packetSize = 207

packetValues :: FramePacket -> V.Vector Float
packetValues (FramePacket values) = values

lowerFrame :: FramePlan -> Either FrameEncodingError FramePacket
lowerFrame frame
  | V.all finite encoded = Right (FramePacket encoded)
  | otherwise = Left NonFiniteEncoding
  where
    finite x = not (isNaN x || isInfinite x)
    encoded = V.fromList (map realToFrac (frameValues frame))

frameValues :: FramePlan -> [Double]
frameValues frame = values
  where
    g = geometry frame
    ts = targets frame
    sun = sunDirection frame
    clip = clipFromRender
    values =
      [1, fromIntegral (rasterWidth ts), fromIntegral (rasterHeight ts),
       fromIntegral (shadowResolution ts), fromIntegral (reflectionWidth ts),
       fromIntegral (reflectionHeight ts), semanticTime g]
      ++ pointList (renderOrigin g) ++ pointList (referenceEye g) ++ directionList sun
      ++ matrixColumns (carTransform frame)
      ++ matrixColumns (clip (shadowPass frame))
      ++ matrixColumns (clip (reflectionPass frame))
      -- World lengths, not unexplained normalized-depth constants.
      ++ [shadowTexel frame, shadowDepthSpan, shadowBaseBias, shadowSlopeBias]
      ++ concatMap observation [mainPass frame, shadowPass frame, reflectionPass frame]
      ++ matrixColumns (clip (mainPass frame))
    observation pr = matrixColumns (viewMatrix pr) ++ matrixColumns (projectionMatrix pr)
      ++ pointList (passEye pr) ++ directionList (passForward pr) ++ [0,1,0]
