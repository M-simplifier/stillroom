-- | A projected image and the map used to sample it are one value.
--
-- The target may be a pure image description or a backend-owned resource.
-- This module knows no particular world, frame schedule, pass names, or GPU
-- packet. Several observations can use one realized geometry description.
module Stillroom.Graphics.Observation
  ( Observation, ObservationError (..), observe, mapTarget,
    imageTarget, viewMatrix, projectionMatrix, clipFromRender, passEye, passForward,
  )
where

import Stillroom.Graphics.Space

data Observation target = Observation
  { observedImage :: !target,
    observedView :: !(Transform Render View),
    observedProjection :: !(Transform View Clip),
    observedClip :: !(Transform Render Clip),
    observedEye :: !(Point Render),
    observedForward :: !(Direction Render)
  }
  deriving (Eq, Show)

data ObservationError = InvalidObservation deriving (Eq, Show)

-- | Construct the writing and sampling map once. The observer cannot replace
-- a geometry-selection view or its clock: neither is part of this interface.
observe ::
  target -> Point Render -> Direction Render -> Transform View Clip ->
  Either ObservationError (Observation target)
observe image eye forward projection
  | all finite (matrixColumns view ++ matrixColumns projection ++ matrixColumns clip) =
      Right (Observation image view projection clip eye forward)
  | otherwise = Left InvalidObservation
  where
    view = look eye forward
    clip = compose projection view
    finite x = not (isNaN x || isInfinite x)

-- | Change a target's representation without rebuilding its projection.
-- Identity and composition follow the ordinary functor laws.
mapTarget :: (a -> b) -> Observation a -> Observation b
mapTarget f (Observation image view projection clip eye forward) =
  Observation (f image) view projection clip eye forward


-- Plain query functions, deliberately not exported record selectors.
-- Exporting a selector would let a client use record update even while the
-- data constructor is hidden, breaking the image/projection correspondence.
imageTarget :: Observation target -> target
imageTarget = observedImage
viewMatrix :: Observation target -> Transform Render View
viewMatrix = observedView
projectionMatrix :: Observation target -> Transform View Clip
projectionMatrix = observedProjection
clipFromRender :: Observation target -> Transform Render Clip
clipFromRender = observedClip
passEye :: Observation target -> Point Render
passEye = observedEye
passForward :: Observation target -> Direction Render
passForward = observedForward
