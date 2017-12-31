{- |
Datatypes and functions that are used by several Advancer modules.
-}
module Joiners.AdvanceSupport(Perimeters(..), AdvancingCPoint(..), justifyPerimeters, appendAdvancingCpointToJoinedCpointsE) where

import CornerPoints.CornerPoints(CornerPoints(..))

data Perimeters =
  OuterPerimeter {_outerPerimeter :: [CornerPoints]}
  |
  InnerPerimeters {_innerPerimeters :: [[CornerPoints]]}
  |
  InnerPerimeter {_innerPerimeter :: [CornerPoints]}
  deriving (Show, Eq)

--ToDo: Change this to a newtype for efficiency as it only has 1 constructor. But will that change.
data AdvancingCPoint =
 AdvancingCPoint {_advancingCpoint :: CornerPoints}
  deriving (Show, Eq)

justifyPerimeters :: Perimeters -> Maybe Perimeters
justifyPerimeters (OuterPerimeter outerPerimeter) =
      case (length outerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ OuterPerimeter outerPerimeter
justifyPerimeters (InnerPerimeter innerPerimeter) =
      case (length innerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ InnerPerimeter innerPerimeter
justifyPerimeters (InnerPerimeters innerPerimeters) =
      case (length innerPerimeters) == 0 of
        True -> Nothing
        False -> Just $ InnerPerimeters innerPerimeters

--version used in delaunayB
appendAdvancingCpointToJoinedCpointsE :: AdvancingCPoint -> [CornerPoints] -> [CornerPoints]
appendAdvancingCpointToJoinedCpointsE (AdvancingCPoint advancingCpoint) joinedCpoints = advancingCpoint : joinedCpoints
