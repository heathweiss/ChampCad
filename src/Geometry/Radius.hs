module Geometry.Radius(doubleCylinder) where

import CornerPoints.Create(Angle(..), getQuadrantAngle)
import CornerPoints.Radius(Radius(..))

import Math.Trigonometry(sinDegrees, cosDegrees)
{- |
 Use cosDegrees/sinDegrees to create a double cylinder from a single [Radius] [Angle] pair.
-}
--ToDo: create a separate version for cos/sinDegrees which are 90 degrees opposed to each other.
doubleCylinder :: [Radius] -> [Angle] -> [Radius]
doubleCylinder    radii       angles      =
  --map (squaredOffAdjustmentFunction' power) radii
  zipWith
    (doubleCylinderAdjuster )
    radii
    (map getQuadrantAngle angles)

doubleCylinderAdjuster :: Radius -> Angle -> Radius
doubleCylinderAdjuster    (Radius radius') angle'  =
                                            Radius $ 
                                               ((cosDegrees (angle angle')) * radius')
                                               
