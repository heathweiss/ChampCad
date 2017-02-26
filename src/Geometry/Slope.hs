module Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle) where

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))

import Math.Trigonometry(sinDegrees, cosDegrees, coTanDegrees)
--ToDo: Tests for this are probably in somewhere like cornerPoints.create tests

{-
Used for:
Neg/Pos XY Slope: 
-Describe the slope resulting from the combining of the Neg/Pos X/Y slopes
-Neg slopes downwards from the neg x/y quadrants
-Pos slopes upwards from the neg x/y quadrants

The NegYSlope and PosYSlope describe a slope(change of z axis) along the y axis.
NegYSlope:
-in quadrants 1 & 4, which are the -y quads, slope upwards from the origin to the front faces.
-in quads 2 & 3, the + y quads, slope downwards from the origin to the front faces.

PosYSlope:
-in quads 1 & 4 , the - y quads, slope from the origin, downwards to the front faces.
-in quads 2 & 3, the  + y quads, slope from the origin, updwards to the front faces.

The NegXSlope and PosXSlope describe a slope(change of z axis) along the x axis.
NegXSlope
-in quads 1 & 2, the + x quads, slope from the origin, downwards to the front faces.
-in quads 3 & 4, the - x quads, slope from the origin, upwards to the front faces.

PosXSlope
-in quads 1 & 2, the + x quads, slope from the origin, upwards to the front faces.
-in quads 3 & 4, the - x quads, slope from the origin, downwards to the front faces.
-}
data Slope = NegSlope {slope :: Double}
           | PosSlope {slope :: Double}
           | NegXSlope {slope :: Double}
           | PosXSlope {slope :: Double}
           | NegYSlope {slope :: Double}
           | PosYSlope {slope :: Double}
           
  deriving (Show, Eq)

-- |Many shapes, if not most, do not have sloped tops, therefore it is handy
-- to have easily callable no-slope values.
flatXSlope = PosXSlope 0 
flatYSlope = PosYSlope 0


{-
Functions for calculating the current Z plane angle. It is a combination of the slope,
and the current XY angle. It has to be calculated because the slope changes as you work around the center.
This change in slope is because cubes radiate from the center, so their endpoints will be at various slopes from the center.
When all done, they should all run together at the target slope.

It is a wrapper around slopeAdjustedForVerticalAngleBase, so that the calculation of the trigAngle only
has to be written out once, instead of in each function.
-}
slopeAdjustedForVerticalAngle :: Slope -> Slope -> Angle -> Slope
slopeAdjustedForVerticalAngle xSlope ySlope verticalAngle =
  slopeAdjustedForVerticalAngleBase xSlope ySlope (getQuadrantAngle verticalAngle)

slopeAdjustedForVerticalAngleBase :: Slope -> Slope -> Angle -> Slope
slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant1Angle verticalAngle)
  | ((sinDegrees verticalAngle) * xSlope) >= ((cosDegrees verticalAngle) * ySlope)  =
      PosSlope $ abs $ ((sinDegrees verticalAngle) * xSlope) - ((cosDegrees verticalAngle) * ySlope)
  | otherwise = NegSlope $ abs $ ((sinDegrees verticalAngle) * xSlope) - ((cosDegrees verticalAngle) * ySlope)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant1Angle verticalAngle) =
  PosSlope $ ((sinDegrees verticalAngle) * xSlope) + ((cosDegrees verticalAngle) * ySlope)

slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant1Angle verticalAngle) =
   NegSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant1Angle verticalAngle)
  | (getYSlope ySlope verticalAngle) >= (getXSlope xSlope verticalAngle) = PosSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant2Angle verticalAngle) =
  PosSlope $(getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant2Angle verticalAngle)
  | (getXSlope xSlope verticalAngle) >= (getYSlope ySlope verticalAngle)  =
      PosSlope $ abs $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)
  | otherwise = NegSlope $ abs $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant2Angle verticalAngle)
  | (getYSlope ySlope verticalAngle) <= (getXSlope xSlope verticalAngle) = PosSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant2Angle verticalAngle) =
  NegSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant3Angle verticalAngle)
  | (getYSlope ySlope verticalAngle)  >= (getXSlope xSlope verticalAngle) =
      PosSlope $ abs $  (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ abs $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle) 

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant3Angle verticalAngle) =
  NegSlope $ ((sinDegrees verticalAngle) * xSlope) + ((cosDegrees verticalAngle) * ySlope)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant3Angle verticalAngle)  =
  PosSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant3Angle verticalAngle)
  | (getXSlope xSlope verticalAngle) >= (getYSlope ySlope verticalAngle) = PosSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)
  | otherwise = NegSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant4Angle verticalAngle) =
  NegSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant4Angle verticalAngle)
  | (getYSlope ySlope verticalAngle)  >= (getXSlope xSlope verticalAngle) =
      PosSlope $ abs $  (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ abs $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant4Angle verticalAngle)
  | (getXSlope xSlope verticalAngle) >= (getYSlope ySlope verticalAngle) = PosSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)
  | otherwise = NegSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant4Angle verticalAngle) =
  PosSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

--Used to keep slopeAdjustedForVerticalAngleBase DRY.
getXSlope xSlope' verticalAngle' = ((sinDegrees verticalAngle') * xSlope')
getYSlope ySlope' verticalAngle' = ((cosDegrees verticalAngle') * ySlope')

