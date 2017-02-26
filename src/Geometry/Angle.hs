module Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..)) where

{-
There are 4 quadrants to work with therfore the Quadarant1/2/3/4Angle
They should be eliminated later, when I use quadrant correcting as per trig rules for obtuse angles.

Angle:
This will be the replacement for all of the others. It is simply a wrapper around Double.
Once the others are gone, should make it a newtype, for efficiency.
-}
--ToDo: Create a Angle module and move this there.
data Angle =         Quadrant1Angle  { angle::Double}
                   | Quadrant2Angle  { angle::Double}
                   | Quadrant3Angle  { angle::Double}
                   | Quadrant4Angle  { angle::Double}
                   | Angle           { angle::Double}
                   
  deriving (Show, Eq)

type RotateFactor = Double
{- |
Rotate the angle forward or backwards by amount of rotateFactor.
Neg value will rotate back, pos will rotate forward.
Keep the value between 0 and 360
-}
--ToDo: Create a Angle module and move this there.
rotateAngle :: RotateFactor -> Angle -> Angle
rotateAngle rotateFactor (Angle angle') =
  let rotated = angle' + rotateFactor
  in  case (rotated < 0) of
        True -> --Angle $ rotated + 360
                rotateAngle rotateFactor (Angle $ angle' + 360) 
        False -> case (rotated > 360) of
                   True -> --Angle $ rotated - 360
                           rotateAngle rotateFactor (Angle $ angle' - 360)
                   False -> Angle rotated



{-
Each quadrant will be 90 degrees with Quadrant 1 being 0-90 degrees.
The resulting angle is such that, for all quadrants:
 -use sin to calculate x-axis
 -use cos to calculate y-axis
-}
getQuadrantAngle :: Angle ->  Angle
getQuadrantAngle (Angle currAngle )
  | currAngle < 0 = getQuadrantAngle (Angle (360 - currAngle))
  | currAngle <= 90 = Quadrant1Angle currAngle
  | currAngle <= 180 = Quadrant2Angle (180 - currAngle)
  | currAngle <= 270 = Quadrant3Angle $ currAngle - 180   -- 90 - (270 - currAngle)
  | currAngle <= 360 = Quadrant4Angle (360 - currAngle)
  | currAngle > 360 = getQuadrantAngle (Angle(currAngle - 360))

