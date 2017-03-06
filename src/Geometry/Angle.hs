module Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), getXYAngle, getQuadrant, Quadrant(..)) where

import CornerPoints.Points(Point(..))
import Math.Trigonometry(tanDegrees, atanDegrees)

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
                   -- | An angle which has not been adjusted for quadrant.
                   
  deriving (Show, Eq)

-- | Which quadrant of the xy plane.
data Quadrant =   Quadrant1
                | Quadrant2
                | Quadrant3
                | Quadrant4
  deriving (Eq, Show)

type RotateFactor = Double
{- |
Rotate the angle forward or backwards by amount of rotateFactor.
Neg value will rotate back, pos will rotate forward.
Keep the value between 0 and 360
-}
{-
[rotateFactor
 Angle
]
|
|rotate the angle
|ensure angle >= 0 and angel <= 360
|
[rotatedAngle
]
-}
rotateAngle :: RotateFactor -> Angle -> Angle
rotateAngle rotateFactor (Angle angle') =
  let rotated = angle' + rotateFactor
  in  case (rotated < 0) of
        True -> rotateAngle rotateFactor (Angle $ angle' + 360) 
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


--get the quadrant that an angle is in
getQuadrant :: Angle -> Quadrant
getQuadrant (Angle angle')
  | angle' < 0 = getQuadrant $ Angle $ angle' + 360
  | angle'  <= 90 = Quadrant1
  | angle' <= 180 = Quadrant2
  | angle' <= 270 = Quadrant3
  | angle' <= 360 = Quadrant4
  | otherwise = getQuadrant $ Angle $ angle' - 360


getQuadrant (Quadrant1Angle _) = Quadrant1
getQuadrant (Quadrant2Angle _) = Quadrant2
getQuadrant (Quadrant3Angle _) = Quadrant3
getQuadrant (Quadrant4Angle _) = Quadrant4

-- | Calculate the current xy angle of a Point in reference to another point.
getXYAngle :: Point -> Point -> Angle
getXYAngle (Point x_origin y_origin z_origin) (Point x y z) 
 
   | (x_axis centeredPoint) >= 0 =  -- has a positive X, therefore quad1/2
       case (y_axis centeredPoint) <= 0 of
         --is quad1
         True -> Angle (atanDegrees  (x_axis centeredPoint) (abs $ y_axis centeredPoint))
                 
         --is quad2
         False ->  --Angle $ 90 +  (atanDegrees (y_axis centeredPoint) (x_axis centeredPoint))
                   Angle $ 180 -  (atanDegrees  (x_axis centeredPoint) (y_axis centeredPoint))                 
   | otherwise = --has neg X therefore quad3/4
        case (y_axis centeredPoint) >= 0 of
          --quad3
          True -> Angle $ 180 +  (atanDegrees  (abs $ x_axis centeredPoint) (y_axis centeredPoint))
          --quad4
          False -> --Angle $ 270 +  (atanDegrees  (abs $ x_axis centeredPoint) (abs $ y_axis centeredPoint))
                   Angle $ 360 - (atanDegrees  (abs $ x_axis centeredPoint) (abs $ y_axis centeredPoint))
   where
     centeredPoint = Point
                       (x - x_origin)
                       (y - y_origin)
                       (z - z_origin)


{-orig before testing
getXYAngle :: Point -> Point -> Angle
getXYAngle (Point x_origin y_origin z_origin) (Point x y z) 
 
   | (x_axis centeredPoint) > 0 =  
       case (y_axis centeredPoint) < 0 of
         True -> Angle (atanDegrees (abs $ y_axis centeredPoint) (x_axis centeredPoint))
         False ->  Angle $ 90 +  (atanDegrees (y_axis centeredPoint) (x_axis centeredPoint))                 
   | otherwise =
        case (y_axis centeredPoint) >= 0 of
          True -> Angle $ 180 +  (atanDegrees (y_axis centeredPoint) (abs $ x_axis centeredPoint))
          False -> Angle $ 270 +  (atanDegrees (abs $ y_axis centeredPoint) (abs $ x_axis centeredPoint))
   where
     centeredPoint = Point
                       (x - x_origin)
                       (y - y_origin)
                       (z - z_origin)
-}
