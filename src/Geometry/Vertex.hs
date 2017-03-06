module Geometry.Vertex(getXWithQuadrant, getYWithQuadrant, Vertex(..), adjustPointAxis) where

import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))

import Geometry.Angle(getQuadrantAngle, Angle(..))

import Math.Trigonometry(sinDegrees, cosDegrees)

{- |
Vertex is defined as a quantity and direction.
Here the quantity is the length, broken up by each of the axis(x y z).
The direction is given by which Quandrant it is in.

Eg:
Point 1 2 3 would result in:
Quad1X = 1
Quad2Y = 2
Z: The Z axis has never had quadrants assigned to it, and so does not get defined here.
   Will that change as requirments dictate? Perhaps as the Geometry.Rotation module gets developed, that will be required.
-}
data Vertex =   Quad1X {length :: Double}
              | Quad2X {length :: Double}
              | Quad3X {length :: Double}
              | Quad4X {length :: Double}
              | Quad1Y {length :: Double}
              | Quad2Y {length :: Double}
              | Quad3Y {length :: Double}
              | Quad4Y {length :: Double}
  deriving (Eq, Show)
-- | The 3 axis associated with R3.
data Axis =   X
            | Y
            | Z


{- |
Get the length of the x-axis, along with which quadrant it is in.
The quadrant gives the direction.

Eg: Quad1X says it is in quadrant 1 so it is a positive value as the is in the positive x-axis quadrant.
    So if the Vertex is now added to a Point, it will increase the x-axis.
-}
--orig sinDegrees
xTrig = sinDegrees
getXWithQuadrant :: Angle -> Radius -> Vertex

getXWithQuadrant (Angle angle') (Radius radius') =
  case getQuadrantAngle (Angle angle') of
    Quadrant1Angle angle'' -> Quad1X $ (xTrig angle'') * radius'
    Quadrant2Angle angle'' -> Quad2X $ (xTrig angle'') * radius'
    Quadrant3Angle angle'' -> Quad3X $ (xTrig angle'') * radius'
    Quadrant4Angle angle'' -> Quad4X $ (xTrig angle'') * radius'

getXWithQuadrant quadrantAngle (Radius radius') =
  case quadrantAngle of
   Quadrant1Angle angle' -> Quad1X $ (xTrig angle') * radius'
   Quadrant2Angle angle' -> Quad2X $ (xTrig angle') * radius'
   Quadrant3Angle angle' -> Quad3X $ (xTrig angle') * radius'
   Quadrant4Angle angle' -> Quad4X $ (xTrig angle') * radius'
--orig: cosDegrees
--seems to have straightened out the RotationTest: rotateCornerPointAroundZAxisTest2
--Now all VertexTest is NFG. Need to go back and confirm those tests.
yTrig = cosDegrees
{- |
Get the length of the y-axis, along with which quadrant it is in.
The quadrant gives the direction.

Eg: Quad1Y says it is in quadrant 1 so it is a positive value as the is in the positive y-axis quadrant.
    So if the Vertex is now added to a Point, it will increase the y-axis.
-}
getYWithQuadrant :: Angle -> Radius -> Vertex

getYWithQuadrant (Angle angle') (Radius radius') =
  case getQuadrantAngle (Angle angle') of
    Quadrant1Angle angle'' -> Quad1Y $ (yTrig angle'') * radius'
    Quadrant2Angle angle'' -> Quad2Y $ (yTrig angle'') * radius'
    Quadrant3Angle angle'' -> Quad3Y $ (yTrig angle'') * radius'
    Quadrant4Angle angle'' -> Quad4Y $ (yTrig angle'') * radius'

getYWithQuadrant quadrantAngle (Radius radius') =
  case quadrantAngle of
   Quadrant1Angle angle' -> Quad1Y $ (yTrig angle') * radius'
   Quadrant2Angle angle' -> Quad2Y $ (yTrig angle') * radius'
   Quadrant3Angle angle' -> Quad3Y $ (yTrig angle') * radius'
   Quadrant4Angle angle' -> Quad4Y $ (yTrig angle') * radius'


{- |
Adjust a Point using a Vertex
Eg: Adusting a Point 1 2 3 with a Quad1Y 10 will decrease the y-axis resulting in Point 1 -8 3 
-}
adjustPointAxis :: Vertex -> Point -> Point
adjustPointAxis vertex point =
  case vertex of
    Quad1X x -> point {x_axis = (x_axis point) + x}
    Quad2X x -> point {x_axis = (x_axis point) + x}
    Quad3X x -> point {x_axis = (x_axis point) - x}
    Quad4X x -> point {x_axis = (x_axis point) - x}
    Quad1Y y -> point {y_axis = (y_axis point) - y}
    Quad2Y y -> point {y_axis = (y_axis point) + y}
    Quad3Y y -> point {y_axis = (y_axis point) + y}
    Quad4Y y -> point {y_axis = (y_axis point) - y}
