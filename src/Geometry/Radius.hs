module Geometry.Radius(doubleCylinderZip, doubleCylinder, squaredOff, calcultateDistance) where



import CornerPoints.Create(Angle(..), getQuadrantAngle)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..) )

import Math.Trigonometry(sinDegrees, cosDegrees)

{- |
Manipulate Radius using various geometric formulas, in order to change the resulting CornerPoints.

Examples: Examples.Primitives.ComposableExample

Test in test/GeometryRadiusTest
-}

{- |
 Use cosDegrees/sinDegrees to create a double cylinder from a Radius Angle
 that would normally create a single cylinder.
 

[Radius:                                               [ Radius
 -to be adjusted                                           -adjusted for double circular pattern.
 Angle:                                                 -
 -current angle                                         
]--------------------adjust radius                 --->]
-}
doubleCylinder :: Radius -> Angle -> Radius
doubleCylinder (Radius radius') angle' =
  Radius $ ((cosDegrees (angle $ getQuadrantAngle angle')) * radius')



{- |
 Use doubleCylinder on a [Radius] [Angle] via zipWith

[[Radius]:                                               [ [Radius]
 -to be adjusted                                           -adjusted for double circular pattern.
 [Angles]:                                                 -
 -angles                                         
]--------------------zipWith doubleCylinder  ------- --->]
-}
--ToDo: create a separate version for cos/sinDegrees which are 90 degrees opposed to each other.
doubleCylinderZip :: [Radius] -> [Angle] -> [Radius]
doubleCylinderZip    radii       angles      =
      zipWith
        (doubleCylinder)
        radii
        angles


{- |
 Create a rectangle with rounded corners, by manipulating Angle Radius.
 

[power:                                    [current x                     [Radius:
 -how strongly to square                    current y                      -squared off
 radius:                                    radius
 -to be adjusted                            angle
 angle':
 current angle
                   ]--------------caluclate x/y-->  ]-------------------adust radius-->]
-}                               
type Power = Double
squaredOff :: Power -> Radius -> Angle -> Radius
squaredOff power (Radius radius') angle' =
  let quadrantAngle = (angle $ getQuadrantAngle angle')
      --gaurd against 0 as it creates stl with nan.
      x' = (sinDegrees quadrantAngle) * radius'
      x = case (x' == 0) of
           True -> 0.01
           False -> x'
           
      y' = (cosDegrees quadrantAngle) *  radius'
      y = case (y' == 0) of
           True -> 0.01
           False -> y'
  in  Radius $
      (radius'**2)/
        (((x**power) + (y**power))**(1/power)) 

-- | Given a 2 points, caluclate the distance between the points.
calcultateDistance :: Point -> Point -> Radius
calcultateDistance    point1   point2  =
  let
      distance :: Point -> Point -> Point
      distance    (Point x y z)    (Point x1 y1 z1) =
        Point (abs $ x - x1) (abs $ y - y1) (abs $ z - z1)
      p = distance point1 point2
      x = x_axis p
      y = y_axis p
      z = z_axis p
  in
      Radius $ sqrt (x**2 + y**2 + z**2) 
  


