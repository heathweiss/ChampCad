{-# LANGUAGE ParallelListComp #-}
{- |
Creates a radial shape using polar cood's.

 degrees start at the negative y axis, and rotates clockwise into the x axis.


-}



module CornerPoints.Create(
  adjustRadiusForSlope,
  Origin(..),
  createCornerPointSquaredOff,
  createCornerPoint
  ) where

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), )
import CornerPoints.Transpose (transposeZ)
import CornerPoints.Radius(Radius(..))

import Geometry.CornerPoints(squaredOffAdjustmentFunction)
--import  Geometry.Radius(calcultateDistance)
import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)
import Geometry.Radius(calcultateDistance)

import Math.Trigonometry(sinDegrees, cosDegrees, coTanDegrees)

type Power = Double


{-
Create a CornerPoint from raw values, including a CornerPoints constructor.
Differs from CornerPoints.Create.createCornerPoint in that it does not use Slope.
This should eventually replace CornerPoints.Create.createCornerPoint.
-}
--ToDo: Replace CornerPoints.Create.createCornerPoint with this version so Slope is no longer required.
createCornerPoint :: (Point-> CornerPoints) -> Origin -> Radius ->  Angle -> CornerPoints
createCornerPoint cPoint origin horizRadius verticalAngle   =
                             let 
                                 --currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope verticalAngle
                                                                                      
                                 --radiusAdjustedForSlope = radius (adjustRadiusForSlope horizRadius currentSlope)

                                 baseOfAngle = (angle $ getQuadrantAngle verticalAngle)
                                 sinOfVerticalAngle = sinDegrees baseOfAngle
                                 cosOfVerticalAngle = cosDegrees baseOfAngle
                                 
                                 setXaxis =
                                   --let length = radiusAdjustedForSlope * sinOfVerticalAngle
                                   let length = (radius horizRadius) * sinOfVerticalAngle
                                       x_axis' = x_axis origin
                                   in
                                      
                                    case getQuadrantAngle verticalAngle of
                                      (Quadrant1Angle _) -> x_axis' + length
                                      (Quadrant2Angle _) -> x_axis' + length
                                      (Quadrant3Angle _) -> x_axis' - length
                                      (Quadrant4Angle _) -> x_axis' - length

                                 
                                 setYaxis =
                                   --let length = radiusAdjustedForSlope * cosOfVerticalAngle
                                   let length = (radius horizRadius) * cosOfVerticalAngle
                                       y_axis' = y_axis origin
                                   in
                                     
                                    case getQuadrantAngle verticalAngle of
                                      (Quadrant1Angle _) -> y_axis' - length
                                      (Quadrant2Angle _) -> y_axis' + length
                                      (Quadrant3Angle _) -> y_axis' + length
                                      (Quadrant4Angle _) -> y_axis' - length
                                   


                                 setZaxis = z_axis origin
                                 {-
                                 setZaxis'' = 
                                   let -- length = (radius horizRadius) * (sinDegrees (slope currentSlope))
                                       z_axis' = z_axis origin
                                   in
                                    case currentSlope of
                                     --(PosSlope _) -> z_axis' +  length
                                     (PosSlope _) -> z_axis' +  horizRadius
                                     --(NegSlope _)  -> z_axis' - length
                                     (NegSlope _)  -> z_axis' - horizRadius
                                 
                                 -}
                             in       
                                 cPoint (Point setXaxis setYaxis setZaxis )



createCornerPointSquaredOff :: (Point-> CornerPoints) -> Origin -> Radius ->  Angle -> Slope -> Slope -> Power -> CornerPoints
createCornerPointSquaredOff cPoint origin horizRadius verticalAngle xSlope ySlope power =
  createCornerPointWithSlopeAndXYAdjustmentFx (squaredOffAdjustmentFunction power) cPoint origin horizRadius verticalAngle xSlope ySlope {-power-}

{-
Creates a cornerpoint that is adjusted for slope and for an xy plane adjustment factor, such as being squared off..

Given: 
adjuster
A passed in fx that will modify the x/y values according to logic contained within that function.
This function takes the current radius, which has been adjusted for slope, the current x/y values.

---------------------createCornerPointWithSlopeAndXYAdjustmentFx----
The original call and it's parameters.
[(Double -> Double -> Double -> Double ) (adjuster)
 (Point-> CornerPoints)                  (cornerPointsConstructor)
 Origin                                  (origin)
 Radius                                  (horizRadius)
 Angle                                   (radialAngle)
 Slope                                   (xSlope)
 Slope ]                                 (ySlope)
 ----------------------------------slopeAdjustedForVerticalAngle (xSlope ySlope radialAngle)---------->
Now start listing the internal state, without the orignal parameters.
[currentSlope]
------------------------------radiusAdjustedForSlope(adjustRadiusForSlope horizRadius currentSlope)------------.
[currentSlope
 radiusAdjustedForSlope ]
-------------------------------getQuadrantAngle( radialAngle)------------------------------->
[currentSlope
 radiusAdjustedForSlope 
 quadrantAngle]
--------------------------------------adjuster (radiusAdjustedForSlope getX getY )-------------->
[currentSlope
 radiusAdjustedForSlope 
 quadrantAngle
 adjustedRadius]
----------------------------adjust x/y for adjustedRadius------------------>
[currentSlope
 radiusAdjustedForSlope 
 quadrantAngle
 adjustedRadius
 xAdjusted,
 yAdjusted]
--------------------------------- adjust z-axis for slope------------------------->
[currentSlope
 radiusAdjustedForSlope 
 quadrantAngle
 adjustedRadius
 xAdjusted
 yAdjusted
 setZaxis]
----------------------------------------cornerPointConstructor---------------->
[Point x y z]

-}
createCornerPointWithSlopeAndXYAdjustmentFx :: (Double -> Double -> Double -> Double ) ->
                                   (Point-> CornerPoints) -> Origin -> Radius ->  Angle -> Slope -> Slope ->  CornerPoints
createCornerPointWithSlopeAndXYAdjustmentFx    radiusAdjuster cornerPointConstructor origin horizRadius radialAngle xSlope ySlope   =
                             let 
                                 currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope radialAngle
                                                                                      
                                 radiusAdjustedForSlope = radius (adjustRadiusForSlope horizRadius currentSlope)

                                 quadrantAngle = (angle $ getQuadrantAngle radialAngle)
                                 
                                 --used to adjust the current point x/y values, according to the logic of the passed in
                                 --adjuster function fx. Eg: a function that squares off the shape.
                                 --rename to adjustedRadius
                                 adjustedRadius = radiusAdjuster
                                                   radiusAdjustedForSlope
                                                   ((sinDegrees quadrantAngle) * radiusAdjustedForSlope) --getX
                                                   ((cosDegrees quadrantAngle) * radiusAdjustedForSlope)--getY
                                 
                                 
                                 xAdjusted =
                                   let length  = (sinDegrees quadrantAngle) * adjustedRadius
                                       x_axis' = x_axis origin
                                   in
                                      
                                    case getQuadrantAngle radialAngle of
                                      (Quadrant1Angle _) -> x_axis' + length  
                                      (Quadrant2Angle _) -> x_axis' + length  
                                      (Quadrant3Angle _) -> x_axis' - length 
                                      (Quadrant4Angle _) -> x_axis' - length 
                                 
                                 yAdjusted =
                                   let length = (cosDegrees quadrantAngle) *  adjustedRadius
                                       y_axis' = y_axis origin
                                   in
                                     
                                    case getQuadrantAngle radialAngle of
                                      (Quadrant1Angle _) -> y_axis' - length  
                                      (Quadrant2Angle _) -> y_axis' + length  
                                      (Quadrant3Angle _) -> y_axis' + length  
                                      (Quadrant4Angle _) -> y_axis' - length 
                                 
                                 
                                 zAdjusted =
                                   let length = (radius horizRadius) * (sinDegrees (slope currentSlope))
                                       z_axis' = z_axis origin
                                   in
                                    case currentSlope of
                                     (PosSlope _) -> z_axis' +  length
                                     (NegSlope _)  -> z_axis' - length
                                 
                                 
                                                
                                   
                                 
                             in       
                                 cornerPointConstructor (Point xAdjusted yAdjusted zAdjusted)





  
-- |The center of a radial shape.
type Origin = Point


           
-- length of an axis.
--A 3D cartesian point is the result of x,y,z AxisLengths from the origin
type AxisLength = Double



{-
Shorten the radius on the xy plane, for the changes in the z plane.
As per standard 3D polar to cartesian conversion methods.
-}
adjustRadiusForSlope :: Radius -> Slope -> Radius 
adjustRadiusForSlope (Radius radius) xySlope = Radius $ radius * (cosDegrees (slope xySlope))

