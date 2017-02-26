{-# LANGUAGE ParallelListComp #-}
{- |
Creates a radial shape using polar cood's.

 degrees start at the negative y axis, and rotates clockwise into the x axis.


-}



module CornerPoints.Create(
  adjustRadiusForSlope,
  Origin(..),
  createCornerPointSquaredOff,
  AngleRadius(..),
  extractAngles,
  extractRadii,
  ) where

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), )
import CornerPoints.Transpose (transposeZ)
import CornerPoints.Radius(Radius(..))

import Geometry.CornerPoints(squaredOffAdjustmentFunction)
--import  Geometry.Radius(calcultateDistance)
import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

import Math.Trigonometry(sinDegrees, cosDegrees, coTanDegrees)

type Power = Double





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

--ToDo: Figure out a more logical place/module to put the AngleRadius functionality.

{- |
Combine an Angle and a Radius in a single datatype.
Makes it easier to manipulate them, as opposed to when they are in separate lists.
Afterwards the [Angle] [Radius] can be extracted using extract<Angle><Radius> functions,
so that they can be used in the normal manner to generate [CornerPoints] through typical fucntions
such as createBottomFaces or cylinder.

Known uses:
Expand shorter lists of [Radius] [Angle] so they can be matched up with a longer set of lists.
For instance, adding the radii from an octagon, to the 36 angles from a typical scan, would
require using filler values in the octagon list.
An example of this is in: Examples.Scan.OpenBionicsDotComDesignWork
-}
data AngleRadius = AngleRadius
                       { posDegree :: Angle,
                         posRadius :: Radius
                       }

instance Show AngleRadius where
  show (AngleRadius posDegree' posRadius') = (show posDegree') ++ " : " ++ (show posRadius')

{- | Extract the [Angle] from a [AngleRadius]-}
extractAngles :: [AngleRadius] -> [Angle]
extractAngles anglesRadii =
  extractAngles' (reverse anglesRadii) []

extractAngles' :: [AngleRadius] -> [Angle] -> [Angle]
extractAngles' (x:[]) angles =
  posDegree x : angles

extractAngles' (x:xs) angles =
  extractAngles' xs (posDegree x : angles)

{- | Extract the [Radius] from a [AngleRadius]-}
extractRadii :: [AngleRadius] -> [Radius]
extractRadii anglesRadii =
  extractRadii' (reverse anglesRadii) []

extractRadii' :: [AngleRadius] -> [Radius] -> [Radius]
extractRadii'(x:[]) radii =
  posRadius x : radii

extractRadii' (x:xs) radii =
  extractRadii' xs (posRadius x : radii)


{- |
Add a slope to a CornerPoionts
need to be able to figure out the radius.

-}
{-
addSlope :: Slope -> Slope -> Angle -> Point -> CornerPoints -> CornerPoints
addSlope    xSlope   ySlope   xyAngle  origin   cpoint =
  let 
    currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope (xyAngle)

    quadrantAngle = (angle $ getQuadrantAngle (xyAngle))

    extractPoint :: CornerPoints -> Point
    extractPoint (F1 p) = p
    extractPoint (F2 p) = p
    extractPoint (F3 p) = p
    extractPoint (F4 p) = p
    
                                                    
    xyRadius = calcultateDistance origin $ extractPoint cpoint

    adjustedRadius = radius (adjustRadiusForSlope (xyRadius) currentSlope)
    
    xAdjusted =
       let length  = (sinDegrees quadrantAngle) * adjustedRadius
           x_axis' = x_axis (origin)
       in  
                          
           case getQuadrantAngle (xyAngle) of
             (Quadrant1Angle _) -> x_axis' + length  
             (Quadrant2Angle _) -> x_axis' + length  
             (Quadrant3Angle _) -> x_axis' - length 
             (Quadrant4Angle _) -> x_axis' - length 
                                 
    yAdjusted =
       let length = (cosDegrees quadrantAngle) *  adjustedRadius
           y_axis' = y_axis (origin)
       in
               
          case getQuadrantAngle (xyAngle) of
            (Quadrant1Angle _) -> y_axis' - length  
            (Quadrant2Angle _) -> y_axis' + length  
            (Quadrant3Angle _) -> y_axis' + length  
            (Quadrant4Angle _) -> y_axis' - length 
                                 
                                 
    zAdjusted =
       let length = (radius xyRadius) * (sinDegrees (slope currentSlope))
           z_axis' = z_axis (origin)
       in
          case currentSlope of
            (PosSlope _) -> z_axis' +  length
            (NegSlope _)  -> z_axis' - length
                                 
                                 
                                                
                                   
                                 
  in       
    --composer {_cpoint = (cpointSetter (cpoint) (Point xAdjusted yAdjusted zAdjusted)), _xyRadius = (Radius adjustedRadius) }
    cpointSetter cpoint (Point xAdjusted yAdjusted zAdjusted)



--used in createCornerPointComposableSloped to get the CornerPoints constructor
cpointSetter :: CornerPoints -> Point -> CornerPoints
cpointSetter (F1 f) point' = F1 point'
cpointSetter (F2 f) point' = F2 point'
cpointSetter (F3 f) point' = F3 point'
cpointSetter (F4 f) point' = F4 point'
cpointSetter _ _ = CornerPointsError "cpointSetter missing pattern match"

-}
