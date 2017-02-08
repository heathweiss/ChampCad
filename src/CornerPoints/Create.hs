{-# LANGUAGE ParallelListComp #-}
module CornerPoints.Create(
  slopeAdjustedForVerticalAngle,
  adjustRadiusForSlope,
  createCornerPoint,
  Slope(..),
  flatXSlope,
  flatYSlope,
  Origin(..),
  Angle(..),
  createCornerPointSquaredOff,
  getQuadrantAngle,
  AngleRadius(..),
  extractAngles,
  extractRadii
  ) where
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), )
import Math.Trigonometry(sinDegrees, cosDegrees)
import CornerPoints.Transpose (transposeZ)
import CornerPoints.Radius(Radius(..))

{--------------------overview----------------------------------------
Creates a radial shape using polar cood's.

 degrees is as the max neg y axis, and rotates clockwise into the pos x axis.


safari books: Triginometry 3rd edition is a good ref.
-}


{-
For trig xy calculations, the neg/pos aspect of y will depend upon the current quadrant of the xy plane


setYPolarityForQuadrant :: QuadrantAngle -> Double -> Double
setYPolarityForQuadrant angle val = case getCurrentQuadrant angle of
                                     Quadrant1 -> negate val
                                     Quadrant2 -> val
                                     Quadrant3 -> val
                                     Quadrant4 -> negate val
-}
        

{-
Change slope and xy angle into a single Slope value.
Change radius into a Radius

Given:
cPoint : (Point-> CornerPoints)
-A CornerPoints constructor such as F1

adjustedRadius: Radius: 
-The radius, after being adjusted for slope
-Should get rid of this, and do the calculations inside createCornerPoint.
 This would simplify calling this function.
 It was used before for pattern matching, but that is no longer done.
 However, will need to pass in xSlope and ySlope, instead of just the xySlope

slope: Slope:
The slope, having already been adjusted for x/y slopes and xy quadrant

orign: Point
-The starting point which gets adjusted to give the return point. That way, this point can be created relative to some position, instead of at an origin of 0 0 0

-}

type Power = Double


createCornerPoint :: (Point-> CornerPoints) -> Origin -> Radius ->  Angle -> Slope -> Slope -> CornerPoints
createCornerPoint cPoint origin horizRadius verticalAngle xSlope ySlope  =
                             let 
                                 currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope verticalAngle
                                                                                      
                                 radiusAdjustedForSlope = radius (adjustRadiusForSlope horizRadius currentSlope)

                                 baseOfAngle = (angle $ getQuadrantAngle verticalAngle)
                                 sinOfVerticalAngle = sinDegrees baseOfAngle
                                 cosOfVerticalAngle = cosDegrees baseOfAngle
                                 
                                 setXaxis =
                                   let length = radiusAdjustedForSlope * sinOfVerticalAngle
                                       x_axis' = x_axis origin
                                   in
                                      
                                    case getQuadrantAngle verticalAngle of
                                      (Quadrant1Angle _) -> x_axis' + length
                                      (Quadrant2Angle _) -> x_axis' + length
                                      (Quadrant3Angle _) -> x_axis' - length
                                      (Quadrant4Angle _) -> x_axis' - length

                                 
                                 setYaxis =
                                   let length = radiusAdjustedForSlope * cosOfVerticalAngle
                                       y_axis' = y_axis origin
                                   in
                                     
                                    case getQuadrantAngle verticalAngle of
                                      (Quadrant1Angle _) -> y_axis' - length
                                      (Quadrant2Angle _) -> y_axis' + length
                                      (Quadrant3Angle _) -> y_axis' + length
                                      (Quadrant4Angle _) -> y_axis' - length
                                   
                                 
                                 setZaxis =
                                   let length = (radius horizRadius) * (sinDegrees (slope currentSlope))
                                       z_axis' = z_axis origin
                                   in
                                    case currentSlope of
                                     (PosSlope _) -> z_axis' +  length
                                     (NegSlope _)  -> z_axis' - length
                                 
                                 
                             in       
                                 cPoint (Point setXaxis setYaxis setZaxis )


{- Base on the idea taken from Christopher Olah's blog
https://christopherolah.wordpress.com/2011/11/06/manipulation-of-implicit-functions-with-an-eye-on-cad/

However, the function has been modified to cause a cylindrical shape to take on a square shape on the xy plane.
The squareness of the shape depends on the power parameter.

Gets combined with createCornerPointWithSlopeAndXYAdjustmentFx via the squaredOffAdjustment function.

Making it the inverse of (radiusAdjustedForSlope**2) squared it off immensely,
as opposed to just making it x*n + y*n = r*n
Why does it work. What does inversing do.
Making it 1/ instead of (radiusAdjustedForSlope**2) squares it off, but the dimensions
are totally out. What does (radiusAdjustedForSlope**2)\ do compared to 1/
xyAdjustment = (radiusAdjustedForSlope**2)/ (((getX**power) + (getY**power)) **(1/power))
-}
squaredOffAdjustmentFunction :: Double -> Double ->               Double -> Double -> Double
squaredOffAdjustmentFunction    power'    radiusAdjustedForSlope' x         y          =
                                               (radiusAdjustedForSlope'**2)
                                                /
                                                (
                                                   (
                                                      (x**power') + (y**power')
                                                   )
                                                   **(1/power')
                                               )     

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


{---------------------------------------------------------------------------
e

Functions for calculating the current x and y values adjusted for xy degrees

-}

-- |Many shapes, if not most, do not have sloped tops, therefore it is handy
-- to have easily callable no-slope values.
flatXSlope = PosXSlope 0 
flatYSlope = PosYSlope 0

  
-- |The center of a radial shape.
type Origin = Point


 



{-
There are 4 quadrants to work with therfore the Quadarant1/2/3/4Angle
They should be eliminated later, when I use quadrant correcting as per trig rules for obtuse angles.

Angle:
This will be the replacement for all of the others. It is simply a wrapper around Double.
Once the others are gone, should make it a newtype, for efficiency.
-}
data Angle =         Quadrant1Angle  { angle::Double}
                   | Quadrant2Angle  { angle::Double}
                   | Quadrant3Angle  { angle::Double}
                   | Quadrant4Angle  { angle::Double}
                   | Angle           { angle::Double}
                   
  deriving (Show, Eq)


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

-- length of an axis.
--A 3D cartesian point is the result of x,y,z AxisLengths from the origin
type AxisLength = Double

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


{-
Shorten the radius on the xy plane, for the changes in the z plane.
As per standard 3D polar to cartesian conversion methods.
-}
adjustRadiusForSlope :: Radius -> Slope -> Radius 
--adjustRadiusForSlope (Radius radius) (PosSlope xySlope) = UpRadius $ radius * (cosDegrees (xySlope))
adjustRadiusForSlope (Radius radius) xySlope = Radius $ radius * (cosDegrees (slope xySlope))
{-
adjustRadiusForSlope :: Radius -> Slope -> Radius 
adjustRadiusForSlope (Radius radius) (PosSlope xySlope) = UpRadius $ radius * (cosDegrees (xySlope))
adjustRadiusForSlope (Radius radius) (NegSlope xySlope) = DownRadius $ radius * (cosDegrees (xySlope))
-}

  

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

