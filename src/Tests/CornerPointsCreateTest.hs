module Tests.CornerPointsCreateTest(cornerPointsCreateTestDo ) where
import Test.HUnit
import  CornerPoints.Create(
  slopeAdjustedForVerticalAngle,
  adjustRadiusForSlope,
  createCornerPoint,
  Slope(..),
  flatXSlope,
  flatYSlope,
  Origin(..),
  Angle(..),
  createCornerPointSquaredOff,
  getQuadrantAngle
  )
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@))
import CornerPoints.Points (Point(..))
import CornerPoints.Radius(Radius(..))
import Math.Trigonometry(sinDegrees, cosDegrees)
import Primitives.Cylindrical(cylinderWallsNoSlope)



cornerPointsCreateTestDo = do

  

  runTestTT adjustRadiusForSlopeTest
  
  runTestTT getXTest
  runTestTT getYTest
  runTestTT getZTest
  runTestTT createCornerPointSquaredOffTest
  
cubePoints = (BottomFace
              {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}})
              @+++#@
              ((transposeZ(+1)) . upperFaceFromLowerFace)

adjustRadiusForSlopeTest = TestCase $ assertEqual
  "adjustRadiusForSlope"
  (Radius 1)
  
  (
     (adjustRadiusForSlope (Radius 1) (PosSlope 0))
  )

getXTest = TestCase $ assertEqual
  "getXTest"
  (1.745240643728351e-2)
  ( let degrees = (sinDegrees  (angle $ getQuadrantAngle (Angle 1)) )
        radius' = (radius $ adjustRadiusForSlope (Radius 1) (PosSlope 0))
    in  degrees  * radius'
  )

getYTest = TestCase $ assertEqual
  "getYTest"
  (0.9998476951563913)
  ( let degrees = (cosDegrees  (angle $ getQuadrantAngle (Angle 1)) )
        radius' = (radius $ adjustRadiusForSlope (Radius 1) (PosSlope 0))
    in  degrees  * radius'
  )




getZTest = TestCase $ assertEqual
  "getZTest"
  (3.1058285412302484,11.591109915468818)
  ( let angle' = Angle 15
        slope = PosSlope 0
        radius' =  (radius $ adjustRadiusForSlope (Radius 12) slope)
        sinX = (sinDegrees  (angle $ getQuadrantAngle angle') )
        getX = sinX  * radius'
        cosY = (cosDegrees  (angle $ getQuadrantAngle angle') )
        getY = cosY  * radius'
        power = 2
        squaredRadius =(radius'**2)/(((getX**power) + (getY**power))**(1/power))
    in  ((sinX * squaredRadius),(cosY * squaredRadius))
  )


createCornerPointSquaredOffTest = TestCase $ assertEqual
  "createCornerPointImplicitTest"
  (F1 {f1 = Point {x_axis = 0.9330329915368074, y_axis = -0.9330329915368076, z_axis = 0.0}})
  (createCornerPointSquaredOff (F1) (Point 0 0 0) (Radius 1) (Angle 45) flatXSlope flatYSlope 10 )


