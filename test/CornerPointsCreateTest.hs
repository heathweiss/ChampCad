module CornerPointsCreateTest(cornerPointsCreateTestDo ) where

import Test.HUnit

import  CornerPoints.Create(
  adjustRadiusForSlope,
  Origin(..),
  createCornerPointSquaredOff,
  )
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@))
import CornerPoints.Points (Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.Create(createCornerPoint)

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

import Math.Trigonometry(sinDegrees, cosDegrees)



cornerPointsCreateTestDo = do
  putStrLn ""
  putStrLn "CornerPointsCreateTest"

  runTestTT adjustRadiusForSlopeTest
  
  runTestTT getXTest
  runTestTT getYTest
  runTestTT getZTest
  runTestTT createCornerPointSquaredOffTest
  runTestTT rotateForwardWithinBounds
  runTestTT rotateForwardBeyondBounds
  runTestTT rotateForwardToBounds
  runTestTT rotateBackWithinBounds
  runTestTT rotateBackBeyondBounds
  runTestTT rotateBackToBounds
  runTestTT rotateForwardTwiceBeyondBounds
  runTestTT rotateBackTwiceBeyondBounds
  runTestTT createCornerPointTest
  
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


rotateForwardWithinBounds = TestCase $ assertEqual
  "rotateForwardWithinBounds"
  (Angle 10)
  (rotateAngle (10 :: RotateFactor) (Angle 0))

rotateForwardBeyondBounds = TestCase $ assertEqual
  "rotateForwardBeyondBounds"
  (Angle 1)
  (rotateAngle (10 :: RotateFactor) (Angle 351))

rotateForwardToBounds = TestCase $ assertEqual
  "rotateForwardToBounds"
  (Angle 360)
  (rotateAngle (10 :: RotateFactor) (Angle 350))

rotateForwardTwiceBeyondBounds = TestCase $ assertEqual
  "rotateForwardTwiceBeyondBounds"
  (Angle 350)
  (rotateAngle (720 :: RotateFactor) (Angle 350))  

rotateBackWithinBounds = TestCase $ assertEqual
  "rotateBackWithinBounds"
  (Angle 10)
  (rotateAngle ((-10) :: RotateFactor) (Angle 20))

rotateBackBeyondBounds = TestCase $ assertEqual
  "rotateBackBeyondBounds"
  (Angle 350)
  (rotateAngle ((-20) :: RotateFactor) (Angle 10))

rotateBackTwiceBeyondBounds = TestCase $ assertEqual
  "rotateBackTwiceBeyondBounds"
  (Angle 0)
  (rotateAngle ((-720) :: RotateFactor) (Angle 0))

rotateBackToBounds = TestCase $ assertEqual
  "rotateBackToBounds"
  (Angle 0)
  (rotateAngle ((-10) :: RotateFactor) (Angle 10))

createCornerPointTest = TestCase $ assertEqual
  "createCornerPointTest"
  ([F1 (Point 0 (-10) 0),
    F1 (Point 0 (10) 0)
   ])
  ([createCornerPoint (F1) (Point 0 0 0) (Radius 10) (Angle 0),
    createCornerPoint (F1) (Point 0 0 0) (Radius 10) (Angle 180)
   ]
  )
