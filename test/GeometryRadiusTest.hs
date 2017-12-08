module GeometryRadiusTest (geometryRadiusTestDo) where

import  Geometry.Radius(doubleCylinderZip, doubleCylinder, squaredOff, calculateDistance, calculateXYDistance)
import Geometry.Angle(RotateFactor, getXYAngle, Angle(..), getQuadrantAngle, rotateAngle, )
import Geometry.Vertex(getXWithQuadrant, getYWithQuadrant, Vertex(..), adjustPointAxis)

import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))


import Test.HUnit



geometryRadiusTestDo = do
  putStrLn "\n\n" 
  putStrLn "geometryRadiusTestDo tests"
  runTestTT pos1Test
  runTestTT neg1Test
  runTestTT calculateXYDistancePos1Test
  runTestTT calculateXYDistanceNeg1Test
  runTestTT calculateXYDistanceUnEvenTest
  runTestTT calculateXYDistanceAfterRotatingTest
  runTestTT lookAtXTestLength
  runTestTT lookAtXYRadiusTestLength
  runTestTT lookAtXTestRotatedAngle
  runTestTT lookAtXTestXYAngle
  runTestTT rotationsTestFailureTest

rotationsTestFailureTest = TestCase $ assertEqual
  "see why RotationsTest.rotateCornerPointAroundZAxisTest fails. Is it the distance caluclation"
  (Radius 7.07106781186547550)
  (calculateDistance
     (Point 5 (-5) 0)
     (Point 0 0 0)
  )


pos1Test = TestCase $ assertEqual
  "pos1Test"
  (Radius 1.73)
  (calculateDistance
     (Point 0 0 0)
     (Point 1 1 1)
  )



neg1Test = TestCase $ assertEqual
  "neg1Test"
  (Radius 1.73)
  (calculateDistance
     (Point 1 1 1)
     (Point 0 0 0)
  )

calculateXYDistancePos1Test = TestCase $ assertEqual
  "calculateXYDistancePos1Test"
  (Radius 1.414)
  (calculateXYDistance
     (Point 0 0 0)
     (Point 1 1 1)
  )

calculateXYDistanceNeg1Test = TestCase $ assertEqual
  "calculateXYDistancePos1Test"
  (Radius 1.414)
  (calculateXYDistance
     (Point 1 1 1)
     (Point 0 0 0)
  )

  
calculateXYDistanceUnEvenTest = TestCase $ assertEqual
  "calculateXYDistanceUnEvenTest"
  (Radius 2.236068)
  (calculateXYDistance
     (Point 0 0 0)
     (Point 1 (-2) 1)
  )

calculateXYDistanceAfterRotatingTest = TestCase $ assertEqual
  "calculateXYDistanceUnEvenTest"
  (Radius 1.41)
  ( let
       pointToRotate = (Point 1 (1) 10)
       origin = (Point 0 0 0)
       rotateFactor = 10
       rotatedAngle = rotateAngle rotateFactor $ getXYAngle (Point 0 0 0) pointToRotate
       xyRadius = calculateXYDistance origin pointToRotate
       rotatedPoint = (adjustPointAxis (getXWithQuadrant rotatedAngle xyRadius)) . (adjustPointAxis (getYWithQuadrant rotatedAngle xyRadius)) $ origin
    in
    calculateXYDistance
     origin
     rotatedPoint
  )

lookAtXTestLength = TestCase $ assertEqual
  "lookAtXTestLength"
  (Quad1X 2.7212895296815116)
  
  ( let
       pointToRotate = (Point 1 (-10) 10)
       origin = (Point 0 0 0)
       rotateFactor = 10
       rotatedAngle = rotateAngle rotateFactor $ getXYAngle (Point 0 0 0) pointToRotate
       xyRadius = calculateXYDistance origin pointToRotate
       --rotatedPoint = (adjustPointAxis (getXWithQuadrant rotatedAngle xyRadius)) . (adjustPointAxis (getYWithQuadrant rotatedAngle xyRadius)) $ origin
    in
      getXWithQuadrant rotatedAngle xyRadius
  )

lookAtXYRadiusTestLength = TestCase $ assertEqual
  "lookAtXYRadiusTestLength"
  (Radius 10.04987562112089)
  
  ( let
       pointToRotate = (Point 1 (-10) 10)
       origin = (Point 0 0 0)
       xyRadius = calculateXYDistance origin pointToRotate
    in
      xyRadius
  )

lookAtXTestRotatedAngle = TestCase $ assertEqual
  "lookAtXTestRotatedAngle"
  (Angle 15.710593137499643)
  

  ( let
       pointToRotate = (Point 1 (-10) 10)
       origin = (Point 0 0 0)
       rotateFactor = 10
       rotatedAngle = rotateAngle rotateFactor $ getXYAngle (Point 0 0 0) pointToRotate
       
    in
      rotatedAngle
  )

lookAtXTestXYAngle = TestCase $ assertEqual
  "lookAtXTestXYAngle"
  (Angle 5.710593137499643)
  --but got: Quad2X {length = 10.02172570778901}

  ( let
       pointToRotate = (Point 1 (-10) 10)
       origin = (Point 0 0 0)
       
       xyAngle = getXYAngle (Point 0 0 0) pointToRotate
       
    in
      xyAngle
  )
