{-# LANGUAGE TemplateHaskell #-}
module GeometryRadiusTest (geometryRadiusTestDo) where

import Geometry.Radius(doubleCylinderZip, doubleCylinder, squaredOff)
import Geometry.Angle(RotateFactor, getXYAngle, Angle(..), getQuadrantAngle, rotateAngle, )
import Geometry.Vertex(getXWithQuadrant, getYWithQuadrant, Vertex(..), adjustPointAxis)

import CornerPoints.Points(Point(..), calculateDistance, calculateXYDistance)
import CornerPoints.Radius(Radius(..))

import Math.Distance(Distance(..))

import Test.HUnit

import Control.Lens

makeLenses ''Distance

geometryRadiusTestDo = do
  putStrLn "\n\n" 
  putStrLn "geometryRadiusTestDo tests"
  runTestTT calculateXYDistanceAfterRotatingTest
  runTestTT lookAtXTestLength
  runTestTT lookAtXYRadiusTestLength
  runTestTT lookAtXTestRotatedAngle
  runTestTT lookAtXTestXYAngle
  

calculateXYDistanceAfterRotatingTest = TestCase $ assertEqual
  "calculateXYDistanceUnEvenTest"
  (Radius 1.41)
  ( let
       pointToRotate = (Point 1 (1) 10)
       origin = (Point 0 0 0)
       rotateFactor = 10
       rotatedAngle = rotateAngle rotateFactor $ getXYAngle (Point 0 0 0) pointToRotate
       xyRadius = Radius $ (calculateXYDistance origin pointToRotate)^.distance
       rotatedPoint = (adjustPointAxis (getXWithQuadrant rotatedAngle xyRadius)) . (adjustPointAxis (getYWithQuadrant rotatedAngle xyRadius)) $ origin
    in
    Radius $ (calculateXYDistance origin rotatedPoint)^.distance
  )

lookAtXTestLength = TestCase $ assertEqual
  "lookAtXTestLength"
  (Quad1X 2.7212895296815116)
  
  ( let
       pointToRotate = (Point 1 (-10) 10)
       origin = (Point 0 0 0)
       rotateFactor = 10
       rotatedAngle = rotateAngle rotateFactor $ getXYAngle (Point 0 0 0) pointToRotate
       xyRadius = Radius $ (calculateXYDistance origin pointToRotate)^.distance
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
       xyRadius = Radius $ (calculateXYDistance origin pointToRotate)^.distance
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
