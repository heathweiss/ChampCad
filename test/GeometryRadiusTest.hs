module GeometryRadiusTest (geometryRadiusTestDo) where

import  Geometry.Radius(doubleCylinderZip, doubleCylinder, squaredOff, calcultateDistance)
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))

import Test.HUnit

geometryRadiusTestDo = do
  runTestTT pos1Test
  runTestTT neg1Test

pos1Test = TestCase $ assertEqual
  "pos1Test"
  (Radius 1.73)
  (calcultateDistance
     (Point 0 0 0)
     (Point 1 1 1)
  )


neg1Test = TestCase $ assertEqual
  "neg1Test"
  (Radius 1.73)
  (calcultateDistance
     (Point 1 1 1)
     (Point 0 0 0)
  )
