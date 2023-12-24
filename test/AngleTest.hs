module AngleTest(angleTestDo) where

import Test.HUnit

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), getXYAngle, getQuadrant, Quadrant(..))
import CornerPoints.Points(Point(..))

angleTestDo = do
  putStrLn "" 
  putStrLn "AngleTest"
  runTestTT quad1CenteredTest
  runTestTT quad1X1Y10Test
  runTestTT quad1X10Y100OffsetTest
  runTestTT quad1X11Y101OffsetIntoQuad2Test -----------------------------------------
  runTestTT quad1OffsetIntoQuad4Test
  runTestTT quad2CenteredTest
  runTestTT quad2OffsetIntoQuad1Test
  runTestTT quad3CenteredTest
  runTestTT quad4CenteredTest
  -- rotatations
  runTestTT quad4ToQuad1RotateTest
  runTestTT quad1ToQuad1RotateTest
  --getQuadrant
  runTestTT getQuadrantLessThanZeroTest
  runTestTT getQuadrantGreaterThanZeroTest
  runTestTT getQuadrant1ZeroTest
  runTestTT getQuadrant1Test
  runTestTT getQuadrant1_90Test
  runTestTT getQuadrant2Test
  runTestTT getQuadrant2_180Test
  runTestTT getQuadrant3Test
  runTestTT getQuadrant3_270Test
  runTestTT getQuadrant4Test
  runTestTT getQuadrant4_360Test
  runTestTT getXYAngleTest1

quad1CenteredTest = TestCase $ assertEqual
  "quad1CenteredTest"
  (Angle 45)
  (getXYAngle (Point 0 0 0) (Point 1 (-1) 0))

quad1X1Y10Test = TestCase $ assertEqual
  "quad1X1Y10Test"
  (Angle 5.710593137499643)
  (getXYAngle (Point 0 0 0) (Point 1 (-10) 10))

quad1X10Y100OffsetTest = TestCase $ assertEqual
  "quad1X10Y100OffsetTest"
  (Angle 5.710593137499643)
  (getXYAngle (Point 1 (-1) 0) (Point 11 (-101) 0))

quad1X11Y101OffsetIntoQuad2Test = TestCase $ assertEqual
  "quad1X11Y101OffsetIntoQuad2Test"
  (Angle 5.710593137499643)
  (getXYAngle (Point 1 1 0) (Point 11 (-99) 0))



quad1OffsetIntoQuad4Test = TestCase $ assertEqual
  "quad1OffsetIntoQuad4Test"
  (Angle 45)
  (getXYAngle (Point (-1) (-1) 0) (Point 0 (-2) 0))
  
quad2CenteredTest = TestCase $ assertEqual
  "quad2CenteredTest"
  (Angle 168.6900675259798)
  (getXYAngle (Point 0 0 0) (Point 1 5 0))

quad2OffsetIntoQuad1Test = TestCase $ assertEqual
  "quad2OffsetIntoQuad1Test"
  (Angle 168.6900675259798)
  (getXYAngle (Point (1) (-1) 0) (Point 2 (4) 0))

quad3CenteredTest = TestCase $ assertEqual
  "quad3CenteredTest"
  (Angle 191.3099324740202)
  (getXYAngle (Point 0 0 0) (Point (-1) 5 0))

quad4CenteredTest = TestCase $ assertEqual
  "quad4CenteredTest"
  (Angle 348.69006752597977)
  (getXYAngle (Point 0 0 0) (Point (-1) (-5) 0))

getXYAngleTest1 = TestCase $ assertEqual
  "try failing flexisocket problem"
  (Angle 0)
  (getXYAngle
     (Point {x_axis = 5.0, y_axis = 10.0, z_axis = 20.0})
     (Point {x_axis = 5.0, y_axis = 10.0, z_axis = 0.0})
  )

quad4ToQuad1RotateTest = TestCase $ assertEqual
  "quad4ToQuad1RotateTest"
  (Angle 10)
  (rotateAngle 20 $ Angle 350)

quad1ToQuad1RotateTest = TestCase $ assertEqual
  "quad1ToQuad1RotateTest"
  (Angle 30)
  (rotateAngle 20 $ Angle 10)

getQuadrantLessThanZeroTest = TestCase $ assertEqual
  "getQuadrantLessThanZeroTest"
  (Quadrant4)
  (getQuadrant $ Angle (-10))

getQuadrantGreaterThanZeroTest = TestCase $ assertEqual
  "getQuadrantGreaterThanZeroTest"
  (Quadrant1)
  (getQuadrant $ Angle (380))

getQuadrant1ZeroTest = TestCase $ assertEqual
  "getQuadrant1ZeroTest"
  (Quadrant1)
  (getQuadrant $ Angle (0))

getQuadrant1Test = TestCase $ assertEqual
  "getQuadrant1Test"
  (Quadrant1)
  (getQuadrant $ Angle (10))

getQuadrant1_90Test = TestCase $ assertEqual
  "getQuadrant1_90Test"
  (Quadrant1)
  (getQuadrant $ Angle (90))

getQuadrant2Test = TestCase $ assertEqual
  "getQuadrant2Test"
  (Quadrant2)
  (getQuadrant $ Angle (110))

getQuadrant2_180Test = TestCase $ assertEqual
  "getQuadrant2_180Test"
  (Quadrant2)
  (getQuadrant $ Angle (180))

getQuadrant3Test = TestCase $ assertEqual
  "getQuadrant3Test"
  (Quadrant3)
  (getQuadrant $ Angle (190))

getQuadrant3_270Test = TestCase $ assertEqual
  "getQuadrant3_270Test"
  (Quadrant3)
  (getQuadrant $ Angle (270))

getQuadrant4Test = TestCase $ assertEqual
  "getQuadrant4Test"
  (Quadrant4)
  (getQuadrant $ Angle (290))

getQuadrant4_360Test = TestCase $ assertEqual
  "getQuadrant4_360Test"
  (Quadrant4)
  (getQuadrant $ Angle (360))
