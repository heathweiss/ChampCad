module PointsTest(pointsTestDo) where

import CornerPoints.Points(Point(..), calculateDistance, calculateXYDistance)

import Test.HUnit

pointsTestDo = do
  putStrLn ""
  putStrLn "pointsTestDo"
  runTestTT onlyZDiffersTest
  runTestTT allAxisDifferTest
  runTestTT onlyZIsTheSameTest

  runTestTT calculateDistance1
  runTestTT calculateDistance2
  runTestTT calculateDistance3
  runTestTT calculateDistance4
  runTestTT calculateDistance5
  runTestTT calculateDistance6

onlyZDiffersTest = TestCase $ assertEqual
  "only z differs"
  (False)
  (
    (Point {x_axis = 5.0, y_axis = 10.0, z_axis = 20.0})
    ==
    (Point {x_axis = 5.0, y_axis = 10.0, z_axis = 0.0})
  )

onlyZIsTheSameTest = TestCase $ assertEqual
  "only the z axis is same"
  (False)
  (
    (Point {x_axis = 15.0, y_axis = 10.0, z_axis = 20.0})
    ==
    (Point {x_axis = 25.0, y_axis = 110.0, z_axis = 20.0})
  )

allAxisDifferTest = TestCase $ assertEqual
  "only z differs"
  (False)
  (
    (Point {x_axis = 5.0, y_axis = 10.0, z_axis = 20.0})
    ==
    (Point {x_axis = 15.0, y_axis = 20.0, z_axis = 30.0})
  )
  
  
-- ============================= calculate distances =================================
calculateDistance1 = TestCase $ assertEqual
  "see why RotationsTest.rotateCornerPointAroundZAxisTest fails. Is it the distance caluclation"
  (7.07106781186547550)
  (calculateDistance
     (Point 5 (-5) 0)
     (Point 0 0 0)
  )


calculateDistance2 = TestCase $ assertEqual
  "pos1Test"
  (1.73)
  (calculateDistance
     (Point 0 0 0)
     (Point 1 1 1)
  )



calculateDistance3 = TestCase $ assertEqual
  "neg1Test"
  (1.73)
  (calculateDistance
     (Point 1 1 1)
     (Point 0 0 0)
  )

calculateDistance4 = TestCase $ assertEqual
  "calculateXYDistancePos1Test"
  (1.414)
  (calculateXYDistance
     (Point 0 0 0)
     (Point 1 1 1)
  )

calculateDistance5 = TestCase $ assertEqual
  "calculateXYDistancePos1Test"
  (1.414)
  (calculateXYDistance
     (Point 1 1 1)
     (Point 0 0 0)
  )

  
calculateDistance6 = TestCase $ assertEqual
  "calculateXYDistanceUnEvenTest"
  (2.236068)
  (calculateXYDistance
     (Point 0 0 0)
     (Point 1 (-2) 1)
  )

