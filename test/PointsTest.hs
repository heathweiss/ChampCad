module PointsTest(pointsTestDo) where

import CornerPoints.Points(Point(..))

import Test.HUnit

pointsTestDo = do
  putStrLn ""
  putStrLn "pointsTestDo"
  runTestTT onlyZDiffersTest
  runTestTT allAxisDifferTest
  runTestTT onlyZIsTheSameTest

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
  
  
