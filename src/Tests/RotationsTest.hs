module Tests.RotationsTest where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Rotations(rotateAlongYaxis90, rotateAlongYaxis180)
import CornerPoints.Points (Point(..))

rotationsTestDo = do
  let rotate90Test = TestCase $ assertEqual
        "rotate90Test"
        (CubePoints
                      (Point 20 50 0)--f1
                      (Point 0 50 0)--f2
                      (Point 5 50 10)--f3
                      (Point 15 50 10)--f4
                      (Point 20 0 10)--b1
                      (Point 0 0 0)--b2
                      (Point 5 0 10)--b3
                      (Point 15 0 10)--b4
        )
        (
          let cube = CubePoints
                      (Point 0 50 0)--f1
                      (Point 5 50 10)--f2
                      (Point 15 50 10)--f3
                      (Point 20 50 0)--f4
                      (Point 0 0 0)--b1
                      (Point 5 0 10)--b2
                      (Point 15 0 10)--b3
                      (Point 20 0 10)--b4
          in
          rotateAlongYaxis90 cube
        )
  runTestTT rotate90Test  

  let rotate180Test = TestCase $ assertEqual
        "rotate180Test"
        (CubePoints
                      (Point 15 50 10)--f1
                      (Point 20 50 0)--f2
                      (Point 0 50 0)--f3
                      (Point 5 50 10)--f4
                      (Point 15 0 10)--b1
                      (Point 20 0 10)--b2
                      (Point 0 0 0)--b3
                      (Point 5 0 10)--b4
        )
        (
          let cube = CubePoints
                      (Point 0 50 0)--f1
                      (Point 5 50 10)--f2
                      (Point 15 50 10)--f3
                      (Point 20 50 0)--f4
                      (Point 0 0 0)--b1
                      (Point 5 0 10)--b2
                      (Point 15 0 10)--b3
                      (Point 20 0 10)--b4
          in
          rotateAlongYaxis180 cube
        )
  runTestTT rotate180Test  
