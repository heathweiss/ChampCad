module DistanceTest(distanceTestDo) where

import CornerPoints.Points(Point(..), )

import Math.Distance(Distance(..), Distant, calculateDistance)
  
import Test.HUnit

distanceTestDo = do
  putStrLn ""
  putStrLn "distanceTestDo"
  runTestTT shouldBeEqual1
  runTestTT shouldBeEqual2
  runTestTT shouldBeEqual3


shouldBeEqual1 = TestCase $ assertEqual
  "should be equal"
  (Distance 5.0)
  (calculateDistance (Point 4 0 0) (Point 0 3 0))

shouldBeEqual2 = TestCase $ assertEqual
  "should be equal 2"
  (Distance 5.0)
  (calculateDistance (Point 4 0 0) (Point 0 (-3) 0))

shouldBeEqual3 = TestCase $ assertEqual
  "should be equal 2"
  (Distance 5.0)
  (calculateDistance (Point (-4) 0 0) (Point 0 (-3) 0))
