module DistanceTest(distanceTestDo) where

import CornerPoints.Points(Point(..), )
import CornerPoints.CornerPoints(CornerPoints(..), )

import Math.Distance(Distance(..), Distant, calculateDistance,
                    DistanceA(..), calculateDistanceA)
  
import Test.HUnit

distanceTestDo = do
  putStrLn "\nDistanceTest"
  runTestTT shouldBeEqual1
  runTestTT shouldBeEqual2
  runTestTT shouldBeEqual3

  runTestTT seeTheDistanceATest
  runTestTT seeTheDistanceATest2


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

-- ================================================ look at distances ================================
-- Look at some distances, to be able to compare them when doing other work.
seeTheDistanceATest = TestCase $ assertEqual
  "have a look at distance"
  (Right (DistanceA {distance = 5.830951894845301}))
  (calculateDistanceA (BottomLeftLine {b1 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}, f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}})
                      (F1 $ Point 6 (-17) 0)
  )

seeTheDistanceATest2 = TestCase $ assertEqual
  "have a look at distance"
  (Right (DistanceA {distance = 7.0710678118654755}))
  (calculateDistanceA (BottomLeftLine {b1 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}, f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}})
                      (F1 $ Point 10 (-11) 0)
  )
