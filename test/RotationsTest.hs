module RotationsTest(rotationsTestDo) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points (Point(..))

import Geometry.Rotation(rotatePointAroundZAxis, rotateCornerPointAroundZAxis)
{-
The CornerPoints.Rotations module does not work. Should be removed.

These tests are for Geometry.Rotation
-}

rotationsTestDo = do
  putStrLn "\n\n" 
  putStrLn "rotationsTestDo tests"
  runTestTT rotatePointAroundZAxisQ1ToQ2Test
  runTestTT rotatePointAroundZAxisQ1ZeroDegreesTest
  runTestTT rotatePointAroundZAxisQ1ToQ1Test
  runTestTT rotatePointAroundZAxisQ2ToQ3Test
  runTestTT rotatePointAroundZAxisQ2ToQ1Test
  runTestTT rotatePointAroundZAxisQ4ToQ1Test
  runTestTT rotateCornerPointAroundZAxisTest
  runTestTT rotateCornerPointAroundZAxisTest2
  runTestTT rotateCornerPointAroundZAxisB1Test
  runTestTT rotateCornerPointAroundZAxisB2Test
  runTestTT rotateCornerPointAroundZAxisB3Test
  runTestTT rotateCornerPointAroundZAxisB4Test
  runTestTT rotateCornerPointAroundZAxisF1Test
  runTestTT rotateCornerPointAroundZAxisF2Test
  runTestTT rotateCornerPointAroundZAxisF3Test
  runTestTT rotateCornerPointAroundZAxisF4Test

rotatePointAroundZAxisQ1ToQ2Test = TestCase $ assertEqual
  "rotatePointAroundZAxisQ1ToQ2Test"
  (Point 1 1 100)
  (rotatePointAroundZAxis 90 (Point 0 0 0) (Point 1 (-1) 100))

rotatePointAroundZAxisQ1ZeroDegreesTest = TestCase $ assertEqual
  "rotatePointAroundZAxisQ1ZeroDegreesTest"
  (Point 1     (-10) 100)
  --     10.0, -1,   100.0
  (rotatePointAroundZAxis 0 (Point 0 0 0) (Point 1 (-10) 100)  )


rotatePointAroundZAxisQ1ToQ1Test = TestCase $ assertEqual
  "rotatePointAroundZAxisQ1ToQ1Test"
  (Point 4.3 (-2.5) 100)
  {x_axis = 4.286607049870561, y_axis = -2.474873734152917, z_axis = 100.0}
  (rotatePointAroundZAxis 15 (Point 0 0 0) (Point 3.5 (-3.5) 100))
  

rotatePointAroundZAxisQ2ToQ3Test = TestCase $ assertEqual
  "rotatePointAroundZAxisQ2ToQ3Test"
  (Point (-1) 1 100)
  (rotatePointAroundZAxis 90 (Point 0 0 0) (Point 1 (1) 100))

rotatePointAroundZAxisQ2ToQ1Test = TestCase $ assertEqual
  "rotatePointAroundZAxisQ2ToQ1Test"
  (Point (1) (-1) 100)
  (rotatePointAroundZAxis (-90) (Point 0 0 0) (Point 1 (1) 100))

rotatePointAroundZAxisQ4ToQ1Test = TestCase $ assertEqual
  "rotatePointAroundZAxisQ4ToQ1Test"
  (Point (5) (-5) 0)
  (rotatePointAroundZAxis (90) (Point 0 0 0) (Point (-5) (-5) 0))


rotateCornerPointAroundZAxisTest = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisTest"
  --(TopFace (Point 0 0 0) (Point 0 0 0) (Point 0 0 0) (Point 0 0 0))
  (TopFace  {b2 = Point {x_axis = 5.0, y_axis = 5, z_axis = 0.0},
               f2 = Point {x_axis = 2.5, y_axis = 2.5, z_axis = 0.0},
               b3 = Point {x_axis = 6.0, y_axis = 4.0, z_axis = 0.0},
               f3 = Point {x_axis = 3.5, y_axis = 1.5, z_axis = 0.0}})
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (TopFace (Point 5 (-5) 0) (Point 2.5 (-2.5) 0) (Point 6 (-4) 0) (Point 3.5 (-1.5) 0))
     
     
  )

rotateCornerPointAroundZAxisTest2 = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisTest2"
  (TopFace {b2 = Point {x_axis = 15.0, y_axis = -25, z_axis = 0.0},
              f2 = Point {x_axis = 10, y_axis = -20, z_axis = 0.0},
              b3 = Point {x_axis = 6, y_axis = -4, z_axis = 0.0},
              f3 = Point {x_axis = 3.5, y_axis = -17, z_axis = 0.0}})
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (TopFace (Point (-25) (-15) 0) (Point (-20) (-10) 0) (Point (-4) (-6) 0) (Point  (-17) (-3.5) 0))
     

  )


rotateCornerPointAroundZAxisB1Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisB1Test"
  (B1 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (B1 (Point (-25) (-20) 0))
  )


rotateCornerPointAroundZAxisB2Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisB2Test"
  (B2 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (B2 (Point (-25) (-20) 0))
  )

rotateCornerPointAroundZAxisB3Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisB3Test"
  (B3 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (B3 (Point (-25) (-20) 0))
  )

rotateCornerPointAroundZAxisB4Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisB4Test"
  (B4 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (B4 (Point (-25) (-20) 0))
  )
rotateCornerPointAroundZAxisF1Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisF1Test"
  (F1 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (F1 (Point (-25) (-20) 0))
  )


rotateCornerPointAroundZAxisF2Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisF2Test"
  (F2 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (F2 (Point (-25) (-20) 0))
  )

rotateCornerPointAroundZAxisF3Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisF3Test"
  (F3 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (F3 (Point (-25) (-20) 0))
  )

rotateCornerPointAroundZAxisF4Test = TestCase $ assertEqual
  "rotateCornerPointAroundZAxisF4Test"
  (F4 (Point 20 (-25) 0) )
  (rotateCornerPointAroundZAxis
     90
     (Point 0 0 0)
     (F4 (Point (-25) (-20) 0))
  )
