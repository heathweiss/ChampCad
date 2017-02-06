module VerticalFacesTest(verticalFacesTestDo)where

import Test.HUnit
import CornerPoints.VerticalFaces(createRightFaces, createRightFacesNoSlope, createLeftFaces, createLeftFacesNoSlope, createHorizontallyAlignedCubes, createHorizontallyAlignedCubesNoSlope , createLeftFacesMultiColumns, createLeftFacesMultiColumnsNoSlope,TransposeFactor,
                                 createVerticalWalls)
import CornerPoints.Create(Slope(..), Origin(..), createCornerPoint, Angle(..), flatXSlope, flatYSlope)
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(MultiDegreeRadii(..), Radius(..), SingleDegreeRadii(..), Degree(..))

import TypeClasses.Transposable(transpose)

verticalFacesTestDo = do
  runTestTT createRightFacesWithSlopeTest
  runTestTT createRightFacesNoSlopeTest
  runTestTT createLeftFacesWithSlopeTest
  runTestTT createLeftFacesNoSlopeTest
  runTestTT createLeftFacesMultiColumnWithSlopeTest
  runTestTT createLeftFacesMultiColumnNoSlopeTest
  runTestTT createHorizontallyAlignedCubesWithSlopeTest
  runTestTT createHorizontallyAlignedCubesNoSlopeTest
  runTestTT createVerticalWallsTest

singleDegree0RadiiTestData = SingleDegreeRadii 0 (map (Radius) [1,1,1])
singleDegree90RadiiTestData = SingleDegreeRadii 90 (map (Radius) [1,1,1])
singleDegree180RadiiTestData = SingleDegreeRadii 180 (map (Radius) [1,1,1])

createRightFacesWithSlopeTest = TestCase $ assertEqual
  "createRightFacesWithSlopeTestTest"
  ([RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
               b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
               f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}},
    RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
               f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0},
               f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}}] )
  (createRightFaces (Point 0 0 10) singleDegree0RadiiTestData flatXSlope flatYSlope [1..])

createRightFacesNoSlopeTest = TestCase $ assertEqual
  "createRightFacesNoSlopeTestTest"
  ([RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
               b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
               f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}},
    RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
               f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0},
               f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}}] )
  (createRightFacesNoSlope (Point 0 0 10) singleDegree0RadiiTestData [1..])



createLeftFacesWithSlopeTest = TestCase $ assertEqual
  "createLeftFacesWithSlopeTest"
  ([LeftFace  {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
               f1 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}},
    LeftFace  {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
               f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0},
               f1 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}}] )
  (createLeftFaces (Point 0 0 10) singleDegree0RadiiTestData flatXSlope flatYSlope [1..])

createLeftFacesNoSlopeTest = TestCase $ assertEqual
  "createLeftFacesNoSlopeTest"
  ([LeftFace  {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
               f1 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}},
    LeftFace  {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
               f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0},
               f1 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}}] )
  (createLeftFacesNoSlope (Point 0 0 10) singleDegree0RadiiTestData [1..])

createLeftFacesMultiColumnWithSlopeTest = TestCase $ assertEqual
  "createLeftFacesMultiColumnWithSlopeTest"
  (
    [[
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}},
      
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}}],
      
     [LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0}},
      
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 8.0}, 
                f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}}]]
  )
  (
    createLeftFacesMultiColumns
      (Point 0 0 10)
      [singleDegree90RadiiTestData, singleDegree180RadiiTestData]
      flatXSlope flatYSlope
      [1..]
  )


createLeftFacesMultiColumnNoSlopeTest = TestCase $ assertEqual
  "createLeftFacesMultiColumnNoSlopeTest"
  (
    [[
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}},
      
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}}],
      
     [LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0}},
      
      LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 8.0}, 
                f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}}]]
  )
  (
    createLeftFacesMultiColumnsNoSlope
      (Point 0 0 10)
      [singleDegree90RadiiTestData, singleDegree180RadiiTestData]
      
      [1..]
  )



createHorizontallyAlignedCubesWithSlopeTest = TestCase $ assertEqual
  "createHorizontallyAlignedCubesWithSlopeTest"
  (
    [
     [
      CubePoints {f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}, 
                 f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, 
                 f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}},
     
     CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0}, 
                 f3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}, 
                 f4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}}
     ],
     [
     CubePoints {f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                 f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}, 
                 f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}},
           
     CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 8.0}, 
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                 f3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}}
     ]
    ]
 
  )
  (
    createHorizontallyAlignedCubes
      (createRightFaces (Point 0 0 10) singleDegree0RadiiTestData flatXSlope flatYSlope [1..])
      (createLeftFacesMultiColumns
        (Point 0 0 10)
        [singleDegree90RadiiTestData, singleDegree180RadiiTestData]
        flatXSlope flatYSlope
        [1..]
      )
  )

createHorizontallyAlignedCubesNoSlopeTest = TestCase $ assertEqual
  "createHorizontallyAlignedCubesNoSlopeTest"
  (
    [
     [
      CubePoints {f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}, 
                 f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, 
                 f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}},
     
     CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0}, 
                 f3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0}, 
                 f4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}}
     ],
     [
     CubePoints {f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                 f2 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 9.0}, 
                 f4 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 8.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}},
           
     CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 8.0}, 
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 9.0}, 
                 f3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 9.0}, 
                 f4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 8.0}, 
                 b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}, 
                 b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, 
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0}}
     ]
    ]
 
  )
  (
    createHorizontallyAlignedCubesNoSlope
      (Point 0 0 10)
      (MultiDegreeRadii "name" [singleDegree0RadiiTestData, singleDegree90RadiiTestData, singleDegree180RadiiTestData])
      [1..]
    
  )


{-
Need to find the reason why sockets are somehow flipped, seemingly around an axis.
Is is related to the rewrite of createVerticalWalls in commit 228de7095e72f75165ab8b07b0d52d993ba66185
-}
singleDegree0RadiiTestData2 = SingleDegreeRadii 0 (map (Radius) [1,2,3])
singleDegree90RadiiTestData2 = SingleDegreeRadii 90 (map (Radius) [1,2,3])
singleDegree180RadiiTestData2 = SingleDegreeRadii 180 (map (Radius) [1,2,3])

createVerticalWallsTest = TestCase $ assertEqual
  "createVerticalWallsTest"
  ( {-
    createVerticalWalls'
      (MultiDegreeRadii "one" [singleDegree0RadiiTestData2, singleDegree90RadiiTestData2, singleDegree180RadiiTestData2])
      (MultiDegreeRadii "two" (map (transpose (+3)) [singleDegree0RadiiTestData2, singleDegree90RadiiTestData2, singleDegree180RadiiTestData2]))
      (Point 0 0 10)
      [1..]
    -}
    [[CubePoints {f1 = Point {x_axis = 5.0, y_axis = -3.061616997868383e-16, z_axis = -1.0}, f2 = Point {x_axis = 4.0, y_axis = -2.4492935982947064e-16, z_axis = 0.0}, f3 = Point {x_axis = 0.0, y_axis = -4.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = -1.0}, b1 = Point {x_axis = 2.0, y_axis = -1.2246467991473532e-16, z_axis = -1.0}, b2 = Point {x_axis = 1.0, y_axis = -6.123233995736766e-17, z_axis = 0.0}, b3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 0.0}, b4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = -1.0}},CubePoints {f1 = Point {x_axis = 0.0, y_axis = 5.0, z_axis = -1.0}, f2 = Point {x_axis = 0.0, y_axis = 4.0, z_axis = 0.0}, f3 = Point {x_axis = 4.0, y_axis = -2.4492935982947064e-16, z_axis = 0.0}, f4 = Point {x_axis = 5.0, y_axis = -3.061616997868383e-16, z_axis = -1.0}, b1 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = -1.0}, b2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, b3 = Point {x_axis = 1.0, y_axis = -6.123233995736766e-17, z_axis = 0.0}, b4 = Point {x_axis = 2.0, y_axis = -1.2246467991473532e-16, z_axis = -1.0}}],[CubePoints {f1 = Point {x_axis = 6.0, y_axis = -3.6739403974420594e-16, z_axis = -2.0}, f2 = Point {x_axis = 5.0, y_axis = -3.061616997868383e-16, z_axis = -1.0}, f3 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = -1.0}, f4 = Point {x_axis = 0.0, y_axis = -6.0, z_axis = -2.0}, b1 = Point {x_axis = 3.0, y_axis = -1.8369701987210297e-16, z_axis = -2.0}, b2 = Point {x_axis = 2.0, y_axis = -1.2246467991473532e-16, z_axis = -1.0}, b3 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = -1.0}, b4 = Point {x_axis = 0.0, y_axis = -3.0, z_axis = -2.0}},CubePoints {f1 = Point {x_axis = 0.0, y_axis = 6.0, z_axis = -2.0}, f2 = Point {x_axis = 0.0, y_axis = 5.0, z_axis = -1.0}, f3 = Point {x_axis = 5.0, y_axis = -3.061616997868383e-16, z_axis = -1.0}, f4 = Point {x_axis = 6.0, y_axis = -3.6739403974420594e-16, z_axis = -2.0}, b1 = Point {x_axis = 0.0, y_axis = 3.0, z_axis = -2.0}, b2 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = -1.0}, b3 = Point {x_axis = 2.0, y_axis = -1.2246467991473532e-16, z_axis = -1.0}, b4 = Point {x_axis = 3.0, y_axis = -1.8369701987210297e-16, z_axis = -2.0}}]]

  )
  (
    createVerticalWalls
       [singleDegree0RadiiTestData2, singleDegree90RadiiTestData2, singleDegree180RadiiTestData2]
       (map (transpose (+3)) [singleDegree0RadiiTestData2, singleDegree90RadiiTestData2, singleDegree180RadiiTestData2])
       (Point 0 0 0)
       [1..]
    
  )
