module CornerPointsTest(cornerPointsTestDo ) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (++++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@),
                                cornerPointsError, isCubePoints, isCubePointsList)
import CornerPoints.Points (Point(..), center ,(<-|->))
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)

import Math.Distance(Distance(..),calculateDistance)

cornerPointsTestDo = do

  putStrLn ""
  putStrLn "cornerPointsTestDo"
  runTestTT leftFacePPPBottomLeftLineTest
  runTestTT topLeftLinePPPBottomLeftLineTest
  runTestTT rightFacePPPBottomRightLineTest
  runTestTT topRightLinePlusPlusPlusBtmRightLineTest
  runTestTT f4PlusPlusPlusB4Test
  runTestTT f1PlusPlusPlusB1Test
  runTestTT bottomRightLinePlusPlusPlusBottomLeftLineTest
  runTestTT bottomFacePlusPlusPlusBottomLeftLineTest
  runTestTT f3PlusPlusPlusB3Test
  runTestTT f2PlusPlusPlusB2Test
  runTestTT topRightLinePlusPlusPlusTopLeftLineTest
  runTestTT topFacePlusPlusPlusTopLeftLineTest
  runTestTT topFacePlusPlusPlusBottomFaceTest
  runTestTT topLeftLinePlusPlusPlusTopFaceTest
  runTestTT topRightLineRolledIntoTopLeftLinesTest
  runTestTT bottomRightLineRolledIntobottomLeftLinesTest
  runTestTT topRightLineRolledIntoTopLeftLinesTestP4bottomRightLineRolledIntobottomLeftLinesTest
  runTestTT bottomFrontLinePlusPlusPlusF1Test
  runTestTT f4PlusPlusPlusF1Test
  runTestTT f1PlusPlusPlusbottomFrontLineTest
  runTestTT backBottomLinePlusPlusPlusBottomFrontLineTest
  runTestTT f1PlusPlusPlusF4Test 
  runTestTT bottomFrontLinePlusPlusPlusbackBottomLineTest
  runTestTT f3PlusPlusPlusb3Test
  runTestTT  bottomLeftLinePlusPlusPlusBottomRightLineTest

  runTestTT f2PlusPlusPlusB2PlusPlusFwdF3PlusPlusPlusB3

  -- equality tests
  runTestTT shouldBeEqualF3
  runTestTT shouldBeEqualPoints
  
  --isCubePoints and isCubePointsList tests
  runTestTT isCubePointsTest
  runTestTT isNotCubePointsTest
  runTestTT isCubePointsWithNothingTest
  runTestTT isCubePointsListTest
  runTestTT isCubePointsNothingListTest
  runTestTT isNotCubePointsListTest

  --error tests
  runTestTT isCornerPointsErrorTest
  runTestTT isNotCornerPointsErrorTest

  runTestTT centerTest1
  runTestTT centerTest2
  runTestTT centerTest3

  runTestTT distanceTest1
  runTestTT distanceTest2
  runTestTT distanceTest3
  runTestTT distanceTest4

  runTestTT nonStandaredTest1
-------------------- isCubePoints tests-------
isCubePointsTest = TestCase $ assertEqual
  "isCubePointsTest"
  True
  (isCubePoints (CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)
                                (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)))

isNotCubePointsTest = TestCase $ assertEqual
  "isNotCubePointsTest"
  False
  (isCubePoints (LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1) ))

isCubePointsWithNothingTest = TestCase $ assertEqual
  "isNotCubePointsTest"
  True
  (isCubePoints CornerPointsNothing)

isCubePointsListTest = TestCase $ assertEqual
  "isCubePointsListTest"
  True
  (isCubePointsList [CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)
                                (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)])

isCubePointsNothingListTest = TestCase $ assertEqual
  "isCubePointsNothingListTest"
  True
  (isCubePointsList [CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)
                                (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1),
                     CornerPointsNothing
                    ])

isNotCubePointsListTest = TestCase $ assertEqual
  "isNotCubePointsListTest"
  False
  (isCubePointsList [CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)
                                (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1),
                     LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1)]
  )

------------------- errors tests-------------  
isCornerPointsErrorTest = TestCase $ assertEqual
  "isCornerPointsError"
  (True)
  (cornerPointsError $ CornerPointsError "this is an error.")

isNotCornerPointsErrorTest = TestCase $ assertEqual
  "isNotCornerPointsError"
  (False)
  (cornerPointsError $ F1 (Point 1 2 3))


  ------------------------ equality tests ------------------------------
shouldBeEqualPoints = TestCase $ assertEqual
  "points should be equal:-6.123233995736766e-1 "
  (Point {x_axis = 1.0, y_axis = (-6.123233995736766e-17), z_axis = 50.0})
  (Point {x_axis = 1.0, y_axis = (-6.123233995736766e-17), z_axis = 50.0})

shouldBeEqualF3 = TestCase $ assertEqual
  "these cornerpoints should be equal: -6.123233995736766e-17 "
  (F3 (Point {x_axis = 1.0, y_axis = (-6.123233995736766e-17), z_axis = 50.0}))
  (F3 (Point {x_axis = 1.0, y_axis = (-6.123233995736766e-17), z_axis = 50.0}))

{-
Test +++
-}

leftFacePPPBottomLeftLineTest = TestCase $ assertEqual
  "LeftFace +++ BottomLeftLine = LeftFace"
  (LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1) )
  (    (LeftFace  (Point 0 0 1)  (Point 0 0 2)  (Point 0 1 1) (Point 0 1 2) ) +++   (BottomLeftLine (Point 0 0 0) (Point 0 1 0) )  )

topLeftLinePPPBottomLeftLineTest = TestCase $ assertEqual
  "TopLeftLine +++ BottomLeftLine = LeftFace"
  (LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1) )
  ( (TopLeftLine (Point 0 0 1) (Point 0 1 1))  +++   (BottomLeftLine (Point 0 0 0) (Point 0 1 0) ) )

rightFacePPPBottomRightLineTest = TestCase $ assertEqual
  "RightFace +++ BottomRightLine = RightFace"
  (RightFace (Point 1 0 5) (Point 1 0 4) (Point 1 5 5) (Point 1 5 4))
  (   (RightFace (Point 1 0 6) (Point 1 0 5) (Point 1 5 6) (Point 1 5 5))   +++      (BottomRightLine (Point 1 0 4) (Point 1 5 4))              )

topRightLinePlusPlusPlusBtmRightLineTest = TestCase $ assertEqual
  "TopRightLine +++ BottomRightLine = RightFace"
  (RightFace (Point 1 0 1) (Point 1 0 0) (Point 1 1 1) (Point 1 1 0))
  (    (TopRightLine (Point 1 0 1) (Point 1 1 1))  +++  (BottomRightLine) (Point 1 0 0) (Point 1 1 0)  )

f3PlusPlusPlusb3Test = TestCase $ assertEqual
  "F3 +++ B3 = TopRightLine"
  (TopRightLine (Point 0 0 0) (Point 0 10 0))
  (     (F3 (Point 0 10 0)) +++ (B3 (Point 0 0 0))        )

f4PlusPlusPlusF1Test = TestCase $ assertEqual
  "F4 +++ F1 = BottomFrontLine"
  (BottomFrontLine (Point 2 2 2) (Point 1 1 1)) 
  ((F4 (Point 1 1 1)) +++ (F1 (Point 2 2 2)))

f1PlusPlusPlusF4Test = TestCase $ assertEqual
  "F1 +++ F4 = BottomFrontLine"
  (BottomFrontLine (Point 1 1 1) (Point 2 2 2)) 
  ((F1 (Point 1 1 1)) +++ (F4 (Point 2 2 2)))

f4PlusPlusPlusB4Test = TestCase $ assertEqual
  "F4 +++ B4 = BottomRightLine"
  (BottomRightLine (Point 2 2 2) (Point 1 1 1)) 
  ((F4 (Point 1 1 1)) +++ (B4 (Point 2 2 2)))

f1PlusPlusPlusB1Test = TestCase $ assertEqual
  "F1 +++ B1 = BottomLeftLine"
  (BottomLeftLine (Point 2 2 2) (Point 1 1 1)) 
  ((F1 (Point 1 1 1)) +++ (B1 (Point 2 2 2)))

bottomRightLinePlusPlusPlusBottomLeftLineTest = TestCase $ assertEqual
  "BottomRightLine +++ BottomLeftLine = BottomFace"
  (BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
               f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
               b4 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0},
               f4 = Point {x_axis = 10.0, y_axis = 1.0, z_axis = 0.0}}
  )
  ((BottomRightLine (Point 10 0 0) (Point 10 1 0)) +++ (BottomLeftLine (Point 0 0 0) (Point 0 1 0)))

bottomLeftLinePlusPlusPlusBottomRightLineTest = TestCase $ assertEqual
  "BottomLeftLine +++ BottomRightLine = BottomFace"
  (BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
               f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
               b4 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0},
               f4 = Point {x_axis = 10.0, y_axis = 1.0, z_axis = 0.0}}
  )
  ((BottomLeftLine (Point 0 0 0) (Point 0 1 0)) +++ (BottomRightLine (Point 10 0 0) (Point 10 1 0)))

bottomFacePlusPlusPlusBottomLeftLineTest = TestCase $ assertEqual
  "BottomFace +++ BottomLeftLine = BottomFace"
  (BottomFace (Point 2 2 2) (Point 2 2 2) (Point 1 1 1) (Point 1 1 1)) 
  ((BottomFace (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)) +++ (BottomLeftLine (Point 2 2 2) (Point 2 2 2)))

f3PlusPlusPlusB3Test = TestCase $ assertEqual
  "F3 +++ B3 = BottomRightLine"
  (TopRightLine (Point 2 2 2) (Point 1 1 1)) 
  ((F3 (Point 1 1 1)) +++ (B3 (Point 2 2 2)))

f2PlusPlusPlusB2Test = TestCase $ assertEqual
  "F2 +++ B2 = TopLeftLine"
  (TopLeftLine (Point 2 2 2) (Point 1 1 1)) 
  ((F2 (Point 1 1 1)) +++ (B2 (Point 2 2 2)))

bottomFrontLinePlusPlusPlusF1Test = TestCase $ assertEqual
 "bottomFrontLinePlusPlusPlusF1" 
  (BottomFrontLine {f1 = Point {x_axis = 7.0, y_axis = 8.0, z_axis = 9.0}, f4 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 3.0}})
  ((BottomFrontLine {f1 = (Point 1 2 3), f4 = (Point 4 5 6)}) +++ (F1 (Point 7 8 9)))

f1PlusPlusPlusbottomFrontLineTest = TestCase $ assertEqual
 "f1PlusPlusPlusbottomFrontLineTest" 
  (BottomFrontLine {f1 = (Point 7 8 9), f4 = (Point 1 2 3)})
  ((F1 (Point 7 8 9)) +++ (BottomFrontLine {f1 = (Point 1 2 3), f4 = (Point 4 5 6)}) )
  

topRightLinePlusPlusPlusTopLeftLineTest = TestCase $ assertEqual
  "TopRightLine +++ TopLeftLine = TopFace"
  (TopFace (Point 2 2 2) (Point 2 2 2) (Point 1 1 1) (Point 1 1 1)) 
  ((TopRightLine (Point 1 1 1) (Point 1 1 1)) +++ (TopLeftLine (Point 2 2 2) (Point 2 2 2)))

topFacePlusPlusPlusTopLeftLineTest = TestCase $ assertEqual
  "TopFace +++ TopLeftLine = TopFace"
  (TopFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11)) 
  ((TopFace (Point 11 11 11) (Point 11 11 11) (Point 1 1 1) (Point 1 1 1)) +++ (TopLeftLine (Point 2 2 2) (Point 2 2 2)))

topFacePlusPlusPlusBottomFaceTest = TestCase $ assertEqual
  "TopFace +++ BottomFace = Cube"
  (CubePoints {f1=(Point 1 1 1), f2=(Point 2 2 2), f3=(Point 3 3 3), f4=(Point 4 4 4), b1=(Point 5 5 5), b2=(Point 6 6 6), b3=(Point 7 7 7), b4=(Point 8 8 8)}) 
  ((TopFace {b2=(Point 6 6 6), f2=(Point 2 2 2), b3=(Point 7 7 7), f3=(Point 3 3 3)}) +++ (BottomFace {b1=(Point 5 5 5), f1=(Point 1 1 1), b4=(Point 8 8 8), f4=(Point 4 4 4)}))

topLeftLinePlusPlusPlusTopFaceTest = TestCase $ assertEqual
  "TopLeftLine +++ TopFace = TopFace"
  (TopFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11)) 
  ((TopLeftLine (Point 2 2 2) (Point 2 2 2)) +++ (TopFace (Point 11 11 11) (Point 11 11 11) (Point 1 1 1) (Point 1 1 1)))

backBottomLinePlusPlusPlusBottomFrontLineTest = TestCase $ assertEqual
  "backBottomLinePlusPlusPlusBottomFrontLineTest"
  (BottomFace {b1=(Point 5 5 5), f1=(Point 7 7 7 ), b4=(Point 6 6 6), f4=(Point 8 8 8) })
  ((BackBottomLine {b1=(Point 5 5 5), b4=(Point 6 6 6)}) +++ (BottomFrontLine {f1=(Point 7 7 7), f4=(Point 8 8 8)}) )

bottomFrontLinePlusPlusPlusbackBottomLineTest = TestCase $ assertEqual
  "bottomFrontLinePlusPlusPlusbackBottomLineTest"
  (BottomFace {b1=(Point 5 5 5), f1=(Point 7 7 7 ), b4=(Point 6 6 6), f4=(Point 8 8 8) })
  ((BottomFrontLine {f1=(Point 7 7 7), f4=(Point 8 8 8)}) +++ (BackBottomLine {b1=(Point 5 5 5), b4=(Point 6 6 6)}) )

--As everything is 1's, values don't matter. Is just testing for inexhaustive pattern matching.
topRightLineRolledIntoTopLeftLinesTest = TestCase $ assertEqual
  "TopRightLine +++> [TopLeftLine, TopLeftLine] = [TopFace, TopFace]"
  ([(TopFace (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)), (TopFace (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)) ]) 
  ((TopRightLine (Point 1 1 1) (Point 1 1 1)) +++>  [(TopLeftLine (Point 1 1 1) (Point 1 1 1)), (TopLeftLine (Point 1 1 1) (Point 1 1 1))] )

--As everything is 1's, values don't matter. Is just testing for inexhaustive patter matching.
bottomRightLineRolledIntobottomLeftLinesTest = TestCase $ assertEqual
  "bottomRightLine +++> [bottomLeftLine, bottomLeftLine] = [TopFace, TopFace]"
  ([(BottomFace (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)), (BottomFace (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)) ]) 
  ((BottomRightLine (Point 1 1 1) (Point 1 1 1)) +++>  [(BottomLeftLine (Point 1 1 1) (Point 1 1 1)), (BottomLeftLine (Point 1 1 1) (Point 1 1 1))] )

{-
Combine the above 2 tests using |+++|
-}
topRightLineRolledIntoTopLeftLinesTestP4bottomRightLineRolledIntobottomLeftLinesTest = TestCase $ assertEqual
  "add a list of TopFaces to a list of BottomFaces"
  ([(CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)),
    (CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1))
   ]
  ) 
  (
   (TopRightLine (Point 1 1 1) (Point 1 1 1)) +++>  [(TopLeftLine (Point 1 1 1) (Point 1 1 1)), (TopLeftLine (Point 1 1 1) (Point 1 1 1))] 
   |+++|
   (BottomRightLine (Point 1 1 1) (Point 1 1 1)) +++>  [(BottomLeftLine (Point 1 1 1) (Point 1 1 1)), (BottomLeftLine (Point 1 1 1) (Point 1 1 1))] 
  )


{-
Test infix ordering of +++ vs +++>
-}
f2PlusPlusPlusB2PlusPlusFwdF3PlusPlusPlusB3 = TestCase $ assertEqual
  "F2 +++ B2 +++> [F3 +++ B3]"
  [(TopFace (Point 2 2 2) (Point 2 2 2) (Point 2 2 2) (Point 2 2 2))]
  ( (F2 (Point 2 2 2)) +++ (B2 (Point 2 2 2)) +++>  [(F3 (Point 2 2 2)) +++ (B3 (Point 2 2 2))])

-- =================================================== centers =============================================================
centerTest1 = TestCase $ assertEqual
  "centerTest1"
  (Point 2 1.5 0 )
  (center
    (BottomLeftLine
      (Point 4 0 0)
      (Point 0 3 0)
    )
  )

centerTest2 = TestCase $ assertEqual
  "centerTest2"
  (Point 2 1.5 0 )
  (center
    (BottomRightLine
      (Point 4 0 0)
      (Point 0 3 0)
    )
  )

centerTest3 = TestCase $ assertEqual
  "centerTest3"
  (Point 1 0.75 0 )
  (let
    bottomRightLine = 
      
       (BottomRightLine
         (Point 4 0 0)
         (Point 0 3 0)
       )
   in
    bottomRightLine <-|-> (B1 $ Point 0 0 0)
  )

-- ====================================================== distances =====================================================
distanceTest1  = TestCase $ assertEqual
  "distanceTest1"
  (Distance 2.5)
  (calculateDistance
    (BottomRightLine
      (Point 4 0 0)
      (Point 0 3 0)
    )
    (B1 $ Point 0 0 0)
  )

distanceTest2  = TestCase $ assertEqual
  "distanceTest2"
  (Distance 2.5)
  (calculateDistance
    (BottomRightLine
      (Point 4 0 0)
      (Point 0 3 0)
    )
    (F1 $ Point 0 0 0)
  )


distanceTest3  = TestCase $ assertEqual
  "distanceTest3"
  (Distance 2.5)
  (calculateDistance
    (BottomLeftLine
      (Point 4 0 0)
      (Point 0 3 0)
    )
    (B1 $ Point 0 0 0)
  )

distanceTest4  = TestCase $ assertEqual
  "distanceTest4"
  (Distance 2.5)
  (calculateDistance
    (BottomLeftLine
      (Point 4 0 0)
      (Point 0 3 0)
    )
    (F1 $ Point 0 0 0)
  )


--------------------------------------------------- ++++ of non-standard -----------------------------------------------

leftFaceNs =
  LeftFace
    {b1 = Point 0 0 0,
     b2 = Point 0 0 1,
     f1 = Point 1 1 0,
     f2 = Point 1 1 1
    }

frontLeftLineNs =
  FrontLeftLine
      {f1 = Point 1 1 0,
       f2= Point 1 1 1
      }

rightFaceNs =
  RightFace
      {b3 = Point 0 0 1,
       b4 = Point 0 0 0,
       f3 = Point 0 (-1) 1,
       f4 = Point 0 (-1) 0
      }
nonStandaredTest1  = TestCase $ assertEqual
  "nonStandaredTest1"
  (Right leftFaceNs  )
  ( rightFaceNs ++++ frontLeftLineNs
  )
