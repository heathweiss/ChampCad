module CornerPointsCollectionsTest(cornerPointsCollectionsTestDo) where

import Test.HUnit

import CornerPoints.Collections(CPointsCollection(..), (|+++|), (+++>)) 
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..))

import Data.Sequence(fromList)

cornerPointsCollectionsTestDo = do
  putStrLn "\n\n"
  putStrLn "CornerPointsCollectionsTest"
  runTestTT addListsTest
  runTestTT pushCornerPointIntoListTest
  runTestTT pushCornerPointIntoSeqenceTest
  runTestTT addSeqToSeqTest
  runTestTT addSeqToListTest
  runTestTT addListToSeqTest


addListsTest = TestCase $ assertEqual
  "add 2 CPointsList together"
  (CPointsList $
   [(CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1)),
    (CubePoints (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1) (Point 1 1 1))
   ]
  ) 
  (
   (
     (TopRightLine (Point 1 1 1) (Point 1 1 1)) +++>  ( CPointsList [(TopLeftLine (Point 1 1 1) (Point 1 1 1)), (TopLeftLine (Point 1 1 1) (Point 1 1 1))])
   )
   |+++|
   (
     (BottomRightLine (Point 1 1 1) (Point 1 1 1)) +++>  (CPointsList [(BottomLeftLine (Point 1 1 1) (Point 1 1 1)), (BottomLeftLine (Point 1 1 1) (Point 1 1 1))])
   )
  )

addSeqToSeqTest = TestCase $ assertEqual
 "CPointsSeq |+++| CPointsSeq"
 (CPointsSeq
  {cSeq = fromList
          [TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                    f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                    b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0},
                    f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}]}
 )
 ((CPointsSeq $ fromList $ [TopRightLine (Point 1 0 1) (Point 1 1 1)])
  |+++|
  (CPointsSeq $ fromList $ [(TopLeftLine (Point 0 0 1) (Point 0 1 1))])
 )

addSeqToListTest = TestCase $ assertEqual
 "CPointsSeq |+++| CPointsSeq"
 (CPointsSeq
  {cSeq = fromList
          [TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                    f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                    b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0},
                    f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}]}
 )
 ((CPointsSeq $ fromList $ [TopRightLine (Point 1 0 1) (Point 1 1 1)])
  |+++|
  (CPointsList [(TopLeftLine (Point 0 0 1) (Point 0 1 1))])
 )

addListToSeqTest = TestCase $ assertEqual
 "CPointsSeq |+++| CPointsSeq"
 (CPointsSeq
  {cSeq = fromList
          [TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                    f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                    b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0},
                    f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}]}
 )
 ((CPointsList [TopRightLine (Point 1 0 1) (Point 1 1 1)])
  |+++|
  (CPointsSeq $ fromList $ [(TopLeftLine (Point 0 0 1) (Point 0 1 1))])
 )

pushCornerPointIntoListTest = TestCase $ assertEqual
 "cornerpoint +++> CPointsList"
 (CPointsList
  {cList =
   [TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
             f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
             b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0},
             f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}},
    TopFace {b2 = Point {x_axis = 2.0, y_axis = 0.0, z_axis = 1.0},
             f2 = Point {x_axis = 2.0, y_axis = 1.0, z_axis = 1.0},
             b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
             f3 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}}]}
 )
 ((TopRightLine (Point 1 0 1) (Point 1 1 1)) +++>  ( CPointsList [(TopLeftLine (Point 0 0 1) (Point 0 1 1)), (TopLeftLine (Point 2 0 1) (Point 2 1 1))]))


pushCornerPointIntoSeqenceTest = TestCase $ assertEqual
 "cornerpoint +++> CPointsSeq"
 (CPointsSeq
  {cSeq = fromList
   [TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
             f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
             b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0},
             f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}},
    TopFace {b2 = Point {x_axis = 2.0, y_axis = 0.0, z_axis = 1.0},
             f2 = Point {x_axis = 2.0, y_axis = 1.0, z_axis = 1.0},
             b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
             f3 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}}]}
 )
 ((TopRightLine (Point 1 0 1) (Point 1 1 1)) +++>  ( CPointsSeq $ fromList [(TopLeftLine (Point 0 0 1) (Point 0 1 1)), (TopLeftLine (Point 2 0 1) (Point 2 1 1))]))
