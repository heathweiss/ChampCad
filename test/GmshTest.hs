module GmshTest(gmshTestDo) where

import qualified GMSH.Hashable.Points as GP --(insert, Changes(..))
import qualified GMSH.Lines as GL --(toLines)
import qualified GMSH.Common as GC

import Test.HUnit
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (===), (|===|))


gmshTestDo = do
  putStrLn "" 
  putStrLn "gmsh tests"
  runLinesTests
  runHashPointTests

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------- GMSH.Lines ---------------------------------------------------------
{- Run all the test for hashing and inserting points into a hash map.-}
runLinesTests = do
  runTestTT linesTestB1
  runTestTT linesTestB2
  runTestTT linesTestB3
  runTestTT linesTestB4
  runTestTT linesTestF1
  runTestTT linesTestF2
  runTestTT linesTestF3
  runTestTT linesTestF4

  runTestTT linesTestBackFace
  runTestTT linesTestFrontFace

  runTestTT insertBackTopLineIntoEmptyMap
  runTestTT insertBackTopLineIntoMap
  runTestTT insertFrontFaceIntoEmptyMap
  runTestTT insertUnhandledCPointIntoEmptyMap
  
------------------------------------------------------------------------------------
--ensure no missing pattern matches for the corners which can't be converted to lines
-------------------------------------------------------------------------------------
linesTestB1 = TestCase $ assertEqual
  "extract Line from B1"
  (Left "B1 can't be converted into a line.")
  (GL.toLines $ B1 $ Point 1 1 1)

linesTestB2 = TestCase $ assertEqual
  "extract Line from B2"
  (Left "B2 can't be converted into a line.")
  (GL.toLines $ B2 $ Point 1 1 1)

linesTestB3 = TestCase $ assertEqual
  "extract Line from B3"
  (Left "B3 can't be converted into a line.")
  (GL.toLines $ B3 $ Point 1 1 1)

linesTestB4 = TestCase $ assertEqual
  "extract Line from B4"
  (Left "B4 can't be converted into a line.")
  (GL.toLines $ B4 $ Point 1 1 1)

linesTestF1 = TestCase $ assertEqual
  "extract Line from F1"
  (Left "F1 can't be converted into a line.")
  (GL.toLines $ F1 $ Point 1 1 1)

linesTestF2 = TestCase $ assertEqual
  "extract Line from F2"
  (Left "F2 can't be converted into a line.")
  (GL.toLines $ F2 $ Point 1 1 1)

linesTestF3 = TestCase $ assertEqual
  "extract Line from F3"
  (Left "F3 can't be converted into a line.")
  (GL.toLines $ F3 $ Point 1 1 1)

linesTestF4 = TestCase $ assertEqual
  "extract Line from F4"
  (Left "F4 can't be converted into a line.")
  (GL.toLines $ F4 $ Point 1 1 1)

-----------------------------------------------------------------
--extract lines from valid Cubes and Faces
-----------------------------------------------------------------
linesTestBackFace = TestCase $ assertEqual
  "extract lines from BackFace"
  (Right True
  )
  ( let
      temp = (GL.toLines $ BackFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4))
    in
      case temp of
        Left e -> Left e
        Right val ->
          val
          |===|
          [BackLeftLine {b1 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, b2 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}},
           BackTopLine {b2 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}, b3 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}},
           BackRightLine {b3 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}, b4 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}},
           BackBottomLine {b1 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, b4 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}}
          ]
  )

linesTestFrontFace = TestCase $ assertEqual
  "extract lines from FrontFace"
  (Right True
  )
  ( let
      temp = (GL.toLines $ FrontFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4))
    in
      case temp of
        Left e -> Left e
        Right val ->
          val
          |===|
          [FrontLeftLine {f1 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f2 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}},
           FrontTopLine {f2 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}, f3 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}},
           FrontRightLine {f3 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}, f4 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}},
           BottomFrontLine {f1 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}}
          ]
  )


-----------------------------------------------------------------------------------------
--insert CornerPoints into a hash map
-----------------------------------------------------------------------------------------
insertBackTopLineIntoEmptyMap  = TestCase $ assertEqual
  "insert a BackTopLine into an empty map."
  (Right $ GC.Changed $ HM.fromList [(1497486222234613753,1)])
  (
   let
     backTopLine = BackTopLine (Point 1 1 1) (Point 2 2 2)
   in
     GL.insert backTopLine 1 HM.empty
  )

insertBackTopLineIntoMap  = TestCase $ assertEqual
  "insert a BackTopLine into a map that already contains it."
  (Right $ GC.UnChanged $ HM.fromList [(1497486222234613753,1)])
  (
   let
     backTopLine = BackTopLine (Point 1 1 1) (Point 2 2 2)
   in
     GL.insert backTopLine 1 $ HM.fromList [(1497486222234613753,1)]
  )

insertFrontFaceIntoEmptyMap  = TestCase $ assertEqual
  "insert a FrontFace into an empty map. FrontFace cannot be inserted, only Lines."
  (Left "GMSH.Lines.insert: FrontFace cannot be inserted.")
  (
   let
     frontFace = FrontFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 3 3 3)
   in
     GL.insert frontFace 1 HM.empty
  )

insertUnhandledCPointIntoEmptyMap  = TestCase $ assertEqual
  "insert a TopFace into an empty map. TopFace will have missing pattern match."
  (Left "GMSH.Lines.insert: missing pattern match for TopFace")
  (
   let
     topFace = TopFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 3 3 3)
   in
     GL.insert topFace 1 HM.empty
  )
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------- GMSH.Hashable.Points ---------------------------------------------------------
{- Run all the test for hashing and inserting points into a hash map.-}
runHashPointTests = do
  runTestTT hashWithSaltPointTest
  runTestTT hashPointTest
  runTestTT insertPointTest
  runTestTT insertPointTest2
  runTestTT insertPointTest3

----------------------------------------------------------------------
--Shows how to use Data.HashMap.Strict and Data.Hashable hash and hashWiuthSalt fx's
----------------------------------------------------------------------

--Hash a point, using a salt, and insert into a hashmap.
hashWithSaltPointTest = TestCase $ assertEqual
  "run hashWithSalt on a Point"
  (HM.fromList [(2171024669747360587,1)])
  (HM.insert (H.hashWithSalt 1 $ Point 1 2 3) 1 HM.empty)

hashPointTest = TestCase $ assertEqual
  "run hash on a Point"
  (HM.fromList [(2171024669747360587,1)])
  (HM.insert (H.hash $ Point 1 2 3) 1 HM.empty)

----------------------------------------------------------------------
--Shows how to do use GMSH.Hashable.Points insert
----------------------------------------------------------------------
--insert a hashed point, and dummy value into an emtpy map.
--As it is empty, it will be hashed and inserted.
insertPointTest = TestCase $ assertEqual
  "insert a Point into an empty map"
  (GC.Changed $ HM.fromList [(2171024669747360587,1)])
  (GP.insert  (Point 1 2 3) 1 HM.empty)

--Insert a Point into a map that already contains the point.
--As it is already in the map, map will not be modified, as indicated by the GP.UnChanged constructor.
insertPointTest2 = TestCase $ assertEqual
  "insert a Point into a map that already contains the point"
  (GC.UnChanged $ HM.fromList [(2171024669747360587,1)])
  ( let
      pointAllreadyInMap = Point 1 2 3
      dummyVal1 = 1
      dummyVal2 = 2
      mapWithThePointAlreadyInserted = HM.insert (H.hash pointAllreadyInMap) dummyVal1 HM.empty
      
    in
      GP.insert pointAllreadyInMap dummyVal2 mapWithThePointAlreadyInserted
  )

--Insert a Point into a hash map that already contains a different point.
--The point will be inserted, as it does not already exist.
--The hashmap will be modified, as indicated by the GP.Changed constructor.
insertPointTest3 = TestCase $ assertEqual
  "insert a Point into a map that already contains a diff. point"
  (GC.Changed $ HM.fromList [(2171024669747360587,1),(2177780069188416331,2)])
  ( let
      preExistentPoint = Point 1 2 3
      nonExistentPoint = Point 1 2 4
      dummyVal1 = 1
      dummyVal2 = 2
      mapWithADiffPointAlreadyInserted = HM.insert  (H.hash preExistentPoint) dummyVal1 HM.empty
      
    in
      GP.insert nonExistentPoint dummyVal2 mapWithADiffPointAlreadyInserted
  )
