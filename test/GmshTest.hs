{-# LANGUAGE TemplateHaskell #-}
module GmshTest(gmshTestDo) where

import qualified GMSH.Points as GP --(insert, Changes(..))
import qualified GMSH.Lines as GL --(toLines)
import qualified GMSH.Common as GC
import qualified GMSH.Builder as GB
import qualified  GMSH.Writer as GW

import Test.HUnit
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (===), (|===|))

import Control.Lens
makeLenses ''GC.BuilderData

gmshTestDo = do
  putStrLn "" 
  putStrLn "gmsh tests"

  runBuildWithMonadTests
  runBuilderTests 
  runHashPointTests
  runLinesTests
  runWriterTests

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------- GMSH.Writer ---------------------------------------------------------
{- Run all the test for writing points/lines. Does not test writing to console/file. -}
runWriterTests = do
  
  runTestTT writePointsTest


writePointsTest = TestCase $ assertEqual
  "writePointsTest"
  ("Point(1) = {1.0,1.0,1.0};")
  (GW.gmshPointString (Point 1 1 1) 1)

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
  runTestTT insertBackTopLineIntoEmptyMapAndLookAtRemainingIds
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
{-
Insert CornerPoints into a hash map, without using the builder monad.
-}
-----------------------------------------------------------------------------------------
insertBackTopLineIntoEmptyMap  = TestCase $ assertEqual
  "insert a BackTopLine into an empty map."
  --(Right $ HM.fromList [(1497486222234613753,1)])
  (Right $ GC.newBuilderData {GC._linesMap =  HM.fromList [(1497486222234613753,1)]})
  (
   let
     backTopLine = BackTopLine (Point 1 1 1) (Point 2 2 2)
   in
     GL.insert backTopLine GC.newBuilderData
  )

insertBackTopLineIntoEmptyMapAndLookAtRemainingIds  = TestCase $ assertEqual
  "insert a BackTopLine into an empty map and see that the [id] has been reduced by 1."
  (2)
  (
   let
     backTopLine = BackTopLine (Point 1 1 1) (Point 2 2 2)
     inserted  = GL.insert backTopLine GC.newBuilderData --[1..] [] HM.empty
   in
     case inserted of
       Right builderData -> head $ builderData ^. linesId
  )

insertBackTopLineIntoMap  = TestCase $ assertEqual
  "insert a BackTopLine into a map that already contains it."
  (Right $ GC.newBuilderData {GC._linesMap = HM.fromList [(1497486222234613753,1)]})
  (
   let
     backTopLine = BackTopLine (Point 1 1 1) (Point 2 2 2)
   in
     GL.insert backTopLine $ GC.newBuilderData {GC._linesMap = HM.fromList [(1497486222234613753,1)]}
  )

insertFrontFaceIntoEmptyMap  = TestCase $ assertEqual
  "insert a FrontFace into an empty map."
  (Right $ GC.newBuilderData {GC._linesMap =  (HM.fromList [(-405284620329420807,2),(6588805600976959481,4),(-7473684235487414279,3),(1497486222234613753,1)])})
  (
   let
     frontFace = FrontFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 3 3 3)
   in
     GL.insert frontFace GC.newBuilderData --[1..] [] HM.empty
  )

insertUnhandledCPointIntoEmptyMap  = TestCase $ assertEqual
  "insert a TopFace into an empty map. TopFace will have missing pattern match."
  (Left "GMSH.Lines.insert: missing pattern match for TopFace")
  (
   let
     topFace = TopFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 3 3 3)
   in
     GL.insert topFace GC.newBuilderData
  )

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------- GMSH.Builder ------- ---------------------------------------------------------
{-
Extract values from the GMSH.Builder  using <exec/eval/run>State.
The State can't actually be built within the Builder monad, so manually create the BuilderData.
Could change this once it can be built.

------------------------go back to this once GC.Changes is deleted to simplify.
-}
runBuilderTests = do
  runTestTT builderTest
  runTestTT builderTest2
  runTestTT builderTest3
  
builderTest = TestCase $ assertEqual
  "Use execState to extract the current state."
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)]) (HM.fromList []) [] [])
  (let
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "builderTest: " [BackTopLine (Point 1 1 1) (Point 11 11 11)]
        return inserted
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
                                           
   
  )

builderTest2 = TestCase $ assertEqual
  "Use evalState to extract the current value from state."
  (Right [B1 $ Point 1 1 1])
  (let
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "builderTest2" [B1 $ Point 1 1 1]
        return inserted
      
   in
   ((SL.evalState $ E.runExceptT builder ) GB.newBuilderData)
   )

builderTest3 = TestCase $ assertEqual
  "Use runState to extract the current value/state from state."
  ((Right $ [BackTopLine (Point 1 1 1) (Point 11 11 11)], (GB.BuilderData (HM.fromList [(2050866026447763449,1)]) (HM.fromList []) [] [])))
  (let
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "builderTest3" [BackTopLine (Point 1 1 1) (Point 11 11 11)]
        return inserted
      
   in
   ((SL.runState $ E.runExceptT builder ) GB.newBuilderData)
  )


-----------------------------------------------------------
--work with a single [CornerPoints]
-----------------------------------------------------------
runBuildWithMonadTests = do
  runTestTT buildWithMonadTest
  runTestTT buildWithMonadTestWithoutBuilder
  runTestTT buildWithMonadTest2
  runTestTT buildWithMonadTest3
  runTestTT buildWithMonadTest4
  runTestTT buildWithMonadTest5
  runTestTT buildWithMonadTest6
  
--insert valid CornerPoints line into the state that has no pre-existing lines.
--cx the Lines map to see it was inserted
buildWithMonadTest = TestCase $ assertEqual
  "Use execState to extract the current state."
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)]) (HM.fromList []) [] [])
  (let
      validPointToInsert = BackTopLine (Point 1 1 1) (Point 11 11 11)

      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        --code not yet ready to insert the B1 inside the Builder so just return the B1 manually.
        inserted <- GB.buildCubePointsListSingle "testing" [validPointToInsert]
        return inserted
      
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
  )

buildWithMonadTestWithoutBuilder = TestCase $ assertEqual
  "Use execState to extract the current state. Dont use Builder to see if that is where my error lies."
  (Right $ GB.BuilderData (HM.fromList [(2050866026447763449,1)]) (HM.fromList []) [] [])
  (let
      validPointToInsert = BackTopLine (Point 1 1 1) (Point 11 11 11)

      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        --code not yet ready to insert the B1 inside the Builder so just return the B1 manually.
        inserted <- GB.buildCubePointsListSingle "testing" [validPointToInsert]
        return inserted
      
   in
   (GL.insert validPointToInsert GB.newBuilderData)
  )

--try to insert CornerPointsError into the state that has no pre-existing lines.
--cx the Lines map to see it was not inserted
buildWithMonadTest2 = TestCase $ assertEqual
  "Use execState to extract the current state when inserting CornerPointsError"
  (GB.BuilderData (HM.fromList []) (HM.fromList []) [] [])
  (let
      invalidPointToInsert = CornerPointsError "error"

      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "testing" [invalidPointToInsert]
        return inserted
      
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
  )

{-
Insert [valid CPts line, valid CPts line] into the state that has no pre-existing lines.
-they are different BackTopLine's, so both should be inserted.

It also has sequential gmsh line ID's starting at 1, which is req'd to pass test.
-}
buildWithMonadTest3 = TestCase $ assertEqual
  "Use execState to extract the current state."
  (GB.BuilderData (HM.fromList [(2050866026447763449,1),(-4228383307129817095,2)]) (HM.fromList []) [] [])
  (let
      validPointsToInsert = [BackTopLine (Point 1 1 1) (Point 11 11 11), BackTopLine (Point 21 21 21) (Point 211 211 211)]

      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "testing" validPointsToInsert
        return inserted
      
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
  )

{-
Insert [BackTopLine, identical BackTopLine].
Only one gets inserted.
-}
buildWithMonadTest4 = TestCase $ assertEqual
  "Identical BackTopLine's. Only one gets inserted."
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)]) (HM.fromList []) [] [])
  (let
      backTopLine = BackTopLine (Point 1 1 1) (Point 11 11 11)
      
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "testing" [backTopLine,backTopLine]
        return inserted
      
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
  )

{-
Insert [B1, BackTopLine, B1].
Only one BackTopLine gets inserted into map, as B1 is not a line
-}
buildWithMonadTest5 = TestCase $ assertEqual
  "BackTopLine and a B1 Only BackTopLine gets inserted."
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)]) (HM.fromList []) [] [])
  (let
      backTopLine = BackTopLine (Point 1 1 1) (Point 11 11 11)
      b1 = (B1 (Point 1 1 1))
      b2 = (B1 (Point 21 21 21))
      
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "testing" [b1,backTopLine,b2]
        return inserted
      
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
  )

{-
Insert [B1, BackTopLine, B1].
Only the BackTopLine gets inserted inserted into the map as B1 is not a line,
however all the CornerPoints are put into 'a' of the (a,s)
-}
buildWithMonadTest6 = TestCase $ assertEqual
  "BackTopLine and a B1 Only BackTopLine gets inserted."
  (Right
   [B1 {b1 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}},
    BackTopLine {b2 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                 b3 = Point {x_axis = 11.0, y_axis = 11.0, z_axis = 11.0}},
    B1 {b1 = Point {x_axis = 21.0, y_axis = 21.0, z_axis = 21.0}}]
  )
  (let
      backTopLine = BackTopLine (Point 1 1 1) (Point 11 11 11)
      b1 = (B1 (Point 1 1 1))
      b2 = (B1 (Point 21 21 21))
      
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "testing" [b1,backTopLine,b2]
        return inserted
      
   in
   ((SL.evalState $ E.runExceptT builder ) GB.newBuilderData)
  )

--next
--pass in a B1 as 2nd cpnt, to see that it does not get inserted.
--pass in a B1 as 1st cpnt, to see that it does not get inserted, but 2nd one does.
-----------------------------------------------------------------------------------------
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
removeIDS_noEither :: (HM.HashMap Int Int,[Int]) -> (HM.HashMap Int Int)
removeIDS_noEither (hashmap, _) =  hashmap

--insert a hashed point, and dummy value into an emtpy map.
--As it is empty, it will be hashed and inserted.
insertPointTest = TestCase $ assertEqual
  "insert a Point into an empty map"
  (GC.BuilderData HM.empty ( HM.fromList [(2171024669747360587,1)]) [1..] [1..])
  (GP.insert  [Point 1 2 3] $ GC.BuilderData HM.empty HM.empty [1..] [1..]) -- [1..] HM.empty)


--Insert a Point into a map that already contains the point.
--As it is already in the map, map will not be modified, as indicated by the GP.UnChanged constructor.
insertPointTest2 = TestCase $ assertEqual
  "insert a Point into a map that already contains the point"
  (GC.BuilderData HM.empty (HM.fromList [(2171024669747360587,1)]) [1..] [1..])
  ( let
      pointAllreadyInMap = Point 1 2 3
      dummyVal1 = 1
      dummyVal2 = 2
      mapWithThePointAlreadyInserted = HM.insert (H.hash pointAllreadyInMap) dummyVal1 HM.empty
      
    in
      GP.insert [pointAllreadyInMap] $ GC.BuilderData HM.empty mapWithThePointAlreadyInserted [1..] [1..]  --[2..] mapWithThePointAlreadyInserted
  )

--Insert a Point into a hash map that already contains a different point.
--The point will be inserted, as it does not already exist.
--The hashmap will be modified, as indicated by the GP.Changed constructor.
insertPointTest3 = TestCase $ assertEqual
  "insert a Point into a map that already contains a diff. point"
  (GC.BuilderData HM.empty (HM.fromList [(2171024669747360587,1),(-8294074226866474165,2)]) [1..] [1..] )
  ( let
      mapWithADiffPointAlreadyInserted = HM.insert  (H.hash $ Point 1 2 3) 1 HM.empty
      
    in
      GP.insert [Point 11 22 33] $ GC.BuilderData HM.empty mapWithADiffPointAlreadyInserted [1..] [2..] 
  )

