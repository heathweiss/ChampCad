{-# LANGUAGE TemplateHaskell #-}
module GmshLinesTest(gmshLinesTestDo) where

import qualified GMSH.Points as GP 
import qualified GMSH.Lines as GL 
import qualified GMSH.Common as GC
import qualified GMSH.Builder as GB
import qualified  GMSH.Writer as GW

import Test.HUnit
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified Control.Monad.Writer as W

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (===), (|===|))

import Control.Lens
makeLenses ''GC.BuilderData

gmshLinesTestDo = do
  putStrLn "" 
  putStrLn "gmsh tests"

  runBuildWithMonadTests
  runBuilderTests 
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
  runTestTT insertBottomFrontLineIntoMap
  runTestTT insertFrontFaceIntoEmptyMap
  runTestTT insertFrontLeftLineIntoEmptyMap
  runTestTT insertFrontTopLineIntoEmptyMap
  runTestTT insertFrontTopLineAndFrontLeftLineIntoEmptyMap
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
  (Right $ GC.newBuilderData
   {GC._pointsMap =
      HM.fromList [(-4271374597206133941,GC.PointsBuilderData 2 (Point 2 2 2)),
                   (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))],
    GC._linesMap = HM.fromList [(1497486222234613753,1)]
   }
  )
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

insertBottomFrontLineIntoMap  = TestCase $ assertEqual
  "insert a BackTopLine into a map that already contains it."
  (Right $ GC.newBuilderData
             {GC._linesMap = HM.fromList [(1497486222234613753,1)],
              GC._pointsMap = HM.fromList [(-4271374597206133941,GC.PointsBuilderData 2 (Point 2 2 2)),
                                           (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))]
             }
  )
  (
   let
     bottomFrontLine = BottomFrontLine (Point 1 1 1) (Point 2 2 2)
   in
     GL.insert bottomFrontLine $ GC.newBuilderData {GC._linesMap = HM.fromList []}
  )

insertFrontFaceIntoEmptyMap  = TestCase $ assertEqual
  "insert a FrontFace into an empty map."
  (Right $ GC.newBuilderData
   {GC._linesMap =  (HM.fromList [(-6980540076290344967,3),(-405284620329420807,2),(-8685152535250077703,4),(1497486222234613753,1)]),
    GC._pointsMap =
      (HM.fromList [(819944781536211787,GC.PointsBuilderData 3 (Point 3 3 3)),
                    (5911264160278557515,GC.PointsBuilderData 4 (Point 4 4 4)),
                    (-4271374597206133941,GC.PointsBuilderData 2 (Point 2 2 2)),
                    (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))
                   ]
      )
   }
  )
  (
   let
     frontFace = FrontFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4)
   in
     GL.insert frontFace GC.newBuilderData --[1..] [] HM.empty
  )

insertFrontLeftLineIntoEmptyMap  = TestCase $ assertEqual
  "insert a FrontLeftLine into an empty map."
  (Right $ GC.newBuilderData
   {GC._linesMap =  (HM.fromList [(1497486222234613753,1)]),
    GC._pointsMap =
      (HM.fromList [(-4271374597206133941,GC.PointsBuilderData 2 (Point 2 2 2)),
                    (3308183575658410827, GC.PointsBuilderData 1 (Point 1 1 1))]) 
   }
  )
  (
   let
     frontLeftLine = FrontLeftLine (Point 1 1 1) (Point 2 2 2) 
   in
     GL.insert frontLeftLine GC.newBuilderData --[1..] [] HM.empty
  )  

insertFrontTopLineIntoEmptyMap  = TestCase $ assertEqual
  "insert a FrontLeftLine into an empty map."
  (Right $ GC.newBuilderData
   {GC._linesMap =  (HM.fromList [(-405284620329420807,1)]),
    GC._pointsMap =
      (HM.fromList [(819944781536211787,GC.PointsBuilderData 2 (Point 3 3 3)),
                    (-4271374597206133941,GC.PointsBuilderData 1 (Point 2 2 2))
                   ]
      )
   }
  )
  (
   let
     frontTopLine = FrontTopLine (Point 2 2 2) (Point 3 3 3)  
   in
     GL.insert frontTopLine GC.newBuilderData --[1..] [] HM.empty
  )

insertFrontTopLineAndFrontLeftLineIntoEmptyMap  = TestCase $ assertEqual
  "insert a FrontLeftLine and FrontLeftLine into an empty map."
  (Right $ GC.newBuilderData
   {GC._linesMap =  (HM.fromList [(-405284620329420807,2),(1497486222234613753,1)]),
    GC._pointsMap = (HM.fromList [(819944781536211787,GC.PointsBuilderData 3 (Point 3 3 3)),
                                  (-4271374597206133941,GC.PointsBuilderData 2 (Point 2 2 2)),
                                  (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))
                                 ]
                    )
   }
  )
  (
   let
     frontLeftLine = FrontLeftLine (Point 1 1 1) (Point 2 2 2) 
     frontTopLine = FrontTopLine (Point 2 2 2) (Point 3 3 3)
     builderDataWithFrontLeftLineInserted = GL.insert frontLeftLine GC.newBuilderData
   in
     case builderDataWithFrontLeftLineInserted of
       Right builderDataWithFrontLeftLineInserted' -> GL.insert frontTopLine builderDataWithFrontLeftLineInserted'
       Left e -> Left e
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
--These tests have been removed till I as I added IO to the stack, and how do I test that?
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
  runTestTT builderTest4
  

builderTest = TestCase $ assertEqual
  "Use execState to extract the current state."
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)])
                  (HM.fromList [(-6488834463732681909,GC.PointsBuilderData 2 (Point 11 11 11)),
                                (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))])
    [] []
  )
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
  (Right $ [BackTopLine (Point 1 1 1) (Point 11 11 11)],
            (GB.BuilderData (HM.fromList [(2050866026447763449,1)])
                            (HM.fromList [(-6488834463732681909, GC.PointsBuilderData 2 (Point 11 11 11)),
                                          (3308183575658410827, GC.PointsBuilderData 1 (Point 1 1 1))]
                            )
              [] []
            )
   )
  
  (let
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "builderTest3" [BackTopLine (Point 1 1 1) (Point 11 11 11)]
        return inserted
      
   in
   ((SL.runState $ E.runExceptT builder ) GB.newBuilderData)
  )

builderTest4 = TestCase $ assertEqual
  "Use execState to extract the current state."
  (GC.newBuilderData
   {GC._linesMap =  (HM.fromList [(-6980540076290344967,3),(-405284620329420807,2),(-8685152535250077703,4),(1497486222234613753,1)]),
    GC._pointsMap = (HM.fromList [(819944781536211787,GC.PointsBuilderData 3 (Point 3 3 3)),
                                  (5911264160278557515,GC.PointsBuilderData 4 (Point 4 4 4)),
                                  (-4271374597206133941,GC.PointsBuilderData 2 (Point 2 2 2)),
                                  (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))])
   }
  )
  (let
      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "builderTest: " [FrontFace (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4)]
        return inserted
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
                                           
   
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
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)])
                  (HM.fromList [(-6488834463732681909,GC.PointsBuilderData 2 (Point 11 11 11)),(3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))])
                  [] []
  )
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
  (Right $ GB.BuilderData (HM.fromList [(2050866026447763449,1)])
                          (HM.fromList [(-6488834463732681909,GC.PointsBuilderData 2 (Point 11 11 11)),
                                        (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))])
                          [] []
  )
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
  (GB.BuilderData
   (HM.fromList [(2050866026447763449,1),(-4228383307129817095,2)])
   (HM.fromList [(6408630444126286667,GC.PointsBuilderData 3 (Point 21 21 21)),
                 (-6488834463732681909,GC.PointsBuilderData 2 (Point 11 11 11)),
                 (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1)),
                 (-3101670147112914101,GC.PointsBuilderData 4 (Point 211 211 211))])
   [] []
  )
  (let
      validPointsToInsert = [BackTopLine (Point 1 1 1) (Point 11 11 11), BackTopLine (Point 21 21 21) (Point 211 211 211)]

      builder :: GB.ExceptStackCornerPointsBuilder
      builder = do
        inserted <- GB.buildCubePointsListSingle "testing" validPointsToInsert
        return inserted
      
   in
   ((SL.execState $ E.runExceptT builder ) GB.newBuilderData)
  )

buildWithMonadTest4 = TestCase $ assertEqual
  "Identical BackTopLine's. Only one gets inserted."
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)])
                  (HM.fromList [(-6488834463732681909,GC.PointsBuilderData 2 (Point 11 11 11)),
                                (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))]) [] [])
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
  (GB.BuilderData (HM.fromList [(2050866026447763449,1)])
                  (HM.fromList [(-6488834463732681909,GC.PointsBuilderData 2 (Point 11 11 11)),
                                (3308183575658410827,GC.PointsBuilderData 1 (Point 1 1 1))])
                  [] []
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


