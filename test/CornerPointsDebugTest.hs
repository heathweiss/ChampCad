module CornerPointsDebugTest(cornerPointsDebugTestDo) where
import Test.HUnit
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|))
import CornerPoints.Points (Point(..)) 
import CornerPoints.Debug((+++^?), (++^?), CubeName(..), CubeDebug(..), CubeDebugs(..), showDebugMsg)



cornerPointsDebugTestDo = do
  putStrLn ""
  putStrLn "CornerPointsDebugTest"
  --lines
  runTestTT backBottomLinePlusBackBottomLine
  runTestTT backBottomLinesPlusBackBottomLines
  runTestTT backBottomLinesPlusBackBottomLineFrontBottomLine
  runTestTT backTopLinePlusBackTopLine
  runTestTT bottomFrontLinePlusBottomFrontLine
  runTestTT frontTopLinePlusFrontTopLine
  --faces
  runTestTT frontFacePlusFrontFace
  runTestTT backFacePlusBackFace
  runTestTT topFacePlusTopFace
  runTestTT bottomFacePlusBottomFace
  runTestTT leftFacePlusLeftFace
  

------------------------------- lines +++ lines ---------------------------

backBottomLinePlusBackBottomLine = TestCase $ assertEqual
  "BackBottomLine +++ BackBottomLine = CornerPointsError"
  (CubeDebug "BackBottomLines: illegal BackBottomLine +++ BackBottomLine operation") 
  (
   (CubeName "BackBottomLines")
   ++^?
   ((BackBottomLine (Point 11 11 11) (Point 11 11 11))
    +++ 
   (BackBottomLine (Point 11 11 11) (Point 11 11 11)))
  )

{-Add 2 lists of backBottomLines.
  Will result in a list of errors.-}
backBottomLinesPlusBackBottomLines = TestCase $ assertEqual
  "BackBottomLines +++ BackBottomLines = CornerPointsError"
  (
   [
    (CubeDebug "BackBottomLines: illegal BackBottomLine +++ BackBottomLine operation"),
    (CubeDebug "BackBottomLines: illegal BackBottomLine +++ BackBottomLine operation")
   ]
  )
  (
   ([CubeName "BackBottomLines" | x <- [1..]])
   +++^?
   ([(BackBottomLine (Point 11 11 11) (Point 11 11 11)), (BackBottomLine (Point 11 11 11) (Point 11 11 11))]
    |+++| 
   [(BackBottomLine (Point 11 11 11) (Point 11 11 11)), (BackBottomLine (Point 11 11 11) (Point 11 11 11))])
  )

{-Add a list of backBottomLines to a list with BackBottomLine and a FrontBottomLine.
  Will result in a list of 1 error and 1 non-error.-}
backBottomLinesPlusBackBottomLineFrontBottomLine = TestCase $ assertEqual
  "BackBottomLines +++ BackAndFrontBottomLines = CornerPointsError"
  (
   [
    (CubeDebug "BackBottomLines: illegal BackBottomLine +++ BackBottomLine operation"),
    (CubeDebug "BackBottomLines: good")
   ]
  )
  (
   ([CubeName "BackBottomLines" | x <- [1..]])
   +++^?
   ([(BackBottomLine (Point 11 11 11) (Point 11 11 11)), (BackBottomLine (Point 11 11 11) (Point 11 11 11))]
    |+++| 
   [(BackBottomLine (Point 11 11 11) (Point 11 11 11)), (BottomFrontLine (Point 11 11 11) (Point 11 11 11))])
  )


backTopLinePlusBackTopLine = TestCase $ assertEqual
  "BackTopLine +++ BackTopLine = CornerPointsError"
  (CubeDebug "BackTopLines: illegal BackTopLine +++ BackTopLine operation") 
  (
   (CubeName "BackTopLines")
   ++^?
   ((BackTopLine (Point 11 11 11) (Point 11 11 11))
    +++ 
   (BackTopLine (Point 11 11 11) (Point 11 11 11)))
  )

bottomFrontLinePlusBottomFrontLine  = TestCase $ assertEqual
  "BottomFrontLine +++ BottomFrontLine = CornerPointsError"
  (CubeDebug "BottomFrontLines: illegal BottomFrontLine +++ BottomFrontLine operation") 
  (
   (CubeName "BottomFrontLines")
   ++^?
   ((BottomFrontLine (Point 11 11 11) (Point 11 11 11))
    +++ 
   (BottomFrontLine (Point 11 11 11) (Point 11 11 11)))
  )

frontTopLinePlusFrontTopLine  = TestCase $ assertEqual
  "FrontTopLine +++ FrontTopLine = CornerPointsError"
  (CubeDebug "FrontTopLines: illegal FrontTopLine +++ FrontTopLine operation") 
  (
   (CubeName "FrontTopLines")
   ++^?
   ((FrontTopLine (Point 11 11 11) (Point 11 11 11))
    +++ 
   (FrontTopLine (Point 11 11 11) (Point 11 11 11)))
  )

-------------------------------- faces +++ faces --------------------------
backFacePlusBackFace = TestCase $ assertEqual
  "BackFace +++ BackFace = CornerPointsError"
  (CubeDebug "BackFaces: illegal BackFace +++ BackFace operation") 
  (
   (CubeName "BackFaces")
   ++^?
   ((BackFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11))
    +++ 
   (BackFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11)))
  )

bottomFacePlusBottomFace = TestCase $ assertEqual
  "BottomFace +++ BottomFace = CornerPointsError"
  (CubeDebug "BottomFaces: illegal BottomFace +++ BottomFace operation") 
  (
   (CubeName "BottomFaces")
   ++^?
   ((BottomFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11))
    +++ 
   (BottomFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11)))
  )
 

frontFacePlusFrontFace = TestCase $ assertEqual
  "FrontFace +++ FrontFace = CornerPointsError"
  (CubeDebug "FrontFaces: illegal FrontFace +++ FrontFace operation") 
  (
   (CubeName "FrontFaces")
   ++^?
   ((FrontFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11))
    +++ 
   (FrontFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11)))
  )

leftFacePlusLeftFace = TestCase $ assertEqual
  "LeftFace +++ LeftFace = CornerPointsError"
  (CubeDebug "LeftFaces: illegal LeftFace +++ LeftFace operation") 
  (
   (CubeName "LeftFaces")
   ++^?
   ((LeftFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11))
    +++ 
   (LeftFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11)))
  )


topFacePlusTopFace = TestCase $ assertEqual
  "TopFace +++ TopFace = CornerPointsError"
  (CubeDebug "TopFaces: illegal TopFace +++ TopFace operation") 
  (
   (CubeName "TopFaces")
   ++^?
   ((TopFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11))
    +++ 
   (TopFace (Point 2 2 2) (Point 2 2 2) (Point 11 11 11) (Point 11 11 11)))
  )
