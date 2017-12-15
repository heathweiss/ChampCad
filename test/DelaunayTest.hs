module DelaunayTest(delaunayTestDo) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@),
                                cornerPointsError, isCubePoints, isCubePointsList)
import CornerPoints.Points (Point(..), center ,(<-|->))

import Joiners.Delaunay(delaunay, removeIfUsed)

delaunayTestDo = do

  putStrLn ""
  putStrLn "delaunayTestDo"
  runTestTT test1
  

leftFace =
  LeftFace
    {b1 = Point 0 0 0,
     b2 = Point 0 0 1,
     f1 = Point 1 1 0,
     f2 = Point 1 1 1
    }

frontLeftLine =
  FrontLeftLine
      {f1 = Point 1 1 0,
       f2= Point 1 1 1
      }

rightFace =
  RightFace
      {b3 = Point 0 0 1,
       b4 = Point 0 0 0,
       f3 = Point 0 (-1) 1,
       f4 = Point 0 (-1) 0
      }

test1 = TestCase $ assertEqual
  "test1"
   (Right [])
   (let
         f1 = Point 0 1 0
         f2 = Point 0 1 1
         b1 = Point 0 0 0
         b2 = Point 0 0 1
         frontLeftLine  = FrontLeftLine f1 f2
         leftFace = LeftFace   b1 b2 f1 f2
         
    in
    
     removeIfUsed
      [frontLeftLine]
      leftFace
   )
