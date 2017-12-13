module JoinerTest(joinerTestDo) where

import Test.HUnit

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|))
import CornerPoints.FaceExtraction(extractFrontRightLine, extractFrontLeftLine)
import CornerPoints.FaceConversions(toFrontFace)
import CornerPoints.Transpose(transposeX)

import Joiners.Manual(Joiner(..),joiner)


joinerTestDo = do
  putStrLn "" 
  putStrLn "joinerTestDo tests"
  runTestTT test1
  runTestTT test2
  runTestTT test3
  runTestTT test4
  runTestTT test5
  runTestTT test6
  runTestTT test7
  runTestTT test8
  runTestTT test9

frontFace1 =
  FrontFace
  {f1 = Point 0 0 0,
   f2 = Point 0 0 1,
   f3 = Point 1 0 1,
   f4 = Point 1 0 0
  }

frontFace2 =
  frontFace1
  +++
  (transposeX (+1) $ extractFrontRightLine frontFace1)

frontFace3 =
  frontFace2
  +++
  (transposeX (+1) $ extractFrontRightLine frontFace2)
  

test1 = TestCase $ assertEqual
  "[Take, Take, Take] [frontFace1,frontFace2, frontFace3]"
  ([frontFace1,frontFace2, frontFace3])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontLeftLine)
      rawCpoints = [frontFace1,frontFace2, frontFace3]
      joiners   = [Take, Take, Take]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )

test2 = TestCase $ assertEqual
  "[Take, Take] [FrontFace1, FrontFace2]"
  ([frontFace1,frontFace2])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontLeftLine)
      rawCpoints = [frontFace1,frontFace2]
      joiners   = [Take, Take]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )

test3 = TestCase $ assertEqual
  "[Take] [frontFace1]"
  ([frontFace1])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontLeftLine)
      rawCpoints = [frontFace1]
      joiners   = [Take]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )

test4 = TestCase $ assertEqual
  "[] [frontFace1]"
  ([CornerPointsError {errMessage = "empty [Joiner] passed into joiner"}])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontLeftLine)
      rawCpoints = [frontFace1]
      joiners   = []
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )

test5 = TestCase $ assertEqual
  "[Take] []"
  ([CornerPointsError {errMessage = "empty [CornerPoints] passed into joiner"}])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontLeftLine)
      rawCpoints = []
      joiners   = [Take]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )

test6 = TestCase $ assertEqual
  "[Take, Take] [FrontFace1, FrontFace2, FrontFace3]"
  ([frontFace1,frontFace2])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontLeftLine)
      rawCpoints = [frontFace1,frontFace2, frontFace3]
      joiners   = [Take, Take]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )

test7 = TestCase $ assertEqual
  "[Take, Take, Take] [FrontFace1, FrontFace2]"
  ([frontFace1,frontFace2])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontLeftLine)
      rawCpoints = [frontFace1,frontFace2]
      joiners   = [Take, Take, Take]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )


test8 = TestCase $ assertEqual
  "[Take, Take, HoldLeft] [FrontFace1, FrontFace2, FrontFace3]"
  ([frontFace1,frontFace2, toFrontFace $ extractFrontLeftLine frontFace2])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontRightLine)
      rawCpoints = [frontFace1,frontFace2, frontFace3]
      joiners   = [Take, Take, HoldLeading]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )

test9 = TestCase $ assertEqual
  "[Take, Take, TakeLeft] [FrontFace1, FrontFace2] missing last FrontFace"
  ([frontFace1,frontFace2, toFrontFace $ extractFrontLeftLine frontFace2])
  ( let
      takeLeftFx = (toFrontFace . extractFrontLeftLine)
      takeRightFx = (toFrontFace . extractFrontRightLine)
      rawCpoints = [frontFace1,frontFace2]
      joiners   = [Take, Take, HoldLeading]
    in
      joiner takeLeftFx takeRightFx joiners rawCpoints
  )
