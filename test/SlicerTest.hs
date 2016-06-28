module SlicerTest (splicerTestDo) where

import Test.HUnit

import CornerPoints.Slicer(slice)
import CornerPoints.CornerPoints (CornerPoints(..),(+++))
import CornerPoints.Points (Point(..))

splicerTestDo = do

  --have not cx'd to see if numbers are correct.
  let putTopOnCube = TestCase $ assertEqual
        "putTopOnCube"
        (TopFace {b2 = Point {x_axis = 4.0, y_axis = 0.0, z_axis = 5.0},
                  f2 = Point {x_axis = 4.0, y_axis = 10.0, z_axis = 5.0},
                  b3 = Point {x_axis = 8.0, y_axis = 0.0, z_axis = 5.0},
                  f3 = Point {x_axis = 8.0, y_axis = 10.0, z_axis = 5.0}}
        )
        (
          let frontTopLine = FrontTopLine       (Point 4 10 10) (Point 8 10 10)
              bottomFrontLine = BottomFrontLine (Point 4 10 0)  (Point 8 10 0)
              
              backTopLine = BackTopLine         (Point 4 0 10) (Point 8 0 10)
              backBottomLine = BackBottomLine   (Point 4 0 0)  (Point 8 0 0)
                        
          in
          slice 5 (frontTopLine +++ backTopLine) (backBottomLine +++ bottomFrontLine)
         )
  runTestTT putTopOnCube

  let putFlatTopOnFrontLines = TestCase $ assertEqual
        "putFlatTopOnFrontLines"
        (FrontTopLine (Point 1 1 5) (Point 5 1 5))
        (
          let frontTopLine = FrontTopLine    (Point 2 2 10) (Point 6 2 10)
              btmFrontLine = BottomFrontLine (Point 0 0 0)  (Point 4 0 0)
          in
          slice 5 frontTopLine btmFrontLine
        )

  let putFlatBottomOnFrontLines = TestCase $ assertEqual
        "putFlatBottomOnFrontLines"
        (BottomFrontLine (Point 1.4 2.8 7) (Point 11.4 2.8 7))
        (
          let frontTopLine = FrontTopLine    (Point 2 4 10) (Point 12 4 10)
              btmFrontLine = BottomFrontLine (Point 0 0 0)  (Point 10 0 0)
          in
          slice 7 btmFrontLine frontTopLine
        )

  runTestTT putFlatBottomOnFrontLines

  let putFlatTopOnBackLines = TestCase $ assertEqual
        "putFlatTopOnBackLines"
        (BackTopLine (Point 1 1 5) (Point 5 1 5))
        (
          let backTopLine = BackTopLine    (Point 2 2 10) (Point 6 2 10)--b2 b3
              backBtmLine = BackBottomLine (Point 0 0 0)  (Point 4 0 0)--b1 b4
          in
          slice 5 backTopLine backBtmLine
        )

  runTestTT putFlatTopOnBackLines

  let putFlatBottomOnBackLines = TestCase $ assertEqual
        "putFlatBottomOnBackLines"
        (BackBottomLine (Point 1.4 2.8 7) (Point 11.4 2.8 7))
        (
          let backTopLine = BackTopLine    (Point 2 4 10) (Point 12 4 10)
              backBottomLine = BackBottomLine (Point 0 0 0)  (Point 10 0 0)
          in
          slice 7 backBottomLine backTopLine
        )

  runTestTT putFlatBottomOnBackLines

  let putFlatTopOnF2F1 = TestCase $ assertEqual
        "putFlatTopOnF2F1"
        (F2 (Point 1.4 2.8 7))
        (
          slice 7 (F2 (Point 2 4 10)) (F1 (Point 0 0 0))
        )

  runTestTT putFlatTopOnF2F1

  let putFlatBottomOnF1F2 = TestCase $ assertEqual
        "putFlatBottomOnF1F2"
        (F1 (Point 1.4 2.8 7))
        (
          slice 7 (F1 (Point 2 4 10)) (F2 (Point 0 0 0))
        )

  runTestTT putFlatBottomOnF1F2

  let putFlatTopOnF3F4 = TestCase $ assertEqual
        "putFlatTopOnF3F4"
        (F3 (Point 1.4 2.8 7))
        (
          slice 7 (F3 (Point 2 4 10)) (F4 (Point 0 0 0))
        )

  runTestTT putFlatTopOnF3F4

  let putFlatTBottomOnF4F3 = TestCase $ assertEqual
        "putFlatTBottomOnF4F3"
        (F4 (Point 1.4 2.8 7))
        (
          slice 7 (F4 (Point 2 4 10)) (F3 (Point 0 0 0))
        )

  runTestTT putFlatTBottomOnF4F3
  
  let putFlatTopOnB2B1 = TestCase $ assertEqual
        "putFlatTopOnB2B1"
        (B2 (Point 1.4 2.8 7))
        (
          slice 7 (B2 (Point 2 4 10)) (B1 (Point 0 0 0))
        )

  runTestTT putFlatTopOnB2B1

  let putFlatBtmOnB1B2 = TestCase $ assertEqual
        "putFlatBtmOnB1B2"
        (B1 (Point 1.4 2.8 7))
        (
          slice 7 (B1 (Point 2 4 10)) (B2 (Point 0 0 0))
        )

  runTestTT putFlatBtmOnB1B2

  let putFlatTopOnB3B4 = TestCase $ assertEqual
        "putFlatTopOnB3B4"
        (B3 (Point 1.4 2.8 7))
        (
          slice 7 (B3 (Point 2 4 10)) (B4 (Point 0 0 0))
        )

  runTestTT putFlatTopOnB3B4

  let putFlatBtmOnB4B3 = TestCase $ assertEqual
        "putFlatBtmOnB4B3"
        (B4 (Point 1.4 2.8 7))
        (
          slice 7 (B4 (Point 0 0 0)) (B3 (Point 2 4 10))
        )

  runTestTT putFlatBtmOnB4B3
