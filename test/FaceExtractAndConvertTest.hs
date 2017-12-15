module FaceExtractAndConvertTest(faceExtractAndConvertTestDo) where
import Test.HUnit
import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(reverseNormal, toBackFace, toLeftFace, toBottomLeftLine, raisedTo)
import CornerPoints.FaceExtraction(extractFrontFace, extractBackFace, extractRightFace, extractFrontLeftLine, 
                                   contains)

faceExtractAndConvertTestDo = do
  let helloTest = TestCase $ assertEqual
       "helloTest"
       (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 5 5 5) (Point 6 6 6) (Point 7 7 7) (Point 8 8 8))
       (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 5 5 5) (Point 6 6 6) (Point 7 7 7) (Point 8 8 8))

  runTestTT helloTest

  --Gets the right face of the large cube, as the back face.
  let getBackFaceFromRightFaceFromCubeTest = TestCase $ assertEqual
       "getBackFaceFromRightFaceFromCubeTest"
       (
         BackFace {b1 = Point {x_axis = 14.0, y_axis = 14.0, z_axis = 14.0},
                   b2 = Point {x_axis = 13.0, y_axis = 13.0, z_axis = 13.0},
                   b3 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0},
                   b4 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}}

       )
       (toBackFace $ extractRightFace (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 11 11 11) (Point 12 12 12) (Point 13 13 13) (Point 14 14 14)))
  runTestTT getBackFaceFromRightFaceFromCubeTest


  let extractFrontFaceAndInvertTest = TestCase $ assertEqual
       "extractFrontFaceAndInvertTest"
       ( FrontFace {f1 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}, f2 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}, f3 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}})
       (reverseNormal $ extractFrontFace (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 11 11 11) (Point 12 12 12) (Point 13 13 13) (Point 14 14 14)))
  runTestTT extractFrontFaceAndInvertTest

  let addPerpendicularCubesTest = TestCase $ assertEqual
       ("addPerpendicularCubesTest")
       (
         CubePoints {f1 = Point {x_axis = 101.0, y_axis = 101.0, z_axis = 101.0},
                     f2 = Point {x_axis = 102.0, y_axis = 102.0, z_axis = 102.0},
                     f3 = Point {x_axis = 103.0, y_axis = 103.0, z_axis = 103.0},
                     f4 = Point {x_axis = 104.0, y_axis = 104.0, z_axis = 104.0},
                     b1 = Point {x_axis = 14.0, y_axis = 14.0, z_axis = 14.0},
                     b2 = Point {x_axis = 13.0, y_axis = 13.0, z_axis = 13.0},
                     b3 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0},
                     b4 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}}

       )
       (let smallCube = (CubePoints (Point 101 101 101) (Point 102 102 102) (Point 103 103 103) (Point 104 104 104) (Point 201 2011 201) (Point 202 202 202) (Point 203 203 203) (Point 204 204 204))
            largeCube = (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 11 11 11) (Point 12 12 12) (Point 13 13 13) (Point 14 14 14))
            smallInvertedFrontFace = {-reverseNormal $-} extractFrontFace smallCube
            largeRightFaceAsBackFace = toBackFace $ extractRightFace largeCube
        in  largeRightFaceAsBackFace +++ smallInvertedFrontFace
       )
  runTestTT addPerpendicularCubesTest

  

  let toLeftFaceTest1 = TestCase $ assertEqual
       "toLeftFaceTest1"
       (LeftFace
               {b1 = Point 0 0 0,
                b2 = Point 0 0 1,
                f1 = Point 1 1 0,
                f2 = Point 1 1 1
               }
       )
       
       (let
           rightFace =
             RightFace
               {b4 = Point 0 0 0,
                b3 = Point 0 0 1,
                f4 = Point 1 1 0,
                f3 = Point 1 1 1
               }
        in
         toLeftFace rightFace
          
       )
       
  runTestTT toLeftFaceTest1

  let extractFrontLeftLineTest = TestCase $ assertEqual
       "extractFrontLeftLineTest"
       (FrontLeftLine {f1 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}})
       
       (let
           leftFace =
             LeftFace
               {b1 = Point 0 0 0,
                b2 = Point 0 0 1,
                f1 = Point 1 1 0,
                f2 = Point 1 1 1
               }
        in
         extractFrontLeftLine leftFace
          
       )
       
  runTestTT extractFrontLeftLineTest

  let toBottomLeftLineTest1 = TestCase $ assertEqual
       "toBottomLeftLineTest1"
       (BottomLeftLine
               {b1 = Point 0 0 0,
                f1 = Point 1 1 0
               })
       
       (let
           bottomRightLine =
             BottomRightLine
               {b4 = Point 0 0 0,
                f4 = Point 1 1 0
               }
        in
         toBottomLeftLine  bottomRightLine
          
       )
       
  runTestTT toBottomLeftLineTest1

  let toBottomLeftLineTest2 = TestCase $ assertEqual
       "toBottomLeftLineTest2"
       (CornerPointsError {errMessage = "FaceConversions.toBottomLeftLine: unhandled or illegal pattern match for B4"})
       
       (let
           b4 =
             B4
               {b4 = Point 0 0 0
               }
        in
         toBottomLeftLine  b4
          
       )
       
  runTestTT toBottomLeftLineTest2

--------------------------------------------------------- raisedTo ------------------------------------------------------------------
  let raisedToTest1 = TestCase $ assertEqual
       "raisedToTest1"
       (Right $ LeftFace
                 {b1 = Point 0 0 0,
                  b2 = Point 0 0 1,
                  f1 = Point 1 1 0,
                  f2 = Point 1 1 1
                 })
       
       (let
           frontLeftLine =
             FrontLeftLine (Point 1 1 0) (Point 1 1 1)
           leftFace =
             LeftFace
               {b1 = Point 0 0 0,
                b2 = Point 0 0 1,
                f1 = Point 11 11 10,
                f2 = Point 12 12 12
               }
        in
         frontLeftLine `raisedTo`  leftFace
          
       )
       
  runTestTT raisedToTest1

  let raisedToTest2 = TestCase $ assertEqual
       "raisedToTest2"
       (Right $ BottomLeftLine {b1 = Point 10 10 10, f1 = Point 1 1 1})
       
       (let
           bottomLeftLine =
             BottomLeftLine {b1 = Point 0 0 0, f1 = Point 1 1 1}
           b1 =
             B1
               {b1 = Point 10 10 10}
        in
         b1 `raisedTo`  bottomLeftLine
          
       )
       
  runTestTT raisedToTest2
 ------------------------------------------------------ contains -----------------------------------------------------------------------

  let containsTest1 = TestCase $ assertEqual
       "containsTest1"
       (Right True)
       (let
         f1 = Point 0 0 0
         bottomLeftLine = BottomLeftLine   (Point 1 1 1) f1
        in
          bottomLeftLine `contains` (F1 f1)
       )
  
  runTestTT containsTest1

  let containsTest2 = TestCase $ assertEqual
       "containsTest2"
       (Right True)
       (let
         f1 = Point 0 1 0
         f2 = Point 0 1 1
         b1 = Point 0 0 0
         b2 = Point 0 0 1
         frontLeftLine  = FrontLeftLine f1 f2
         leftFace = LeftFace   b1 b2 f1 f2 
        in
          leftFace `contains` frontLeftLine
       )
  
  runTestTT containsTest1
