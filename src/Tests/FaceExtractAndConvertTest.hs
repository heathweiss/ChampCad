module Tests.FaceExtractAndConvertTest(faceExtractAndConvertTestDo) where
import Test.HUnit
import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(reverseNormal, toBackFace)
import CornerPoints.FaceExtraction(extractFrontFace, extractBackFace, extractRightFace)
import CornerPoints.FaceExtractAndConvert(getFrontFaceAsBackFace)

faceExtractAndConvertTestDo = do
  let helloTest = TestCase $ assertEqual
       "helloTest"
       (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 5 5 5) (Point 6 6 6) (Point 7 7 7) (Point 8 8 8))
       (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 5 5 5) (Point 6 6 6) (Point 7 7 7) (Point 8 8 8))

  runTestTT helloTest

  --Gets the right face of the large cube, as the back face.
  let getBackFaceFromRightFaceFromCubeTest = TestCase $ assertEqual
       "getBackFaceFromRightFaceFromCubeTest"
       ( BackFace {b1 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}, b2 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}, b3 = Point {x_axis = 13.0, y_axis = 13.0, z_axis = 13.0}, b4 = Point {x_axis = 14.0, y_axis = 14.0, z_axis = 14.0}})
       (toBackFace $ extractRightFace (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 11 11 11) (Point 12 12 12) (Point 13 13 13) (Point 14 14 14)))
  runTestTT getBackFaceFromRightFaceFromCubeTest


  let extractFrontFaceAndInvertTest = TestCase $ assertEqual
       "extractFrontFaceAndInvertTest"
       ( FrontFace {f1 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}, f2 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}, f3 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}})
       (reverseNormal $ extractFrontFace (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 11 11 11) (Point 12 12 12) (Point 13 13 13) (Point 14 14 14)))
  runTestTT extractFrontFaceAndInvertTest

  let addPerpendicularCubesTest = TestCase $ assertEqual
       ("addPerpendicularCubesTest")
       (CubePoints {f1 = Point {x_axis = 101.0, y_axis = 101.0, z_axis = 101.0}, f2 = Point {x_axis = 102.0, y_axis = 102.0, z_axis = 102.0}, f3 = Point {x_axis = 103.0, y_axis = 103.0, z_axis = 103.0}, f4 = Point {x_axis = 104.0, y_axis = 104.0, z_axis = 104.0}, b1 = Point {x_axis = 4.0, y_axis = 4.0, z_axis = 4.0}, b2 = Point {x_axis = 3.0, y_axis = 3.0, z_axis = 3.0}, b3 = Point {x_axis = 13.0, y_axis = 13.0, z_axis = 13.0}, b4 = Point {x_axis = 14.0, y_axis = 14.0, z_axis = 14.0}} )
       (let smallCube = (CubePoints (Point 101 101 101) (Point 102 102 102) (Point 103 103 103) (Point 104 104 104) (Point 201 2011 201) (Point 202 202 202) (Point 203 203 203) (Point 204 204 204))
            largeCube = (CubePoints (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4) (Point 11 11 11) (Point 12 12 12) (Point 13 13 13) (Point 14 14 14))
            smallInvertedFrontFace = {-reverseNormal $-} extractFrontFace smallCube
            largeRightFaceAsBackFace = toBackFace $ extractRightFace largeCube
        in  largeRightFaceAsBackFace +++ smallInvertedFrontFace
       )
  runTestTT addPerpendicularCubesTest

  

  
