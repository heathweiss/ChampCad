module Tests.StlAutoGenerateTest(stlAutoGenerateTest) where

import Test.HUnit

import Data.List(findIndex, deleteBy)

import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>))
import CornerPoints.FaceExtraction(extractBackFace, extractBottomFace, extractFrontFace, extractLeftFace, extractRightFace, extractTopFace, )
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )

stlAutoGenerateTest = do

  ----------------------------------------------------------  TriangleBuilder -------------------------------------------------------------------
  --create a test cube
  let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
      cube1 = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )
      cube2 = cube1
              +++
              (((transposeY (+1)) . extractFrontFace) cube1)

  {-Make sure cube1 and cube2 are ready to go.-}
  let cubesAreGood = TestCase $ assertEqual
        "cubesAreGood"
        ([CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                      f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
                      b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                      b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}},
          CubePoints {f1 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 1.0},
                      f3 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 0.0},
                      b1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                      b3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}
         ]
        ) 
        ( [cube1, cube2]
        )
  runTestTT cubesAreGood

  {-Work through a cube, appending each face to a list, without checking if anything exists.-}
  let breakUpCubeNoChecksTest = TestCase $ assertEqual
        "breakUpCubeNoChecksTest"
        ([BackFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                    b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}},
          BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                      b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
          FrontFace {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                     f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
          LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                    f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}},
          RightFace {b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                     f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
          TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                   b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}])
        (simpleBreakUp cube1)
  runTestTT breakUpCubeNoChecksTest

  

  let frontFaceEqualToBackFaceTest  = TestCase $ assertEqual
        "frontFaceEqualToBackFaceTest"
        True
        (let frontFace = FrontFace (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
             backFace  = BackFace  (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
         in  frontFace >==< backFace
        )
  runTestTT frontFaceEqualToBackFaceTest

  let backFaceEqualToFrontFaceTest  = TestCase $ assertEqual
        "backFaceEqualToFrontFaceTest"
        True
        (let frontFace = FrontFace (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
             backFace  = BackFace  (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
         in  backFace >==< frontFace 
        )
  runTestTT backFaceEqualToFrontFaceTest

  let backFaceNotEqualToFrontFaceTest  = TestCase $ assertEqual
        "backFaceNotEqualToFrontFaceTest"
        False
        (let frontFace = FrontFace (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
             backFace  = BackFace  (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1111111111 1 0)
         in  backFace >==< frontFace 
        )
  runTestTT backFaceNotEqualToFrontFaceTest

  let leftFaceNotEqualToRightFaceTest  = TestCase $ assertEqual
        "leftFaceNotEqualToRightFaceTest"
        (False)
        (let leftFace = LeftFace (Point 0 0 0) (Point 0 0 1) (Point 0 1 0) (Point 0 1 1)
             rightFace = RightFace (Point 1 0 1) (Point 1 0 0) (Point 1 1 1) (Point 1 1 0)
         in  leftFace >==< rightFace
         )
  runTestTT leftFaceNotEqualToRightFaceTest

  let leftFaceEqualToRightFaceTest  = TestCase $ assertEqual
        "leftFaceEqualToRightFaceTest"
        (True)
        (let leftFace = LeftFace (Point 0 0 0) (Point 0 0 1) (Point 0 1 0) (Point 0 1 1)
             rightFace = RightFace (Point 0 0 1) (Point 0 0 0) (Point 0 1 1) (Point 0 1 0)
         in  leftFace >==< rightFace
         )
  runTestTT leftFaceEqualToRightFaceTest

  let rightFaceEqualToLeftFaceTest  = TestCase $ assertEqual
        "rightFaceEqualToLeftFaceTest"
        (True)
        (let leftFace = LeftFace (Point 0 0 0) (Point 0 0 1) (Point 0 1 0) (Point 0 1 1)
             rightFace = RightFace (Point 0 0 1) (Point 0 0 0) (Point 0 1 1) (Point 0 1 0)
         in  rightFace >==< leftFace  
         )
  runTestTT rightFaceEqualToLeftFaceTest

  let btmFaceEqualToTopFaceTest  = TestCase $ assertEqual
        "btmFaceEqualToTopFaceTest"
        (True)
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace    (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
         in  btmFace >==< topFace  
         )
  runTestTT btmFaceEqualToTopFaceTest

  let topFaceEqualToBtmFaceTest  = TestCase $ assertEqual
        "topFaceEqualToBtmFaceTest"
        (True)
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace    (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
         in  topFace >==< btmFace    
         )
  runTestTT topFaceEqualToBtmFaceTest

  let topFaceNotEqualToBtmFaceTest  = TestCase $ assertEqual
        "topFaceNotEqualToBtmFaceTest"
        (False)
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace    (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 11111 0)
         in  topFace >==< btmFace    
         )
  runTestTT topFaceNotEqualToBtmFaceTest
  
{- -------------------------------------------------------- auto generate functionality:------------------------------------------------
ToDo:
Move this all over to a new StlAutoGenerate module once it is working.
-}

{-break up a cube into a list of faces.
This will need to have condition insertion of the faces into a list.-}
simpleBreakUp :: CornerPoints -> [CornerPoints]
simpleBreakUp cubeIn = extractBackFace cubeIn   :
                       extractBottomFace cubeIn :
                       extractFrontFace cubeIn  :
                       extractLeftFace cubeIn   :
                       extractRightFace cubeIn  :
                       extractTopFace cubeIn    :
                       []

--findAndRemoveOpposingFaceFromListOfFaces [CornerPoints] -> CornerPoints -> CornerPoints
--findAndRemoveOpposingFaceFromListOfFaces = facesList       inFace       =
doesOpposingFaceExistInList :: [CornerPoints] -> CornerPoints -> Bool
doesOpposingFaceExistInList    facesList          inFace      =
  case findIndex (inFace >==< ) facesList of
    Just x -> True
    Nothing -> False

doesSameFaceExistInList ::  [CornerPoints] -> CornerPoints -> Bool
doesSameFaceExistInList     facesList          inFace      =
  case findIndex (inFace == ) facesList of
    Just x -> True
    Nothing -> False
  
changeList ::  [CornerPoints] -> CornerPoints -> [CornerPoints]
changeList     facesList          inFace      
  | (doesOpposingFaceExistInList facesList inFace) = deleteBy (>==<) inFace facesList
  | doesSameFaceExistInList  facesList inFace      = facesList
  | otherwise = inFace : facesList
  

{-See if two faces are opposites of each other.
So the face exists in the same position in space, but face the opposite directions.
Can only be true for the following pairs:
back/front
left/right
top/bottom-}
(>==<) :: CornerPoints -> CornerPoints -> Bool

BackFace b1 b2 b3 b4  >==< FrontFace f1' f2' f3' f4' =
 b1 == f1' &&
 b2 == f2' &&
 b3 == f3' &&
 b4 == f4'

FrontFace f1' f2' f3' f4'  >==< BackFace b1 b2 b3 b4 =
  BackFace b1 b2 b3 b4  >==< FrontFace f1' f2' f3' f4'

LeftFace b1 b2 f1 f2 >==< RightFace b3' b4' f3' f4' =
  b1 == b4' &&
  b2 == b3' &&
  f2 == f3' &&
  f1 == f4'

RightFace b3' b4' f3' f4' >==< LeftFace b1 b2 f1 f2 =
  LeftFace b1 b2 f1 f2  >==< RightFace b3' b4' f3' f4'

BottomFace b1 f1 b4 f4 >==< TopFace b2 f2 b3 f3 =
  b1 == b2 &&
  f1 == f2 &&
  b4 == b3 &&
  f4 == f3

TopFace b2 f2 b3 f3 >==< BottomFace b1 f1 b4 f4 =
  BottomFace b1 f1 b4 f4 >==< TopFace b2 f2 b3 f3
