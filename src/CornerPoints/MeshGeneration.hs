module CornerPoints.MeshGeneration( doesOpposingFaceExistInList, doesSameFaceExistInList,
                                    autoGenerateEachCube, autoGenerateEachFace) where

{-Testing is in Tests.StlAutoGenerateTest-}
import Data.List(findIndex, deleteBy)
import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>))
import CornerPoints.FaceExtraction(extractBackFace, extractBottomFace, extractFrontFace, extractLeftFace, extractRightFace, extractTopFace, )
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )

import Test.HUnit 

{-Does the opposite face exsit in a list.-}
doesOpposingFaceExistInList :: [CornerPoints] -> CornerPoints -> Bool
doesOpposingFaceExistInList    facesList          inFace      =
  case findIndex (inFace >==< ) facesList of
    Just x -> True
    Nothing -> False

{-Does this face already exist in a list.-}
doesSameFaceExistInList ::  [CornerPoints] -> CornerPoints -> Bool
doesSameFaceExistInList     facesList          inFace      =
  case findIndex (inFace == ) facesList of
    Just x -> True
    Nothing -> False


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

BackFace _ _ _ _ >==< BackFace _ _ _ _ = False
BackFace _ _ _ _ >==< LeftFace _ _ _ _ = False
BackFace _ _ _ _ >==< RightFace _ _ _ _ = False
BackFace _ _ _ _ >==< TopFace _ _ _ _ = False
BackFace _ _ _ _ >==< BottomFace _ _ _ _ = False

FrontFace _ _ _ _ >==< FrontFace _ _ _ _ = False
FrontFace _ _ _ _ >==< TopFace _ _ _ _ = False
FrontFace _ _ _ _ >==< BottomFace _ _ _ _ = False
FrontFace _ _ _ _ >==< LeftFace _ _ _ _ = False
FrontFace _ _ _ _ >==< RightFace _ _ _ _ = False

TopFace _ _ _ _ >==< TopFace _ _ _ _ = False
TopFace _ _ _ _ >==< FrontFace _ _ _ _ = False
TopFace _ _ _ _ >==< BackFace _ _ _ _ = False
TopFace _ _ _ _ >==< LeftFace _ _ _ _ = False
TopFace _ _ _ _ >==< RightFace _ _ _ _ = False

BottomFace _ _ _ _ >==< BottomFace _ _ _ _ = False
BottomFace _ _ _ _ >==< FrontFace _ _ _ _ = False
BottomFace _ _ _ _ >==< BackFace _ _ _ _ = False
BottomFace _ _ _ _ >==< LeftFace _ _ _ _ = False
BottomFace _ _ _ _ >==< RightFace _ _ _ _ = False

LeftFace _ _ _ _ >==< LeftFace _ _ _ _ = False
LeftFace _ _ _ _ >==< FrontFace _ _ _ _ = False
LeftFace _ _ _ _ >==< BackFace _ _ _ _ = False
LeftFace _ _ _ _ >==< TopFace _ _ _ _ = False
LeftFace _ _ _ _ >==< BottomFace _ _ _ _ = False

RightFace _ _ _ _ >==< RightFace _ _ _ _ = False
RightFace _ _ _ _ >==< FrontFace _ _ _ _ = False
RightFace _ _ _ _ >==< BackFace _ _ _ _ = False
RightFace _ _ _ _ >==< TopFace _ _ _ _ = False
RightFace _ _ _ _ >==< BottomFace _ _ _ _ = False


{-
If the opposing face is present in the list, remove it. Do not put in the new face.
This would be where 2 adjoining cubes meet, and so no faces should exist.

If the face is already present, do not add another.
Not sure how this would ever happen. Perhaps testing will show that this can be removed.

Otherwise the face needs to be added to the list.

Probably not needed.
Instead: have Builder.Monad use this logic with the State.
-}
pushToList ::  [CornerPoints] -> CornerPoints -> [CornerPoints]
pushToList     facesList          inFace      
  | (doesOpposingFaceExistInList facesList inFace) = deleteBy (>==<) inFace facesList
  | doesSameFaceExistInList  facesList inFace      = facesList
  | otherwise = inFace : facesList
  
{-Break up a CubePoints into a list of faces.

Return list with single CornerPointsError if not a CubePoints. 
-}
extractFaces :: CornerPoints -> [CornerPoints]
extractFaces (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) =
                  let cubeIn = CubePoints f1 f2 f3 f4 b1 b2 b3 b4
                  in  
                       extractTopFace cubeIn    :
                       extractBackFace cubeIn   : 
                       extractFrontFace cubeIn  :
                       extractLeftFace cubeIn   :
                       extractRightFace cubeIn  : 
                       extractBottomFace cubeIn : 
                       []

extractFaces nonCubePoints = [CornerPointsError "attempted 'extractFaces' of non CubePoints"]


{- |
Recursive function to run through [CornerPoints]. To generate manifold stl, must be [CubePoints].
It breaks them up into Faces, and pushes onto a list for autogenerated stl requirements.
That is to say: no duplicate faces, or opposing faces.
-}
autoGenerateEachCube :: [CornerPoints] -> [CornerPoints] -> [CornerPoints]
autoGenerateEachCube finalListOfFaces [] = finalListOfFaces

autoGenerateEachCube finalListOfFaces (lastCube : []) =
  autoGenerateEachFace finalListOfFaces $ extractFaces lastCube 

autoGenerateEachCube finalListOfFaces (currCube : allCubes) =
  let currFaces = extractFaces currCube
      facesPushed = autoGenerateEachFace finalListOfFaces currFaces
  in  autoGenerateEachCube facesPushed allCubes


{-
This should take in a CubePoint so it can pattern match on that.
This is the one that would be called by autoGenerateEachCube
Then break up the CubePoint and process it with: autoGenerateEachFace'


autoGenerateEachFace' will be the one that breaks up the CubePoints, and processes it.
-}
autoGenerateEachFace :: [CornerPoints] -> [CornerPoints] -> [CornerPoints]
autoGenerateEachFace finalListOfFaces [] = finalListOfFaces
autoGenerateEachFace finalListOfFaces (currFace : allFaces) =
  autoGenerateEachFace (pushToList finalListOfFaces currFace) allFaces

--Test functions that do not need to be exported.
internalFunctionTesting = do
  --some test cubes to work with
  let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
      cube1 = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )
      cube2 = cube1
              +++
              (((transposeY (+1)) . extractFrontFace) cube1)
              
  {-Work through a cube, appending each face to a list, without checking if anything exists.-}
  let extratFacesFromCubePointsTest = TestCase $ assertEqual
        "extratFacesFromCubePointsTest"
        ([TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                   b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}},
          BackFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                    b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}},
          FrontFace {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                     f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
          LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                    f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}},
          RightFace {b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                     f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
          BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                      b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}
         ]
        )
        (extractFaces cube1)
  runTestTT extratFacesFromCubePointsTest

  let extractFacesFromNonCubePointsTest = TestCase $ assertEqual
        "extractFacesFromNonCubePointsTest"
        [CornerPointsError "attempted 'extractFaces' of non CubePoints"]
        (extractFaces btmFace)
  runTestTT extractFacesFromNonCubePointsTest
        
  let pushTopFaceToListContainingBottomFaceTest = TestCase $ assertEqual
        "pushTopFaceToListContainingBottomFaceTest"
        []
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
         in
         pushToList [btmFace] topFace
        )
  runTestTT pushTopFaceToListContainingBottomFaceTest

  let pushBottomFaceToListContainingTopFaceTest = TestCase $ assertEqual
        "pushBottomFaceToListContainingTopFaceTest"
        []
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
         in
         pushToList [topFace] btmFace
        )
  runTestTT pushBottomFaceToListContainingTopFaceTest

  let removeOpposingBottomFaceInList = TestCase $ assertEqual
        "removeOpposingBottomFaceInList"
        [RightFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)]
        --                       b1            f1            b4             f4
        ( let list = [BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0),
                      RightFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
                     ]
          --                            b2            f2            b3            f3
          in  pushToList list (TopFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0))
        )
  runTestTT removeOpposingBottomFaceInList

  let removeOpposingTopFaceInList = TestCase $ assertEqual
        "removeOpposingTopFaceInList"
        [RightFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)]
        ( let list = [TopFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0),
                      RightFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
                     ]
          in  pushToList list (BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0))
              
        )
  runTestTT removeOpposingTopFaceInList

  let frontFaceOppositeToBackFaceTest  = TestCase $ assertEqual
        "frontFaceOppositeToBackFaceTest"
        True
        (let frontFace = FrontFace (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
             backFace  = BackFace  (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
         in  frontFace >==< backFace
        )
  runTestTT frontFaceOppositeToBackFaceTest

  let backFaceOppositeToFrontFaceTest  = TestCase $ assertEqual
        "backFaceOppositeToFrontFaceTest"
        True
        (let frontFace = FrontFace (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
             backFace  = BackFace  (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
         in  backFace >==< frontFace 
        )
  runTestTT backFaceOppositeToFrontFaceTest

  let backFaceNotOppositeToFrontFaceTest  = TestCase $ assertEqual
        "backFaceNotOppositeToFrontFaceTest"
        False
        (let frontFace = FrontFace (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1 1 0)
             backFace  = BackFace  (Point 0 1 0) (Point 0 1 1) (Point 1 1 1) (Point 1111111111 1 0)
         in  backFace >==< frontFace 
        )
  runTestTT backFaceNotOppositeToFrontFaceTest

  let leftFaceNotOppositeToRightFaceTest  = TestCase $ assertEqual
        "leftFaceNotOppositeToRightFaceTest"
        (False)
        (let leftFace = LeftFace (Point 0 0 0) (Point 0 0 1) (Point 0 1 0) (Point 0 1 1)
             rightFace = RightFace (Point 1 0 1) (Point 1 0 0) (Point 1 1 1) (Point 1 1 0)
         in  leftFace >==< rightFace
         )
  runTestTT leftFaceNotOppositeToRightFaceTest

  let leftFaceOppositeToRightFaceTest  = TestCase $ assertEqual
        "leftFaceOppositeToRightFaceTest"
        (True)
        (let leftFace = LeftFace (Point 0 0 0) (Point 0 0 1) (Point 0 1 0) (Point 0 1 1)
             rightFace = RightFace (Point 0 0 1) (Point 0 0 0) (Point 0 1 1) (Point 0 1 0)
         in  leftFace >==< rightFace
         )
  runTestTT leftFaceOppositeToRightFaceTest

  let rightFaceOppositeToLeftFaceTest  = TestCase $ assertEqual
        "rightFaceOppositeToLeftFaceTest"
        (True)
        (let leftFace = LeftFace (Point 0 0 0) (Point 0 0 1) (Point 0 1 0) (Point 0 1 1)
             rightFace = RightFace (Point 0 0 1) (Point 0 0 0) (Point 0 1 1) (Point 0 1 0)
         in  rightFace >==< leftFace  
         )
  runTestTT rightFaceOppositeToLeftFaceTest

  let btmFaceOppositeToTopFaceTest  = TestCase $ assertEqual
        "btmFaceOppositeToTopFaceTest"
        (True)
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace    (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
         in  btmFace >==< topFace  
         )
  runTestTT btmFaceOppositeToTopFaceTest

  let topFaceOppositeToBtmFaceTest  = TestCase $ assertEqual
        "topFaceOppositeToBtmFaceTest"
        (True)
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace    (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
         in  topFace >==< btmFace    
         )
  runTestTT topFaceOppositeToBtmFaceTest

  let topFaceNotOppositeToBtmFaceTest  = TestCase $ assertEqual
        "topFaceNotOppositeToBtmFaceTest"
        (False)
        (let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             topFace = TopFace    (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 11111 0)
         in  topFace >==< btmFace    
         )
  runTestTT topFaceNotOppositeToBtmFaceTest

  let topFaceNotEqualToRightFaceTest  = TestCase $ assertEqual
        "topFaceNotEqualToRightFaceTest"
        (False)
        (let rightFace = RightFace (Point 0 0 1) (Point 0 0 0) (Point 0 1 1) (Point 0 1 0)
             topFace = TopFace    (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 11111 0)
         in  topFace >==< rightFace    
         )
  runTestTT topFaceNotEqualToRightFaceTest

  
