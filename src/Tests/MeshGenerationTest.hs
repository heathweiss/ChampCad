module Tests.MeshGenerationTest(meshGenerationTest, ) where

import Test.HUnit

import Data.List(findIndex, deleteBy)

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.FaceExtraction(extractFrontFace )
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace)
import CornerPoints.MeshGeneration( doesOpposingFaceExistInList, doesSameFaceExistInList)

meshGenerationTest = do

  --create a test cube
  let btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
      cube1 = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )
      cube2 = cube1
              +++
              (((transposeY (+1)) . extractFrontFace) cube1)

  
  let opposingBottomFaceExistsInListTest = TestCase $ assertEqual
        "opposingBottomFaceExistsInListTest"
        True
        (let list = [BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)]
         in  doesOpposingFaceExistInList list (TopFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0))
        )
  runTestTT opposingBottomFaceExistsInListTest

  

  
              
