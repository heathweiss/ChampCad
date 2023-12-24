module MeshGenerationTest(meshGenerationTest) where

import Test.HUnit

import Data.List(findIndex, deleteBy)

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.FaceExtraction(extractFrontFace, extractBottomFrontLine, extractBackBottomLine)
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace, toFrontTopLine, toBackTopLine)
import CornerPoints.MeshGeneration( doesOpposingFaceExistInList, doesSameFaceExistInList, hasSurfaceArea, extractFaces)

meshGenerationTest = do
  putStrLn "" 
  putStrLn "MeshGenerationTest"
  runTestTT opposingBottomFaceExistsInListTest
  runTestTT opposingTopFaceExistsInListTest
  --front
  runTestTT frontFaceHasSurfaceAreaTest1
  runTestTT frontFaceHasSurfaceAreaTest2
  runTestTT frontFaceHasSurfaceAreaTest3
  runTestTT frontFaceHasSurfaceAreaTest4
  runTestTT frontFaceHasSurfaceAreaTest5
  runTestTT frontFaceHasSurfaceAreaTest6
  runTestTT frontFaceHasSurfaceAreaTest7
  runTestTT frontFaceHasSurfaceAreaTest8
  --back
  runTestTT backFaceHasSurfaceAreaTest1
  runTestTT backFaceHasSurfaceAreaTest2
  runTestTT backFaceHasSurfaceAreaTest3
  runTestTT backFaceHasSurfaceAreaTest4
  runTestTT backFaceHasSurfaceAreaTest5
  runTestTT backFaceHasSurfaceAreaTest6
  runTestTT backFaceHasSurfaceAreaTest7
  runTestTT backFaceHasSurfaceAreaTest8

  runTestTT removeFrontFaceWithNoSurfaceAreaTest
  runTestTT removeBackFaceWithNoSurfaceAreaTest
  

opposingBottomFaceExistsInListTest = TestCase $ assertEqual
        "opposing BottomFace exists in [CornerPoinsts]"
        True
        (let
             opposingTopFace            = (TopFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0))
             listWithOpposingBtmFace = [BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)]
         in  doesOpposingFaceExistInList
               listWithOpposingBtmFace
               opposingTopFace
        )

opposingTopFaceExistsInListTest = TestCase $ assertEqual
        "opposing TopFace exists in [CornerPoinsts]"
        True
        (let
             opposingBottomFace            = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0)
             listWithOpposingTopFace        = [(TopFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0))]
               
         in  doesOpposingFaceExistInList
               listWithOpposingTopFace
               opposingBottomFace
        ) 
-- ============================================== hasSurfaceArea tests ======================================

-- ========= FrontFace ======
frontFaceHasSurfaceAreaTest1 = TestCase $ assertEqual
  "is a line because f1 f3 are equal"
  False
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 1 1 1) (Point 0 0 0) (Point 2 2 2))
  )

frontFaceHasSurfaceAreaTest2 = TestCase $ assertEqual
  "is a line because f2 f4 are equal"
  False
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 1 1 1) (Point 2 2 2) (Point 1 1 1))
  )

frontFaceHasSurfaceAreaTest3 = TestCase $ assertEqual
  "is a line because (f1 == f4) && (f2 == f3) are equal"
  False
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 1 1 1) (Point 1 1 1) (Point 0 0 0))
  )

frontFaceHasSurfaceAreaTest4 = TestCase $ assertEqual
  "is a line because (f1 == f2) && (f4 == f3) are equal"
  False
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 0 0 0) (Point 1 1 1) (Point 1 1 1))
  )

frontFaceHasSurfaceAreaTest5 = TestCase $ assertEqual
  "is a not a line because only (f1 == f2)  are equal"
  True
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 0 0 0) (Point 1 1 1) (Point 2 2 2))
  )

frontFaceHasSurfaceAreaTest6 = TestCase $ assertEqual
  "is a not a line because only (f3 == f4)  are equal"
  True
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 1 1 1) (Point 2 2 2) (Point 2 2 2))
  )

frontFaceHasSurfaceAreaTest7 = TestCase $ assertEqual
  "is a not a line because only (f1 == f4)  are equal"
  True
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 1 1 1) (Point 2 2 2) (Point 0 0 0))
  )

frontFaceHasSurfaceAreaTest8 = TestCase $ assertEqual
  "is a not a line because only (f2 == f3)  are equal"
  True
  (hasSurfaceArea (FrontFace (Point 0 0 0) (Point 1 1 1) (Point 1 1 1) (Point 2 2 2))
  )


-- ========= BackFace ======
backFaceHasSurfaceAreaTest1 = TestCase $ assertEqual
  "is a line because b1 b3 are equal"
  False
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 1 1 1) (Point 0 0 0) (Point 2 2 2))
  )

backFaceHasSurfaceAreaTest2 = TestCase $ assertEqual
  "is a line because b2 b4 are equal"
  False
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 1 1 1) (Point 2 2 2) (Point 1 1 1))
  )

backFaceHasSurfaceAreaTest3 = TestCase $ assertEqual
  "is a line because (b1 == b4) && (b2 == b3) are equal"
  False
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 1 1 1) (Point 1 1 1) (Point 0 0 0))
  )

backFaceHasSurfaceAreaTest4 = TestCase $ assertEqual
  "is a line because (b1 == b2) && (b4 == b3) are equal"
  False
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 0 0 0) (Point 1 1 1) (Point 1 1 1))
  )

backFaceHasSurfaceAreaTest5 = TestCase $ assertEqual
  "is a not a line because only (b1 == b2)  are equal"
  True
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 0 0 0) (Point 1 1 1) (Point 2 2 2))
  )

backFaceHasSurfaceAreaTest6 = TestCase $ assertEqual
  "is a not a line because only (b3 == b4)  are equal"
  True
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 1 1 1) (Point 2 2 2) (Point 2 2 2))
  )

backFaceHasSurfaceAreaTest7 = TestCase $ assertEqual
  "is a not a line because only (b1 == b4)  are equal"
  True
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 1 1 1) (Point 2 2 2) (Point 0 0 0))
  )

backFaceHasSurfaceAreaTest8 = TestCase $ assertEqual
  "is a not a line because only (b2 == b3)  are equal"
  True
  (hasSurfaceArea (BackFace (Point 0 0 0) (Point 1 1 1) (Point 1 1 1) (Point 2 2 2))
  )


removeFrontFaceWithNoSurfaceAreaTest = TestCase $ assertEqual
        "remove front face with no surface area during extractFaces"
        


       -- should have no FrontFace
       [TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                 b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0},
                 f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
        BackFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                  b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                  b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0},
                  b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}},
        LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                  b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                  f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                  f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}},
        RightFace {b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 10.0},
                   b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                   f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
                   f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
        BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                    f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                    b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                    f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}]


        (let btmFace  = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
             topFace  = (toFrontTopLine . extractBottomFrontLine $ btmFace)
                        +++
                        ((transposeZ (+10)) . toBackTopLine . extractBackBottomLine $ btmFace)
             cube     = btmFace +++ topFace
         in  extractFaces cube
        )

  
removeBackFaceWithNoSurfaceAreaTest = TestCase $ assertEqual
        "remove back face with no surface area during extractFaces"
        


       -- should have no BackFace
       [TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                 f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0},
                 b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                 f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 10.0}},
        FrontFace {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                   f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0},
                   f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 10.0},
                   f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
        LeftFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                  b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                  f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                  f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 10.0}},
        RightFace {b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                   b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                   f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 10.0},
                   f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
        BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                    f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                    b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0},
                    f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}]


        (let btmFace  = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
             topFace  = ((transposeZ (+10)) . toFrontTopLine . extractBottomFrontLine $ btmFace)
                        +++
                        ( toBackTopLine . extractBackBottomLine $ btmFace)
             cube     = btmFace +++ topFace
         in  extractFaces cube
        )

  
  
{-
btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
             cube1 = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )
             cube2 = cube1
              +++
              (((transposeY (+1)) . extractFrontFace) cube1)
-}
