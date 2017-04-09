module DiamondCutterTest(diamondCutterTestDo) where

import Test.HUnit

import Primitives.DiamondCutter(DiamondBuilder(..), runDiamondBuilder, runFrontToBackDiamondBuilder, OffSet(..),
                               isTheFrontDiamondDone, isTheBackDiamondDone)

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace, 
                                    f34LineFromF12Line, toBackFace, reverseNormal, toBottomFrontLine, toFrontTopLine,
                                    toFrontLeftLine, toFrontRightLine, toBackBottomLine, toBackTopLine, toBottomFace, toBackRightLine)
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)

diamondCutterTestDo = do
  putStrLn ""
  putStrLn "DiamnondCutter tests"
  runTestTT lookAtTopOfDiamondEmergent
  runTestTT topOfDiamondFront
  runTestTT btmOfDiamondCenterFrontPoint
  runTestTT btmOfDiamondFrontF4ShiftedInt
  runTestTT frontTopFaceTest
  runTestTT frontBtmFaceTest
  runTestTT lookAtBackTopCenterPointOfDiamond
  runTestTT topOfDiamondBack
  runTestTT btmOfDiamondCenterBackPoint
  runTestTT btmOfDiamondBackB4ShiftedInt
  runTestTT backTopFaceTest
  runTestTT btmOfDiamondBackB2ShiftedInt
  runTestTT backBtmFaceTest
  runTestTT lookAtFrontRightCenterPointOfDiamond
  runTestTT lookAtFrontLeftCenterPointOfDiamond
  runTestTT rightOfDiamondFront
  runTestTT leftOfDiamondFront
  runTestTT lookAtBackRightCenterPointOfDiamond
  runTestTT lookAtBackLeftCenterPointOfDiamond
  runTestTT backRightOfDiamondCPoint
  runTestTT backLeftOfDiamondCPoint
  runTestTT frontRightTestFaceTest
  runTestTT frontLeftTestFaceTest
  runTestTT backRightFaceTest
  runTestTT backleftFaceTest
  runTestTT frontTopRightCornerTest
  

-- |
-- Create a defualt DiamondBuilder.
-- Offsets are all set at 25% in form edges of containing cube.
{- ================================================================================================================================
Use for testing. Tests good.
-}
defaultDiamondBuilder :: CornerPoints -> DiamondBuilder
defaultDiamondBuilder cube =
      Diamond { outerCube = cube,
                topDiamondFace = Nothing,
                topDiamondCorner = Nothing,
                topCenterPoint = Nothing,
                topDiamondHorizontalOffsets = (OffSet 0.5 0.5 0.5),
                topDiamondVertiacalOffsets = (OffSet 0.25 0.25 0.25),
                topRightDiamondFace = Nothing,
                rightDiamondFace = Nothing,
                rightCenterPoint = Nothing,
                rightDiamondCorner = Nothing,
                rightDiamondHorizontalOffsets = (OffSet 0.25 0.25 0.25),
                rightDiamondVerticalOffsets = (OffSet 0.5 0.5 0.5),
                bottomRightDiamondFace = Nothing,
                bottomDiamondFace = Nothing,
                bottomDiamondCorner = Nothing,
                bottomCenterPoint = Nothing,
                bottomDiamondHorizontalOffsets = (OffSet 0.5 0.5 0.5),
                bottomDiamondVerticalOffsets = (OffSet 0.25 0.25 0.25),
                bottomLeftDiamondFace = Nothing,

                leftDiamondFace = Nothing,
                leftCenterPoint = Nothing,
                leftDiamondCorner = Nothing,
                --try opposite values did not work: (OffSet 0.75 0.75 0.75),
                leftDiamondHorizontalOffsets = (OffSet 0.25 0.25 0.25),
                leftDiamondVerticalOffsets  = (OffSet 0.5 0.5 0.5),
                
                topLeftDiamondFace = Nothing
              }


  
buildTestCubeTest = TestCase $ assertEqual 
  "buildTestCubeTest"
  (CubePoints {
      f1 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
      f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 50.0},
      f3 = Point {x_axis = 100.0, y_axis = 10.0, z_axis = 50.0},
      f4 = Point {x_axis = 100.0, y_axis = 10.0, z_axis = 0.0},
      b1 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0},
      b2 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 50.0},
      b3 = Point {x_axis = 100.0, y_axis = -10.0, z_axis = 50.0},
      b4 = Point {x_axis = 100.0, y_axis = -10.0, z_axis = 0.0}}
   )
   (
     let btmFrontLine = (F1 (Point 0 10 0)) +++ (F4 (Point 100 10 0))
         topFrontLine = (F2 (Point 0 10 50)) +++ (F3 (Point 100 10 50))
         frontFace    = btmFrontLine +++ topFrontLine
     in
      frontFace
      +++
      ({-reverseNormal .-} toBackFace . (transposeY (+(-20))) $ frontFace)
   )


-- =================offset xyz ====================
firstCubeOfSocket =
  CubePoints {f1 = Point {x_axis = 4.961580236616474, y_axis = -28.138519792497544, z_axis = 494.54022988505744},
              f2 = Point {x_axis = 4.403832696848794, y_axis = -24.975376310276598, z_axis = 500.0},
              f3 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
              f4 = Point {x_axis = 0.0, y_axis = -29.88294664396197, z_axis = 494.54022988505744},
              b1 = Point {x_axis = 4.440635703615683, y_axis = -25.184096533460924, z_axis = 494.54022988505744},
              b2 = Point {x_axis = 3.882888163848002, y_axis = -22.020953051239964, z_axis = 500.0},
              b3 = Point {x_axis = 0.0, y_axis = -23.234224965706463, z_axis = 500.0},
              b4 = Point {x_axis = 0.0, y_axis = -26.88294664396197, z_axis = 494.54022988505744}
             }
-- front of cube dimensions  
centerOfTopFrontlinePoint = Point {x_axis = 2.2000000000000006, y_axis = -25.6, z_axis = 500.0}
centerOfTopFrontLine = F2 centerOfTopFrontlinePoint
centerOfBtmFrontLinePoint = Point {x_axis = 2.48, y_axis = -29, z_axis = 494.54}

f2ShiftedIn = F2 $ Point 2.27 (-26.45) 498.64
f4ShiftedIn = F4 $ Point 2.41 (-28.15) 495.91

frontTopFaceOfFirstSocketCube  =
     (FrontFace {f1 = Point {x_axis = 2.271634790895357, y_axis = -26.456283783051088, z_axis = 498.63505747126436},
                 f2 = Point {x_axis = 4.403832696848794, y_axis = -24.975376310276598, z_axis = 500.0},
                 f3 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
                 f4 = Point {x_axis = 2.271634790895357, y_axis = -26.456283783051088, z_axis = 498.63505747126436}})

frontBottomFaceOfFirstSocketCube =
  (FrontFace {f1 = Point {x_axis = 2.411071675837277, y_axis = -28.1592500731702, z_axis = 495.9051724137931},
              f2 = Point {x_axis = 0.0, y_axis = -29.88294664396197, z_axis = 494.54022988505744},
              f3 = Point {x_axis = 4.961580236616474, y_axis = -28.138519792497544, z_axis = 494.54022988505744},
              f4 = Point {x_axis = 2.411071675837277, y_axis = -28.1592500731702, z_axis = 495.9051724137931}})

-- back of cube dimensions
-- These have not been validated.
centerOfTopBackLinePoint = Point {x_axis = 1.941444081924001, y_axis = -22.627589008473215, z_axis = 500.0}
topBackDiamondCorner = B2 {b2 = Point {x_axis = 2.0111625243949613, y_axis = -23.479072153532773, z_axis = 498.63505747126436}}
centerOfBtmBackLinePoint = Point {x_axis = 2.2203178518078417, y_axis = -26.033521588711444, z_axis = 494.54022988505744}
--just a copy of b4ShiftedIn
b2ShiftedIn = B2 {b2 = Point {x_axis = 2.0111625243949613, y_axis = -23.479072153532773, z_axis = 498.63505747126436}}
b4ShiftedIn = B4 {b4 = Point {x_axis = 2.1505994093368814, y_axis = -25.182038443651887, z_axis = 495.9051724137931}}
--needs proper values entered.
backTopFaceOfFirstSocketCube =
  BackFace {b1 = Point {x_axis = 2.0111625243949613, y_axis = -23.479072153532773, z_axis = 498.63505747126436},
            b2 = Point {x_axis = 3.882888163848002, y_axis = -22.020953051239964, z_axis = 500.0},
            b3 = Point {x_axis = 0.0, y_axis = -23.234224965706463, z_axis = 500.0},
            b4 = Point {x_axis = 2.0111625243949613, y_axis = -23.479072153532773, z_axis = 498.63505747126436}}

backBtmFaceOfFirstSocketCube =
  BackFace {b1 = Point {x_axis = 2.1505994093368814, y_axis = -25.182038443651887, z_axis = 495.9051724137931},
            b2 = Point {x_axis = 0.0, y_axis = -26.88294664396197, z_axis = 494.54022988505744},
            b3 = Point {x_axis = 4.440635703615683, y_axis = -25.184096533460924, z_axis = 494.54022988505744},
            b4 = Point {x_axis = 2.1505994093368814, y_axis = -25.182038443651887, z_axis = 495.9051724137931}}

frontRightCenteredPointOfFirstSocketCube =
  Point {x_axis = 0.0, y_axis = -28.058585804834216, z_axis = 497.2701149425287}

frontLeftCenteredPointOfFirstSocketCube =
 Point {x_axis = 4.682706466732634, y_axis = -26.55694805138707, z_axis = 497.2701149425287}

f3ShiftedIn = F3 {f3 = Point {x_axis = 1.1706766166831586, y_axis = -27.68317636647243, z_axis = 497.2701149425287}}
f1ShiftedIn = F1 {f1 = Point {x_axis = 3.5120298500494758, y_axis = -26.932357489748856, z_axis = 497.2701149425287}}
b3ShiftedIn = B3 {b3 = Point {x_axis = 1.0404404834329606, y_axis = -24.694570551713273, z_axis = 497.2701149425287}}
b1ShiftedIn = B1 {b1 = Point {x_axis = 3.1213214502988818, y_axis = -23.966540045471387, z_axis = 497.2701149425287}}
rightBackCenterPointOfFirstCube = Point {x_axis = 0.0, y_axis = -25.058585804834216, z_axis = 497.2701149425287}
leftBackCenteredPointOfFirstCube = Point {x_axis = 4.161761933731842, y_axis = -23.602524792350444, z_axis = 497.2701149425287}
frontRightDiamondFaceOfFirstCube =
  FrontFace {f1 = Point {x_axis = 1.1706766166831586, y_axis = -27.68317636647243, z_axis = 497.2701149425287},
             f2 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
             f3 = Point {x_axis = 0.0, y_axis = -29.88294664396197, z_axis = 494.54022988505744},
             f4 = Point {x_axis = 1.1706766166831586, y_axis = -27.68317636647243, z_axis = 497.2701149425287}}
frontLeftDiamondFaceOfFirstCube =
  FrontFace {f1 = Point {x_axis = 3.5120298500494758, y_axis = -26.932357489748856, z_axis = 497.2701149425287},
             f2 = Point {x_axis = 4.961580236616474, y_axis = -28.138519792497544, z_axis = 494.54022988505744},
             f3 = Point {x_axis = 4.403832696848794, y_axis = -24.975376310276598, z_axis = 500.0},
             f4 = Point {x_axis = 3.5120298500494758, y_axis = -26.932357489748856, z_axis = 497.2701149425287}}
backRightDiamondFaceOfFirstSocket =
  BackFace {b1 = Point {x_axis = 1.0404404834329606, y_axis = -24.694570551713273, z_axis = 497.2701149425287},
            b2 = Point {x_axis = 0.0, y_axis = -23.234224965706463, z_axis = 500.0},
            b3 = Point {x_axis = 0.0, y_axis = -26.88294664396197, z_axis = 494.54022988505744},
            b4 = Point {x_axis = 1.0404404834329606, y_axis = -24.694570551713273, z_axis = 497.2701149425287}}
backLeftDiamondFaceOfFirstSocket =
  BackFace {b1 = Point {x_axis = 3.1213214502988818, y_axis = -23.966540045471387, z_axis = 497.2701149425287},
            b2 = Point {x_axis = 4.440635703615683, y_axis = -25.184096533460924, z_axis = 494.54022988505744},
            b3 = Point {x_axis = 3.882888163848002, y_axis = -22.020953051239964, z_axis = 500.0},
            b4 = Point {x_axis = 3.1213214502988818, y_axis = -23.966540045471387, z_axis = 497.2701149425287}}
frontTopRightCornerOfFirstSocket =
  FrontFace {f1 = Point {x_axis = 2.271634790895357, y_axis = -26.456283783051088, z_axis = 498.63505747126436},
             f2 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
             f3 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
             f4 = Point {x_axis = 1.1706766166831586, y_axis = -27.68317636647243, z_axis = 497.2701149425287}}
-- =================================================================== emergent: ===============================================================

frontTopRightCornerTest = TestCase $ assertEqual
  "look at top front right corner of 1st socket cube"
  (Just frontTopRightCornerOfFirstSocket)
  (topRightDiamondFace $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )
  
lookAtTopOfDiamondEmergent = TestCase $ assertEqual
  "look at the f1 of the top of diamond for first socket cube"
  (Just $ centerOfTopFrontlinePoint)
  (topCenterPoint $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

lookAtBackTopCenterPointOfDiamond = TestCase $ assertEqual
  "look at the b1 of the back top of diamond for first socket cube"
  (Just $ centerOfTopBackLinePoint)
  (topCenterPoint $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

lookAtBackRightCenterPointOfDiamond = TestCase $ assertEqual
  "look at the  back right center point of diamond for first socket cube"
  (Just rightBackCenterPointOfFirstCube)
  (rightCenterPoint $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

lookAtBackLeftCenterPointOfDiamond = TestCase $ assertEqual
  "look at the  back left center point of diamond for first socket cube"
  (Just $ leftBackCenteredPointOfFirstCube)
  (leftCenterPoint $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

lookAtFrontRightCenterPointOfDiamond = TestCase $ assertEqual
  "look at the front right centered point of diamond for first socket cube"
  (Just $ frontRightCenteredPointOfFirstSocketCube)
  (rightCenterPoint $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

lookAtFrontLeftCenterPointOfDiamond = TestCase $ assertEqual
  "look at the front Left centered point of diamond for first socket cube"
  (Just $ frontLeftCenteredPointOfFirstSocketCube)
  (leftCenterPoint $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )


topOfDiamondFront = TestCase $ assertEqual
  "look at the F2 of the top front of diamond for first socket cube"
  (Just $ f2ShiftedIn )
  (topDiamondCorner $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

rightOfDiamondFront = TestCase $ assertEqual
  "look at the F3 of the right front of diamond for first socket cube"
  (Just $ f3ShiftedIn )
  (rightDiamondCorner $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

leftOfDiamondFront = TestCase $ assertEqual
  "look at the F1 of the left front of diamond for first socket cube"
  (Just $ f1ShiftedIn )
  (leftDiamondCorner $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )
  
topOfDiamondBack = TestCase $ assertEqual
  "look at the B2 of the top  of diamond for first socket cube"
  (Just $ topBackDiamondCorner )
  (topDiamondCorner $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

btmOfDiamondCenterBackPoint = TestCase $ assertEqual
  "look at the center of the btm back of diamond for first socket cube"
  (Just $ centerOfBtmBackLinePoint)
  (bottomCenterPoint $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

backRightOfDiamondCPoint = TestCase $ assertEqual
  "look at the back right cpoint of diamond for first socket cube"
  (Just b3ShiftedIn)
  (rightDiamondCorner $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

backLeftOfDiamondCPoint = TestCase $ assertEqual
  "look at the back left cpoint of diamond for first socket cube"
  (Just b1ShiftedIn)
  (leftDiamondCorner $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

btmOfDiamondCenterFrontPoint = TestCase $ assertEqual
  "look at the center of the btm front of diamond for first socket cube"
  (Just $ centerOfBtmFrontLinePoint)
  (bottomCenterPoint $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )



btmOfDiamondFrontF4ShiftedInt = TestCase $ assertEqual
  "look at the F4 of diamond for first socket cube"
  (Just $ f4ShiftedIn)
  (bottomDiamondCorner $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

btmOfDiamondBackB4ShiftedInt = TestCase $ assertEqual
  "look at the B4 of diamond for first socket cube"
  (Just $ b4ShiftedIn)
  (bottomDiamondCorner $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

btmOfDiamondBackB2ShiftedInt = TestCase $ assertEqual
  "look at the B2 of diamond for first socket cube"
  (Just $ b2ShiftedIn)
  (topDiamondCorner $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )
-- ========================================================== faces========================================
frontTopFaceTest = TestCase $ assertEqual
  "look at the front top face of diamond for first socket cube"
  (Just frontTopFaceOfFirstSocketCube)
  
  (topDiamondFace $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

frontBtmFaceTest = TestCase $ assertEqual
  "look at the front btm face of diamond for first socket cube"
  (Just frontBottomFaceOfFirstSocketCube)
  
  (bottomDiamondFace $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

frontRightTestFaceTest = TestCase $ assertEqual
  "look at the front right face of diamond for first socket cube"
  (Just frontRightDiamondFaceOfFirstCube)
  
  (rightDiamondFace $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )



frontLeftTestFaceTest = TestCase $ assertEqual
  "look at the front left face of diamond for first socket cube"
  (Just frontLeftDiamondFaceOfFirstCube)
  
  (leftDiamondFace $
   isTheFrontDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )


backTopFaceTest = TestCase $ assertEqual
  "look at the back top face of diamond for first socket cube"
  (Just backTopFaceOfFirstSocketCube)
  
  (topDiamondFace $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

backRightFaceTest = TestCase $ assertEqual
  "look at the back right face of diamond for first socket cube"
  (Just backRightDiamondFaceOfFirstSocket)
  
  (rightDiamondFace $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

backBtmFaceTest = TestCase $ assertEqual
  "look at the back btm face of diamond for first socket cube"
  (Just backBtmFaceOfFirstSocketCube)
  
  (bottomDiamondFace $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )

-- =========================================== ============== current===========================================
backleftFaceTest = TestCase $ assertEqual
  "look at the back left face of diamond for first socket cube"
  (Just backLeftDiamondFaceOfFirstSocket)
  
  (leftDiamondFace $
   isTheBackDiamondDone
   (defaultDiamondBuilder firstCubeOfSocket )
  )
