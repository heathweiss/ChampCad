{-# LANGUAGE ParallelListComp #-}
--for persist
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{- |
Create a shortened socket to fit inside the SocketMount, and but up against the Wrist.

Cut diamond(?) shapes in each cube to give it more flex, and allow it to breath better.

Will be printed in flexible filament.

-}
module Examples.OpenHand.FlexiSocket(flexiSocketTestsDo, flexSocketStlGenerator, testCubeStlGenerator, testCubeShowCubes) where

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)


import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine,
                                    extractBackTopLine, extractRightFace, extractFrontRightLine, extractFrontLeftLine, extractBottomFrontLine,
                                    extractF2, extractF3, extractF4, extractF1)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    f34LineFromF12Line, toBackFace, reverseNormal, toBottomFrontLine, toFrontTopLine,
                                    toFrontLeftLine, toFrontRightLine)
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.HorizontalFaces(createTopFaces,  createBottomFaces, createTopFacesSquaredOff)

import TypeClasses.Transposable(transpose)


import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), )
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)
import Geometry.Rotation(rotateCornerPointAroundZAxis)
import Geometry.AngleRadius(AngleRadius(..), extractAngles, extractRadii,)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)

import Helpers.List((++:))

import Primitives.Cylindrical.Walled(squaredYLengthenedCylinder, squaredCylinder, cylinder)

import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Scan.ParseJuicy(getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                      calculateRadiusFrom)
import  Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)
import Helpers.List((++:),(++::))

import Builder.Sequence((@~+++@|>))
import Builder.List (newCornerPointsWith10DegreesBuilder, (||@~+++^||))
import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList, buildCubePointsListWithAdd, buildCubePointsListSingle)

import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))


import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Flow as Flw
import Control.Lens



--import qualified Test.HUnit as T
import Test.HUnit hiding (State)

--for the Builder system, but does not work with Radial Degrees system.
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

pixelsPerMM = 696/38
type RowReductionFactor = Int
type ExtensionHeight = Double
type PlateRadius = Double
type PixelsPerMillimeter = Double
type TransposeFactor = Double
type Thickness = Double
type Height = Double
type Power = Double
type LengthenYFactor = Double
type Yslope = Double
type Offset = Double


-- ======================================================== database ========================================
-- ================================================= socket generator =======================================
removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']

flexSocket :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter ->
               ExceptT BuilderError (State CpointsStack ) CpointsList
flexSocket    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM = do
  let transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = Point 0 0 500

  cubes   <- buildCubePointsListSingle "wristCubes"
             ( concat $ map (cutTheDiamond)
                            (concat $ take 2 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
             )

  return cubes




flexSocketStlGenerator :: IO ()
flexSocketStlGenerator  = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let rowReductionFactor = 100::RowReductionFactor
            innerSleeveTransposeFactor = (+6) --3 is that starndard value. Make it 6 as this has to fit over the wrist and ninjaflex
            innerSleeveMDR = (transpose innerSleeveTransposeFactor) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                degrees'
                              )
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (flexSocket (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

-- ==================================================== test cube========================================================
cutTheDiamond :: CornerPoints -> [CornerPoints]
cutTheDiamond cube =
  [cutTheDiamondTopFace cube,
   cutTheDiamondTopRightCorner cube,
   cutTheDiamondRightFace cube,
   cutTheDiamondBtmRightCorner cube,
   cutTheDiamondBtmFace cube,
   cutTheDiamondBtmLeftCorner cube,
   cutTheDiamondLeftFace cube,
   cutTheDiamondTopLeftCorner cube
  ]

cutTheDiamondTopFace :: CornerPoints -> CornerPoints
cutTheDiamondTopFace cube  =
  let cubeFrontTopLine = extractFrontTopLine cube
      f2NewAsBtmFace = toBottomFrontLine (cutTheDiamondF2ShiftedIn cube) 
      f2NewFrontFace = cubeFrontTopLine +++ f2NewAsBtmFace
      f2NewBackFace = (transposeY (+(-10))) . {-reverseNormal .-} toBackFace $ f2NewFrontFace

  in f2NewBackFace +++ f2NewFrontFace

cutTheDiamondTopRightCorner :: CornerPoints -> CornerPoints
cutTheDiamondTopRightCorner cube =
  let f2F3AsBtmFace = {-reverseNormal .-} toBottomFrontLine $ ((cutTheDiamondF2ShiftedIn cube) +++ (cutTheDiamondF3ShiftedIn cube))
      f3AsTopFace = toFrontTopLine $ extractF3 cube
      frontFace = f2F3AsBtmFace +++ f3AsTopFace
      backFace  = (transposeY (+(-10))) . {-reverseNormal $-} toBackFace $ frontFace
  in  frontFace +++ backFace

cutTheDiamondRightFace :: CornerPoints -> CornerPoints
cutTheDiamondRightFace cube =
  let f3AsBtmFrontLine = toBottomFrontLine $ cutTheDiamondF3ShiftedIn cube
      frontTopLine     = toFrontTopLine . extractFrontRightLine $ cube
      frontFace        = f3AsBtmFrontLine +++ frontTopLine
      backFace         = (transposeY (+(-10))) . toBackFace $ frontFace
  in  backFace +++ frontFace

cutTheDiamondBtmRightCorner :: CornerPoints -> CornerPoints
cutTheDiamondBtmRightCorner cube =
  let f3f4AsBtmFrontLine = reverseNormal . toBottomFrontLine $ ((cutTheDiamondF4ShiftedIn cube) +++ (cutTheDiamondF3ShiftedIn cube))
      f4AsFrontTopLine = toFrontTopLine $ extractF4 cube
      frontFace = f3f4AsBtmFrontLine +++ f4AsFrontTopLine
      backFace = (transposeY (+(-10))) . toBackFace $ frontFace
  in  backFace +++ frontFace


cutTheDiamondBtmFace :: CornerPoints -> CornerPoints
cutTheDiamondBtmFace cube =
  let cubeBtmFrontLineAsTopLine = reverseNormal . toFrontTopLine . extractBottomFrontLine $ cube
      f4AsBtmFrontLine = toBottomFrontLine (cutTheDiamondF4ShiftedIn cube)
      frontFace   = cubeBtmFrontLineAsTopLine +++ f4AsBtmFrontLine
      backFace    = (transposeY (+(-10))) . toBackFace $ frontFace
  in backFace +++ frontFace

cutTheDiamondBtmLeftCorner :: CornerPoints -> CornerPoints
cutTheDiamondBtmLeftCorner cube =
  let btmFrontLine = reverseNormal $ (cutTheDiamondF1ShiftedIn cube) +++ (cutTheDiamondF4ShiftedIn cube)
      topFrontLine = toFrontTopLine $ extractF1 cube
      frontFace    = btmFrontLine +++ topFrontLine
      backFace     = (transposeY (+(-10))) . toBackFace $ frontFace
  in  backFace +++ frontFace
      
cutTheDiamondLeftFace :: CornerPoints -> CornerPoints
cutTheDiamondLeftFace cube =
  let btmFrontLine = toBottomFrontLine (cutTheDiamondF1ShiftedIn cube)
      topFrontLine = toFrontTopLine ((extractF1 cube) +++ (extractF2 cube))
      frontFace    = btmFrontLine +++ topFrontLine
      backFace     = (transposeY (+(-10))) . toBackFace $ frontFace
  in  backFace +++ frontFace

cutTheDiamondTopLeftCorner :: CornerPoints -> CornerPoints
cutTheDiamondTopLeftCorner cube =
  let btmFrontLine = reverseNormal $ toBottomFrontLine ((cutTheDiamondF1ShiftedIn cube) +++ (cutTheDiamondF2ShiftedIn cube))
      topFrontLine = toFrontTopLine $ extractF2 cube
      frontFace    = btmFrontLine +++ topFrontLine
      backFace     = (transposeY (+(-10))) . toBackFace $ frontFace
  in  frontFace +++ backFace

cutTheDiamondF1Centered :: CornerPoints -> CornerPoints
cutTheDiamondF1Centered cube =
  let frontLeftLine = extractFrontLeftLine cube
  in  offsetCornerPoints  0.5 0.5 0.5 (extractF2 frontLeftLine) (extractF1 frontLeftLine) (F1)
     
cutTheDiamondF2Centered :: CornerPoints -> CornerPoints
cutTheDiamondF2Centered cube =
  let cubeFrontTopLine = extractFrontTopLine cube
  in offsetCornerPoints  0.5 0.5 0.5 (extractF2 cubeFrontTopLine) (extractF3 cubeFrontTopLine) (F2)

cutTheDiamondF3Centered :: CornerPoints -> CornerPoints
cutTheDiamondF3Centered cube =
  let cubeFrontRightLine = extractFrontRightLine cube
  in  offsetCornerPoints  0.5 0.5 0.5 (extractF3 cubeFrontRightLine) (extractF4 cubeFrontRightLine) (F3)  

cutTheDiamondF4Centered :: CornerPoints -> CornerPoints
cutTheDiamondF4Centered cube =
  let cubeBtmFrontLine = extractBottomFrontLine cube
  in offsetCornerPoints  0.5 0.5 0.5 (extractF1 cubeBtmFrontLine) (extractF4 cubeBtmFrontLine) (F4)

cutTheDiamondF1ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF1ShiftedIn cube =
  offsetCornerPoints  0.25 0.5 0.5 (cutTheDiamondF1Centered cube) (cutTheDiamondF3Centered cube) (F1)

cutTheDiamondF2ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF2ShiftedIn cube =
  offsetCornerPoints  0 0 0.25 (cutTheDiamondF2Centered cube) (cutTheDiamondF4Centered cube) (F2)
  
cutTheDiamondF3ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF3ShiftedIn cube =
  offsetCornerPoints 0.25 0.5 0.5 (cutTheDiamondF3Centered cube) (cutTheDiamondF1Centered cube) (F3)

cutTheDiamondF4ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF4ShiftedIn cube =
  offsetCornerPoints 0 0 0.25 (cutTheDiamondF4Centered cube) (cutTheDiamondF2Centered cube) (F4)
  
testCube1 = 
  CubePoints {
            f1 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
            f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
            f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
            f4 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
            b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
            b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
            b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
            b4 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0}}
  
testCube :: ExceptT BuilderError (State CpointsStack ) CpointsList
testCube = do
  

  {-
  cube
    <- buildCubePointsListSingle "cube"
          [cube']
  -}
  cutterCubes <-
     buildCubePointsListSingle "cube"
     (cutTheDiamond testCube1)

  return cutterCubes


testCubeStlGenerator :: IO ()
testCubeStlGenerator = do
  let cpoints =  ((execState $ runExceptT   testCube       ) [])
  writeStlToFile $ newStlShape "motorMount"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

testCubeShowCubes :: IO ()
testCubeShowCubes = do
  let cpoints =  ((evalState $ runExceptT   testCube       ) [])
  print $ show cpoints
  -- =================================================== centering functions  ===========================================
{-
The offset should always be adjusted in relation to the 1st point to keep everthing standard.
Otherwise the offset will differ depending on the angle around a radial shape.
-}
offsetPoint :: Offset -> Offset -> Offset -> Point ->         Point -> Point
offsetPoint    offsetX   offsetY   offsetZ  (Point x1 y1 z1) (Point x2 y2 z2) =
  let
     setX = x1 + (((x1 - x2) * offsetX * (-1)))
        
     setY = y1 + ((y1 - y2) * offsetY * (-1))
        
     setZ = z1 + ((z1 - z2) * offsetZ * (-1))
        
  in
    Point setX setY setZ

{-
Offset CornerPoints, based on the underlying Points.
Obeys the same rules as offsetPoint.
Uses a CornerPoints constructor to set the resulting CornerPoint 
-}
offsetCornerPoints :: Offset -> Offset -> Offset -> CornerPoints -> CornerPoints -> (Point -> CornerPoints) -> CornerPoints
offsetCornerPoints    offsetX   offsetY   offsetZ   (F1 f1')        (F4 f4')        cornerPointConst =
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f1' f4'

offsetCornerPoints    offsetX   offsetY   offsetZ   (F1 f1')        (F3 f3')        cornerPointConst =
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f1' f3'   
 
offsetCornerPoints    offsetX   offsetY   offsetZ   (F2 f2')        (F3 f3')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f2' f3'

offsetCornerPoints    offsetX   offsetY   offsetZ   (F2 f2')        (F4 f4')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f2' f4'

offsetCornerPoints    offsetX   offsetY   offsetZ   (F2 f2')        (F1 f1')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f2' f1'

-- =================================================================================================================================
offsetCornerPoints    offsetX   offsetY   offsetZ   (F3 f3')  (F1 f1')                cornerPointConst =
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f3' f1'   

offsetCornerPoints    offsetX   offsetY   offsetZ   (F4 f4') (F2 f2')   cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f4' f2'

offsetCornerPoints    offsetX   offsetY   offsetZ   (F1 f1')        (F2 f2')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f1' f2' 

offsetCornerPoints    offsetX   offsetY   offsetZ   (F4 f4')        (F3 f3')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f4' f3'

offsetCornerPoints    offsetX   offsetY   offsetZ (F3 f3') (F4 f4')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  f3' f4' 

offsetCornerPoints    offsetX   offsetY   offsetZ   (B1 b1')        (B4 b4')        cornerPointConst =
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b1' b4'     
 
offsetCornerPoints    offsetX   offsetY   offsetZ   (B2 b2')        (B3 b3')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b2' b3'

offsetCornerPoints    offsetX   offsetY   offsetZ   (B1 b1')        (B2 b2')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b1' b2' 

offsetCornerPoints    offsetX   offsetY   offsetZ   (B4 b4')        (B3 b3')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b4' b3'




offsetCornerPoints    _ _ _  unhandled cornerpoints  _ =
  CornerPointsError "Missing pattern match for offsetCornerPoints"
-- ==================================================== local tests =========================================
flexiSocketTestsDo = do
  runTestTT buildTestCubeTest
  -- =offset xyz
  runTestTT offsetXYZTest
  runTestTT offsetXYZReversedTest
  runTestTT offsetXYZTest1
  runTestTT offsetXYZReversedTest1
  runTestTT offsetXYZTest2
  runTestTT offsetXYZReversedTest2
  runTestTT offsetXYZTest3
  runTestTT offsetXYZReversedTest3
  runTestTT offsetXYZTest4
  runTestTT offsetXYZTest5
  runTestTT offsetXYZReversedTest5
  -- = offset cornerpoints
  runTestTT offsetUnhandledCornerPointsTest
  runTestTT offSetF1F4CornerPointsTest
  runTestTT offSetF2F3CornerPointsTest
  -- cut some diamonds
  runTestTT cutDiamondTopFaceDissectedTest
  runTestTT cutDiamondTopFaceTest
  runTestTT cutTheDiamondBtmFaceTest
  runTestTT cutTheDiamondF3CenteredTest
  runTestTT cutTheDiamondF1CenteredTest
  runTestTT cutTheDiamondF4CenteredTest
  runTestTT cutTheDiamondF3ShiftedInTest
  runTestTT cutTheDiamondF3ShiftedInOffsetTest
  runTestTT cutTheDiamondF1ShiftedInTest
  runTestTT cutTheDiamondTopRightCornerTest
  runTestTT cutTheDiamondBtmRightCornerTest
  runTestTT cutTheDiamondRightFaceTest
  runTestTT cutTheDiamondBtmLeftCornerTest
  runTestTT cutTheDiamondLeftFaceTest
  runTestTT cutTheDiamondTopLeftCornerTest

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
offsetXYZTest = TestCase $ assertEqual 
  "offsetXYZTest"
  --75% of diff(10) = 7.5 75 750 respectively.
  --This gets adjusted from the 1st point.
  (Point 12.5 125 1250)
  (offsetPoint
     0.75
     0.75
     0.75
     (Point 20 200 2000)
     (Point 10 100 1000)
  )

offsetXYZReversedTest = TestCase $ assertEqual 
  "offsetXYZReversedTest"
  --75% of diff(10) = 7.5 75 750 respectively.
  --This gets adjusted from the 1st point.
  (Point 17.5 175 1750)
  (offsetPoint
     0.75
     0.75
     0.75
     (Point 10 100 1000)
     (Point 20 200 2000)
  )

offsetXYZTest1 = TestCase $ assertEqual 
  "offsetXYZTest1"
  (Point 2.5 25 250)
  (offsetPoint
     0.75
     0.75
     0.75
     (Point (-20) (-200) (-2000))
     (Point   10    100    1000)
  )

offsetXYZReversedTest1 = TestCase $ assertEqual 
  "offsetXYZReversedTest1"
  (Point (-12.5) (-125) (-1250))
  (offsetPoint
     0.75
     0.75
     0.75
     (Point   10    100    1000)
     (Point (-20) (-200) (-2000))
  )

offsetXYZTest2 = TestCase $ assertEqual 
  "offsetXYZTest2"
  (Point 17.5 175 1750)
  (offsetPoint
     0.75
     0.75
     0.75
     (Point 10 100 1000)
     (Point 20 200 2000)
  )

offsetXYZReversedTest2 = TestCase $ assertEqual 
  "offsetXYZReversedTest2"
  (Point 12.5 125 1250)
  (offsetPoint
     0.75
     0.75
     0.75
     (Point 20 200 2000)
     (Point 10 100 1000)
  )

offsetXYZTest3 = TestCase $ assertEqual 
  "offsetXYZTest3"
  (Point (-5) 0 0)
  (offsetPoint
     0.5
     0.5
     0.5
     (Point 10 0 0)
     (Point (-20) 0 0)
  )

offsetXYZReversedTest3 = TestCase $ assertEqual 
  "offsetXYZReversedTest3"
  (Point 2.5 25 250)
  (offsetPoint
     0.75
     0.75
     0.75
     (Point (-20) (-200) (-2000))
     (Point   10    100    1000)
  )

offsetXYZTest4 = TestCase $ assertEqual 
  "offsetXYZTest4"
  (Point 10 100 1000)
  (offsetPoint
     0.75
     0.75
     0.75
     (Point 10 100 1000)
     (Point 10 100 1000)
  )

offsetXYZTest5 = TestCase $ assertEqual 
  "offsetXYZTest5"
  (Point (-17.5) (-175) (-1750))
  (offsetPoint
     0.75
     0.75
     0.75
     (Point (-10) (-100)(-1000))
     (Point (-20) (-200)(-2000))
  )

offsetXYZReversedTest5 = TestCase $ assertEqual 
  "offsetXYZReversedTest5"
  (Point (-12.5) (-125) (-1250))
  (offsetPoint
     0.75
     0.75
     0.75
     (Point (-20) (-200)(-2000))
     (Point (-10) (-100)(-1000))
  )

-- ======== offsett 2 cornerpoints====
offsetUnhandledCornerPointsTest = TestCase $ assertEqual
  "offsetUnhandledCornerPointsTest"
  (CornerPointsError {errMessage = "Missing pattern match for offsetCornerPoints"})
  (offsetCornerPoints
     0.75
     0.75
     0.75
     (F1 $ Point 1 1 1)
     (F1 $ Point 0 0 0)
     (F1)
  )

offSetF1F4CornerPointsTest = TestCase $ assertEqual
  "offSetF1F4CornerPointsTest"
  (F1 $ Point 12.5 125 1250)
  (offsetCornerPoints
     0.75
     0.75
     0.75
     (F1 $ Point 20 200 2000)
     (F4 $ Point 10 100 1000)
     (F1)
  )

offSetF2F3CornerPointsTest = TestCase $ assertEqual
  "offSetF1F4CornerPointsTest"
  (F1 $ Point 12.5 125 1250)
  (offsetCornerPoints
     0.75
     0.75
     0.75
     (F2 $ Point 20 200 2000)
     (F3 $ Point 10 100 1000)
     (F1)
  )


-- =========================== cut the cube=======================


cutDiamondTopFaceDissectedTest = TestCase $ assertEqual 
  "cutDiamondPiecesTest"
  ( -- cubeFrontTopLine
    --FrontTopLine {f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
    --              f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0}}
    -- f2New F2 {f2 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 20.0}}
    -- f4New F4 {f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 0.0}}
    -- f2NewDropped F2 {f2 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0}}
    -- f4NewRaised F4 {f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0}}
    -- f2NewAsBtmFace BottomFrontLine {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
    --                                f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0}}
    -- f2NewFrontFace
    --FrontFace {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
    --           f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
    --           f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
    --           f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0}}
    -- f2NewBackFace
    --BackFace {b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0},
    --          b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
    --          b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
    --         b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}}
    -- final cube
    CubePoints {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
                f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
                f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
                f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
                b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0},
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
                b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
                b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}}



  )
  (
    let cubeFrontTopLine = extractFrontTopLine testCube1
        cubeBtmFrontLine = extractBottomFrontLine testCube1
        f2New = offsetCornerPoints  0.5 0.5 0.5 (extractF2 cubeFrontTopLine) (extractF3 cubeFrontTopLine) (F2)
        f4New = offsetCornerPoints  0.5 0.5 0.5 (extractF1 cubeBtmFrontLine) (extractF4 cubeBtmFrontLine) (F4)
        f2NewDropped = offsetCornerPoints  0 0 0.25 f2New f4New (F2)
        f4NewRaised  = offsetCornerPoints 0 0 0.25 f4New f2New (F4)
        f2NewAsBtmFace = toBottomFrontLine f2NewDropped
        f2NewFrontFace = cubeFrontTopLine +++ f2NewAsBtmFace
        f2NewBackFace = (transposeY (+(-10))) . {-reverseNormal .-} toBackFace $ f2NewFrontFace
    in  f2NewBackFace +++ f2NewFrontFace
        
        
  )


cutDiamondTopFaceTest = TestCase $ assertEqual 
  "cutDiamondTopFaceTest"
  (
    CubePoints {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
                f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
                f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
                f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
                b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0},
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
                b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
                b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}}
  )
  (cutTheDiamondTopFace testCube1)

cutTheDiamondF3CenteredTest  = TestCase $ assertEqual 
  "cutTheDiamondF3CenteredTest"
  (F3 {f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamondF3Centered testCube1)

cutTheDiamondF1CenteredTest = TestCase $ assertEqual 
  "cutTheDiamondF1CenteredTest"
  (F1 {f1 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamondF1Centered testCube1)

cutTheDiamondF4CenteredTest = TestCase $ assertEqual 
  "cutTheDiamondF4CenteredTest"
  (F4 {f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 0.0}})
  (cutTheDiamondF4Centered testCube1)

cutTheDiamondF3ShiftedInTest = TestCase $ assertEqual 
  "cutTheDiamondF3ShiftedInTest"
  (F3 {f3 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10}})
  (cutTheDiamondF3ShiftedIn testCube1)

cutTheDiamondF3ShiftedInOffsetTest = TestCase $ assertEqual 
  "cutTheDiamondF3ShiftedInOffsetTest"
  (Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10})
  (offsetPoint 0.25 0.5 0.5 (Point {x_axis = 10.0, y_axis = 10.0, z_axis = 10.0}) (Point {x_axis = 0.0, y_axis = 10.0, z_axis = 10.0}))

cutTheDiamondF1ShiftedInTest = TestCase $ assertEqual 
  "cutTheDiamondF1ShiftedInTest"
  (F1 {f1 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamondF1ShiftedIn testCube1)

cutTheDiamondTopRightCornerTest = TestCase $ assertEqual 
  "cutTheDiamondTopRightCornerTest"
  ( {-f2F3AsBtmFace
    BottomFrontLine {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
                    f4 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0}}-}
    {-f3AsTopFace
    FrontTopLine {f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
                  f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0}}-}
    {-FrontFace
    FrontFace {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
               f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
               f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
               f4 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0}}-}
    {-
    BackFace {b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0},
              b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
              b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
              b4 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0}}-}

    CubePoints {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
                f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
                f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
                f4 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0},
                b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0},
                b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
                b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
                b4 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0}}


  )
  (cutTheDiamondTopRightCorner testCube1)

cutTheDiamondBtmFaceTest = TestCase $ assertEqual 
  "cutTheDiamondBtmFaceTest"
  ( CubePoints {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0},
                f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
                f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0},
                b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0},
                b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0},
                b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0}}


  )
  (cutTheDiamondBtmFace testCube1)

cutTheDiamondBtmRightCornerTest = TestCase $ assertEqual
  "cutTheDiamondBtmRightCornerTest"
  ( {--f3f4AsBtmFrontLine
    BottomFrontLine {f1 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0},
                     f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0}}-}
    CubePoints {f1 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0},
                f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
                f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
                f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0},
                b1 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0},
                b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0},
                b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0},
                b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0}}


  )
  

  (cutTheDiamondBtmRightCorner testCube1)

cutTheDiamondRightFaceTest = TestCase $ assertEqual
  "cutTheDiamondRightFaceTest"
   
  ( {-
    BottomFrontLine {f1 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0},
                    f4 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0}}-}
    {-
    FrontTopLine {f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
                  f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0}}-}
    {-
    FrontFace {f1 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0},
               f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
               f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
               f4 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0}}-}
    CubePoints {f1 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0},
                f2 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
                f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
                f4 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0},
                b1 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0},
                b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
                b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0},
                b4 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0}}



  )
  (cutTheDiamondRightFace testCube1)

cutTheDiamondBtmLeftCornerTest = TestCase $ assertEqual
  "cutTheDiamondBtmLeftCornerTest"
  ( {-
    BottomFrontLine {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0},
                    f4 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0}})-}
    {-
    FrontTopLine {f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                  f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0}}-}
    CubePoints {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0},
                f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                f4 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0},
                b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0},
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                b4 = Point {x_axis = 2.5, y_axis = 0.0, z_axis = 10.0}}

  )

  (cutTheDiamondBtmLeftCorner testCube1)

cutTheDiamondLeftFaceTest = TestCase $ assertEqual
  "cutTheDiamondLeftFaceTest"
  ( {-
    BottomFrontLine {f1 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0},
                    f4 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0}}-}
    {-
    FrontTopLine {f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                  f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0}}-}
    CubePoints {f1 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0},
                f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
                f4 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0},
                b1 = Point {x_axis = 2.5, y_axis = 0.0, z_axis = 10.0},
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
                b4 = Point {x_axis = 2.5, y_axis = 0.0, z_axis = 10.0}}


  )
  (cutTheDiamondLeftFace testCube1)

cutTheDiamondTopLeftCornerTest = TestCase $ assertEqual
  "cutTheDiamondTopLeftCornerTest"
  ( {-
    BottomFrontLine {f1 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0},
                    f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0}}-}
    {-
    FrontTopLine {f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
                  f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0}}-}
    CubePoints {f1 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0},
                f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
                f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
                f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 15.0},
                b1 = Point {x_axis = 2.5, y_axis = 0.0, z_axis = 10.0},
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
                b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
                b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}}
  )
  (cutTheDiamondTopLeftCorner testCube1)
{-
testCube1 = 
  CubePoints {
            f1 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
            f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
            f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
            f4 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
            b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
            b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
            b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
            b4 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0}}
-}  
