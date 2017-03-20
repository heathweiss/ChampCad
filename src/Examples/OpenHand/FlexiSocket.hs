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
                                    extractF2, extractF3, extractF4, extractF1, extractB1, extractB2, extractB3, extractB4, extractBackRightLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    f34LineFromF12Line, toBackFace, reverseNormal, toBottomFrontLine, toFrontTopLine,
                                    toFrontLeftLine, toFrontRightLine, toBackBottomLine, toBackTopLine)
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

cutTheDiamondFrontBase :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints)
                       -> CornerPoints -> CornerPoints -> CornerPoints
cutTheDiamondFrontBase    reverseBtm reverseTop btmPoints       topPoints       =
  let btmFrontLine  = reverseBtm $ toBottomFrontLine  btmPoints
      topFrontLine  = reverseTop $ toFrontTopLine topPoints
  in  btmFrontLine +++ topFrontLine

cutTheDiamondBackBase :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> CornerPoints -> CornerPoints -> CornerPoints
cutTheDiamondBackBase    reverseBtm reverseTop btmPoints       topPoints       =
  let btmBackLine  = reverseBtm $ toBackBottomLine  btmPoints
      topBackLine  = reverseTop $ toBackTopLine topPoints
  in  btmBackLine +++ topBackLine
      


cutTheDiamondTopFace :: CornerPoints -> CornerPoints
cutTheDiamondTopFace cube  =
  let frontFace = cutTheDiamondFrontBase
                    (id) (id)
                    (cutTheDiamondF2ShiftedIn cube)
                    (extractFrontTopLine cube)
      backFace = cutTheDiamondBackBase (id) (id) (cutTheDiamondB2ShiftedIn cube) (extractBackTopLine cube)
  in backFace +++ frontFace
     
cutTheDiamondTopRightCorner :: CornerPoints -> CornerPoints
cutTheDiamondTopRightCorner cube =
  let frontFace = cutTheDiamondFrontBase
                    (id) (id)
                    ((cutTheDiamondF2ShiftedIn cube) +++ (cutTheDiamondF3ShiftedIn cube))
                    (extractF3 cube)
      backFace  = cutTheDiamondBackBase
                    (id) (id)
                    ((cutTheDiamondB2ShiftedIn cube) +++ (cutTheDiamondB3ShiftedIn cube))
                    (extractB3 cube)
  in  frontFace +++ backFace
      
cutTheDiamondRightFace :: CornerPoints -> CornerPoints
cutTheDiamondRightFace cube =
  let frontFace  = cutTheDiamondFrontBase (id) (id) (cutTheDiamondF3ShiftedIn cube) (extractFrontRightLine $ cube)
      backFace   = cutTheDiamondBackBase (id) (id) (cutTheDiamondB3ShiftedIn cube) (extractBackRightLine $ cube)
  in  backFace +++ frontFace

cutTheDiamondBtmRightCorner :: CornerPoints -> CornerPoints
cutTheDiamondBtmRightCorner cube =
  let frontFace = cutTheDiamondFrontBase (reverseNormal) (id)
                                         ((cutTheDiamondF4ShiftedIn cube) +++ (cutTheDiamondF3ShiftedIn cube))
                                         (extractF4 cube)
      backFace = cutTheDiamondBackBase (reverseNormal) (id) ((cutTheDiamondB4ShiftedIn cube) +++ (cutTheDiamondB3ShiftedIn cube)) (extractB4 cube)
  in  backFace +++ frontFace

cutTheDiamondBtmFace :: CornerPoints -> CornerPoints
cutTheDiamondBtmFace cube =
  let frontFace = cutTheDiamondFrontBase (id) (reverseNormal) (cutTheDiamondF4ShiftedIn cube) (extractBottomFrontLine $ cube)
      backFace    = (transposeY (+(-10))) . toBackFace $ frontFace
  in backFace +++ frontFace

cutTheDiamondBtmLeftCorner :: CornerPoints -> CornerPoints
cutTheDiamondBtmLeftCorner cube =
  let frontFace      = cutTheDiamondFrontBase
                         (reverseNormal) (id)
                         ((cutTheDiamondF1ShiftedIn cube) +++ (cutTheDiamondF4ShiftedIn cube))
                         (extractF1 cube)
      
      backFace     = (transposeY (+(-10))) . toBackFace $ frontFace
  in  backFace +++ frontFace

cutTheDiamondLeftFace :: CornerPoints -> CornerPoints
cutTheDiamondLeftFace cube =
  let frontFace = cutTheDiamondFrontBase (id) (id) (cutTheDiamondF1ShiftedIn cube) ((extractF1 cube) +++ (extractF2 cube)) 
      backFace     = (transposeY (+(-10))) . toBackFace $ frontFace
  in  backFace +++ frontFace

cutTheDiamondTopLeftCorner :: CornerPoints -> CornerPoints
cutTheDiamondTopLeftCorner cube =
  let frontFace = cutTheDiamondFrontBase
                    (reverseNormal) (id)
                    ((cutTheDiamondF1ShiftedIn cube) +++ (cutTheDiamondF2ShiftedIn cube))
                    (extractF2 cube)
      backFace  = (transposeY (+(-10))) . toBackFace $ frontFace
  in  frontFace +++ backFace


     
cutTheDiamondF2Centered :: CornerPoints -> CornerPoints
cutTheDiamondF2Centered cube =
  cutTheDiamond2Centered (extractF2) (extractF3) (F2) cube
  
cutTheDiamondB2Centered :: CornerPoints -> CornerPoints
cutTheDiamondB2Centered cube =
  cutTheDiamond2Centered (extractB2) (extractB3) (B2) cube

cutTheDiamond2Centered :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond2Centered     extract2                          extract3                         constructor                cube             =
  offsetCornerPoints  0.5 0.5 0.5 (extract2 cube) (extract3 cube) (constructor)

cutTheDiamondF3Centered :: CornerPoints -> CornerPoints
cutTheDiamondF3Centered cube =
  cutTheDiamond3Centered (extractF3) (extractF4) (F3) cube

cutTheDiamondF4Centered :: CornerPoints -> CornerPoints
cutTheDiamondF4Centered cube =
  cutTheDiamond4Centered (extractF1) (extractF4)  (F4) cube

cutTheDiamondB4Centered :: CornerPoints -> CornerPoints
cutTheDiamondB4Centered cube =
  cutTheDiamond4Centered (extractB1) (extractB4) (B4) cube

cutTheDiamondF1Centered :: CornerPoints -> CornerPoints
cutTheDiamondF1Centered cube =
  cutTheDiamond1Centered (extractF2) (extractF1) (F1) cube

cutTheDiamondB1Centered :: CornerPoints -> CornerPoints
cutTheDiamondB1Centered cube  =
  cutTheDiamond1Centered (extractB2) (extractB1) (B1) cube

cutTheDiamond1Centered :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond1Centered     extract2                          extract1                          constructor                cube             =
  offsetCornerPoints  0.5 0.5 0.5 (extract2 cube) (extract1 cube) constructor

cutTheDiamondB3Centered :: CornerPoints -> CornerPoints
cutTheDiamondB3Centered cube =
  cutTheDiamond3Centered (extractB3) (extractB4) (B3) cube

cutTheDiamond3Centered :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond3Centered    extract3                          extract4                           constructor                cube             =
  offsetCornerPoints  0.5 0.5 0.5 (extract3 cube) (extract4 cube) constructor

cutTheDiamond4Centered :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond4Centered     extract1                          extract4                         constructor                cube             =
  offsetCornerPoints  0.5 0.5 0.5 (extract1 cube) (extract4 cube) constructor

cutTheDiamondD3Centered :: CornerPoints -> CornerPoints
cutTheDiamondD3Centered cube =
  cutTheDiamond3Centered (extractB3) (extractB4) (B3) cube

cutTheDiamondF1ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF1ShiftedIn cube =
  offsetCornerPoints  0.25 0.5 0.5 (cutTheDiamondF1Centered cube) (cutTheDiamondF3Centered cube) (F1)

cutTheDiamondF2ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF2ShiftedIn cube =
  cutTheDiamond2ShiftedIn (cutTheDiamondF2Centered) (cutTheDiamondF4Centered) (F2) cube

cutTheDiamondB2ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondB2ShiftedIn cube =
  cutTheDiamond2ShiftedIn (cutTheDiamondB2Centered) (cutTheDiamondB4Centered) (B2) cube
  
cutTheDiamond2ShiftedIn :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond2ShiftedIn    center2                           center4                           constructor                cube            =
  offsetCornerPoints  0 0 0.25 (center2 cube) (center4 cube) constructor
  
cutTheDiamondF3ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF3ShiftedIn cube =
  cutTheDiamond3ShiftedIn (cutTheDiamondF3Centered) (cutTheDiamondF1Centered) (F3) cube
{-
cutTheDiamondb3ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondb3ShiftedIn cube =
  cutTheDiamond3ShiftedIn (cutTheDiamondF3Centered) (cutTheDiamondB1Centered) (B3) cube
-}

cutTheDiamond3ShiftedIn :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond3ShiftedIn    center3                           center1                           constructor                cube            =
  offsetCornerPoints  0.25 0.5 0.5 (center3 cube) (center1 cube) constructor

cutTheDiamondB3ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondB3ShiftedIn cube =
  cutTheDiamond3ShiftedIn (cutTheDiamondB3Centered) (cutTheDiamondB1Centered) (B3) cube

cutTheDiamond4ShiftedIn :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond4ShiftedIn    center1                           center4                           constructor                cube            =
  offsetCornerPoints 0.5 0.5 0.25 (center1 cube) (center4 cube) constructor

cutTheDiamondB4ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondB4ShiftedIn cube =
  cutTheDiamond4ShiftedIn (cutTheDiamondB4Centered) (cutTheDiamondB2Centered) (B4) cube
  

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

offsetCornerPoints    offsetX   offsetY   offsetZ   (B2 b2')        (B4 b4')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b2' b4'

offsetCornerPoints    offsetX   offsetY   offsetZ   (B4 b4') (B2 b2')         cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b4' b2'

offsetCornerPoints    offsetX   offsetY   offsetZ   (B1 b1')        (B2 b2')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b1' b2' 

offsetCornerPoints    offsetX   offsetY   offsetZ   (B4 b4')        (B3 b3')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b4' b3'

offsetCornerPoints    offsetX   offsetY   offsetZ   (B3 b3') (B4 b4')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b3' b4'

offsetCornerPoints    offsetX   offsetY   offsetZ   (B3 b3') (B1 b1')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b3' b1'

offsetCornerPoints    offsetX   offsetY   offsetZ   (B2 b2')        (B1 b1')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b2' b1'

offsetCornerPoints    _ _ _  unhandled cornerpoints  _ =
  CornerPointsError "Missing pattern match for offsetCornerPoints"
-- ==================================================== local tests =========================================
flexiSocketTestsDo = do
  runTestTT buildTestCubeTest
  -- =offset xyz
  runTestTT offSetB2B4CornerPointsTest
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
  runTestTT cutTheDiamondBackBaseTopFaceTest
  runTestTT cutTheDiamondB2ShiftedInTest
  runTestTT cutTheDiamondB2CenteredTest
  runTestTT cutTheDiamondB4CenteredTest
  runTestTT cutTheDiamond3CenteredForB3Test
  runTestTT cutTheDiamondD3CenteredTest
  runTestTT cutTheDiamond1CenteredTest
  runTestTT cutTheDiamond1CenteredFrontTest
  runTestTT cutTheDiamondB1CenteredTest
  runTestTT cutTheDiamond3ShiftedInAsF3Test
  runTestTT cutTheDiamondB3CenteredTest
  runTestTT cutTheDiamondB3ShiftedInTest
  runTestTT cutTheDiamondB3ShiftedInTest
  runTestTT cutTheDiamond4ShiftedInTest
  runTestTT cutTheDiamondB4ShiftedInTest

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

offSetB2B4CornerPointsTest = TestCase $ assertEqual
  "used for top face"
  (B2 {b2 = Point {x_axis = 20.0, y_axis = 200.0, z_axis = 1750.0}})
  (offsetCornerPoints
     0.0
     0.0
     0.25
     (B2 $ Point 20 200 2000)
     (B4 $ Point 10 100 1000)
     (B2)
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
    BackFace {b1 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0},
              b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
              b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0},
              b4 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0}}-}

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

cutTheDiamondBackBaseTopFaceTest = TestCase $ assertEqual
 "cutTheDiamondBackBaseTopFaceTest"
 (BackFace {b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0},
            b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
            b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
            b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}})
 (cutTheDiamondBackBase (id) (id) (cutTheDiamondB2ShiftedIn testCube1) (extractBackTopLine testCube1)
 )

cutTheDiamondB2ShiftedInTest = TestCase $ assertEqual
 "cutTheDiamondB2ShiftedInTest"
 (B2 {b2 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}})
 (cutTheDiamondB2ShiftedIn testCube1
 )

cutTheDiamondB2CenteredTest = TestCase $ assertEqual
  "cutTheDiamondB2CenteredTest"
  (B2 {b2 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 20.0}})
  (cutTheDiamondB2Centered testCube1)

cutTheDiamondB4CenteredTest = TestCase $ assertEqual
  "cutTheDiamondB4CenteredTest"
  (B4 {b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 0.0}})
  (cutTheDiamondB4Centered testCube1 )

cutTheDiamond3CenteredForB3Test = TestCase $ assertEqual
  "test cutTheDiamond3Centered For B3"
  (B3 {b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamond3Centered (extractB3) (extractB4) (B3) testCube1)

cutTheDiamondD3CenteredTest = TestCase $ assertEqual
  "cutTheDiamondD3Centered test"
  (B3 {b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamondD3Centered testCube1)

cutTheDiamond1CenteredTest = TestCase $ assertEqual
 "cutTheDiamond1Centered for B1"
 (B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}})
 (cutTheDiamond1Centered (extractB2) (extractB1) (B1) testCube1)

cutTheDiamond1CenteredFrontTest = TestCase $ assertEqual
 "cutTheDiamond1Centered for F1"
 (F1 {f1 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 10.0}})
 (cutTheDiamond1Centered (extractF2) (extractF1) (F1) testCube1)

cutTheDiamondB1CenteredTest = TestCase $ assertEqual
 "cutTheDiamondB1Centered test"
 (B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}})
 (cutTheDiamondB1Centered testCube1)

cutTheDiamond3ShiftedInAsF3Test = TestCase $ assertEqual
  "cutTheDiamond3ShiftedIn as F3"
  (F3 {f3 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamond3ShiftedIn (cutTheDiamondF3Centered) (cutTheDiamondF1Centered) (F3) testCube1)

cutTheDiamondB3CenteredTest = TestCase $ assertEqual
  "cutTheDiamondB3Centered test"
  (B3 {b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamondB3Centered testCube1)

cutTheDiamondB3ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamondB3ShiftedIn test"
  (B3 {b3 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamondB3ShiftedIn testCube1)

cutTheDiamond4ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamond4ShiftedIn test"
  (B4 {b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0}})
  (cutTheDiamond4ShiftedIn (cutTheDiamondB4Centered) (cutTheDiamondB2Centered) (B4) testCube1)

cutTheDiamondB4ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamondB4ShiftedIn test"
  (B4 {b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0}})
  (cutTheDiamondB4ShiftedIn testCube1)
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
