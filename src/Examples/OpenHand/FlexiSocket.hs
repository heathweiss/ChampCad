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
module Examples.OpenHand.FlexiSocket(flexSocketStlGenerator, flexSocketShowCurrentState,
                                     {-testCubeStlGenerator, testCubeShowCubes,
                                     testCubeRotatedStlGenerator,-} flexSocketPlainStlGenerator,
                                     flexSocketPlainStlGeneratorDbStlGeneretor,
                                     initializeDatabase, insertFlexDimensions, FlexDimensions(..),
                                     flexSocketWithRiserDbStlGenerator, uniqueFlexDimensionName) where

import Examples.OpenHand.Common(Dimensions(..), commontDBName, uniqueDimensionName, flexOuterTranspose, setFlexiSocketCommonFactors,
                                Dimensions(..), CommonFactors(..) )

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)


import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackBottomLine,
                                    extractBackTopLine, extractRightFace, extractFrontRightLine, extractFrontLeftLine, extractBottomFrontLine,
                                    extractF2, extractF3, extractF4, extractF1, extractB1, extractB2, extractB3, extractB4, extractBackRightLine,
                                    extractBackLeftLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace, 
                                    f34LineFromF12Line, toBackFace, reverseNormal, toBottomFrontLine, toFrontTopLine,
                                    toFrontLeftLine, toFrontRightLine, toBackBottomLine, toBackTopLine, toBottomFace, toBackRightLine)
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.HorizontalFaces(createTopFaces,  createBottomFaces, createTopFacesSquaredOff)

import Geometry.Angle(RotateFactor, getXYAngle, Angle(..), getQuadrantAngle, rotateAngle)
import Geometry.Radius(calcultateXYDistance)
import Geometry.Vertex(getXWithQuadrant, getYWithQuadrant, Vertex(..), adjustPointAxis)

import TypeClasses.Transposable(transpose)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)

import Helpers.List((++:))

import Primitives.Cylindrical.Walled(squaredYLengthenedCylinder, squaredCylinder, cylinder)
import Primitives.DiamondCutter(DiamondBuilder(..), runDiamondBuilder, runFrontToBackDiamondBuilder, OffSet(..))

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

import Data.Maybe(isNothing, fromJust, isJust)

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

-- =====================================================database============================================================================
-- =========================================================================================================================================
--Data will be part of Common.sql

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FlexDimensions
   name String
   UniqueFlexDimensionName name
   desc String
   riserHeight Double --z distance from btm to top of riser
   socketToRiserHeight Double --height above socket, at which btm of riser starts
   innerRiserRadius Double 
   xAdjustment Double
   yAdjustment Double
  deriving Show
|]

uniqueFlexDimensionName = UniqueFlexDimensionName

-- | Initialize a new database with all tables. Will alter tables of existing db.
initializeDatabase :: IO ()
initializeDatabase = runSqlite commontDBName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "flex socket db initialized"

-- | Insert a new flex socket Dimensions into the database.
insertFlexDimensions :: IO ()
insertFlexDimensions     = runSqlite commontDBName $ do
  dimensionsId
            <- insert $ FlexDimensions
               "sharkfin" 
               "make it the same dimensions as the shark swim fin which fits him good in Mar/17"
               20 --riserHeight
               10 --socketToRiserHeight
               20 --innerRiserRadius
               0  --x adjust
               (-10)--y adjust
               
  --insert $ CurrentDimensions dimensionsId
  liftIO $ putStrLn "flex dimensions inserted"
-- ===================================================socket with round riser ==============================================================
-- =========================================================================================================================================
{-
Add a round riser to the top for joining to the finger joint section.
The motor mount socket will glue to the outside of this, with no need to join up at the riser section.
This riser, flexible material, should go over top of the finger joints riser, to give it strength, as finger section will be solid material.
For now, does not used diamond cutter, as it does not yet work.

Should be based on the socket made for the openBionics.com socket: Examples.OpenBionicsCom.OpenBionicsDotComDesignWork.shortSocketToLargeShaft
-}

flexSocketWithRiserDbStlGenerator :: String -> IO ()
flexSocketWithRiserDbStlGenerator dimensionsName  = runSqlite commontDBName $ do
  maybeCommonDimensions <- getBy $ uniqueDimensionName dimensionsName
  maybeFlexDimensions <- getBy $ UniqueFlexDimensionName dimensionsName
  case maybeCommonDimensions of
        Nothing -> liftIO $ putStrLn "common dimensions not found"
        Just (Entity commonDimensionsId commonDimensions) -> do
          --liftIO $ flexSocketPlainStlGenerator $ setFlexiSocketCommonFactors commonDimensions
          liftIO $ putStrLn "common dimensions found"
          case maybeFlexDimensions of 
           Nothing -> liftIO $ putStrLn "flex dimensions not found"
           Just (Entity flexDimensionsId flexDimensions) -> do
             liftIO $ putStrLn "flex dimensions found"
             liftIO $ flexSocketWithRiserStlGenerator (setFlexiSocketCommonFactors commonDimensions) flexDimensions


flexSocketWithRiserStlGenerator :: CommonFactors ->  FlexDimensions -> IO ()
flexSocketWithRiserStlGenerator (CommonFactors innerTranspose outerTranspose drop' take' ) flexDimensions = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let rowReductionFactor = 100::RowReductionFactor
            innerSleeveMDR = (transpose (+ innerTranspose)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                degrees'
                              )

            outerSleeveMDR = transpose (+ (outerTranspose - innerTranspose) ) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (flexSocketWithRiser (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM
                                                 (CommonFactors innerTranspose outerTranspose drop' take' )
                                                 flexDimensions  ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"


flexSocketWithRiser :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter ->
                       CommonFactors ->  FlexDimensions ->
                       ExceptT BuilderError (State CpointsStack ) CpointsList
flexSocketWithRiser innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM 
                    (CommonFactors innerTranspose outerTranspose drop' take' )
                    (FlexDimensions _ _ riserHeight socketToRiserHeight innerRiserRadius xAdjust yAdjust)  = do
   
  let angles = map Angle  [0,10..360]
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = Point 0 0 50
      getShaftOrigin :: Point -> Double -> Point
      --getShaftOrigin socketOrigin newZ = socketOrigin {z_axis = newZ}
      getShaftOrigin (Point x y z) newZ = Point (x + xAdjust) (y + yAdjust) newZ
  
  mainSocket
    <- buildCubePointsListSingle "wristCubes"
             ( concat $ map (runFrontToBackDiamondBuilder flexSocketDiamondBuilder)
               ( concat $ take take' $ drop drop'  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
             )
  {- without the diamond cutter
  mainSocket
    <- buildCubePointsListSingle "wristCubes"
             ( concat $ take take' $ drop drop'  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
  -}
  
  mainSocketTopFaces
    <- buildCubePointsListSingle "mainSocketTopFaces"
       (map (extractTopFace) $ head $ drop drop'  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
  
  let topOfSocketZaxis = z_axis . f2 . extractF2 $ head mainSocketTopFaces
      shaftOrigin = (getShaftOrigin origin (topOfSocketZaxis + socketToRiserHeight))
  
  riserTransistionTopFrontLines
    <- buildCubePointsListSingle "riserTransistionTopFrontLines"
                       (map extractFrontTopLine
                        (createTopFaces
                          shaftOrigin
                          (repeat $ Radius $ innerRiserRadius + (outerTranspose - innerTranspose))
                          angles
                          
                        )
                       )
                       

  
  riserTransitionBackTopLines
    <- buildCubePointsListSingle "riserTransitionBackTopLines"
                       --(map (backTopLineFromFrontTopLine . extractFrontTopLine)
                      (map (toBackTopLine . extractFrontTopLine)
                        (createTopFaces
                          shaftOrigin
                          (repeat $ Radius innerRiserRadius)
                          angles
                          
                        )
                       )
                       
                   

  
  riserTransitionTopFaces
    <- buildCubePointsListWithAdd "riserBtmFacesAsTopFaces"
                       riserTransistionTopFrontLines
                       riserTransitionBackTopLines

  riserTransitionCubes
    <- buildCubePointsListSingle "riserTransitionCubes"
       (concat $ map (runFrontToBackDiamondBuilder flexSocketDiamondBuilder)
        (riserTransitionTopFaces
        |+++|
        ((map (toBottomFace) mainSocketTopFaces))
        )
       )
{-
  riserTransitionCubes
    <- buildCubePointsListWithAdd "riserTransitionCubes"
       riserTransitionTopFaces
       (map (toBottomFace) mainSocketTopFaces)
-}
 
  riserCubes
    <- buildCubePointsListWithAdd "riserCubes"
       (map
         (transposeZ (+riserHeight) )
          --riserTransitionTopFaces
          riserTransitionTopFaces
       )
       (map (toBottomFace) riserTransitionTopFaces)
{-
  riserCubes
    <- buildCubePointsListWithAdd "riserCubes"
       (map
        (transposeZ (+riserHeight) )
        riserTransitionTopFaces
       )
       (riserTransitionCubes)
-}
  
  
  return mainSocket

       
-- ================================================== plain socket generator ===============================================================
-- =========================================================================================================================================

{-
Output a plain socket for testing.
Does not use diamond cutter, as it does not yet work.
Does not use a round riser for joining to other pieces, which is not good.
-}
flexSocketPlain :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> Int ->    Int ->
               ExceptT BuilderError (State CpointsStack ) CpointsList
flexSocketPlain    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM            flexDrop' flexTake' = do
  let transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = Point 0 0 50
      
  
  cubes   <- buildCubePointsListSingle "wristCubes"
             --( concat $ take 3 $ drop 4  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
             ( concat $ take flexTake' $ drop flexDrop'  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )

  
  return cubes

flexSocketPlainStlGenerator :: CommonFactors ->                                            IO ()
flexSocketPlainStlGenerator (CommonFactors innerTranspose outerTranspose drop' take' )     = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let rowReductionFactor = 100::RowReductionFactor
            innerSleeveMDR = (transpose (+ innerTranspose)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                degrees'
                              )

            outerSleeveMDR = transpose (+ (outerTranspose - innerTranspose) ) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (flexSocketPlain (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM drop' take'  ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

flexSocketPlainStlGeneratorDbStlGeneretor :: String -> IO ()
flexSocketPlainStlGeneratorDbStlGeneretor dimensionsName = runSqlite commontDBName $ do
  maybeDimensions <- getBy $ uniqueDimensionName dimensionsName
  case maybeDimensions of
        Nothing -> liftIO $ putStrLn "common dimensions not found"
        Just (Entity commonDimensionsId commonDimensions) -> do
          liftIO $ flexSocketPlainStlGenerator $ setFlexiSocketCommonFactors commonDimensions
          liftIO $ putStrLn "plain flex socket stl has been output"


-- =================================================cut diamond socket generator =======================================




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


flexSocketShowCurrentState :: IO ()
flexSocketShowCurrentState = do
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
        in
            print $ show ((evalState $ runExceptT (flexSocket (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM) ) [])
        --in  --writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"
--  print $ show  ((evalState $ runExceptT (hammerHeadSharkHeadSection originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)

removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']

flexSocket :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter ->
               ExceptT BuilderError (State CpointsStack ) CpointsList
flexSocket    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM = do
  let transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = Point 0 0 500

  {-take just a few pieces of the diamond cuts.
    Uses the new diamond cutter.-}
  
  cubes   <- buildCubePointsListSingle "wristCubes"
             
               (take 8 $ concat $  map (runFrontToBackDiamondBuilder flexSocketDiamondBuilder)
                            (concat $ take 2 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
               )
  
            


  
  
  return cubes





flexSocketDiamondBuilder :: CornerPoints -> DiamondBuilder
flexSocketDiamondBuilder cube =
  let defaultEdgeOffset = (OffSet 0.1 0.1 0.1)
  in
      Diamond { outerCube = cube,
                topDiamondFace = Nothing,
                topDiamondCorner = Nothing,
                topCenterPoint = Nothing,
                topDiamondHorizontalOffsets = (OffSet 0.5 0.5 0.5),
                topDiamondVertiacalOffsets = defaultEdgeOffset, --(OffSet 0.15 0.15 0.15),
                topRightDiamondFace = Nothing,
                rightDiamondFace = Nothing,
                rightCenterPoint = Nothing,
                rightDiamondCorner = Nothing,
                rightDiamondHorizontalOffsets = defaultEdgeOffset, -- (OffSet 0.15 0.15 0.15),
                rightDiamondVerticalOffsets = (OffSet 0.5 0.5 0.5),
                bottomRightDiamondFace = Nothing,
                bottomDiamondFace = Nothing,
                bottomDiamondCorner = Nothing,
                bottomCenterPoint = Nothing,
                bottomDiamondHorizontalOffsets = (OffSet 0.5 0.5 0.5),
                bottomDiamondVerticalOffsets = defaultEdgeOffset, --(OffSet 0.15 0.15 0.15),
                bottomLeftDiamondFace = Nothing,

                leftDiamondFace = Nothing,
                leftCenterPoint = Nothing,
                leftDiamondCorner = Nothing,
                --try opposite values did not work: (OffSet 0.75 0.75 0.75),
                leftDiamondHorizontalOffsets = defaultEdgeOffset, -- (OffSet 0.15 0.15 0.15),
                leftDiamondVerticalOffsets  = (OffSet 0.5 0.5 0.5),
                
                topLeftDiamondFace = Nothing
              }


-- ============================================================== Orig Diamond =========================================================
-- ============================================================== Orig Diamond =========================================================
-- ============================================================== Orig Diamond =========================================================
{-
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
{-
cutTheDiamondFrontBase :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints)
                       -> CornerPoints -> CornerPoints -> CornerPoints
cutTheDiamondFrontBase    reverseBtm reverseTop btmPoints       topPoints       =
  let btmFrontLine  = reverseBtm $ toBottomFrontLine  btmPoints
      topFrontLine  = reverseTop $ toFrontTopLine topPoints
  in  btmFrontLine +++ topFrontLine
-}
cutTheDiamondBackBase :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> CornerPoints -> CornerPoints -> CornerPoints
cutTheDiamondBackBase    reverseBtm reverseTop btmPoints       topPoints       =
  let btmBackLine  = reverseBtm $ toBackBottomLine  btmPoints
      topBackLine  = reverseTop $ toBackTopLine topPoints
  in  btmBackLine +++ topBackLine
-}
-- ===============================================================================================================================================================================
{-
cutTheDiamondTopLeftCorner :: CornerPoints -> CornerPoints
cutTheDiamondTopLeftCorner cube =
  let frontFace = cutTheDiamondFrontBase
                    (reverseNormal) (id)
                    ((cutTheDiamondF1ShiftedIn cube) +++ (cutTheDiamondF2ShiftedIn cube))
                    (extractF2 cube)
      --backFace  = (transposeY (+(-10))) . toBackFace $ frontFace
      backFace = cutTheDiamondBackBase
                   (reverseNormal) (id)
                   ((cutTheDiamondB1ShiftedIn cube) +++ (cutTheDiamondB2ShiftedIn cube))
                    (extractB2 cube)
  in  frontFace +++ backFace

cutTheDiamondLeftFace :: CornerPoints -> CornerPoints
cutTheDiamondLeftFace cube =
  let frontFace = cutTheDiamondFrontBase (id) (id) (cutTheDiamondF1ShiftedIn cube) ((extractF1 cube) +++ (extractF2 cube)) 
      --backFace     = (transposeY (+(-10))) . toBackFace $ frontFace
      backFace  = cutTheDiamondBackBase (id) (id) (cutTheDiamondB1ShiftedIn cube) ((extractB1 cube) +++ (extractB2 cube)) 
  in  backFace +++ frontFace

cutTheDiamondBtmLeftCorner :: CornerPoints -> CornerPoints
cutTheDiamondBtmLeftCorner cube =
  let frontFace      = cutTheDiamondFrontBase
                         (reverseNormal) (id)
                         ((cutTheDiamondF1ShiftedIn cube) +++ (cutTheDiamondF4ShiftedIn cube))
                         (extractF1 cube)
      
      --backFace     = (transposeY (+(-10))) . toBackFace $ frontFace
      
      backFace = cutTheDiamondBackBase
                         (reverseNormal) (id)
                         ((cutTheDiamondB1ShiftedIn cube) +++ (cutTheDiamondB4ShiftedIn cube))
                         (extractB1 cube)
  in  backFace +++ frontFace
      
cutTheDiamondBtmFace :: CornerPoints -> CornerPoints
cutTheDiamondBtmFace cube =
  let frontFace = cutTheDiamondFrontBase (id) (reverseNormal) (cutTheDiamondF4ShiftedIn cube) (extractBottomFrontLine $ cube)
      backFace  = cutTheDiamondBackBase (id) (reverseNormal) (cutTheDiamondB4ShiftedIn cube) (extractBackBottomLine $ cube)
  in backFace +++ frontFace
     

cutTheDiamondTopFace :: CornerPoints -> CornerPoints
cutTheDiamondTopFace cube  =
  let frontFace = cutTheDiamondFrontBase
                    (id) (id)
                    (cutTheDiamondF2ShiftedIn cube) --F2
                    (extractFrontTopLine cube) --FrontTopLine
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
-}
{-how do I add frontTopLine to F3
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
-}
{-
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
  offsetCornerPoints  0.25 0.25 0.25 (cutTheDiamondF1Centered cube) (cutTheDiamondF3Centered cube) (F1)

cutTheDiamondF2ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF2ShiftedIn cube =
  cutTheDiamond2ShiftedIn (cutTheDiamondF2Centered) (cutTheDiamondF4Centered) (F2) cube

cutTheDiamondB2ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondB2ShiftedIn cube =
  cutTheDiamond2ShiftedIn (cutTheDiamondB2Centered) (cutTheDiamondB4Centered) (B2) cube

cutTheDiamond1ShiftedIn :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond1ShiftedIn    center1                           center3                           constructor                cube            =
  offsetCornerPoints  0.25 0.25 0.25 (center1 cube) (center3 cube) constructor

cutTheDiamondB1ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondB1ShiftedIn cube =
  cutTheDiamond1ShiftedIn
     --(cutTheDiamondB4Centered) (cutTheDiamondB2Centered) (B1) cube
     (cutTheDiamondB1Centered) (cutTheDiamondB3Centered) (B1) cube
  
cutTheDiamond2ShiftedIn :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond2ShiftedIn    center2                           center4                           constructor                cube            =
  offsetCornerPoints 0.25 0.25 0.25 {-0 0 0.25-} (center2 cube) (center4 cube) constructor
  
cutTheDiamondF3ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF3ShiftedIn cube =
  cutTheDiamond3ShiftedIn (cutTheDiamondF3Centered) (cutTheDiamondF1Centered) (F3) cube

cutTheDiamond3ShiftedIn :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond3ShiftedIn    center3                           center1                           constructor                cube            =
  offsetCornerPoints  0.25 0.25 0.25 (center3 cube) (center1 cube) constructor

cutTheDiamondB3ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondB3ShiftedIn cube =
  cutTheDiamond3ShiftedIn (cutTheDiamondB3Centered) (cutTheDiamondB1Centered) (B3) cube

cutTheDiamond4ShiftedIn :: (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints -> CornerPoints
cutTheDiamond4ShiftedIn    center1                           center4                           constructor                cube            =
  offsetCornerPoints 0.25 0.25 0.25 (center1 cube) (center4 cube) constructor

cutTheDiamondB4ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondB4ShiftedIn cube =
  cutTheDiamond4ShiftedIn (cutTheDiamondB4Centered) (cutTheDiamondB2Centered) (B4) cube
  

cutTheDiamondF4ShiftedIn :: CornerPoints -> CornerPoints
cutTheDiamondF4ShiftedIn cube =
  offsetCornerPoints {-0 0 0.25-}0.25 0.25 0.25 (cutTheDiamondF4Centered cube) (cutTheDiamondF2Centered cube) (F4)

testCubeStandard = 
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
  

  cutterCubes <-
     buildCubePointsListSingle "cube"
     (cutTheDiamond testCubeStandard)

  return cutterCubes


testCubeStlGenerator :: IO ()
testCubeStlGenerator = do
  let cpoints =  ((execState $ runExceptT   testCube       ) [])
  writeStlToFile $ newStlShape "motorMount"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

testCubeShowCubes :: IO ()
testCubeShowCubes = do
  let cpoints =  ((evalState $ runExceptT   testCube       ) [])
  print $ show cpoints
-}

testCubeRotated1QuadClockWise =
  CubePoints {
            f4 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
            f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 20.0},
            b3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 20.0},
            b4 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 0.0},
            f1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
            f2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
            b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
            b1 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0}}
{-
testCubeRotated :: ExceptT BuilderError (State CpointsStack ) CpointsList
testCubeRotated = do
  

  cutterCubes <-
     buildCubePointsListSingle "cube"
     (cutTheDiamond testCubeRotated1QuadClockWise)

  return cutterCubes


testCubeRotatedStlGenerator :: IO ()
testCubeRotatedStlGenerator = do
  let cpoints =  ((execState $ runExceptT   testCubeRotated       ) [])
  writeStlToFile $ newStlShape "motorMount"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

testCubeRotatedShowCubes :: IO ()
testCubeRotatedShowCubes = do
  let cpoints =  ((evalState $ runExceptT   testCubeRotated       ) [])
  print $ show cpoints

-}
  -- =================================================== centering functions  ===========================================
{-
The offset should always be adjusted in relation to the 1st point to keep everthing standard.
Otherwise the offset will differ depending on the angle around a radial shape.

Must use the radial system as used by rotations.
[
 origin::Point --This is the 1st point to adjust from
 pointAtFarEnd::Point --The second point.
]
|
| calculate the angle
|
[
angleBetweenPoints
origin::Point
pointAtFarEnd::Point
]
|
|calculate distance between points
|calulate shortened distance from origin
[
shortenedDistanceFromOrigin
angleBetweenPoints
origin::Point
pointAtFarEnd::Point
]
|
|calculate the new end point using:
|-(adjustPointAxis (getXWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin)) .
| (adjustPointAxis (getYWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin)) $ origin {z_axis = (z_axis origin)}
-}
{-
offsetPoint :: Offset -> Offset -> Offset -> Point ->         Point -> Point
offsetPoint    offsetX   offsetY   offsetZ  (Point x1 y1 z1) (Point x2 y2 z2) =
  let
     origin = Point x1 y1 z1
     pointAtFarEnd = Point x2 y2 z2
     angleBetweenPoints = getXYAngle origin pointAtFarEnd 
     distanceFromOrigin =  calcultateXYDistance origin pointAtFarEnd
     shortenedDistanceFromOrigin =  Radius $  (radius distanceFromOrigin)  * (offsetX)
     --setZ = z1 + ((z1 - z2) * offsetZ * (-1))
     --setZ = z1 + ((abs $ z2 - z1) * offsetZ)
  in
   
               -- $ origin {z_axis = setZ z1 z2 offsetZ}
               Point (setAxis x1 x2 offsetX)
                     (setAxis y1 y2 offsetY)
                     (setAxis z1 z2 offsetZ)
-}
--version to work with the new DiamondBuilder

 
{-
seeAdjustedX :: Offset -> Point ->         Point -> Point
seeAdjustedX    offsetX   (Point x1 y1 z1) (Point x2 y2 z2) =
  let
     origin = Point x1 y1 z1
     pointAtFarEnd = Point x2 y2 z2
     angleBetweenPoints = getXYAngle origin pointAtFarEnd 
     distanceFromOrigin =  calcultateXYDistance origin pointAtFarEnd
     shortenedDistanceFromOrigin =  Radius $  (radius distanceFromOrigin)  * (offsetX)
     
        
  in
    (adjustPointAxis (getXWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin)) -- .
    --(adjustPointAxis (getYWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin))
    $ origin
    
seeShortenedXYDistance  :: Point ->  Point -> Double ->     Radius
seeShortenedXYDistance origin    pointAtFarEnd  offsetX =
  let distanceFromOrigin = calcultateXYDistance origin pointAtFarEnd
      
  in
    Radius $  (radius distanceFromOrigin)  * (1 - offsetX)

seeXYDistance :: Point ->  Point ->      Radius
seeXYDistance    origin    pointAtFarEnd =
  calcultateXYDistance origin pointAtFarEnd

seeAngle :: Point -> Point ->      Angle
seeAngle origin      pointAtFarEnd =
  
     getXYAngle origin pointAtFarEnd 
{-
offsetPoint :: Offset -> Offset -> Offset -> Point ->         Point -> Point
offsetPoint    offsetX   offsetY   offsetZ  (Point x1 y1 z1) (Point x2 y2 z2) =
  let
     setX = x1 + (((x1 - x2) * offsetX * (-1)))
        
     setY = y1 + ((y1 - y2) * offsetY * (-1))
        
     setZ = z1 + ((z1 - z2) * offsetZ * (-1))
        
  in
    Point setX setY setZ

-}
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

offsetCornerPoints    offsetX   offsetY   offsetZ   (B1 b1')        (B3 b3')  cornerPointConst = 
  cornerPointConst $ offsetPoint offsetX   offsetY   offsetZ  b1' b3' 

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
-}
-- ========================================= socket general support functions========================================================
-- ==================================================================================================================================


-- ==================================================== local tests =================================================================
-- ==================================================================================================================================
-- ==================================================================================================================================


-- =================================================================== :emergent ===============================================================



