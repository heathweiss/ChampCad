{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

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
                                     flexSocketWithRiserDbStlGenerator, uniqueFlexDimensionName,
                                     flexBottomForSocketWithRiserDbStlGenerator) where

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
import Geometry.Vertex(getXWithQuadrant, getYWithQuadrant, Vertex(..), adjustPointAxis)

import TypeClasses.Transposable(transpose)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)

import Helpers.List((++:))

import Primitives.Cylindrical.Walled(squaredYLengthenedCylinder, squaredCylinder, cylinder)
import Primitives.DiamondCutter(DiamondBuilder(..), runDiamondBuilder, runFrontToBackDiamondBuilder, OffSet(..), defaultDiamondBuilder)

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
import qualified Persistable.Base as PstB



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
initializeDatabase = runSqlite commontDBName . PstB.asSqlBackendReader $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "flex socket db initialized"

-- | Insert a new flex socket Dimensions into the database.
insertFlexDimensions :: IO ()
insertFlexDimensions     = runSqlite commontDBName . PstB.asSqlBackendReader $ do
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
-- ===================================================socket with round riser: top half==================================================
-- =========================================================================================================================================
{-
Add a round riser to the top for joining to the finger joint section.
The motor mount socket will glue to the outside of this, with no need to join up at the riser section.
This riser, flexible material, should go over top of the finger joints riser, to give it strength, as finger section will be solid material.

Use the DiamondCutter on it.

Output the top half of the socket with the riser because:
The entire socket did not print. Pronsole said it was done printing file. Was it to big for RepRap/Marlin or Slic3r.
So print this top half, and the bottom done separately.

Should be based on the socket made for the openBionics.com socket: Examples.OpenBionicsCom.OpenBionicsDotComDesignWork.shortSocketToLargeShaft
-}

flexSocketWithRiserDbStlGenerator :: String -> IO ()
flexSocketWithRiserDbStlGenerator dimensionsName  = runSqlite commontDBName . PstB.asSqlBackendReader $ do
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
-- ================================================== bottom of socket with diamonds ======================================================
-- ==========================================================================================================================================

flexBottomForSocketWithRiserDbStlGenerator :: String -> IO ()
flexBottomForSocketWithRiserDbStlGenerator dimensionsName  = runSqlite commontDBName . PstB.asSqlBackendReader $ do
  maybeCommonDimensions <- getBy $ uniqueDimensionName dimensionsName
  maybeFlexDimensions <- getBy $ UniqueFlexDimensionName dimensionsName
  case maybeCommonDimensions of
        Nothing -> liftIO $ putStrLn "common dimensions not found"
        Just (Entity commonDimensionsId commonDimensions) -> do
          --liftIO $ flexSocketPlainStlGenerator $ setFlexiSocketCommonFactors commonDimensions
          liftIO $ putStrLn "common dimensions found"
          liftIO $ flexBottomForSocketWithRiserStlGenerator (setFlexiSocketCommonFactors commonDimensions)
          
             

flexBottomForSocketWithRiserStlGenerator :: CommonFactors ->  IO ()
flexBottomForSocketWithRiserStlGenerator (CommonFactors innerTranspose outerTranspose drop' take' ) = do
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
            cpoints =  ((execState $ runExceptT (flexBottomForSocketWithRiser (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM
                                                 (CommonFactors innerTranspose outerTranspose drop' take' )
                                                   ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"


flexBottomForSocketWithRiser :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter ->
                       CommonFactors ->  
                       ExceptT BuilderError (State CpointsStack ) CpointsList
flexBottomForSocketWithRiser innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM 
                    (CommonFactors innerTranspose outerTranspose drop' take' )  = do

  let angles = map Angle  [0,10..360]
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = Point 0 0 50
      layersUsedBySocketWithRiser = drop' + take'
      mergeLayersOfSocket = concat
      mergeDiamondSections = concat

  mainSocket
    <- buildCubePointsListSingle "wristCubes"
             ( mergeDiamondSections $ map (runFrontToBackDiamondBuilder flexSocketDiamondBuilder)
               ( mergeLayersOfSocket  $ drop layersUsedBySocketWithRiser  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
             )

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
flexSocketPlainStlGeneratorDbStlGeneretor dimensionsName = runSqlite commontDBName . PstB.asSqlBackendReader $ do
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




{-
edge offset of .2 seems to be the smallest can go and still have good print.
Next smallest tried was .15.
-}
flexSocketDiamondBuilder :: CornerPoints -> DiamondBuilder
flexSocketDiamondBuilder cube =
  let edgeOffset = (OffSet 0.2 0.2 0.2)
      centerOffset = (OffSet 0.5 0.5 0.5)
      defaultDiamond = defaultDiamondBuilder cube
  in
      defaultDiamond
      
        {  
           topDiamondHorizontalOffsets = centerOffset,
           topDiamondVertiacalOffsets = edgeOffset,
           rightDiamondHorizontalOffsets = edgeOffset, 
           rightDiamondVerticalOffsets = centerOffset,
           bottomDiamondHorizontalOffsets = centerOffset,
           bottomDiamondVerticalOffsets = edgeOffset, 
           leftDiamondHorizontalOffsets = edgeOffset, 

           leftDiamondVerticalOffsets  = centerOffset
        }
              
