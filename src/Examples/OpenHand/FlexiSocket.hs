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
module Examples.OpenHand.FlexiSocket(flexiSocketTestsDo, flexSocketStlGenerator, flexSocketShowCurrentState,
                                     testCubeStlGenerator, testCubeShowCubes,
                                     testCubeRotatedStlGenerator, flexSocketPlainStlGenerator,
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
             

  {-with the orig diamond cutter
 mainSocket
    <- buildCubePointsListSingle "wristCubes"
             ( concat $ map (cutTheDiamond)
               ( concat $ take take' $ drop drop'  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
             )

  without the diamond cutter
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
  {-
  cubes   <- buildCubePointsListSingle "wristCubes"
             
               (take 8 $ concat $  map (runFrontToBackDiamondBuilder)
                            (concat $ take 2 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
               ) 

    take just a few pieces of the diamond cuts.
    Uses the original diamond cutter 
  cubes   <- buildCubePointsListSingle "wristCubes"
             
               (concat $  map (cutTheDiamond)
                            (concat $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
               )-}

            


  {- original top 2 layers with diamonds cut. Has errors.
  cubes   <- buildCubePointsListSingle "wristCubes"
             ( concat $ map (cutTheDiamond)
                            (concat $ take 2 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
             )-}
  
  {-
  --get the 1st cube to see its numbers
  cubes   <- buildCubePointsListSingle "wristCubes"
             ( --concat $ map (cutTheDiamond)
               take 1 (concat $ take 2 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
             )-}
  
  return cubes

{- The 1st cube of the socket, without any diamonds cut
  CubePoints {f1 = Point {x_axis = 4.961580236616474, y_axis = -28.138519792497544, z_axis = 494.54022988505744},
              f2 = Point {x_axis = 4.403832696848794, y_axis = -24.975376310276598, z_axis = 500.0},
              f3 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
              f4 = Point {x_axis = 0.0, y_axis = -29.88294664396197, z_axis = 494.54022988505744},
              b1 = Point {x_axis = 4.440635703615683, y_axis = -25.184096533460924, z_axis = 494.54022988505744},
              b2 = Point {x_axis = 3.882888163848002, y_axis = -22.020953051239964, z_axis = 500.0},
              b3 = Point {x_axis = 0.0, y_axis = -23.234224965706463, z_axis = 500.0},
              b4 = Point {x_axis = 0.0, y_axis = -26.88294664396197, z_axis = 494.54022988505744}}-}

{-should be to top center of the diamond, of 1st cube
  CubePoints {f1 = Point {x_axis = 2.3413532333663167, y_axis = -27.30776692811064, z_axis = 498.63505747126436},
              f2 = Point {x_axis = 4.403832696848794, y_axis = -24.975376310276598, z_axis = 500.0},
              f3 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
              f4 = Point {x_axis = 2.3413532333663167, y_axis = -27.30776692811064, z_axis = 498.63505747126436},
              b1 = Point {x_axis = 2.080880966865921, y_axis = -24.33055529859233, z_axis = 498.63505747126436},
              b2 = Point {x_axis = 3.882888163848002, y_axis = -22.020953051239964, z_axis = 500.0},
              b3 = Point {x_axis = 0.0, y_axis = -23.234224965706463, z_axis = 500.0},
              b4 = Point {x_axis = 2.080880966865921, y_axis = -24.33055529859233, z_axis = 498.63505747126436}}-}

{-the top right corner of diamond of 1st cube
  CubePoints {f1 = Point {x_axis = 2.3413532333663167, y_axis = -27.30776692811064, z_axis = 498.63505747126436},
              f2 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
              f3 = Point {x_axis = 0.0, y_axis = -26.234224965706463, z_axis = 500.0},
              f4 = Point {x_axis = 1.1706766166831584, y_axis = -27.68317636647243, z_axis = 497.2701149425287},
              b1 = Point {x_axis = 2.080880966865921, y_axis = -24.33055529859233, z_axis = 498.63505747126436},
              b2 = Point {x_axis = 0.0, y_axis = -23.234224965706463, z_axis = 500.0},
              b3 = Point {x_axis = 0.0, y_axis = -23.234224965706463, z_axis = 500.0},
              b4 = Point {x_axis = 1.0404404834329608, y_axis = -24.694570551713273, z_axis = 497.2701149425287}}-}



-- ============================================================== New Diamond =========================================================
-- ============================================================== New Diamond =========================================================
-- ============================================================== New Diamond =========================================================
data DiamondBuilder =
  Diamond
    {outerCube :: CornerPoints,
     
     topDiamondFace :: Maybe CornerPoints,
     topDiamondCorner :: Maybe CornerPoints,
     topCenterPoint :: Maybe Point,
     topDiamondHorizontalOffsets :: OffSet,
     topDiamondVertiacalOffsets :: OffSet,
     
     topRightDiamondFace :: Maybe CornerPoints,

     rightDiamondFace :: Maybe CornerPoints,
     rightCenterPoint ::  Maybe Point,
     rightDiamondCorner :: Maybe CornerPoints,
     rightDiamondHorizontalOffsets :: OffSet,
     rightDiamondVerticalOffsets :: OffSet,
     
     bottomRightDiamondFace :: Maybe CornerPoints,
     
     bottomDiamondFace :: Maybe CornerPoints,
     bottomDiamondCorner :: Maybe CornerPoints,
     bottomCenterPoint :: Maybe Point,
     bottomDiamondHorizontalOffsets :: OffSet,
     bottomDiamondVerticalOffsets :: OffSet,

     bottomLeftDiamondFace :: Maybe CornerPoints,

     leftDiamondFace :: Maybe CornerPoints,
     leftCenterPoint ::  Maybe Point,
     leftDiamondCorner :: Maybe CornerPoints,
     leftDiamondHorizontalOffsets :: OffSet,
     leftDiamondVerticalOffsets :: OffSet,
     
     
     
     
     topLeftDiamondFace :: Maybe CornerPoints
     
     
    }

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


-- |
-- Run a single DiamondBuilder and output the resulting [diamond cubes].
-- If any of the cubes did not turn out, return a [CornerPointsError]
runDiamondBuilder :: DiamondBuilder -> [CornerPoints]
runDiamondBuilder diamondBuilder
  | isNothing $ topDiamondFace diamondBuilder = [CornerPointsError "topDiamondFace is Nothing "]
  | isNothing $ bottomDiamondFace diamondBuilder = [CornerPointsError "bottomDiamondFace is Nothing "]
  | isNothing $ rightDiamondFace diamondBuilder = [CornerPointsError "rightDiamondFace is Nothing "]
  | isNothing $ leftDiamondFace diamondBuilder = [CornerPointsError "leftDiamondFace is Nothing "]
  | isNothing $ topRightDiamondFace diamondBuilder = [CornerPointsError "topRightDiamondFace is Nothing "]
  | isNothing $ bottomRightDiamondFace diamondBuilder = [CornerPointsError "bottomRightDiamondFace is Nothing "]
  | isNothing $ bottomLeftDiamondFace diamondBuilder = [CornerPointsError "bottomLeftDiamondFace is Nothing "]
  | isNothing $ topLeftDiamondFace diamondBuilder = [CornerPointsError "topLeftDiamondFace is Nothing "] 
  | otherwise = [(fromJust $ topDiamondFace diamondBuilder),
                 (fromJust $ bottomDiamondFace diamondBuilder),
                 (fromJust $ rightDiamondFace diamondBuilder),
                 (fromJust $ leftDiamondFace diamondBuilder),
                 (fromJust $ topRightDiamondFace diamondBuilder),
                 (fromJust $ bottomRightDiamondFace diamondBuilder),
                 (fromJust $ bottomLeftDiamondFace diamondBuilder),
                 (fromJust $ topLeftDiamondFace diamondBuilder)]


-- |
-- Run the opposing DiamondBuilders, and add together the resulting [CornerPoints]

runFrontToBackDiamondBuilder :: (CornerPoints -> DiamondBuilder) -> CornerPoints -> [CornerPoints]
runFrontToBackDiamondBuilder diamondBuilder' cube =
  let diamondBuilder = diamondBuilder' cube
  in
  (runDiamondBuilder $ isTheFrontDiamondDone  diamondBuilder)
  |+++|
  (runDiamondBuilder $ isTheBackDiamondDone diamondBuilder)
{-
runFrontToBackDiamondBuilder :: CornerPoints -> [CornerPoints]
runFrontToBackDiamondBuilder cube =
  let diamondBuilder = defaultDiamondBuilder cube
  in
  (runDiamondBuilder $ isTheFrontDiamondDone  diamondBuilder)
  |+++|
  (runDiamondBuilder $ isTheBackDiamondDone diamondBuilder)
-}

data OffSet =
  OffSet {xOffset :: Double,
          yOffset :: Double,
          zOffset :: Double
         }


{-
A base used by Front and Back DiamondBuilders.
If all the cubes of a diamond are good, return them, else the error of the bad cube.
-}
isTheDiamondDoneBase :: (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> (DiamondBuilder -> DiamondBuilder) -> (DiamondBuilder -> DiamondBuilder)
                     -> DiamondBuilder -> DiamondBuilder
isTheDiamondDoneBase    topDiamondBuilder                     bottomDiamondBuilder
                        rightDiamondBuilder                   leftDiamondBuilder
                        topRightDiamondBuilder                bottomRightDiamondBuilder
                        bottomLeftDiamondBuilder              topLeftDiamondBuilder
                        diamondBuilder
  | (isNothing  $ topDiamondFace diamondBuilder) = topDiamondBuilder diamondBuilder
  | (isNothing  $ bottomDiamondFace diamondBuilder) = bottomDiamondBuilder diamondBuilder
  | (isNothing  $ rightDiamondFace diamondBuilder) = rightDiamondBuilder diamondBuilder
  | (isNothing  $ leftDiamondFace diamondBuilder) = leftDiamondBuilder diamondBuilder
  | (isNothing  $ topRightDiamondFace diamondBuilder) = topRightDiamondBuilder diamondBuilder
  | (isNothing  $ bottomRightDiamondFace diamondBuilder) = bottomRightDiamondBuilder diamondBuilder
  | (isNothing  $ bottomLeftDiamondFace diamondBuilder) = bottomLeftDiamondBuilder diamondBuilder
  | (isNothing  $ topLeftDiamondFace diamondBuilder) = topLeftDiamondBuilder diamondBuilder
  | otherwise  = diamondBuilder


-- |
-- Use isTheDiamondDoneBase for FrontFaces.
isTheFrontDiamondDone :: DiamondBuilder -> DiamondBuilder
isTheFrontDiamondDone diamondBuilder =
  isTheDiamondDoneBase (topDiamondFrontFaceBuilder) (bottomDiamondFrontFaceBuilder)
                       (frontRightDiamondBuilder) (frontLeftDiamondBuilder)
                       (frontTopRightDiamondBuilder) (frontBottomRightDiamondBuilder)
                       (frontBottomLeftDiamondBuilder) (frontTopLeftDiamondBuilder)
                       diamondBuilder

-- |
-- Use isTheDiamondDoneBase for BackFaces.
isTheBackDiamondDone :: DiamondBuilder -> DiamondBuilder
isTheBackDiamondDone diamondBuilder =
  isTheDiamondDoneBase (topDiamondBackFaceBuilder) (bottomDiamondBackFaceBuilder)
                       (backRightDiamondBuilder) (backLeftDiamondBuilder)
                       (backTopRightDiamondBuilder) (backBottomRightDiamondBuilder)
                       (backBottomLeftDiamondBuilder) (backTopLeftDiamondBuilder)
                       diamondBuilder

-- ==================================================== Top Diamond ==========================================================
{-
Base function used by Front and Back Faces.
Use to create the top cube of the diamond, that goes from the top point of the diamond, to the top face of the outer cube.
-}
topDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                          -> (CornerPoints -> CornerPoints) -> (DiamondBuilder -> DiamondBuilder)
                          -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
topDiamondFaceBuilderBase point2Extractor point3Extractor topDiamondCornerConstructor extractTopLine toBottomLine bottomDiamondFaceBuilder isTheDiamondDone diamondBuilder =
  let topCenterPoint' =  case isNothing $ topCenterPoint diamondBuilder of
                           True -> 
                             
                             offsetPoint (xOffset $  topDiamondHorizontalOffsets diamondBuilder)
                                         (yOffset $ topDiamondHorizontalOffsets diamondBuilder)
                                         (zOffset $ topDiamondHorizontalOffsets diamondBuilder)
                                         (point2Extractor $ outerCube diamondBuilder) (point3Extractor $ outerCube diamondBuilder)

                           False -> fromJust $ topCenterPoint diamondBuilder
  in
  case (bottomCenterPoint diamondBuilder) of
    Nothing ->  bottomDiamondFaceBuilder $ diamondBuilder {topCenterPoint = Just topCenterPoint'}
    Just bottomCenterPoint' ->
      let topDiamondCorner' =  topDiamondCornerConstructor $ offsetPoint' (topDiamondVertiacalOffsets diamondBuilder)  topCenterPoint' bottomCenterPoint'
          topDiamondFace' = (extractTopLine $ outerCube diamondBuilder) +++ (toBottomLine $  topDiamondCorner')
      in  isTheDiamondDone $ diamondBuilder {topDiamondFace = Just topDiamondFace', topDiamondCorner = Just topDiamondCorner' , topCenterPoint = Just topCenterPoint'}
          
-- |
-- Use topDiamondFaceBuilderBase to build the FrontFace of the top cube.
topDiamondFrontFaceBuilder :: DiamondBuilder -> DiamondBuilder
topDiamondFrontFaceBuilder diamondBuilder =
  topDiamondFaceBuilderBase (f2) (f3) (F2) (extractFrontTopLine) (toBottomFrontLine) (bottomDiamondFrontFaceBuilder) (isTheFrontDiamondDone) diamondBuilder

-- |
-- Use topDiamondFaceBuilderBase to build the BackFace of the top cube.
topDiamondBackFaceBuilder :: DiamondBuilder -> DiamondBuilder
topDiamondBackFaceBuilder diamondBuilder =
  topDiamondFaceBuilderBase (b2) (b3) (B2) (extractBackTopLine) (toBackBottomLine) (bottomDiamondBackFaceBuilder) (isTheBackDiamondDone) diamondBuilder

-- =========================================================== Bottom Diamond =================================================================
{-
Base function used by Front and Back Faces.
Use to create the bottom cube of the diamond, that goes from the bottom point of the diamond, to the bottom face of the outer cube.
-}
bottomDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                             -> (CornerPoints -> CornerPoints) -> (CornerPoints -> CornerPoints)
                             -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
bottomDiamondFaceBuilderBase point1Extractor point4Extractor bottomDiamondCornerConstructor toTopLine extractBottomLine toBottomLine isTheDiamondDone diamondBuilder =
  let bottomCenterPoint' =
        case isNothing $ bottomCenterPoint diamondBuilder of
          True ->  offsetPoint' (bottomDiamondHorizontalOffsets diamondBuilder)
                                (point1Extractor $ outerCube diamondBuilder) (point4Extractor $ outerCube diamondBuilder)
          False -> fromJust $ bottomCenterPoint diamondBuilder
  in
  case topCenterPoint diamondBuilder of
    Nothing -> topDiamondFrontFaceBuilder $ diamondBuilder {bottomCenterPoint =  Just bottomCenterPoint'}
    Just topCenterPoint' ->
      let 
          bottomDiamondCorner' = bottomDiamondCornerConstructor $ offsetPoint' (bottomDiamondVerticalOffsets diamondBuilder)  bottomCenterPoint' topCenterPoint'
          bottomDiamondFace' =
            (reverseNormal $ toTopLine $ extractBottomLine $ outerCube diamondBuilder)
            +++
            (toBottomLine bottomDiamondCorner')
      in
      isTheDiamondDone $ diamondBuilder {bottomCenterPoint = Just bottomCenterPoint', bottomDiamondFace = Just bottomDiamondFace', bottomDiamondCorner = Just bottomDiamondCorner'}

-- |
-- Use bottomDiamondFaceBuilderBase to build the FrontFace of the bottom cube.
bottomDiamondFrontFaceBuilder :: DiamondBuilder -> DiamondBuilder
bottomDiamondFrontFaceBuilder diamondBuilder =
  bottomDiamondFaceBuilderBase (f1) (f4) (F4) (toFrontTopLine) (extractBottomFrontLine) toBottomFrontLine (isTheFrontDiamondDone) diamondBuilder

-- |
-- Use bottomDiamondFaceBuilderBase to build the BackFace of the bottom cube.
bottomDiamondBackFaceBuilder :: DiamondBuilder -> DiamondBuilder
bottomDiamondBackFaceBuilder diamondBuilder =
  bottomDiamondFaceBuilderBase (b1) (b4) (B4) (toBackTopLine) (extractBackBottomLine) (toBackBottomLine) (isTheBackDiamondDone) diamondBuilder

  -- ============================================================ Right Diamond =======================================================
{-A base function for creating the Front and Back faces of the right side of the outer cube.
Started as a copy of topDiamondFaceBuilderBase.
Re-written but not tested.

Needs: leftDiamondFaceBuilder
ToDo: build the front/back functions.
-}

rightDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                          -> (CornerPoints -> CornerPoints) -> (DiamondBuilder -> DiamondBuilder)
                          -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
rightDiamondFaceBuilderBase point3Extractor point4Extractor rightDiamondCornerConstructor extractTopLine toBottomLine leftDiamondFaceBuilder isTheDiamondDone diamondBuilder =
  let rightCenterPoint' =  case rightCenterPoint diamondBuilder of
                           Nothing -> 
                             
                             offsetPoint (xOffset $ rightDiamondVerticalOffsets diamondBuilder)
                                         (yOffset $ rightDiamondVerticalOffsets diamondBuilder)
                                         (zOffset $ rightDiamondVerticalOffsets diamondBuilder)
                                         (point3Extractor $ outerCube diamondBuilder) (point4Extractor $ outerCube diamondBuilder)

                           Just rightCenterPoint'' -> rightCenterPoint''

  in
  case (leftCenterPoint diamondBuilder) of
    Nothing ->  leftDiamondFaceBuilder $ diamondBuilder {rightCenterPoint = Just rightCenterPoint'}
    Just leftCenterPoint' ->
      let rightDiamondCorner' =  rightDiamondCornerConstructor $ offsetPoint' (rightDiamondHorizontalOffsets diamondBuilder)  rightCenterPoint' leftCenterPoint'
          rightDiamondFace' = (extractTopLine $ outerCube diamondBuilder) +++ (toBottomLine $  rightDiamondCorner')
      in  isTheDiamondDone $ diamondBuilder {rightDiamondFace = Just rightDiamondFace', rightDiamondCorner = Just rightDiamondCorner', rightCenterPoint = Just rightCenterPoint'}
          

frontRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontRightDiamondBuilder diamondBuilder =
  rightDiamondFaceBuilderBase (f3) (f4) (F3) (toFrontTopLine . extractFrontRightLine) (toBottomFrontLine) (frontLeftDiamondBuilder) (isTheFrontDiamondDone) diamondBuilder

backRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backRightDiamondBuilder diamondBuilder =
  rightDiamondFaceBuilderBase (b3) (b4) (B3) (toBackTopLine . extractBackRightLine) (toBackBottomLine) (backLeftDiamondBuilder) (isTheBackDiamondDone) diamondBuilder
-- ============================================================ Left Diamond =========================================================
{-A base function for creating the Front and Back faces of the left side of the outer cube.
Started as a copy of rightDiamondFaceBuilderBase.
not yet re-written but not tested.

Needs: leftDiamondFaceBuilder
ToDo: build the front/back functions.
-}
leftDiamondFaceBuilderBase :: (CornerPoints -> Point) -> (CornerPoints -> Point) -> (Point -> CornerPoints) -> (CornerPoints -> CornerPoints)
                          -> (CornerPoints -> CornerPoints) -> (DiamondBuilder -> DiamondBuilder)
                          -> (DiamondBuilder -> DiamondBuilder) -> DiamondBuilder -> DiamondBuilder
leftDiamondFaceBuilderBase topOuterPointExtractor btmOuterPointExtractor innerCPointConstructor extractOuterLine toInnerLine farDiamondBuilder isTheDiamondDone diamondBuilder =
  let myVerticallyOffsetPoint =  case leftCenterPoint diamondBuilder of
                           Nothing -> 
                             
                             offsetPoint (xOffset $ leftDiamondVerticalOffsets diamondBuilder)
                                         (yOffset $ leftDiamondVerticalOffsets diamondBuilder)
                                         (zOffset $ leftDiamondVerticalOffsets diamondBuilder)
                                         (topOuterPointExtractor $ outerCube diamondBuilder) (btmOuterPointExtractor $ outerCube diamondBuilder)

                           Just myVerticallyOffsetPoint' -> myVerticallyOffsetPoint'

  
  in
  case (rightCenterPoint diamondBuilder) of
    Nothing ->  farDiamondBuilder $ diamondBuilder {leftCenterPoint = Just myVerticallyOffsetPoint}
    Just farVerticallyOffsetPoint ->
      let leftDiamondCorner' =  innerCPointConstructor $ offsetPoint' (leftDiamondHorizontalOffsets diamondBuilder) myVerticallyOffsetPoint farVerticallyOffsetPoint
          leftDiamondFace' = (extractOuterLine $ outerCube diamondBuilder) +++ (toInnerLine $  leftDiamondCorner')
      in  isTheDiamondDone $ diamondBuilder {leftDiamondFace = Just leftDiamondFace', leftDiamondCorner = Just leftDiamondCorner', leftCenterPoint = Just myVerticallyOffsetPoint}

frontLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontLeftDiamondBuilder diamondBuilder =
  leftDiamondFaceBuilderBase (f2) (f1) (F1) (toFrontTopLine . extractFrontLeftLine) (toBottomFrontLine) (frontRightDiamondBuilder) (isTheFrontDiamondDone)  diamondBuilder

backLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backLeftDiamondBuilder diamondBuilder =
  leftDiamondFaceBuilderBase (b2) (b1) (B1) (toBackTopLine . extractBackLeftLine) (toBackBottomLine) (backRightDiamondBuilder) (isTheBackDiamondDone)  diamondBuilder

-- ================================================================= corner pieces =====================================================
{-
Try to make a single base that will handle all of the corner pieces.
-}
setMyFieldTopRight :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldTopRight  diamondBuilder cpoints =
  diamondBuilder {
  topRightDiamondFace = Just cpoints}

setMyFieldBottomRight :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldBottomRight  diamondBuilder cpoints =
  diamondBuilder {
  bottomRightDiamondFace = Just cpoints}

setMyFieldBottomLeft :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldBottomLeft  diamondBuilder cpoints =
  diamondBuilder {
  bottomLeftDiamondFace = Just cpoints}

setMyFieldTopLeft :: DiamondBuilder -> CornerPoints -> DiamondBuilder
setMyFieldTopLeft  diamondBuilder cpoints =
  diamondBuilder {
  topLeftDiamondFace = Just cpoints}

cornerPiecesDiamondBuilderBase :: (DiamondBuilder -> Maybe CornerPoints) -> (DiamondBuilder -> DiamondBuilder) 
                               -> (DiamondBuilder -> Maybe CornerPoints) -> (DiamondBuilder -> DiamondBuilder) -> (CornerPoints -> CornerPoints)
                               -> (DiamondBuilder -> CornerPoints -> DiamondBuilder)
                               -> (DiamondBuilder -> DiamondBuilder)  -> DiamondBuilder -> DiamondBuilder
cornerPiecesDiamondBuilderBase    previousDiamondExtractor                   previousDiamondBuilder               
                                  nextDiamondExtractor                       nextDiamondBuilder                    nextLineExtractor
                                  setMyField
                                  isTheDiamondDone diamondBuilder =
  case (previousDiamondExtractor diamondBuilder) of
    Nothing -> previousDiamondBuilder diamondBuilder
    Just previousDiamond ->
      case (nextDiamondExtractor diamondBuilder) of
        Nothing -> nextDiamondBuilder diamondBuilder
        Just nextDiamond ->
          isTheDiamondDone $ setMyField diamondBuilder
            (  previousDiamond
               +++
               (nextLineExtractor  nextDiamond)
            )
        

frontTopRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontTopRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (topDiamondFace) (topDiamondFrontFaceBuilder) (rightDiamondFace) (frontRightDiamondBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldTopRight) isTheFrontDiamondDone diamondBuilder

frontBottomRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontBottomRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (rightDiamondFace) (frontRightDiamondBuilder) (bottomDiamondFace) (bottomDiamondFrontFaceBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldBottomRight) isTheFrontDiamondDone diamondBuilder

frontBottomLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontBottomLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (bottomDiamondFace) (bottomDiamondFrontFaceBuilder) (leftDiamondFace) (frontLeftDiamondBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldBottomLeft) isTheFrontDiamondDone diamondBuilder
-- ===================================================================================================================
frontTopLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
frontTopLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (leftDiamondFace) (frontLeftDiamondBuilder) (topDiamondFace) (topDiamondFrontFaceBuilder)
  (toFrontRightLine . extractFrontLeftLine) (setMyFieldTopLeft) isTheFrontDiamondDone diamondBuilder

backTopRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backTopRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (topDiamondFace) (topDiamondBackFaceBuilder) (rightDiamondFace) (backRightDiamondBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldTopRight) isTheBackDiamondDone diamondBuilder

backBottomRightDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backBottomRightDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (rightDiamondFace) (backRightDiamondBuilder) (bottomDiamondFace) (bottomDiamondBackFaceBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldBottomRight) isTheBackDiamondDone diamondBuilder

backBottomLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backBottomLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (bottomDiamondFace) (bottomDiamondBackFaceBuilder) (leftDiamondFace) (backLeftDiamondBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldBottomLeft) isTheBackDiamondDone diamondBuilder

-- -- ====================================================================================================================================
backTopLeftDiamondBuilder :: DiamondBuilder -> DiamondBuilder
backTopLeftDiamondBuilder diamondBuilder =
  cornerPiecesDiamondBuilderBase (leftDiamondFace) (backLeftDiamondBuilder) (topDiamondFace) (topDiamondFrontFaceBuilder)
  (toBackRightLine . extractBackLeftLine) (setMyFieldTopLeft) isTheBackDiamondDone diamondBuilder

-- ============================================================== Orig Diamond =========================================================
-- ============================================================== Orig Diamond =========================================================
-- ============================================================== Orig Diamond =========================================================
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

-- ===============================================================================================================================================================================
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
   {-
   case ((Point x1 y1 0) == (Point x2 y2 0)) of
     True ->   Point 100 100 100 -- x1 y1 z1
     False ->  
               (adjustPointAxis (getXWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin)) .
               (adjustPointAxis (getYWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin))
               $ origin {z_axis = setZ z1 z2 offsetZ}-}
               --(adjustPointAxis (getXWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin)) .
               --(adjustPointAxis (getYWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin))
               -- $ origin {z_axis = setZ z1 z2 offsetZ}
               Point (setAxis x1 x2 offsetX)
                     (setAxis y1 y2 offsetY)
                     (setAxis z1 z2 offsetZ)
{-
offsetPoint :: Offset -> Offset -> Offset -> Point ->         Point -> Point
offsetPoint    offsetX   offsetY   offsetZ  (Point x1 y1 z1) (Point x2 y2 z2) =
  let
     origin = Point x1 y1 z1
     pointAtFarEnd = Point x2 y2 z2
     angleBetweenPoints = getXYAngle origin pointAtFarEnd 
     distanceFromOrigin =  calcultateXYDistance origin pointAtFarEnd
     shortenedDistanceFromOrigin =  Radius $  (radius distanceFromOrigin)  * (offsetX)
     setZ = z1 + ((z1 - z2) * offsetZ * (-1))
        
  in
    (adjustPointAxis (getXWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin)) .
    (adjustPointAxis (getYWithQuadrant angleBetweenPoints shortenedDistanceFromOrigin))
    $ origin {z_axis = setZ}
-}
--version to work with the new DiamondBuilder
offsetPoint' :: OffSet -> Point ->         Point -> Point
offsetPoint'    offsets (Point x1 y1 z1) (Point x2 y2 z2) =
  let
     origin = Point x1 y1 z1
     pointAtFarEnd = Point x2 y2 z2
     angleBetweenPoints = getXYAngle origin pointAtFarEnd 
     distanceFromOrigin =  calcultateXYDistance origin pointAtFarEnd
     shortenedDistanceFromOrigin =  Radius $  (radius distanceFromOrigin)  * (xOffset offsets)
     
  in
               Point (setAxis x1 x2 (xOffset offsets))
                     (setAxis y1 y2 (yOffset offsets))
                     (setAxis z1 z2 (zOffset offsets))


setX :: Double -> Double -> Double -> Double
setX    x1        x2        offset
      | x1 == x2 = x1
      | x1 < x2  = x1 + ((x2 - x1) * offset)
      | x1 > x2  = x1 - ((x1 - x2) * offset)

setAxis :: Double -> Double -> Double -> Double
setAxis    axis1        axis2        offset
      | axis1 == axis2 = axis1
      | axis1 < axis2  = axis1 + ((axis2 - axis1) * offset)
      | axis1 > axis2  = axis1 - ((axis1 - axis2) * offset)
  

setZ :: Double -> Double -> Double -> Double
setZ    z1        z2        offset
     | z1 > z2 =  z1 - (((abs $ z1 - z2) * offset))
     | z1 < z2 =  z1 + (((abs $ z1 - z2) * offset))
     | otherwise = z1

 

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

-- ========================================= socket general support functions========================================================
-- ==================================================================================================================================


-- ==================================================== local tests =================================================================
-- ==================================================================================================================================
-- ==================================================================================================================================
flexiSocketTestsDo = do
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
  
  runTestTT offsetY1
  runTestTT offsetX1Test
  runTestTT offsetX2Test
  runTestTT offsetZ1Test
  runTestTT offsetZ2Test
  runTestTT offsetZ3Test
  runTestTT offsetZ4Test
  runTestTT buildTestCubeTest
  -- =offset xyz
  runTestTT offSetB2B4CornerPointsTest
  runTestTT seeAdjustedXTest
  runTestTT seeShortenedXYDistanceTest
  runTestTT seeAngleTest
  runTestTT seeXYDistanceTest
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
  runTestTT cutTheDiamondF2ShiftedInOfFirstSocket
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
  runTestTT cutTheDiamondBtmFaceBrokenDownTest
  runTestTT cutTheDiamond1ShiftedInTest
  runTestTT cutTheDiamondB1ShiftedInTest
  runTestTT cutTheDiamondB4ShiftedInTest
  runTestTT cutTheDiamondBtmLeftCornerBrokenDownTest
  runTestTT cutTheDiamondF3CenteredRotatedTest
  runTestTT cutTheDiamondF3ShiftedRotatedTest
  runTestTT offset1stCubeFrontTopLineTest
  runTestTT offset1stCubeBtmFrontLineTest
  runTestTT offset1stCubeF2OfDiamond
  runTestTT cutTheDiamondF2CenteredMainCube
  runTestTT cutTheDiamond4CenteredTest
  

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
             f3 = Point {x_axis = 0.0, y_axis = -29.88294664396197, z_axis = 494.54022988505744},
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

-- =================================================================== :emergent ===============================================================




offset1stCubeFrontTopLineTest = TestCase $ assertEqual
  "cx center of front top line of 1st cube"
  --(Point {x_axis = 2.2000000000000006, y_axis = -25.6, z_axis = 500.0})
  centerOfTopFrontlinePoint
  (offsetPoint
     0.5 0.5 0.5
     --(Point 4.4 (-24.97) 500)
     --mainCubef2
     (f2 $ extractF2 firstCubeOfSocket)
     --(Point 0 (-26.23) 500)
     --mainCubef4
     (f3 $ extractF3 firstCubeOfSocket)
  )
  

cutTheDiamondF2CenteredMainCube = TestCase $ assertEqual
  "use cutTheDiamondF2Centered to get f2 centered"
  centerOfTopFrontLine
  (cutTheDiamondF2Centered firstCubeOfSocket)
  

offset1stCubeBtmFrontLineTest = TestCase $ assertEqual
  "cx center of btm front line of 1st cube"
  --(Point {x_axis = 2.48, y_axis =               -29, z_axis = 494.54})
  centerOfBtmFrontLinePoint

  (offsetPoint
     0.5 0.5 0.5
     --(Point 4.96 (-28.12) 494.54)
     (f1 $ extractF1 firstCubeOfSocket)
     --(Point 0 (-29.88) 494.54)
     (f4 $ extractF4 firstCubeOfSocket)
  )

cutTheDiamond4CenteredTest = TestCase $ assertEqual
  "use cutTheDiamond4Centered to get center of btm front line"
  (F1 centerOfBtmFrontLinePoint)
  (cutTheDiamondF1Centered firstCubeOfSocket)


cutTheDiamondF2ShiftedInOfFirstSocket = TestCase $ assertEqual
  "get F2 shifted in with cutTheDiamondF2ShiftedIn"
  f2ShiftedIn
  (cutTheDiamondF2ShiftedIn firstCubeOfSocket)

-- =========================================================================================================================================================
--next: calculate the top face and make sure f2Shifted in is the same

offset1stCubeF2OfDiamond = TestCase $ assertEqual
  "see the f2 position the diamond"
  (Point {x_axis = 2.3400000000000003, y_axis = -27.3, z_axis = 498.635})
  (
    let btmLine =
         ( offsetPoint
           0.5 0.5 0.5
           (Point 4.96 (-28.12) 494.54)
           (Point 0 (-29.88) 494.54)
         )   
        topLine =
         (offsetPoint
           0.5 0.5 0.5
           (Point 4.4 (-24.97) 500)
           (Point 0 (-26.23) 500)
         )
    in
      offsetPoint 0.5 0.5 0.25 topLine btmLine
 )

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

-- 1st error
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
{-
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
-}
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

seeAngleTest = TestCase $ assertEqual
  "look at angle between 2 points"
  (Angle 354.28940686250036)
  (seeAngle (Point 20 200 2000) (Point 10 100 1000))

seeXYDistanceTest  = TestCase $ assertEqual
  "look at distance between 2 points"
  (Radius 100.4987562112089)
  (seeXYDistance (Point 20 200 2000) (Point 10 100 1000))

seeShortenedXYDistanceTest  = TestCase $ assertEqual
  "see the offset distance between points"
  (Radius 100.4987562112089)
  (seeShortenedXYDistance (Point 20 200 2000) (Point 10 100 1000) 0)

seeAdjustedXTest = TestCase $ assertEqual
  "look at the adjusted x axis"
  (Point {x_axis = 20.0, y_axis = 200.0, z_axis = 2000.0})
  (seeAdjustedX 0 (Point 20 200 2000) (Point 10 100 1000))

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
offsetX1Test = TestCase $ assertEqual
  "offset X in positive direction"
  (7.5)
  (setAxis 5 10 0.5)
-- =======================================================================================================================================================================
offsetY1 = TestCase $ assertEqual
  "check the y offset of the top front line centered of cube 1 of the socket"
  (-25.60480063799153)
  (setAxis
    (-24.975376310276598)
    (-26.234224965706463)
    0.5 
  )



offsetX2Test = TestCase $ assertEqual
  "offset X in negative direction"
  (10.0)
  (setAxis 15 5 0.5)

offsetZ1Test = TestCase $ assertEqual
 "z1 > z2 with 0.25 offset"
 (15)
 (let z1 = 20
      z2 = 0
      offsetZ = 0.25
  in  --z1 + (negate ((abs $ z1 - z2) * offsetZ))
      setAxis z1 z2 offsetZ
 )

offsetZ2Test = TestCase $ assertEqual
 "z1 < z2 with 0.25 offset"
 (5)
 (let z1 = 0
      z2 = 20
      offsetZ = 0.25
  in  --z1 + (negate ((abs $ z1 - z2) * offsetZ))
      setAxis z1 z2 offsetZ
 )

offsetZ3Test = TestCase $ assertEqual
 "z1 = z2 with 0.25 offset"
 (20)
 (let z1 = 20
      z2 = 20
      offsetZ = 0.25
  in  --z1 + (negate ((abs $ z1 - z2) * offsetZ))
      setAxis z1 z2 offsetZ
 )

offsetZ4Test = TestCase $ assertEqual
 "z1 > z2 with 0.5 offset"
 (15)
 (let z1 = 20
      z2 = 0
      offsetZ = 0.25
  in  --z1 + (negate ((abs $ z1 - z2) * offsetZ))
      setAxis z1 z2 offsetZ
 )


--2nd pattern match error
cutDiamondTopFaceDissectedTest = TestCase $ assertEqual 
  "break down offsetCornerPoints and us F2 F4"
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

    --f2New
    {-F2 {f2 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 20.0}}-}
    --f4New
    {-F4 {f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 0.0}}-}
    --f2NewDropped should have z = 15
    {-
    F2 {f2 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 20.0}}}-}
    


  )
  (
    let cubeFrontTopLine = extractFrontTopLine testCubeStandard
        cubeBtmFrontLine = extractBottomFrontLine testCubeStandard
        f2New = offsetCornerPoints  0.5 0.5 0.5 (extractF2 cubeFrontTopLine) (extractF3 cubeFrontTopLine) (F2)
        f4New = offsetCornerPoints  0.5 0.5 0.5 (extractF1 cubeBtmFrontLine) (extractF4 cubeBtmFrontLine) (F4)
        --f2NewDropped = offsetCornerPoints  0 0 0.25 f2New f4New (F2)
        f2NewDropped = F2 $ offsetPoint 0.5 0.5 0.5 (f2 f2New) (f4 f4New)
        f4NewRaised  = offsetCornerPoints 0 0 0.25 f4New f2New (F4)
        f2NewAsBtmFace = toBottomFrontLine f2NewDropped
        f2NewFrontFace = cubeFrontTopLine +++ f2NewAsBtmFace
        f2NewBackFace = (transposeY (+(-10))) . {-reverseNormal .-} toBackFace $ f2NewFrontFace
    in  f2NewBackFace +++ f2NewFrontFace
        --f2NewDropped
        
  )
{-
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
-}  


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
  (cutTheDiamondTopFace testCubeStandard)

cutTheDiamondF3CenteredRotatedTest  = TestCase $ assertEqual 
  "cutTheDiamondF3CenteredTest rotated"
  (F3 {f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamondF3Centered testCubeRotated1QuadClockWise)
-- ====================================================================================================================
cutTheDiamondF3ShiftedRotatedTest  = TestCase $ assertEqual 
  "cutTheDiamondF3 shifted Test rotated"
  (F3 {f3 = Point {x_axis = 0.0, y_axis = 7.5, z_axis = 10.0}})
  (cutTheDiamondF3ShiftedIn testCubeRotated1QuadClockWise )

cutTheDiamondF3CenteredTest  = TestCase $ assertEqual 
  "cutTheDiamondF3CenteredTest"
  (F3 {f3 = Point {x_axis = 10.0, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamondF3Centered testCubeStandard)

cutTheDiamondF1CenteredTest = TestCase $ assertEqual 
  "cutTheDiamondF1CenteredTest"
  (F1 {f1 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamondF1Centered testCubeStandard)

cutTheDiamondF4CenteredTest = TestCase $ assertEqual 
  "cutTheDiamondF4CenteredTest"
  (F4 {f4 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 0.0}})
  (cutTheDiamondF4Centered testCubeStandard)

cutTheDiamondF3ShiftedInTest = TestCase $ assertEqual 
  "cutTheDiamondF3ShiftedInTest"
  (F3 {f3 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10}})
  (cutTheDiamondF3ShiftedIn testCubeStandard)

cutTheDiamondF3ShiftedInOffsetTest = TestCase $ assertEqual 
  "cutTheDiamondF3ShiftedInOffsetTest"
  (Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10})
  (offsetPoint 0.25 0.5 0.5 (Point {x_axis = 10.0, y_axis = 10.0, z_axis = 10.0}) (Point {x_axis = 0.0, y_axis = 10.0, z_axis = 10.0}))

cutTheDiamondF1ShiftedInTest = TestCase $ assertEqual 
  "cutTheDiamondF1ShiftedInTest"
  (F1 {f1 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamondF1ShiftedIn testCubeStandard)

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
  (cutTheDiamondTopRightCorner testCubeStandard)

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
  (cutTheDiamondBtmFace testCubeStandard)


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
  

  (cutTheDiamondBtmRightCorner testCubeStandard)

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
  (cutTheDiamondRightFace testCubeStandard)
-- =============================================================================================================================================================================================
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
    {- but got
    CubePoints {f1 = Point {x_axis = 5.0, y_axis = 10.0, z_axis = 5.0},
                f2 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                f3 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 0.0},
                f4 = Point {x_axis = 2.5, y_axis = 10.0, z_axis = 10.0},
                b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0},
                b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},    
                b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 10.0}} b4 x should be 2.5   -}
  )

  (cutTheDiamondBtmLeftCorner testCubeStandard)

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
  (cutTheDiamondLeftFace testCubeStandard)

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
  (cutTheDiamondTopLeftCorner testCubeStandard)

cutTheDiamondBackBaseTopFaceTest = TestCase $ assertEqual
 "cutTheDiamondBackBaseTopFaceTest"
 (BackFace {b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0},
            b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 20.0},
            b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 20.0},
            b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}})
 (cutTheDiamondBackBase (id) (id) (cutTheDiamondB2ShiftedIn testCubeStandard) (extractBackTopLine testCubeStandard)
 )

cutTheDiamondB2ShiftedInTest = TestCase $ assertEqual
 "cutTheDiamondB2ShiftedInTest"
 (B2 {b2 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 15.0}})
 (cutTheDiamondB2ShiftedIn testCubeStandard
 )

cutTheDiamondB2CenteredTest = TestCase $ assertEqual
  "cutTheDiamondB2CenteredTest"
  (B2 {b2 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 20.0}})
  (cutTheDiamondB2Centered testCubeStandard)

cutTheDiamondB4CenteredTest = TestCase $ assertEqual
  "cutTheDiamondB4CenteredTest"
  (B4 {b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 0.0}})
  (cutTheDiamondB4Centered testCubeStandard )

cutTheDiamond3CenteredForB3Test = TestCase $ assertEqual
  "test cutTheDiamond3Centered For B3"
  (B3 {b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamond3Centered (extractB3) (extractB4) (B3) testCubeStandard)

cutTheDiamondD3CenteredTest = TestCase $ assertEqual
  "cutTheDiamondD3Centered test"
  (B3 {b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamondD3Centered testCubeStandard)

cutTheDiamond1CenteredTest = TestCase $ assertEqual
 "cutTheDiamond1Centered for B1"
 (B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}})
 (cutTheDiamond1Centered (extractB2) (extractB1) (B1) testCubeStandard)

cutTheDiamond1CenteredFrontTest = TestCase $ assertEqual
 "cutTheDiamond1Centered for F1"
 (F1 {f1 = Point {x_axis = 0.0, y_axis = 10.0, z_axis = 10.0}})
 (cutTheDiamond1Centered (extractF2) (extractF1) (F1) testCubeStandard)

cutTheDiamondB1CenteredTest = TestCase $ assertEqual
 "cutTheDiamondB1Centered test"
 (B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}})
 (cutTheDiamondB1Centered testCubeStandard)

cutTheDiamond3ShiftedInAsF3Test = TestCase $ assertEqual
  "cutTheDiamond3ShiftedIn as F3"
  (F3 {f3 = Point {x_axis = 7.5, y_axis = 10.0, z_axis = 10.0}})
  (cutTheDiamond3ShiftedIn (cutTheDiamondF3Centered) (cutTheDiamondF1Centered) (F3) testCubeStandard)

cutTheDiamondB3CenteredTest = TestCase $ assertEqual
  "cutTheDiamondB3Centered test"
  (B3 {b3 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamondB3Centered testCubeStandard)

cutTheDiamondB3ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamondB3ShiftedIn test"
  (B3 {b3 = Point {x_axis = 7.5, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamondB3ShiftedIn testCubeStandard)

cutTheDiamond4ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamond4ShiftedIn test"
  (B4 {b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0}})
  (cutTheDiamond4ShiftedIn (cutTheDiamondB4Centered) (cutTheDiamondB2Centered) (B4) testCubeStandard)

cutTheDiamondBtmFaceBrokenDownTest = TestCase $ assertEqual
  "cutTheDiamondBtmFace broken down test"
  (BackTopLine {b2 = Point {x_axis = 10.0, y_axis = 0.0, z_axis = 0.0}, b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}})
  ((reverseNormal . toBackTopLine . extractBackBottomLine $ testCubeStandard))

cutTheDiamond1ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamond1ShiftedIn test"
  (B1 {b1 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamond1ShiftedIn
     (cutTheDiamondB4Centered)
     (cutTheDiamondB2Centered)
     (B1)
     testCubeStandard
  )

cutTheDiamondB1ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamondB1ShiftedIn"
  (B1 {b1 = Point {x_axis = 2.5, y_axis = 0.0, z_axis = 10.0}})
  (cutTheDiamondB1ShiftedIn testCubeStandard)

cutTheDiamondB4ShiftedInTest = TestCase $ assertEqual
  "cutTheDiamondB4ShiftedIn test"
  (B4 {b4 = Point {x_axis = 5.0, y_axis = 0.0, z_axis = 5.0}})
  (cutTheDiamondB4ShiftedIn testCubeStandard)

cutTheDiamondBtmLeftCornerBrokenDownTest = TestCase $ assertEqual
  "cutTheDiamondBtmLeftCorner broken down = TestCase $ assertEqual"
  ( --cutTheDiamondB1ShiftedIn
    B1 {b1 = Point {x_axis = 2.5, y_axis = 0.0, z_axis = 10.0}})
  ((cutTheDiamondB1ShiftedIn testCubeStandard))
{-
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
-}  
