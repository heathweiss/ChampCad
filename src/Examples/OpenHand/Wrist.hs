{-# LANGUAGE ParallelListComp #-}
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
The wrist section attach to which will be attached:
 The fingers.
 The motor and board sections.
 The main socket made out of semi-flex.
-}
module Examples.OpenHand.Wrist(wristAndDoubleCylinderStlGenerator, wristAndDoubleCylinderShowCubes, wristSquaredOffStlGenerator,
                               wristSquaredOffShowCubes, wristSquaredOffStlFromDbGenerator, initializeDatabase, insertWristDimensions,
                               wristWithRoundRiserDBGenerator) where

import Examples.OpenHand.Common(Dimensions(..), commontDBName, uniqueDimensionName,
                               CommonFactors(..), setWristCommonFactors)
import Examples.OpenHand.FlexiSocket(uniqueFlexDimensionName, FlexDimensions(..)) 


-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.HorizontalFaces(createTopFaces,  createBottomFaces, createTopFacesSquaredOff)

import TypeClasses.Transposable(transpose)


import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), )
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)
import Geometry.Radius(doubleCylinderZip, squaredOff)
import Geometry.Rotation(rotateCornerPointAroundZAxis)

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

import Primitives.Cylindrical.Walled (cylinder, squaredYLengthenedCylinder)

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

--rotate the measured radii.
--will need to be put in some permanent module
rotate :: [a] -> [a]
rotate list = (last list) : (init list)

rotateBack :: [a] -> [a]
rotateBack (x:xs) = xs ++ [x]

-- ==========================================================database=====================================================================================
-- =======================================================================================================================================================
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
WristDimensions
   name String
   UniqueWristDimensionName name
   desc String
   squaredOffRiserHeight Double --height above socket, at which btm of riser starts
   radius Double --radius for the squared off section
   power Double  --the power to apply to the square function
   thickness Double --thickness of the wall
   xAdjustment Double
   yAdjustment Double
   
  deriving Show
|]

-- | Initialize a new database with all tables. Will alter tables of existing db.
initializeDatabase :: IO ()
initializeDatabase = runSqlite commontDBName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "wrist dimensions db initialized"

-- | Insert a new flex socket Dimensions into the database.
insertWristDimensions :: IO ()
insertWristDimensions     = runSqlite commontDBName $ do
  dimensionsId
            <- insert $ WristDimensions
               "sharkfin" 
               "make it the same dimensions as the shark swim fin which fits him good in Mar/17"
               20 --squaredOffRiserHeight
               20 --squaredOffRiserHeight
               5 --power
               3 --thickness
               0 --x adjust
               0 --y adjust

               
  --insert $ CurrentDimensions dimensionsId
  liftIO $ putStrLn "wrist dimensions inserted"
-- =======================================================================================================================================================
-- ===============================================================round shaped riser======================================================================
{-
Use a round shaft for the riser, so that attachment to the socket is always the same, no matter how tall the socket is.


-}
wristWithRoundRiserDBGenerator :: String -> IO ()
wristWithRoundRiserDBGenerator dimensionName = runSqlite commontDBName $ do
  maybeCommonDimensions <- getBy $ uniqueDimensionName dimensionName
  maybeFlexDimensions <- getBy $ uniqueFlexDimensionName dimensionName
  maybeWristDimensions <- getBy $ UniqueWristDimensionName dimensionName
  case maybeCommonDimensions of
        Nothing -> liftIO $ putStrLn "common dimensions not found"
        Just (Entity commonDimensionsId commonDimensions) -> do
          --liftIO $ flexSocketPlainStlGenerator $ setFlexiSocketCommonFactors commonDimensions
          liftIO $ putStrLn "common dimensions found"
          case maybeFlexDimensions of 
           Nothing -> liftIO $ putStrLn "flex dimensions not found"
           Just (Entity flexDimensionsId flexDimensions) -> do
             liftIO $ putStrLn "flex dimensions found"
             case maybeWristDimensions of
              Nothing -> liftIO $ putStrLn "maybeWristDimensions not found"
              Just (Entity wristDimensionsId wristDimensions) -> do
                liftIO $ putStrLn "wrist dimensions found"
                liftIO $ wristWithRoundRiserStlGenerator flexDimensions wristDimensions

wristWithRoundRiserStlGenerator :: FlexDimensions -> WristDimensions -> IO ()
wristWithRoundRiserStlGenerator flexDimensions wristDimensions = 
  let cpoints = execState ( runExceptT $ wristWithRoundRiser flexDimensions wristDimensions ) [] 
  in
      writeStlToFile $ newStlShape "wrist with round riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)



wristWithRoundRiser :: FlexDimensions -> WristDimensions -> ExceptT BuilderError (State CpointsStack ) CpointsList
wristWithRoundRiser (FlexDimensions _ _ riserHeight _ innerRiserRadius _ _)
                    (WristDimensions _ _ squaredOffRiserHeight radius power thickness xAdjust yAdjust) = do
  let angles = (map (Angle) [0,10..360])
      origin = Point 0 0 0
      adjustOrigin inOrigin = inOrigin {x_axis = xAdjust, y_axis = yAdjust}

  --cylinder that makes up the riser
  riser <- buildCubePointsListSingle "riser"
           (cylinder [(Radius (innerRiserRadius - thickness)) |x <- [1..]] [(Radius innerRiserRadius)|x <- [1..]]   angles  origin (riserHeight))

  --cylinder that makes up the squared off finger joiner
  --height will be riser height + joiner height
  --extract the top faces and add to a cutter array
  joinerTransitionTopFaces
        <- buildCubePointsListWithAdd "joinerTransitionTopFaces"
            (map (extractTopFace)
              (squaredCylinder (repeat $ Radius radius) thickness (adjustOrigin origin) angles (riserHeight + (squaredOffRiserHeight/2)) power)
            )
            ([CornerPointsId | x <- [1..5]] ++
             [CornerPointsNothing | x <- [1..26]] ++
             [CornerPointsId | x <- [1..]]
            )

  joinerTransitionCubes
       <- buildCubePointsListWithAdd "joinerCubes"
                 joinerTransitionTopFaces
                 riser

  joinerStraightCubes
       <- buildCubePointsListWithAdd "joinerStraightCubes"
          (map ( (transposeZ (+ (squaredOffRiserHeight/2))) . extractTopFace) joinerTransitionCubes )
          (joinerTransitionCubes)

  return riser
-- =======================================================================================================================================================
-- ===============================================================socket shaped riser=====================================================================
{- |
Wrist section with a squared off partial riser, to which the fingers can be attached.
The squared off section kicks off from the top of the socket which is a poor design.
Depracted for the version that uses a round riser.
-}
wristSquaredOff :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> Int -> Int -> ExceptT BuilderError (State CpointsStack ) CpointsList
wristSquaredOff    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM            drop'   take' = do
  let extensionHeight = 30
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])
      radii = repeat $ Radius 25 -- 15 was too small
      radiiSquared = --take 2 
                     [squaredOff 3 radius' angle'
                       | radius' <- radii
                       | angle'  <- angles
                     ]

   
  wristCubes  <- buildCubePointsListWithAdd "wristCubes"
                --take 3 is normal, but use more to see what is going on. Use
                --(concat $ take 3 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                 (concat $ take take' $ drop drop'  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]

  topOfWristAsLowerFaces
             <- buildCubePointsListSingle "extrudeTop"
                (map (lowerFaceFromUpperFace . extractTopFace) (head (drop drop' $ createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors))
                )
  {-before drop from db
  topOfWristAsLowerFaces
             <- buildCubePointsListSingle "extrudeTop"
                (map (lowerFaceFromUpperFace . extractTopFace) (head (drop 1 $ createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors))
                )
  -}

  let rotate' = 40
        
  squaredTopFaces
             <- buildCubePointsListWithAdd "squaredTopCubes"
                ( rotate . rotate . rotate . rotate . rotate . rotate . rotate $
                  (map
                   ((rotateCornerPointAroundZAxis 80 (Point 0 0 75)) . extractTopFace)
                    (squaredCylinder radii 4 (Point 0 0 65) angles 0 5)
                  )
                ) 
                
                ([CornerPointsId |  x <- [1..4]] ++
                 [CornerPointsNothing | x <- [1..24]] ++
                 [CornerPointsId | x <- [1..]]
                )
            
  squaredTopCubes
             <-  buildCubePointsListWithAdd "newTop"
                 squaredTopFaces
                 topOfWristAsLowerFaces

  return squaredTopCubes
{- before CommonDimensions
wristSquaredOff :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
wristSquaredOff    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM wristSquaredOff  = do
  let extensionHeight = 30
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])
      radii = repeat $ Radius 15 -- 15 looks to big to fit through outer socket
      radiiSquared = --take 2 
                     [squaredOff 3 radius' angle'
                       | radius' <- radii
                       | angle'  <- angles
                     ]

   
  wristCubes  <- buildCubePointsListWithAdd "wristCubes"
                --take 3 is normal, but use more to see what is going on. Use
                (concat $ take 3 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]

  topOfWristAsLowerFaces
             <- buildCubePointsListSingle "extrudeTop"
                (map (lowerFaceFromUpperFace . extractTopFace) (head (drop 1 $ createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors))
                )

  let rotate' = 40
        
  squaredTopFaces
             <- buildCubePointsListWithAdd "squaredTopCubes"
                ( rotate . rotate . rotate . rotate . rotate . rotate . rotate $
                  (map
                   ((rotateCornerPointAroundZAxis 80 (Point 0 0 75)) . extractTopFace)
                    (squaredCylinder radii 4 (Point 0 0 65) angles 0 5)
                  )
                ) 
                
                ([CornerPointsId |  x <- [1..4]] ++
                 [CornerPointsNothing | x <- [1..24]] ++
                 [CornerPointsId | x <- [1..]]
                )
            
  squaredTopCubes
             <-  buildCubePointsListWithAdd "newTop"
                 squaredTopFaces
                 topOfWristAsLowerFaces

  return squaredTopCubes

-}
wristSquaredOffStlFromDbGenerator :: IO ()
wristSquaredOffStlFromDbGenerator = runSqlite commontDBName $ do
  maybeCommonDimensions <- getBy $ uniqueDimensionName "dimensions 1"
  case maybeCommonDimensions of
    Nothing -> liftIO $ putStrLn "common dimensions not found"
    Just (Entity commonDimensionsId commonDimensions) -> do
      --liftIO $ putStrLn "all good so far"
      liftIO $ wristSquaredOffStlGenerator $ setWristCommonFactors commonDimensions

--load the json file and call generate stl
--wristSquaredOffStlGenerator :: IO ()
wristSquaredOffStlGenerator :: CommonFactors -> IO ()
wristSquaredOffStlGenerator (CommonFactors innerTranspose outerTranspose dropFactor takeFactor) = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 100::RowReductionFactor 
            --innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) . (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $ (MultiDegreeRadii name' degrees')
            --innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
            innerSleeveMDR = (transpose (+ innerTranspose)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                --(rotateSDR . rotateSDR . rotateSDR $ degrees')
                                degrees'
                              )
            --outerSleeveMDR = transpose (+3) innerSleeveMDR
            outerSleeveMDR = transpose (+(outerTranspose - innerTranspose)) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (wristSquaredOff (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM dropFactor takeFactor) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"
{-
--before CommonDimensions
wristSquaredOffStlGenerator :: IO ()
wristSquaredOffStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 100::RowReductionFactor 
            --innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) . (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $ (MultiDegreeRadii name' degrees')
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                --(rotateSDR . rotateSDR . rotateSDR $ degrees')
                                degrees'
                              )
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (wristSquaredOff (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"
-}

wristSquaredOffShowCubes :: IO ()
wristSquaredOffShowCubes = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 100::RowReductionFactor 
            --innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) . (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $ (MultiDegreeRadii name' degrees')
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                --(rotateSDR . rotateSDR . rotateSDR $ degrees')
                                degrees'
                              )
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            --cpoints =  ((execState $ runExceptT (wristSquaredOff (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
            cpoints =  ((evalState $ runExceptT (wristSquaredOff (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM 1 4 ) ) [])
        in  --writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
            print $ show cpoints
      Nothing                                ->
        putStrLn "File not decoded"


  
{- |
Wrist section that is topped off with a partial double cylinder.
May work but the curvature could be a problem.
-}
wristAndDoubleCylinder :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
wristAndDoubleCylinder    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM = do
  let extensionHeight = 30
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])
      radii = [Radius x | x <- [15,15..]]

   
  mainCubes  <- buildCubePointsListWithAdd "mainCubes"
                --take 3 is normal, but use more to see what is going on
                (concat $ take 3 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]
  
  
  getTop     <- buildCubePointsListSingle "extrudeTop"
                (map (lowerFaceFromUpperFace . extractTopFace) (head (drop 1 $ createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors))
                )
                
        
  newTop     <- buildCubePointsListWithAdd "newTop"
              --(createTopFaces (Point 0 0 100) (doubleCylinderZip radii angles) angles  )
                ( map
                  (extractTopFace)
                  ( let rotatedAngles = map (rotateAngle 110) angles
                    in
                     cylinder
                      (doubleCylinderZip radii rotatedAngles)
                      (doubleCylinderZip (map (transpose (+2) )   radii) rotatedAngles)
                      angles
                      (Point (-4) (-9) 0)
                      60
                  )
                )
              ([CornerPointsId | x <-[1,2..8]] ++ [CornerPointsNothing | x <-[1,2..16]] ++ [CornerPointsId | x <-[1,2..]])

  

  doubleTop  <- buildCubePointsListWithAdd "newTop"
                   (getTop)
                   (newTop)
  
  return doubleTop

--load the json file and call generate stl
wristAndDoubleCylinderStlGenerator :: IO ()
wristAndDoubleCylinderStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 100::RowReductionFactor 
            --innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) . (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $ (MultiDegreeRadii name' degrees')
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                --(rotateSDR . rotateSDR . rotateSDR $ degrees')
                                degrees'
                              )
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (wristAndDoubleCylinder (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"


wristAndDoubleCylinderShowCubes :: IO ()
wristAndDoubleCylinderShowCubes = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 100::RowReductionFactor 
            --innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) . (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $ (MultiDegreeRadii name' degrees')
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                --(rotateSDR . rotateSDR . rotateSDR $ degrees')
                                degrees'
                              )
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            --cpoints =  ((execState $ runExceptT (wristAndDoubleCylinder (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
            cpoints =  ((evalState $ runExceptT (wristAndDoubleCylinder (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  --writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
            print $ show cpoints
      Nothing                                ->
        putStrLn "File not decoded"



removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']

