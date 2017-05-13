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

module Scan.LineScanner(LineScan(..), Measurement(..), uniqueScanName, getMinHeight, adjustHeight,
                        adjustMeasurementHeightsToStartAtZero, measurementsToLines, adjustRadius,
                        lineScanId, measurementScanId', degree', extractMeasurement, measurementToLinesWithRadiusAdj,
                        linearMeasurementsBase, linearBackToFrontTopFaces, linearBackToFrontBottomFaces,
                        findIndiceOfMeasurementDegree, splitAndReverseBackMeasurementsAtDegree,
                        linearLeftToRightTBottomFaces) where

import CornerPoints.Degree(Degree(..))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.HorizontalFaces(createBottomFaces)
import CornerPoints.CornerPoints(CornerPoints(..), (+++>), (+++), (|+++|))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceConversions(reverseNormal)

import Geometry.Angle(Angle(..))

import Math.Trigonometry(sinDegrees,cosDegrees,tanDegrees,atanDegrees,coTanDegrees)

import Helpers.List(splitAndReverseBackHalf, SplitAtT(..) )

import Data.List(elemIndex, findIndex)

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

type HeightT = Double
type RadiusT = Double
type DegreeT = Double


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineScan
   scanName String
   UniqueScanName scanName
   desc String
   
  deriving Show

Measurement
  scanId LineScanId
  height HeightT -- Double --z distance from btm to top of riser
  degree Degree
  UniqueDegree degree
  radius RadiusT

  deriving Show Eq

Adjustments
  scanId LineScanId
  adjHeight HeightT --probably not needed as the min height is set to 0
  adjRadius RadiusT --adjust the Radius for scanner offset

  deriving Show Eq
|]


uniqueScanName = UniqueScanName
lineScanId = LineScanId
measurementScanId' = MeasurementScanId
degree' = MeasurementDegree

dbName = "src/Examples/ShoeLift/GeoxFlex.sql"

-- | Initialize a new database with all tables. Will alter tables of existing db.
initializeDatabase :: IO ()
initializeDatabase = runSqlite dbName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

insertScan :: IO ()
insertScan     = runSqlite dbName $ do
  scanId
    <- insert $ LineScan
       "heel"
       "heel of the geox shoe"
  liftIO $ putStrLn "scan inserted"

insertMeasurement :: String -> Double -> Double -> Double -> IO ()
insertMeasurement scanName degree height radius = runSqlite dbName $ do
  maybeScan <- getBy $ UniqueScanName "heel"
  case maybeScan of
   Nothing -> liftIO $ putStrLn "scan not found"
   Just (Entity scanId scan) -> do
     insert $ Measurement
              scanId
              height
              degree
              radius
              
     liftIO $ putStrLn "measurement inserted"

insertAdjustments :: HeightT -> RadiusT -> IO ()
insertAdjustments heightAdj radiusAdj = runSqlite dbName $ do
  maybeScan <- getBy $ UniqueScanName "heel"
  case maybeScan of
   Nothing -> liftIO $ putStrLn "scan not found"
   Just (Entity scanId scan) -> do
     insert $ Adjustments
              scanId
              heightAdj
              radiusAdj
     liftIO $ putStrLn "adjustment inserted"

-- ==========================================================back to front scan ============================================================
-- =========================================================================================================================================
linearMeasurementsBase :: (Point -> CornerPoints) -> (Point -> CornerPoints) -> (Point -> CornerPoints) -> (Point -> CornerPoints) -> DegreeT -> [Measurement] -> [CornerPoints]
linearMeasurementsBase initialFrontConst tailFrontConst  initialBackConst tailBackConstr splitAt measurements  = 
  let measurementWithZAdjToZero = adjustMeasurementHeightsToStartAtZero measurements
      (leadingMeasurements, trailingMeasurements) = splitAndReverseBackMeasurementsAtDegree splitAt measurementWithZAdjToZero
      trailingCpoint = createCornerPoint (initialBackConst) (Point 0 0 $ extractHeight $ head leadingMeasurements) (extractRadius $ head leadingMeasurements) (extractAngle $ head leadingMeasurements)
      leadingCpoint = createCornerPoint (initialFrontConst) (Point 0 0 $ extractHeight $ head trailingMeasurements) (extractRadius $ head trailingMeasurements) (extractAngle $ head trailingMeasurements)
  in  (reverseNormal(leadingCpoint +++ trailingCpoint))
      +++> 
      [ (createCornerPoint (tailBackConstr) (Point 0 0 $ extractHeight currBack) (extractRadius currBack) (extractAngle currBack))
        +++
        (createCornerPoint (tailFrontConst) (Point 0 0 $ extractHeight currfront) (extractRadius currfront) (extractAngle currfront))
        
        
        | currfront <- tail leadingMeasurements
        | currBack  <- tail trailingMeasurements
      ]
 
linearBackToFrontBottomFaces :: DegreeT -> [Measurement] -> [CornerPoints]
linearBackToFrontBottomFaces splitAt measurements =
  linearMeasurementsBase (B4) (F4) (B1) (F1) splitAt measurements


linearBackToFrontTopFaces :: DegreeT -> [Measurement] -> [CornerPoints]
linearBackToFrontTopFaces splitAt measurements =
  linearMeasurementsBase (B3) (F3) (B2) (F2) splitAt measurements

linearLeftToRightTBottomFaces :: DegreeT -> [Measurement] -> [CornerPoints]
linearLeftToRightTBottomFaces splitAt measurements =
  linearMeasurementsBase (B1)              (B4)           (F1)              (F4) splitAt measurements
--                       initialFrontConst tailFrontConst  initialBackConst tailBackConstr 
-- ==================================================== measurementsToLines ===============================================================
-- =========================================================================================================================================
{-| Convert [Measurement] to [CornerPoints] which is data captured from scanner, and put into database,
into a series of connected lines. These can be be any line, but will typically be:
Front and Back lines such as BackFrontLine.

If Radius adjustments are required use measurementsToLinesWithRadiusAdj

Problem:
Because it generates Lines from a central radial point, it skews the shape.
ToDo: Create the cubes from the back, and work forwards by splitting the [Measurement], reverse one half, and join them together.
      May need to replicate the 180 degree so both sides get it.

Given:
rightConstructor: The initial point constructor to start the series of lines. This will be typically be on the right side of the cube,
as the degree system runs counter clockwise.

leftConstructor: The point constructor for all the subsequent lines.

example of right/leftConstructor would be a F4 F1 combination to create [BottomFrontLine]

measurement: A [Measurement] that is the data captured by the scanner.

-}
measurementsToLines :: (Point -> CornerPoints) -> (Point -> CornerPoints) -> [Measurement] -> [CornerPoints]

measurementsToLines rightConstructor leftConstructor  (measurement:[]) =
  let measurementWithZAdjToZero = adjustMeasurementHeightsToStartAtZero [measurement]
      origin = Point 0 0 $ extractHeight $ head measurementWithZAdjToZero
  in
  [(createCornerPoint (rightConstructor) origin (extractRadius $ head measurementWithZAdjToZero) (extractAngle $ head measurementWithZAdjToZero))]
   
  
measurementsToLines rightConstructor leftConstructor  (measurement:measurements) =
  let measurementsWithZAdjToZero =  adjustMeasurementHeightsToStartAtZero (measurement:measurements)
      origin = Point 0 0 $ extractHeight $ head measurementsWithZAdjToZero
  in
  (createCornerPoint (rightConstructor) origin (extractRadius $ head measurementsWithZAdjToZero) (extractAngle $ head measurementsWithZAdjToZero)) +++>
  (measurementsToLines' leftConstructor (tail measurementsWithZAdjToZero))

measurementsToLines' :: (Point -> CornerPoints) -> [Measurement] -> [CornerPoints]

measurementsToLines' leftConstructor (measurement:[]) =
  let origin = Point 0 0 $ extractHeight measurement
  in
  [(createCornerPoint (leftConstructor) origin (extractRadius measurement) (extractAngle measurement))]

measurementsToLines' leftConstructor (measurement:measurements) =
  let origin = Point 0 0 $ extractHeight measurement
  in
  (createCornerPoint (leftConstructor) origin (extractRadius measurement) (extractAngle measurement)):
  (measurementsToLines' leftConstructor measurements)


measurementsToLines' _ [] = []

-- ================================================================== measurementsToLinesWithRadisuAdj ===============================================
-- ===================================================================================================================================================
-- | Same as measurementsToLines except:
-- | It allows the Radius to be adjusted with a multiplier factor.
-- | The Radius from the scanner is for the xy plane and is adjusted on the xy plane.
-- | The height also is adjusted to match the new angle.
measurementToLinesWithRadiusAdj :: RadiusT -> (Point -> CornerPoints) -> (Point -> CornerPoints) -> [Measurement] -> [CornerPoints]
measurementToLinesWithRadiusAdj radiusAdjMultiplier rightConstructor leftConstructor  (measurement:[]) =
  let measurementWithZAdjToZero = head $ adjustMeasurementHeightsToStartAtZero [measurement]
      
      zAngle = atanDegrees (extractHeight measurementWithZAdjToZero) (radius $ extractRadius measurementWithZAdjToZero)
      radiusAdjusted = (radius $ extractRadius measurementWithZAdjToZero) * radiusAdjMultiplier
      adjustedHeight = (sinDegrees zAngle) * radiusAdjusted
      currXYAngle = extractAngle measurementWithZAdjToZero
      origin = Point 0 0 adjustedHeight
  in
  [(createCornerPoint (rightConstructor) origin (Radius radiusAdjusted) currXYAngle)]

measurementToLinesWithRadiusAdj radiusAdjMultiplier rightConstructor leftConstructor  (measurement:measurements) =
  let measurementWithZAdjToZero = adjustMeasurementHeightsToStartAtZero (measurement:measurements)
      
      zAngle = atanDegrees (extractHeight $ head measurementWithZAdjToZero) (radius $ extractRadius $ head measurementWithZAdjToZero)
      radiusAdjusted = (radius $ extractRadius $ head measurementWithZAdjToZero) * radiusAdjMultiplier
      adjustedHeight = (sinDegrees zAngle) * radiusAdjusted
      currXYAngle = extractAngle $ head measurementWithZAdjToZero
      origin = Point 0 0 adjustedHeight
  in
  (createCornerPoint (rightConstructor) origin (Radius radiusAdjusted) currXYAngle) +++>
  (measurementToLinesWithRadiusAdj' radiusAdjMultiplier leftConstructor (tail measurementWithZAdjToZero))


measurementToLinesWithRadiusAdj' :: RadiusT -> (Point -> CornerPoints) ->  [Measurement] -> [CornerPoints]
measurementToLinesWithRadiusAdj' radiusAdjMultiplier leftConstructor (measurement:[]) =
  let zAxisAngle = atanDegrees (extractHeight measurement) (radius $ extractRadius measurement)
      radiusAdjusted = (radius $ extractRadius measurement) * radiusAdjMultiplier
      adjustedHeight = (tanDegrees zAxisAngle) * radiusAdjusted
      currXYAngle = extractAngle measurement
      origin = Point 0 0 adjustedHeight
  in
  [(createCornerPoint (leftConstructor) origin (Radius radiusAdjusted) currXYAngle)]



measurementToLinesWithRadiusAdj' radiusAdjMultiplier leftConstructor    (measurement:measurements) =
  let zAxisAngle = atanDegrees (extractHeight measurement) (radius $ extractRadius measurement)
      radiusAdjusted = (radius $ extractRadius measurement) * radiusAdjMultiplier
      adjustedHeight = (tanDegrees zAxisAngle) * radiusAdjusted
      currXYAngle = extractAngle measurement
      origin = Point 0 0 adjustedHeight
  in
  (createCornerPoint (leftConstructor) origin (Radius radiusAdjusted) currXYAngle) :
  measurementToLinesWithRadiusAdj' radiusAdjMultiplier leftConstructor (measurements)


-- ==============================================================================================================================================
-- ======================================================= support functions ====================================================================
findIndiceOfMeasurementDegree :: DegreeT -> [Measurement] -> Int
findIndiceOfMeasurementDegree degree measurements =
  case findIndex (\x -> degree == (angle $ extractAngle x)) measurements of
    Nothing -> 0
    Just index -> index

-- | The target degree is part of the second list. 
splitAndReverseBackMeasurementsAtDegree :: DegreeT -> [Measurement] -> ([Measurement],[Measurement])
splitAndReverseBackMeasurementsAtDegree degree measurements =
  splitAndReverseBackHalf (findIndiceOfMeasurementDegree degree measurements) measurements
  
extractRadius :: Measurement -> Radius
extractRadius (Measurement _ _ _ radius) =
  Radius $ radius

extractAngle :: Measurement -> Angle
extractAngle (Measurement _ _ angle _) =
  Angle angle

extractHeight :: Measurement -> HeightT
extractHeight (Measurement _ height _ _) = height

adjustMeasurementHeightsToStartAtZero :: [Measurement] -> [Measurement]
adjustMeasurementHeightsToStartAtZero measurements =
  let minHeight = getMinHeight measurements
      adjusterValue = 0 - minHeight
      
  in  map (adjustHeight adjusterValue) measurements


getMinHeight :: [Measurement] -> HeightT
getMinHeight measurements =
  let heights = map (extractHeight) measurements
  in  minimum heights
  
adjustHeight :: HeightT -> Measurement -> Measurement
adjustHeight heightT (Measurement scanId height degree radius) =
  Measurement scanId (heightT + height) degree radius 

--might not need this as new adjustment system is being built.
adjustRadius :: RadiusT -> Measurement -> Measurement
adjustRadius radiusT (Measurement scanId height degree radius) =
  Measurement scanId height degree $ radius + radiusT

extractMeasurement :: ((Entity Measurement)) -> Measurement
extractMeasurement (Entity measurementId' measurement) =
  measurement
{-
extractMeasurement eMeasurement =
  case eMeasurement of
    (Entity measurementId' measurement) -> measurement
    -- otherwise ->  Measurement (toSqlKey 1) 0 0 0
    
-}
