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
                        adjustMeasurementHeightsToStartAtZero, measurementsToFrontLines, adjustRadius,
                        lineScanId, measurementScanId', degree', extractMeasurement) where

import CornerPoints.Degree(Degree(..))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.HorizontalFaces(createBottomFaces)
import CornerPoints.CornerPoints(CornerPoints(..), (+++>), (+++), (|+++|))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))

import Geometry.Angle(Angle(..))

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

type HeightT = Double
type RadiusT = Double

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

{-| Convert [Measurement ]to [CornerPoints]

-}
measurementsToFrontLines :: (Point -> CornerPoints) -> (Point -> CornerPoints) -> [Measurement] -> [CornerPoints]

measurementsToFrontLines rightConstructor leftConstructor  (measurement:[]) =
  let measurementWithZAdjToZero = adjustMeasurementHeightsToStartAtZero [measurement]
      origin = Point 0 0 $ extractHeight $ head measurementWithZAdjToZero
  in
  [(createCornerPoint (rightConstructor) origin (extractRadius $ head measurementWithZAdjToZero) (extractAngle $ head measurementWithZAdjToZero))]
   
  
measurementsToFrontLines rightConstructor leftConstructor  (measurement:measurements) =
  let measurementsWithZAdjToZero =  adjustMeasurementHeightsToStartAtZero (measurement:measurements)
      origin = Point 0 0 $ extractHeight $ head measurementsWithZAdjToZero
  in
  (createCornerPoint (rightConstructor) origin (extractRadius $ head measurementsWithZAdjToZero) (extractAngle $ head measurementsWithZAdjToZero)) +++>
  (measurementsToFrontLines' leftConstructor (tail measurementsWithZAdjToZero))

measurementsToFrontLines' :: (Point -> CornerPoints) -> [Measurement] -> [CornerPoints]

measurementsToFrontLines' leftConstructor (measurement:[]) =
  let origin = Point 0 0 $ extractHeight measurement
  in
  [(createCornerPoint (leftConstructor) origin (extractRadius measurement) (extractAngle measurement))]

measurementsToFrontLines' leftConstructor (measurement:measurements) =
  let origin = Point 0 0 $ extractHeight measurement
  in
  (createCornerPoint (leftConstructor) origin (extractRadius measurement) (extractAngle measurement)):
  (measurementsToFrontLines' leftConstructor measurements)


measurementsToFrontLines' _ [] = []


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
