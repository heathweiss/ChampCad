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
module Examples.ShoeLift.GeoxFlex() where

import Scan.LineScanner(LineScan(..), Measurement(..), uniqueScanName, getMinHeight, adjustHeight,
                        adjustMeasurementHeightsToStartAtZero, measurementsToLines, adjustRadius,
                        lineScanId, measurementScanId', degree', extractMeasurement, measurementToLinesWithRadiusAdj,
                        linearBackToFrontBottomFaces, linearBackToFrontTopFaces, linearLeftToRightBottomFaces,
                        linearLeftToRightTopFaces)

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.Transpose(transposeZ)
import CornerPoints.FaceConversions(toTopFace, toBottomFace)
import CornerPoints.FaceExtraction(extractTopFace)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)



-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList, buildCubePointsListWithAdd, buildCubePointsListSingle)
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


databaseName = "src/Examples/ShoeLift/GeoxFlex.sql"
heelName = "heel"

--Takes a function which uses the Builder monad to produce stl or show state. 
--Supplies the db values for that function.
runDatabaseBuilder :: String -> ([Measurement] -> IO ()) -> IO ()
runDatabaseBuilder shapeName runScan = runSqlite databaseName $ do
  maybeScan <- getBy $ uniqueScanName shapeName
  
  case maybeScan of
   Nothing -> liftIO $ putStrLn "scan not found"
   Just (Entity scanId' scan) -> do
     maybeMeasurements <- selectList [measurementScanId' ==. scanId'] [Asc degree'] -- :: [Entity Measurement]
     
     let measurements = map (extractMeasurement) maybeMeasurements
     liftIO $  runScan measurements
     liftIO $ putStrLn "stl has been output"

stlBase :: [Measurement] -> ( [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList ) -> IO ()
stlBase measurements builder = do 
  let
    
    cpoints =  ((execState $ runExceptT (builder measurements)) [])
  
  writeStlToFile $ newStlShape "bottom of heel" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
  
stateBase :: [Measurement] -> ( [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList ) -> IO ()
stateBase measurements builder = do
  let
    
    cpoints =  ((evalState $ runExceptT (builder measurements)) [])
  print $ show cpoints 
-- =================================================================================================================================================
-- ========================================== heel bottom ==========================================================================================
runHeelBottomStl :: IO ()
runHeelBottomStl = runDatabaseBuilder heelName  heelBottomStl


runHeelBottomState :: IO ()
runHeelBottomState = runDatabaseBuilder heelName heelBottomState

heelBottomStl :: [Measurement] -> IO ()
heelBottomStl measurements =
  stlBase measurements heelBottomBuilder
  
heelBottomState :: [Measurement] -> IO ()
heelBottomState measurements =
  stateBase measurements heelBottomBuilder
  

heelBottomBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
heelBottomBuilder measurements = do
         
  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( linearLeftToRightBottomFaces 225 measurements)

  topFaces
    <- buildCubePointsListSingle "top faces"
       ( map ( toTopFace . (transposeZ (\x -> 25)) ) bottomFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


-- =================================================================================================================================================
-- ========================================== heel bottom ==========================================================================================

runHeelTopStl :: IO ()
runHeelTopStl = runDatabaseBuilder heelName  heelTopStl


runHeelTopState :: IO ()
runHeelTopState = runDatabaseBuilder heelName heelTopState

heelTopStl :: [Measurement] -> IO ()
heelTopStl measurements =
  stlBase measurements heelTopBuilder
  
heelTopState :: [Measurement] -> IO ()
heelTopState measurements =
  stateBase measurements heelTopBuilder
  

heelTopBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
heelTopBuilder measurements = do
         
  topFaces
    <- buildCubePointsListSingle "bottom faces"
       ( linearLeftToRightTopFaces 225 measurements)

  bottomFaces
    <- buildCubePointsListSingle "top faces"
       ( map ( toBottomFace . (transposeZ (\x -> (-3))) ) topFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes



-- ============================================================================================================================================
-- ================================================radial processing of heel. Depracated=======================================================
-- ============================================================================================================================================
--processed it radially. Not used as it does not work out will doing radially. Leave here for now as an example of use of
--measurementToLinesWithRadiusAdj and measurementToLines

{-
heelBottomRadialDbStl :: IO ()
heelBottomRadialDbStl = heelBottomDbBase (heelBottomRadialStl)

heelBottomRadialDbState :: IO ()
heelBottomRadialDbState = heelBottomDbBase (heelBottomRadialShowState)

heelBottomRadialStl :: [Measurement] -> IO ()
heelBottomRadialStl measurements = do
  let
    
    cpoints =  ((execState $ runExceptT (heelBottomRadial measurements)) [])
  
  writeStlToFile $ newStlShape "bottom of heel" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

heelBottomRadialShowState :: [Measurement] -> IO ()
heelBottomRadialShowState measurements = do
  let
    
    cpoints =  ((evalState $ runExceptT (heelBottomRadial measurements)) [])
  print $ show cpoints
  



heelBottomRadial :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
heelBottomRadial measurements = do
         
  bottomFrontLinesOutside
    <- buildCubePointsListSingle "bottomFrontLines of the outside wall."
       ( measurementsToLines (F4) (F1) measurements)

  bottomFrontLinesInside
    <- buildCubePointsListSingle "bottomFrontLines of the inside wall."
       ( measurementToLinesWithRadiusAdj 0.5 (B4) (B1) measurements)
  
  bottomFaces
    <- buildCubePointsListWithAdd "build the bottom faces"
       bottomFrontLinesInside
       bottomFrontLinesOutside
  
  bottomCubes 
    <- buildCubePointsListWithAdd "build the top faces and add to bottom faces"
       (map (toTopFace . (transposeZ (+ 10))) bottomFaces)
       bottomFaces

  topCubes
    <- buildCubePointsListWithAdd "transpose up the bottomCubes to get a flat top"
       (map ((transposeZ (\x -> 40) ) . extractTopFace) bottomCubes)
       bottomCubes

  return bottomFrontLinesInside

-}
