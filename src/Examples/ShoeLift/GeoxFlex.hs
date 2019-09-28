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

import TypeClasses.Transposable(transpose)

import  Primitives.DiamondCutter(DiamondBuilder(..), runFrontToBackDiamondBuilder, OffSet(..), defaultDiamondBuilder)
import  Primitives.DiamondCutterDB(uniqueDiamondName, DiamondDB(..), diamondDbToDiamondCutter, diamondDefaultDb)

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Persistable.Base as PstB

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
archName = "arch"
flexJointName = "flexJoint"
flexToeName = "flexToe"

--Takes a function which uses the Builder monad to produce stl or show state. 
--Supplies the db values for that function.
runDatabaseBuilder :: String -> ([Measurement] -> IO ()) -> IO ()
runDatabaseBuilder shapeName runScan = runSqlite databaseName . PstB.asSqlBackendReader $ do
  maybeScan <- getBy $ uniqueScanName shapeName
  
  case maybeScan of
   Nothing -> liftIO $ putStrLn "scan not found"
   Just (Entity scanId' scan) -> do
     maybeMeasurements <- selectList [measurementScanId' ==. scanId'] [Asc degree'] -- :: [Entity Measurement]
     
     let measurements = map (extractMeasurement) maybeMeasurements
     liftIO $  runScan measurements
     liftIO $ putStrLn "stl has been output"

runDatabaseDiamondBuilder :: String -> ([Measurement] -> DiamondBuilder -> IO ()) -> IO ()
runDatabaseDiamondBuilder shapeName runScan = runSqlite databaseName . PstB.asSqlBackendReader $ do
  maybeScan <- getBy $ uniqueScanName shapeName
  
  case maybeScan of
    Nothing -> liftIO $ putStrLn "scan not found"
    Just (Entity scanId' scan) -> do 
     maybeMeasurements <- selectList [measurementScanId' ==. scanId'] [Asc degree'] -- :: [Entity Measurement]
     let measurements = map (extractMeasurement) maybeMeasurements
     maybeDaimondBuilder <- runSqlite diamondDefaultDb . PstB.asSqlBackendReader $ do
       getBy $ uniqueDiamondName "wideBottomNarrowTop"
     case maybeDaimondBuilder of
         Nothing -> liftIO $ putStrLn "diamond cutter not found"
         Just (Entity diamondId diamondDbCutter) -> do
          
          liftIO $  runScan measurements $ diamondDbToDiamondCutter diamondDbCutter
          liftIO $ putStrLn "stl has been output"
      

stlBase :: [Measurement] -> ( [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList ) -> IO ()
stlBase measurements builder = do 
  let
    
    cpoints =  ((execState $ runExceptT (builder measurements)) [])
  
  writeStlToFile $ newStlShape "bottom of heel" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

stlDiamondBase :: [Measurement] -> DiamondBuilder ->  ( [Measurement] -> DiamondBuilder -> ExceptT BuilderError (State CpointsStack ) CpointsList ) -> IO ()
stlDiamondBase measurements diamondBuilder builder = do 
  let
    
    cpoints =  ((execState $ runExceptT (builder measurements diamondBuilder)) [])
  
  writeStlToFile $ newStlShape "bottom of heel" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)  
  
stateBase :: [Measurement] -> ( [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList ) -> IO ()
stateBase measurements builder = do
  let
    
    cpoints =  ((evalState $ runExceptT (builder measurements)) [])
  print $ show cpoints 


riserDiamondBuilder :: DiamondBuilder -> CornerPoints -> DiamondBuilder
riserDiamondBuilder  diamondBuilder cube =
   diamondBuilder { outerCube = cube}

-- =================================================================================================================================================
-- ==================================================== flex toe top ===================================================================================
-- =================================================================================================================================================
runFlexToeTopStl :: IO ()
runFlexToeTopStl = runDatabaseBuilder flexToeName  flexToeTopStl


runFlexToeTopState :: IO ()
runFlexToeTopState = runDatabaseBuilder flexToeName flexToeTopState

flexToeTopStl :: [Measurement] -> IO ()
flexToeTopStl measurements =
  stlBase measurements flexToeTopBuilder
  
flexToeTopState :: [Measurement] -> IO ()
flexToeTopState measurements =
  stateBase measurements flexToeTopBuilder
  

flexToeTopBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
flexToeTopBuilder measurements = do
         
  topFaces
    <- buildCubePointsListSingle "top faces"
       ( linearLeftToRightTopFaces 188 measurements)

  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( map ( toBottomFace . (transposeZ (\x -> (-20))) ) topFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes

-- =================================================================================================================================================
-- ==================================================== flex toe bottom ============================================================================
-- =================================================================================================================================================
runFlexToeBottomStl :: IO ()
runFlexToeBottomStl = runDatabaseBuilder flexToeName  flexToeBottomStl


runFlexToeBottomState :: IO ()
runFlexToeBottomState = runDatabaseBuilder flexToeName flexToeBottomState

flexToeBottomStl :: [Measurement] -> IO ()
flexToeBottomStl measurements =
  stlBase measurements flexToeBottomBuilder
  
flexToeBottomState :: [Measurement] -> IO ()
flexToeBottomState measurements =
  stateBase measurements flexToeBottomBuilder
  

flexToeBottomBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
flexToeBottomBuilder measurements = do
         
  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( linearLeftToRightBottomFaces 188 measurements)

  topFaces
    <- buildCubePointsListSingle "top faces"
       ( map ( toTopFace . (transposeZ (\x -> (40))) ) bottomFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


-- =================================================================================================================================================
-- ==================================================== flex toe riser==============================================================================
-- =================================================================================================================================================
runFlexToeRiserStl :: IO ()
runFlexToeRiserStl = runDatabaseBuilder flexToeName  flexToeRiserStl


runFlexToeRiserState :: IO ()
runFlexToeRiserState = runDatabaseBuilder flexToeName flexToeRiserState

flexToeRiserStl :: [Measurement] -> IO ()
flexToeRiserStl measurements =
  stlBase measurements flexToeRiserBuilder
  
flexToeRiserState :: [Measurement] -> IO ()
flexToeRiserState measurements =
  stateBase measurements flexToeRiserBuilder
  

flexToeRiserBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
flexToeRiserBuilder measurements = do
         
  topFaces
    <- buildCubePointsListSingle "top faces"
       ( map (transposeZ (\x -> 15)) ( linearLeftToRightTopFaces 188 measurements))

  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( map ( toBottomFace . (transposeZ (\x -> (0))) ) topFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


-- =================================================================================================================================================
-- =====================================================flex joint riser with diamonds ===================================================================
runFlexJointRiserWithDiamondsStl :: IO ()
runFlexJointRiserWithDiamondsStl = runDatabaseDiamondBuilder flexJointName flexJointRiserWithDiamondsStl

flexJointRiserWithDiamondsStl :: [Measurement] -> DiamondBuilder -> IO ()
flexJointRiserWithDiamondsStl measurements diamondBuilder =
  stlDiamondBase measurements diamondBuilder flexJointRiserWithDiamondsBuilder

flexJointRiserWithDiamondsBuilder :: [Measurement] -> DiamondBuilder  -> ExceptT BuilderError (State CpointsStack ) CpointsList
flexJointRiserWithDiamondsBuilder measurements diamondBuilder = do
  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       (map ((transposeZ (\x -> 0))) ( linearLeftToRightBottomFaces 226 measurements))

  topFaces
    <- buildCubePointsListSingle "top faces"
       ( map ( toTopFace . (transposeZ (\x -> 10)) ) bottomFaces )

  btmCubes
    <- buildCubePointsListSingle "bottom layer"
       (concatMap (runFrontToBackDiamondBuilder (riserDiamondBuilder diamondBuilder)) (bottomFaces |+++|  topFaces))

  topCubes <-
    buildCubePointsListSingle "top layer"
    (concatMap
     (runFrontToBackDiamondBuilder (riserDiamondBuilder diamondBuilder))
     ((map (transposeZ (+ 10)) topFaces) |+++| (bottomFaces |+++| topFaces))
    )
    

  return topCubes

-- =================================================================================================================================================
-- ==================================================== flex joint with diamonds top ===============================================================
-- =================================================================================================================================================

runFlexJointTopWithDiamondsStl :: IO ()
runFlexJointTopWithDiamondsStl = runDatabaseDiamondBuilder flexJointName flexJointTopWithDiamondsStl

flexJointTopWithDiamondsStl :: [Measurement] -> DiamondBuilder -> IO ()
flexJointTopWithDiamondsStl measurements diamondBuilder =
  stlDiamondBase measurements diamondBuilder flexJointTopWithDiamondsBuilder
  

flexJointTopWithDiamondsBuilder :: [Measurement] -> DiamondBuilder -> ExceptT BuilderError (State CpointsStack ) CpointsList
flexJointTopWithDiamondsBuilder measurements diamondBuilder  = do
         
  topFaces
    <- buildCubePointsListSingle "top faces"
       ( linearLeftToRightTopFaces 226 measurements)

  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( map ( toBottomFace . (transposeZ (\x -> (-5))) ) topFaces )

  cubes
    <- buildCubePointsListSingle "cubes"
       ( concatMap
           (runFrontToBackDiamondBuilder (riserDiamondBuilder diamondBuilder))
           (bottomFaces |+++| topFaces)
       )

  return cubes



-- =================================================================================================================================================
-- ==================================================== flex joint top ===================================================================================
-- =================================================================================================================================================
runFlexJointTopStl :: IO ()
runFlexJointTopStl = runDatabaseBuilder flexJointName  flexJointTopStl


runFlexJointTopState :: IO ()
runFlexJointTopState = runDatabaseBuilder flexJointName flexJointTopState

flexJointTopStl :: [Measurement] -> IO ()
flexJointTopStl measurements =
  stlBase measurements flexJointTopBuilder
  
flexJointTopState :: [Measurement] -> IO ()
flexJointTopState measurements =
  stateBase measurements flexJointTopBuilder
  

flexJointTopBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
flexJointTopBuilder measurements = do
         
  topFaces
    <- buildCubePointsListSingle "top faces"
       ( linearLeftToRightTopFaces 226 measurements)

  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( map ( toBottomFace . (transposeZ (\x -> (-5))) ) topFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


-- =================================================================================================================================================
-- ==================================================== flex joint riser ===================================================================================
-- =================================================================================================================================================
runFlexJointRiserStl :: IO ()
runFlexJointRiserStl = runDatabaseBuilder flexJointName  flexJointRiserStl


runFlexJointRiserState :: IO ()
runFlexJointRiserState = runDatabaseBuilder flexJointName flexJointRiserState

flexJointRiserStl :: [Measurement] -> IO ()
flexJointRiserStl measurements =
  stlBase measurements flexJointRiserBuilder
  
flexJointRiserState :: [Measurement] -> IO ()
flexJointRiserState measurements =
  stateBase measurements flexJointRiserBuilder
  

flexJointRiserBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
flexJointRiserBuilder measurements = do
  
         
  topFaces
    <- buildCubePointsListSingle "top faces"
       (map (transposeZ (\x -> 10)) ( linearLeftToRightTopFaces 226 measurements))

  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( map ( toBottomFace . (transposeZ (\x -> (0))) ) topFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


-- =================================================================================================================================================
-- ==================================================== arch top ===================================================================================
-- =================================================================================================================================================
runArchTopStl :: IO ()
runArchTopStl = runDatabaseBuilder archName  archTopStl


runArchTopState :: IO ()
runArchTopState = runDatabaseBuilder archName archTopState

archTopStl :: [Measurement] -> IO ()
archTopStl measurements =
  stlBase measurements archTopBuilder
  
archTopState :: [Measurement] -> IO ()
archTopState measurements =
  stateBase measurements archTopBuilder
  

archTopBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
archTopBuilder measurements = do
         
  topFaces
    <- buildCubePointsListSingle "top faces"
       ( linearLeftToRightTopFaces 223 measurements)

  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( map ( toBottomFace . (transposeZ (\x -> (-18))) ) topFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


-- =================================================================================================================================================
-- ==================================================== arch bottom  ===================================================================================
-- =================================================================================================================================================
runArchBottomStl :: IO ()
runArchBottomStl = runDatabaseBuilder archName  archBottomStl


runArchBottomState :: IO ()
runArchBottomState = runDatabaseBuilder archName archBottomState

archBottomStl :: [Measurement] -> IO ()
archBottomStl measurements =
  stlBase measurements archBottomBuilder
  
archBottomState :: [Measurement] -> IO ()
archBottomState measurements =
  stateBase measurements archBottomBuilder
  

archBottomBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
archBottomBuilder measurements = do
         
  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( linearLeftToRightBottomFaces 223 measurements)

  topFaces
    <- buildCubePointsListSingle "top faces"
       ( map ( toTopFace . (transposeZ (\x -> (25))) ) bottomFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


-- =================================================================================================================================================
-- ==================================================== arch riser =================================================================================
-- =================================================================================================================================================

runArchRiserStl :: IO ()
runArchRiserStl = runDatabaseBuilder archName  archRiserStl


runArchRiserState :: IO ()
runArchRiserState = runDatabaseBuilder archName archRiserState

archRiserStl :: [Measurement] -> IO ()
archRiserStl measurements =
  stlBase measurements archRiserBuilder
  
archRiserState :: [Measurement] -> IO ()
archRiserState measurements =
  stateBase measurements archRiserBuilder
  

archRiserBuilder :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
archRiserBuilder measurements = do
         
  topFaces
    <- buildCubePointsListSingle "top faces"
       (map (transposeZ (\x -> 50)) ( linearLeftToRightTopFaces 223 measurements))

  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       ( map ( toBottomFace . (transposeZ (\x -> (0))) ) topFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes

-- =================================================================================================================================================
-- =====================================================heel riser with diamonds ===================================================================
runHeelRiserWithDiamondsStl :: IO ()
runHeelRiserWithDiamondsStl = runDatabaseDiamondBuilder heelName heelRiserWithDiamondsStl

heelRiserWithDiamondsStl :: [Measurement] -> DiamondBuilder -> IO ()
heelRiserWithDiamondsStl measurements diamondBuilder =
  stlDiamondBase measurements diamondBuilder heelRiserWithDiamondsBuilder

heelRiserWithDiamondsBuilder :: [Measurement] -> DiamondBuilder  -> ExceptT BuilderError (State CpointsStack ) CpointsList
heelRiserWithDiamondsBuilder measurements diamondBuilder = do
  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       (map ((transposeZ (\x -> 0))) ( linearLeftToRightBottomFaces 225 measurements))

  topFaces
    <- buildCubePointsListSingle "top faces"
       ( map ( toTopFace . (transposeZ (\x -> 10)) ) bottomFaces )

  btmCubes
    <- buildCubePointsListSingle "bottom layer"
       (concatMap (runFrontToBackDiamondBuilder (riserDiamondBuilder diamondBuilder)) (bottomFaces |+++|  topFaces))

  topCubes <-
    buildCubePointsListSingle "top layer"
    (concatMap
     (runFrontToBackDiamondBuilder (riserDiamondBuilder diamondBuilder))
     ((map (transposeZ (+ 10)) topFaces) |+++| (bottomFaces |+++| topFaces))
    )
    

  return topCubes




-- =================================================================================================================================================
-- =====================================================heel riser no diamonds =====================================================================
runHeelRiserStl :: IO ()
runHeelRiserStl = runDatabaseDiamondBuilder heelName heelRiserStl

heelRiserStl :: [Measurement] -> DiamondBuilder -> IO ()
heelRiserStl measurements diamondBuilder =
  stlDiamondBase measurements diamondBuilder heelRiserBuilder

heelRiserBuilder :: [Measurement] -> DiamondBuilder  -> ExceptT BuilderError (State CpointsStack ) CpointsList
heelRiserBuilder measurements diamondBuilder = do
  bottomFaces
    <- buildCubePointsListSingle "bottom faces"
       (map ((transposeZ (\x -> 0))) ( linearLeftToRightBottomFaces 225 measurements))

  topFaces
    <- buildCubePointsListSingle "top faces"
       ( map ( toTopFace . (transposeZ (\x -> 20)) ) bottomFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes


   
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
       ( map ( toTopFace . (transposeZ (\x -> 20)) ) bottomFaces )

  cubes
    <- buildCubePointsListWithAdd "cubes"
       bottomFaces
       topFaces

  return cubes



-- =================================================================================================================================================
-- ========================================== heel top =============================================================================================

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
       ( map ( toBottomFace . (transposeZ (\x -> (-13))) ) topFaces )

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
