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
                        lineScanId, measurementScanId', degree', extractMeasurement, measurementToLinesWithRadiusAdj)

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.Transpose(transposeZ)
import CornerPoints.FaceConversions(toTopFace)
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

heelBottomDbBase :: ([Measurement] -> IO ()) -> IO ()
heelBottomDbBase runScan = runSqlite databaseName $ do
  maybeScan <- getBy $ uniqueScanName "heel"
  
  case maybeScan of
   Nothing -> liftIO $ putStrLn "heel scan not found"
   Just (Entity scanId' scan) -> do
     maybeMeasurements <- selectList [measurementScanId' ==. scanId'] [Asc degree'] -- :: [Entity Measurement]
     
     let measurements = map (extractMeasurement) maybeMeasurements
     liftIO $  runScan measurements
     liftIO $ putStrLn "heel bottom stl has been output"
  
heelBottomDbStl :: IO ()
heelBottomDbStl = heelBottomDbBase (heelBottomStl)

heelBottomDbState :: IO ()
heelBottomDbState = heelBottomDbBase (heelBottomShowState)

heelBottomStl :: [Measurement] -> IO ()
heelBottomStl measurements = do
  let
    
    cpoints =  ((execState $ runExceptT (heelBottom measurements)) [])
  
  writeStlToFile $ newStlShape "bottom of heel" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

heelBottomShowState :: [Measurement] -> IO ()
heelBottomShowState measurements = do
  let
    
    cpoints =  ((evalState $ runExceptT (heelBottom measurements)) [])
  print $ show cpoints
  



heelBottom :: [Measurement] -> ExceptT BuilderError (State CpointsStack ) CpointsList
heelBottom measurements = do
  test <- buildCubePointsListSingle "do I get stack overflow"
          [F1 $ Point 1 1 1]
         
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
