{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ParallelListComp #-}

{- |
Oct 31/17
Create shoe lift for Geox runners.
Use ankle brace from roller blades.
Use bottom tread from golf shoes. Slightly shorter than the Geox, but still usable.
Use 3 wooden dowels as pillars to join the Geox shoe to the golf tread.
-}

module Examples.ShoeLift.GeoxPillarsWithAnkleBrace () where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine)

import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose,
                                TransposeWithList, transposeWithList)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Builder.Monad(BuilderError(..),
                     cornerPointsErrorHandler, buildCubePointsList,
                     buildCubePointsListWithIOCpointsListBase,
                     CpointsStack, CpointsList)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy hiding (get)
import qualified Control.Monad.State.Lazy as ST (get)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

import Persistable.Radial (Layer(..), AngleHeightRadius(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin, loadAndExtractedAngleHeightRadiusFromDB)

import Builder.Monad (BuilderError(..),
                      cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListWithAdd,
                      buildCubePointsListSingle,
                      buildCubePointsListWithIOCpointsListBase,
                      CpointsStack, CpointsList) 


type Height = Double

databaseName = "src/Examples/ShoeLift/geoxPillarsWithAnkleBrace.db"

transposeHeights :: [Height] -> [CornerPoints] -> (Double -> Double -> Double) -> [CornerPoints]
transposeHeights heights cpoints f =
  [transposeZ (f currHeight) currPoint 
    | currHeight <- heights
    | currPoint <- cpoints
  ]
   

entireGeoxCpoints :: [AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList
entireGeoxCpoints geoxAHR origin = do
  let setBottomOriginToZero origin' = (transposeZ (\z -> 0) origin')
  btmFaces <- buildCubePointsListSingle "bottom faces"
              ( 
                createBottomFaces (setBottomOriginToZero origin)  (extractRadii geoxAHR) (extractAngles geoxAHR) 
              )
  
  topFaces <- buildCubePointsListSingle "top faces"
              (
               createTopFacesVariableHeight origin (extractRadii geoxAHR) (extractAngles geoxAHR) (extractHeights geoxAHR)
              )
  
  cubes <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces
              
  
  return cubes

lookAtEntireGeoxStlGenerator :: [AngleHeightRadius] -> Point ->  IO ()
lookAtEntireGeoxStlGenerator angleHeightRadius origin = do
  let cpoints =  ((execState $ runExceptT   (entireGeoxCpoints angleHeightRadius origin  ) ) [])
  writeStlToFile $ newStlShape "entire geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
  putStrLn "done"

runLookAtEntireGeox :: IO ()
runLookAtEntireGeox = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' "top"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "layerId was found"
      liftIO $ lookAtEntireGeoxStlGenerator (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
      
      liftIO $ putStrLn "stl should have been output"

lookAtEntireGeoxState :: [AngleHeightRadius] -> Point ->  IO ()
lookAtEntireGeoxState angleHeightRadius origin = do
   print $ show $ ((evalState $ runExceptT (entireGeoxCpoints angleHeightRadius origin  ) ) [])

runLookAtEntireGeoxState :: IO ()
runLookAtEntireGeoxState = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' "top"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "layerId was found"
      liftIO $ lookAtEntireGeoxState (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
