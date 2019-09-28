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

module Examples.ShoeLift.Pillars.FullScan(runFullTopTreadStlGenerator, runFullBtmTreadStlGenerator, FullScanBuilder(..), FullScanBuilderData(..),
                                          LayerNames(..), layerNames, fullTopBuilder, fullBtmBuilder) where

import Examples.ShoeLift.Pillars.Common(LayerName(..), databaseName)

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Persistable.Base as PstB

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace,
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace,
                                   extractTopFace)

import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose,
                                TransposeWithList, transposeWithList)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Builder.Monad(BuilderError(..),
                     cornerPointsErrorHandler, buildCubePointsList,
                     buildCubePointsListWithIOCpointsListBase,
                     CpointsStack, CpointsList, ExceptStackCornerPointsBuilder)

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

import Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin, loadAndExtractedAngleHeightRadiusFromDB)

import Builder.Monad (BuilderError(..),
                      cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListWithAdd,
                      buildCubePointsListSingle,
                      buildCubePointsListWithIOCpointsListBase,
                      CpointsStack, CpointsList)
import  Joiners.Manual(Joiner(..),joiner, takeLeading, takeTrailing)

import Primitives.Cylindrical.Solid(cylinder)

import Control.Lens

type CylinderTransposer = (Double) -> (Double)
type Height = Double

type FullScanBuilder = FullScanBuilderData -> ExceptStackCornerPointsBuilder 


data FullScanBuilderData =
  FullScanBuilderData
   {_angleHeighRadiusFSBD :: AnglesHeightsRadii,
    _originFSBD :: Point
    }


data LayerNames =
  LayerNames
  {_topLayer :: LayerName,
   _btmLayer :: LayerName
  }

layerNames = LayerNames "top" "bottom"

makeLenses ''FullScanBuilderData
makeLenses ''LayerNames


{---------------------------------------------entire geox tead-----------------------------------------------------------
Build the entire geox tread from the database just to have a look at it.
Also gets used for taking sections out of for building the various pieces: heel, toe, heel&midddle, toe&middle,
as well as the flat geox tread that has no variable height.
-}
--fullTopBuilder :: FullScanBuilderData -> ExceptT BuilderError (State CpointsStack) CpointsList
fullTopBuilder :: FullScanBuilder
fullTopBuilder fullScanBuilderData = do
  let setBottomOriginToZero origin' = (transposeZ (\z -> 0) origin')
      adjustHeightOfTopOrigin origin' = (transposeZ (+ (-7)) origin')
  
  btmFaces <- buildCubePointsListSingle "bottom faces"
              ( 
                createBottomFaces (setBottomOriginToZero $  fullScanBuilderData^.originFSBD)
                (extractRadii $  fullScanBuilderData^.angleHeighRadiusFSBD )
                (extractAngles $ fullScanBuilderData^.angleHeighRadiusFSBD) 
              )
  
  topFaces <- buildCubePointsListSingle "top faces"
              (
               createTopFacesVariableHeight
                 (adjustHeightOfTopOrigin $ fullScanBuilderData^.originFSBD)
                 (extractRadii $  fullScanBuilderData^.angleHeighRadiusFSBD)
                 (extractAngles $  fullScanBuilderData^.angleHeighRadiusFSBD)
                 --(map (+ (-10)) $ extractHeights $ fullScanBuilderData^.angleHeighRadiusFSBD)
                 --make it thinner
                 (map (+ (-15)) $ extractHeights $ fullScanBuilderData^.angleHeighRadiusFSBD)
              )
  
  cubes <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces
  
  return cubes

fullBtmBuilder :: FullScanBuilder
fullBtmBuilder fullScanBuilderData = do
  let setTopOriginTo origin' = (transposeZ (\z -> 20) $ origin')
      adjustHeightOfBtmOrigin origin' = (transposeZ (+ (7)) origin')
      
  btmFaces <- buildCubePointsListSingle "bottom faces"
              (createBottomFacesVariableHeight
                (adjustHeightOfBtmOrigin $  fullScanBuilderData^.originFSBD)
                (extractRadii $  fullScanBuilderData^.angleHeighRadiusFSBD)
                (extractAngles $ fullScanBuilderData^.angleHeighRadiusFSBD)
                (map (+ (-10)) $ extractHeights $ fullScanBuilderData^.angleHeighRadiusFSBD))

  topFaces <- buildCubePointsListSingle "top faces"
              ( 
                createTopFaces
                 (setTopOriginTo $  fullScanBuilderData^.originFSBD)
                 (extractRadii $ fullScanBuilderData^.angleHeighRadiusFSBD)
                 (extractAngles $ fullScanBuilderData^.angleHeighRadiusFSBD) 
              )

  cubes <- buildCubePointsListWithAdd "cubes"
           btmFaces
           topFaces

  return cubes


runFullTopTreadStlGenerator :: IO ()
runFullTopTreadStlGenerator =
  runTreadStlGeneratorBase (fullTopBuilder) (layerNames^.topLayer)

runFullBtmTreadStlGenerator :: IO ()
runFullBtmTreadStlGenerator =
  runTreadStlGeneratorBase (fullBtmBuilder) (layerNames^.btmLayer)

runTreadStlGeneratorBase :: FullScanBuilder -> LayerName -> IO ()
runTreadStlGeneratorBase fullScanBuilder layerName = runSqlite databaseName . PstB.asSqlBackendReader $ do
  layerId <- getBy $ nameUnique' layerName
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "layerId was found"
      
      let
        --Call to Builder used by valCpoints and cpoints. Would it get evaluated here, then shared by both via evalState and execState.
        --Probably not. How to evaluate it once, then look at it from both evalState and execState?
        builder = (fullScanBuilder $ FullScanBuilderData (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)  )

        --the returned value, instead of the state. Used to see if there was a Builder error.
        valCpoints =  ((evalState $ runExceptT builder) [])
        
        --state from the Builder
        cpoints =  ((execState $ runExceptT builder) [])
      
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "entire geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"

