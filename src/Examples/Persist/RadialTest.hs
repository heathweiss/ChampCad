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
Try out the new lineScanner radial scanner.
Scan a simple shape that can be printed off to see if it matches the scanned shape.
-}

module Examples.Persist.RadialTest() where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces)
import CornerPoints.FaceConversions(toTopFace)

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

databaseName = "src/Examples/Persist/radialTest.db"
type Height = Double






runCreateStlTestLayer :: IO ()
runCreateStlTestLayer = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' "layer1"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "layerId was found"
      liftIO $ stlGenerator (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
      
      liftIO $ putStrLn "stl should have been output"
  


createStlTestLayer :: [AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList
createStlTestLayer angleHeightRadius origin = do
  
  btmFaces <- buildCubePointsListSingle "bottom faces"
              (
                createBottomFaces origin (extractRadii angleHeightRadius) (extractAngles angleHeightRadius) 
              )

  cubes <- buildCubePointsListWithAdd "bottom faces"
              btmFaces
              (map                
                ((transposeZ   (\z -> (z + 5))) . toTopFace)
                btmFaces
              )
              
  return cubes

stlGenerator :: [AngleHeightRadius] -> Point ->  IO ()
stlGenerator angleHeightRadius origin = do
  let cpoints =  ((execState $ runExceptT   (createStlTestLayer angleHeightRadius origin  ) ) [])
  writeStlToFile $ newStlShape "test layer" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
  putStrLn "done"
