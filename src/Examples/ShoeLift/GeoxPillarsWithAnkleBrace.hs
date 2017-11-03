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

module Examples.ShoeLift.GeoxPillarsWithAnkleBrace (runGeoxPillarsWithAnkleBrace) where

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
import CornerPoints.FaceConversions(toTopFace, toBackFace)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace)

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

import Primitives.Cylindrical.Solid(cylinder)


type Height = Double

databaseName = "src/Examples/ShoeLift/geoxPillarsWithAnkleBrace.db"

--what to run in main.
--Handy to change it here, instead of going to main.
--Saves running ghci.
runGeoxPillarsWithAnkleBrace = runGeoxHeelCpoints
{---------------------------------------------heel geox-----------------------------------------------------------
Build the heel section only of the geox tread.
Add a center hole where the riser dowel will go.
-}
geoxHeelCpoints ::   [AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList
geoxHeelCpoints geoxAHR origin'  = do
  let  setOriginYaxisToCenterOfHeel origin' = (transposeY (+ (-50)) origin')
  
       take1st45Degrees = take 9
       --first45Angles = extractAngles $ take first45 geoxAHR
       --first45Height = extractHeights $ take first45 geoxAHR
       --first45Radii = extractRadii $ take first45 geoxAHR

  heelOuterFaces
    <- buildCubePointsListSingle "heelOuterFaces"
         (
           map (extractFrontFace) $ (execState $ runExceptT   (entireGeoxCpoints geoxAHR origin'  ) ) []
         )
                    

  pillarFrontFacesAsBackFaces
              <- buildCubePointsListSingle "riser circle outer faces"
                 ( 
                   map (toBackFace . extractFrontFace )
                       $ cylinder (Radius 20) (setOriginYaxisToCenterOfHeel origin') [Angle a | a <- [0,10..360]] (20::Height)
                 )

  first45 <- buildCubePointsListWithAdd "first 45 degrees"
             (take1st45Degrees heelOuterFaces)
             (pillarFrontFacesAsBackFaces)

  return first45


runGeoxHeelCpoints :: IO ()
runGeoxHeelCpoints = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' "top"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    (Just (Entity key layerVal)) -> do
      let
        builder = (geoxHeelCpoints (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)  ) 

        valCpoints = ((evalState $ runExceptT builder) [])

        cpoints =  ((execState $ runExceptT builder) [])

      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "heel geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"
  
{---------------------------------------------entire geox-----------------------------------------------------------
Build the entire geox tread from the database just to have a look at it.
Also gets used for taking sections out of for building the various pieces such as heel, toe, heel&midddle, toe&middle
-}
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

--Checks for errors from the Builder and shows the error if applicable.
--Otherwise it outputs the stl.
runEntireGeoxStlGenerator :: IO ()
runEntireGeoxStlGenerator = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' "top"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "layerId was found"
      
      let
        --Call to Builder used by valCpoints and cpoints. Would it get evaluated here, then shared by both via evalState and execState.
        --Probably not. How to evaluate it once, then look at it from both evalState and execState?
        builder = (entireGeoxCpoints (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)  )

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
      
      

