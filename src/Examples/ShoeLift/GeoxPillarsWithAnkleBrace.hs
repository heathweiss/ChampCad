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
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace,
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine)

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
type LayerName = String

databaseName = "src/Examples/ShoeLift/geoxPillarsWithAnkleBrace.db"

--what to run in main.
--Handy to change it here, instead of going to main.
--Saves running ghci.
runGeoxPillarsWithAnkleBrace = runGeoxStlGenerator
{---------------------------------------------heel geox-----------------------------------------------------------
Common function which can be used to build the heel section of either the geox or bottom tread.
Add a center hole where the dowel collar will go.

The entire tread is passed in via a function which processes the current layer's [AngleHeightRadius].
This is req'd so that this can create either the geox tread, or the bottom tread.

Terms:
    Riser collar:
          The center circle that will hold the dowel.
          Must be the size of the dowel + an inserstion ring(collar) which which goes
          between the dowel and the hole. This collar will be taller than the hole, so can have screws through it,
          and into the dowel.
    Tread: The shape of the tread runner that was loaded from db.
-}
heelCPoints ::   [AngleHeightRadius] -> Point -> Height -> Height -> ([AngleHeightRadius] -> Point -> (ExceptT BuilderError (State CpointsStack) CpointsList))
                 -> ExceptT BuilderError (State CpointsStack) CpointsList
heelCPoints treadAHR origin' cylinderHeight cylinderZAdj treadCpoints = do
  
  let  setOriginYaxisToCenterOfHeel origin' = (transposeY (+ (-70)) origin')
       pillarCylinder = map (transposeZ (+ (cylinderZAdj))) $ cylinder (Radius 20) (setOriginYaxisToCenterOfHeel origin') [Angle a | a <- [0,10..360]] cylinderHeight -- (7::Height)
       
       

  heelOuterFaces
  --Create the entire tread cubes as scanned.
  --Extract the [FrontFace] which will be combined with the riser collar faces.
    <- buildCubePointsListSingle "heelOuterFaces"
         (
           map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
         )
                    

  pillarFrontFacesAsBackFaces
              <- buildCubePointsListSingle "riser circle outer faces"
                 ( 
                   map (toBackFace . extractFrontFace ) pillarCylinder
                 )

  first45Cubes --joining tread and pillar
          <- buildCubePointsListWithAdd "first 45 degrees"
             (take 9 heelOuterFaces)
             (pillarFrontFacesAsBackFaces)
  
  pillar45To90Faces
          <- buildCubePointsListSingle "pillar2nd45Faces"
             (
               ((take 9) . (drop 9)) $ map (toBackFace . extractFrontFace ) pillarCylinder
             )

  
  cubes45To90
          <- buildCubePointsListWithAdd "cubes45To90"
             pillar45To90Faces
             (
               let
                 frontLeftLine  = extractFrontLeftLine $ last first45Cubes
               in --Had to do this here instead of outer monad as it crashed,
                  --probably by trying to evaluate infinite list.
                 [toFrontFace frontLeftLine | x <- [1..]]
             )


  heelLast45Cubes
         <- buildCubePointsListWithAdd "heelLast45Cubes"
            (drop 63 heelOuterFaces)
            (drop 27 pillarFrontFacesAsBackFaces)

  cubes90To315 --the section from 180 of pillar to 315 that gets joined to the start of the last45 of tread
         <- buildCubePointsListWithAdd "buildCubePointsListWithAdd"
            (--get the FrontRightLine of the heelLast45Cubes
              let frontRightLine = extractFrontRightLine $ head heelLast45Cubes
             --convert lazy list of FrontFace from them
              in [toFrontFace frontRightLine | x <- [1..]]
           
            )
            (--get pillar 180 to 315 degrees
              ((take 9) . (drop 18)) pillarFrontFacesAsBackFaces
            )
             
  crossoverCube --The cube which fills the gap between the first 45 to last 45 degrees
          <- buildCubePointsListWithAdd "crossoverCube"
             (--get the FrontLeftLine of the head cubes45To90  and convert to a FrontRightLine
               let
                 frontLeftLine = toFrontLeftLine . extractFrontRightLine $ head cubes90To315 
              --get the FrontRightLine of the head cubes90To315 and convert to a FrontLeftLine
                 frontRightLine = toFrontRightLine . extractFrontLeftLine $ head cubes45To90
              --add them together to get the FrontFace
               in [frontRightLine +++ frontLeftLine]
             )
             (--get the backRightLine of head cubes90To315 and convert to BackFace
               [extractBackFace $ head cubes90To315]
             )
  
  
  return heelLast45Cubes



{-
Call heelCpointsBase with the Builder for the entire geox or bottom tread as required.
Also pass in the layerId for the database that is the scanned layer for the target tread.
-}
heelCpointsBuilderBase :: ([AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList) -> LayerName -> IO ()
heelCpointsBuilderBase fullScanBuilder layerName = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' layerName -- "top"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    (Just (Entity key layerVal)) -> do
      let
        builder = (heelCPoints (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal) (7::Height) ((-20)::Height) (fullScanBuilder)  ) 

        valCpoints = ((evalState $ runExceptT builder) [])

        cpoints =  ((execState $ runExceptT builder) [])

      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "heel geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"


-------------------------------build the heel section of the geox/bottom tread-----------------------------------------
geoxHeelCpoints :: IO ()
geoxHeelCpoints = 
  heelCpointsBuilderBase (entireGeoxCpoints) "top"
  
{---------------------------------------------entire geox-----------------------------------------------------------
Build the entire geox tread from the database just to have a look at it.
Also gets used for taking sections out of for building the various pieces: heel, toe, heel&midddle, toe&middle
-}
entireGeoxCpoints :: [AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList
entireGeoxCpoints geoxAHR origin = do
  let setBottomOriginToZero origin' = (transposeZ (\z -> 0) origin')
      adjustHeightOfTopOrigin origin' = (transposeZ (+ (-7)) origin')
  
  btmFaces <- buildCubePointsListSingle "bottom faces"
              ( 
                createBottomFaces (setBottomOriginToZero origin)  (extractRadii geoxAHR) (extractAngles geoxAHR) 
              )
  
  topFaces <- buildCubePointsListSingle "top faces"
              (
               createTopFacesVariableHeight (adjustHeightOfTopOrigin origin)  (extractRadii geoxAHR) (extractAngles geoxAHR) (map (+ (-10)) $ extractHeights geoxAHR)
              )
  
  cubes <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces
  
  return cubes

runGeoxStlGenerator :: IO ()
runGeoxStlGenerator =
  entireTreadStlGeneratorBase (entireGeoxCpoints) "top"

--Checks for errors from the Builder and shows the error if applicable.
--Otherwise it outputs the stl.
entireTreadStlGeneratorBase :: ([AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList) -> LayerName -> IO ()
entireTreadStlGeneratorBase fullScanBuilder layerName = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' layerName -- "top"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "layerId was found"
      
      let
        --Call to Builder used by valCpoints and cpoints. Would it get evaluated here, then shared by both via evalState and execState.
        --Probably not. How to evaluate it once, then look at it from both evalState and execState?
        builder = (fullScanBuilder (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)  )

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


{-
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

-}
      

