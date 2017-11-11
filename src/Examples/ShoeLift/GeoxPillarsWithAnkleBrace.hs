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
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace,
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine)

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
import  Builder.Joiner(Joiner(..),joiner, takeLeft, takeRight)

import Primitives.Cylindrical.Solid(cylinder)


type Height = Double
type LayerName = String

databaseName = "src/Examples/ShoeLift/geoxPillarsWithAnkleBrace.db"

--what to run in main.
--Handy to change it here, instead of going to main.
--Saves running ghci.
runGeoxPillarsWithAnkleBrace = golfTreadCenterCpoints  --geoxTreadHeelCpoints  -- golfTreadHeelCpoints 

cylinderRadius = Radius 20
{---------------------------------------------function index and terms-----------------------------------------------------------


=========Terms:=====
Riser collar:
  The center circle that will hold the dowel.
Must be the size of the dowel + an inserstion ring(collar) which which goes
between the dowel and the hole. This collar will be taller than the hole, so can have screws through it,
and into the dowel.

golfTread: The shape of the tread of the golf shoe, that was scanned and loaded from db.

geoxTread: The tread of the geox running shoe.

=======functions=======
entireGeoxTreadCpoints:
  The Builder function that puts out the entire geox heel, as scanned, including variable heights.
All the section functions (heel, middle, toe) draw from this for their Cpoints.
The heelFlatCpoints also uses it to generate a flat entire geox heel, which in turn gets used for the section functions.

geoxTreadHeelCpoints:
  Generates the heel of the geox tread, with center collar cut out. Includes variable heights
as it is from entireGeoxTreadCpoints.
Uses heelCpointsBuilderBase

heelCPoints:
  Create a heel section by removing the riser collar section.
Uses whatever full <geox,tread> [cpoints] is passed in.

heelCpointsBuilderBase:
-queries the db for the <geox/golf> tread.
-generates the full tread and passes it into
 heelCPoints if there were no errors.

-}

-- ================================================================= center ==============================================================
--get the scanned [CornerPoints] for the center section of the scan.
--The treadAHR passed in will be either the geox or golf tread.

centerCPoints ::   [AngleHeightRadius] -> Point -> Height -> Height -> ([AngleHeightRadius] -> Point -> (ExceptT BuilderError (State CpointsStack) CpointsList))
                 -> ExceptT BuilderError (State CpointsStack) CpointsList
centerCPoints treadAHR origin' cylinderHeight cylinderZAdj treadCpoints = do
  let  pillarCylinder = map (transposeZ (+ (cylinderZAdj))) $ cylinder cylinderRadius origin' [Angle a | a <- [0,10..360]] cylinderHeight
       
 
  heelOuterFaces
    <- buildCubePointsListSingle "heelOuterFaces"
         ( 
            map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
         )

  pillarFrontFacesAsBackFaces
              <- buildCubePointsListSingle "riser circle outer faces"
                 ( 
                    map (toBackFace . extractFrontFace ) pillarCylinder
                 )

  

  let leadingSideJoiners =
        --Use these Joiners as the limits to the Joiner fx.
        --Took 18 pillars
        --fst = tread
        --snd = pillar
        [(Take, Take),
         (HoldLeft, Take),
         (HoldLeft, Take),
         (HoldLeft, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, HoldLeft),
         (Take, HoldLeft),
         (Take, HoldLeft),
         (Take, HoldLeft),
         (Take, HoldLeft),
         (Take, Take),
         (Take, Take),
         (HoldLeft, Take),
         (HoldLeft, Take),
         (HoldLeft, Take),
         (HoldLeft, Take)
        ]

  leadingSideCubes <- buildCubePointsListWithAdd "first cubes"
         (--                                           the [Joiner] limits the # of heelOuterFaces.
          joiner (takeLeft     ) (takeRight) ( map (fst) leadingSideJoiners) $ drop 9 heelOuterFaces           
         )
         (--                                          the [Joiner] limits # of pillar faces
          joiner (takeLeft       ) (takeRight) (map (snd) leadingSideJoiners) pillarFrontFacesAsBackFaces
         )

  return leadingSideCubes

  

-- ===================================================================== heel ===============================================================

--builder for the heel section, can be the geox or golf tread.
--Cuts out the collar
heelCPoints ::   [AngleHeightRadius] -> Point -> Height -> Height -> ([AngleHeightRadius] -> Point -> (ExceptT BuilderError (State CpointsStack) CpointsList))
                 -> ExceptT BuilderError (State CpointsStack) CpointsList
heelCPoints treadAHR origin' cylinderHeight cylinderZAdj treadCpoints = do
  
  let  setOriginYaxisToCenterOfHeel origin' = (transposeY (+ (-70)) origin')
       pillarCylinder = map (transposeZ (+ (cylinderZAdj))) $ cylinder cylinderRadius (setOriginYaxisToCenterOfHeel origin') [Angle a | a <- [0,10..360]] cylinderHeight -- (7::Height)
       
       

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

  --let first45Cubes = (take 9 heelOuterFaces) |+++|  (pillarFrontFacesAsBackFaces)
  
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


  let topRightCornerTread = 63
      topRighCornerPillarStart = 18
      topRighCornerPillarTake = 5
      topRighCornerPillarEnd = topRighCornerPillarStart + topRighCornerPillarTake
      topRightCornerCube = (head $ drop topRightCornerTread heelOuterFaces) +++ (head $ drop topRighCornerPillarStart pillarFrontFacesAsBackFaces)

  cubesTopRighCorner --the section from 180 of pillar to 315 that gets joined to the start of the last45 of tread
         <- buildCubePointsListWithAdd "buildCubePointsListWithAdd"
            (--get the FrontRightLine of the heelLast45Cubes
              let frontRightLine = extractFrontRightLine topRightCornerCube
             --convert lazy list of FrontFace from them
              in [toFrontFace frontRightLine | x <- [1..]]
           
            )
            (--get pillar 180 to 315 degrees
              ((take $ topRighCornerPillarTake) . (drop topRighCornerPillarStart)) pillarFrontFacesAsBackFaces
            )
  let catchupTreadAfterTopRightCornerBothTake = 5
      endCatchupTreadAfterTopRightCorner = topRightCornerTread + catchupTreadAfterTopRightCornerBothTake
      endCatchupPillarAfterTopRightCorner = topRighCornerPillarEnd + catchupTreadAfterTopRightCornerBothTake
      
  catchupTreadAfterTopRightCorner <-  buildCubePointsListWithAdd "catchupTreadAfterTopRightCorner"
            (
              ((take catchupTreadAfterTopRightCornerBothTake ).(drop topRightCornerTread))
                heelOuterFaces
              
            )
            (
              (
                (take catchupTreadAfterTopRightCornerBothTake ).(drop topRighCornerPillarEnd)
              )
                pillarFrontFacesAsBackFaces
              
            )

  let catchUpPillarTake = 4
      endCatchUpPillar = endCatchupPillarAfterTopRightCorner + catchUpPillarTake
      catchUpTreads = [(toFrontFace . extractFrontLeftLine) $ last catchupTreadAfterTopRightCorner  | t <- [1..]]
  catchUpPillar <- buildCubePointsListWithAdd "catchUpPillar"
            (catchUpTreads)
            (((take catchUpPillarTake) . (drop endCatchupPillarAfterTopRightCorner)) pillarFrontFacesAsBackFaces )

  let runBothTake = 4
  runBoth <- buildCubePointsListWithAdd "runBoth"
             (
               ((take runBothTake).(drop endCatchupTreadAfterTopRightCorner)) heelOuterFaces
             )
             (
               ((take runBothTake).(drop endCatchUpPillar)) pillarFrontFacesAsBackFaces
             )
     
  crossoverCube --The cube which fills the gap between the first 45 to last 45 degrees
          <- buildCubePointsListWithAdd "crossoverCube"
             (--get the FrontLeftLine of the head cubes45To90  and convert to a FrontRightLine
               let
                 frontRightLine = toFrontRightLine . extractFrontLeftLine $ last cubes45To90 
              --get the FrontRightLine of the head cubes90To315 and convert to a FrontLeftLine
                 frontLeftLine = toFrontLeftLine . extractFrontRightLine $ head cubesTopRighCorner
              --add them together to get the FrontFace
               in [frontRightLine +++ frontLeftLine]
             )
             (--get the backRightLine of head cubes90To315 and convert to BackFace
               [toBackFace . extractBackRightLine $ head cubesTopRighCorner]
             )

 
  return cubesTopRighCorner


{-
Call heelCpointsBase with the Builder for the entire geox or bottom tread as required.
Also pass in the layerId for the database that is the scanned layer for the target tread.

=========given:
cylinderHeight: height of the cylinder which makes up the riser collar.

cylinderZAdj: Adjust the position of the riser collar up or down in relation to the tread cpoints.

fullScanBuilder: Builder.Monad fx that supplies the full scanned shape.

sectionBuilder: Builder.Monad fx that will generate the current section.
-}


heelCpointsBuilderBase :: Height -> Height
                          -> ([AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList)
                          -> ([AngleHeightRadius] -> Point -> Height -> Height -> ([AngleHeightRadius] -> Point
                              -> (ExceptT BuilderError (State CpointsStack) CpointsList))
                              -> ExceptT BuilderError (State CpointsStack) CpointsList
                             )
                          -> LayerName -> IO ()
heelCpointsBuilderBase cylinderHeight cylinderZAdj fullScanBuilder sectionBuilder layerName = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' layerName -- "top"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    (Just (Entity key layerVal)) -> do
      let
        builder = (sectionBuilder (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal) cylinderHeight cylinderZAdj (fullScanBuilder)  ) 
        
        valCpoints = ((evalState $ runExceptT builder) [])

        cpoints =  ((execState $ runExceptT builder) [])

      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "heel geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"


-------------------------------run the builders-----------------------------------------
geoxTreadHeelCpoints :: IO ()
geoxTreadHeelCpoints = 
  --heelCpointsBuilderBase 7 (-20) (entireGeoxTreadCpoints) "top"
  heelCpointsBuilderBase 9 (-20) (entireGeoxTreadCpoints) (heelCPoints) "top"

golfTreadHeelCpoints :: IO ()
golfTreadHeelCpoints = 
  heelCpointsBuilderBase 10 10 (entireGolfTreadCpoints) (heelCPoints) "bottom"

golfTreadCenterCpoints :: IO ()
golfTreadCenterCpoints = 
  heelCpointsBuilderBase 10 10 (entireGolfTreadCpoints) (centerCPoints) "bottom"  

{---------------------------------------------entire geox tead-----------------------------------------------------------
Build the entire geox tread from the database just to have a look at it.
Also gets used for taking sections out of for building the various pieces: heel, toe, heel&midddle, toe&middle,
as well as the flat geox tread that has no variable height.
-}
entireGeoxTreadCpoints :: [AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList
entireGeoxTreadCpoints geoxAHR origin = do
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

entireGolfTreadCpoints :: [AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList
entireGolfTreadCpoints geoxAHR origin = do
  let setTopOriginTo origin' = (transposeZ (\z -> 20) origin')
      adjustHeightOfBtmOrigin origin' = (transposeZ (+ (7)) origin')
      
  btmFaces <- buildCubePointsListSingle "bottom faces"
              (createBottomFacesVariableHeight (adjustHeightOfBtmOrigin origin)  (extractRadii geoxAHR) (extractAngles geoxAHR) (map (+ (-10)) $ extractHeights geoxAHR))

  topFaces <- buildCubePointsListSingle "top faces"
              ( 
                createTopFaces (setTopOriginTo origin)  (extractRadii geoxAHR) (extractAngles geoxAHR) 
              )

  cubes <- buildCubePointsListWithAdd "cubes"
           btmFaces
           topFaces

  return cubes

runGolfTreadStlGenerator :: IO ()
runGolfTreadStlGenerator =
  entireTreadStlGeneratorBase (entireGolfTreadCpoints) "bottom"

runGeoxStlGenerator :: IO ()
runGeoxStlGenerator =
  entireTreadStlGeneratorBase (entireGeoxTreadCpoints) "top"

--Checks for errors from the Builder and shows the error if applicable.
--Otherwise it outputs the stl.
entireTreadStlGeneratorBase :: ([AngleHeightRadius] -> Point -> ExceptT BuilderError (State CpointsStack) CpointsList) -> LayerName -> IO ()
entireTreadStlGeneratorBase fullScanBuilder layerName = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' layerName --top or bottom as named in the db.
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


