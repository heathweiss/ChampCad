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
Create flat top and bottom sections, and double sections.
Take existing (double)sections imported from ContourScan, and turn them into a flat section.
Do this by extractiong the already flat <top/bottom>, transposing/converting it, and create a <single/double> flat section.

This will supply the not contoured sections that attach to the contoured sections, allowing for an overlap on the y-axis to lock them
together using the pillars.
-}
module Examples.ShoeLift.Pillars.FlatScan() where

import Examples.ShoeLift.Pillars.ContourScan(runBtmHeelCpointsGenerator, runBtmCenterCpointsGenerator, runBtmToeCpointsGenerator,
                                             runTopHeelCpointsGenerator, runTopCenterCpointsGenerator, runTopToeCpointsGenerator)

import Examples.ShoeLift.Pillars.Common(LayerName(..))

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
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace,
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace,
                                   extractTopFace)

import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose,
                                TransposeWithList, transposeWithList)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

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
                      buildCubePointsListSingle, buildCubePointsListSingleNoPush,
                      buildCubePointsListWithIOCpointsListBase,
                      CpointsStack, CpointsList)
import  Joiners.Manual(Joiner(..),joiner, takeLeading, takeTrailing)

import Primitives.Cylindrical.Solid(cylinder)

import Control.Lens



type FlatSectionBuilder = FlatSectionBuilderData -> ExceptT BuilderError (State CpointsStack) CpointsList

type SectionTransposer = (Double) -> (Double)
btmHeightTransposer = (+(-5.0))
topHeightTransposer = (+10.0)


data FlatSectionBuilderData =
  FlatSectionBuilderData
    {_sectionCpoints ::  [CornerPoints],
     _sectionTransposer :: SectionTransposer
    }



makeLenses ''FlatSectionBuilderData
-- ======================================================== single stl generator bases =============================================
--ToDo: Finish the Builer.Monad.cornerPointsErrorHandlerWithIOCpointsListBase that allows me to build with IO.
  --This should allow the stl generator to work within the Builder system, eliminating these case statements.
singleStlGeneratorBase :: SectionTransposer -> FlatSectionBuilder -> IO [CornerPoints] ->   IO ()
singleStlGeneratorBase transposer builder  cpoints' = do
  cpoints <- cpoints' 
  let bldrFlatCpoints = ((builder $ FlatSectionBuilderData  cpoints transposer))
      valFlatCpoints = ((evalState $ runExceptT $ bldrFlatCpoints) [])
      stateFlatCpoints =  ((execState $ runExceptT $ bldrFlatCpoints) [])
  case valFlatCpoints of
    (Left e) -> print $ show e
    (Right a) -> do
       writeStlToFile $ newStlShape "pillars" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] stateFlatCpoints)
       putStrLn "stl has been output"


singleBtmStlGeneratorBase :: IO [CornerPoints] ->  IO ()
singleBtmStlGeneratorBase cpoints = singleStlGeneratorBase btmHeightTransposer flatBtmBuilder cpoints

singleTopStlGeneratorBase :: IO [CornerPoints] ->  IO ()
singleTopStlGeneratorBase cpoints = singleStlGeneratorBase topHeightTransposer flatTopBuilder cpoints
-- ======================================================== top sections ===================================================
flatTopBuilder :: FlatSectionBuilder
flatTopBuilder flatSectionBuilderData = do
  contouredCpoints <- buildCubePointsListSingleNoPush "contouredCpoints"
                      (flatSectionBuilderData^.sectionCpoints)
                      
  btmFaces <- buildCubePointsListSingle "btmFaces"
              (map (extractBottomFace) contouredCpoints)

  topFaces <- buildCubePointsListSingle "topFaces"
              (map (toTopFace . (transposeZ $ flatSectionBuilderData^.sectionTransposer)) btmFaces)

  cubes    <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces

  return cubes

runTopHeelStlGenerator ::  IO ()
runTopHeelStlGenerator =
  singleTopStlGeneratorBase runTopHeelCpointsGenerator

runTopCenterStlGenerator ::  IO ()
runTopCenterStlGenerator =
  singleTopStlGeneratorBase runTopCenterCpointsGenerator

runTopToeStlGenerator ::  IO ()
runTopToeStlGenerator =
  singleTopStlGeneratorBase runTopToeCpointsGenerator
-- ======================================================== single bottom sections ==================================================
flatBtmBuilder :: FlatSectionBuilder
flatBtmBuilder flatSectionBuilderData = do
  contouredCpoints <- buildCubePointsListSingleNoPush "contouredCpoints"
                      (flatSectionBuilderData^.sectionCpoints)
                      

  topFaces <- buildCubePointsListSingle "topFaces"
              (map (extractTopFace) contouredCpoints)

  btmFaces <- buildCubePointsListSingle "btmFaces"
              (map (toBottomFace . (transposeZ $ flatSectionBuilderData^.sectionTransposer)) topFaces)

  cubes    <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces

  return cubes




runBtmHeelStlGenerator ::  IO ()
runBtmHeelStlGenerator =
  singleBtmStlGeneratorBase runBtmHeelCpointsGenerator

runBtmCenterStlGenerator ::  IO ()
runBtmCenterStlGenerator =
  singleBtmStlGeneratorBase runBtmCenterCpointsGenerator

runBtmToeStlGenerator ::  IO ()
runBtmToeStlGenerator =
  singleBtmStlGeneratorBase runBtmToeCpointsGenerator

-- ============================================================ double stl generator base/run ==================================================
--ToDo: Finish the Builer.Monad.cornerPointsErrorHandlerWithIOCpointsListBase that allows me to build with IO.
  --This should allow the stl generator to work within the Builder system, eliminating these case statements.
doubleStlGeneratorBase :: SectionTransposer -> FlatSectionBuilder -> IO [CornerPoints] -> IO [CornerPoints] ->  IO ()
doubleStlGeneratorBase transposer builder cpoints1 cpoints2 = do
  cpoints1' <- cpoints1
  cpoints2' <- cpoints2
  let bldrFlatCpoints1 = ((builder $ FlatSectionBuilderData  cpoints1' transposer))
      valFlatCpoints1 = ((evalState $ runExceptT $ bldrFlatCpoints1) [])
      stateFlatCpoints1 =  ((execState $ runExceptT $ bldrFlatCpoints1) [])

      bldrFlatCpoints2 = ((builder $ FlatSectionBuilderData  cpoints2' transposer))
      valFlatCpoints2 = ((evalState $ runExceptT $ bldrFlatCpoints2) [])
      stateFlatCpoints2 =  ((execState $ runExceptT $ bldrFlatCpoints2) [])      
  case valFlatCpoints1 of
    (Left e) -> print $ show e
    (Right a) ->
       case valFlatCpoints2 of
         (Left e) -> print $ show e
         (Right a) -> do
           writeStlToFile $ newStlShape "heel geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] (stateFlatCpoints1 ++ stateFlatCpoints2) )
           putStrLn "bottom <heel/center/toe> stl has been output"


runBtmHeelCenterStlGenerator ::  IO ()
runBtmHeelCenterStlGenerator =
  doubleStlGeneratorBase btmHeightTransposer flatBtmBuilder runBtmHeelCpointsGenerator runBtmCenterCpointsGenerator

runBtmCenterToeStlGenerator ::  IO ()
runBtmCenterToeStlGenerator =
  doubleStlGeneratorBase btmHeightTransposer flatBtmBuilder  runBtmCenterCpointsGenerator runBtmToeCpointsGenerator

runTopCenterToeStlGenerator ::  IO ()
runTopCenterToeStlGenerator =
  doubleStlGeneratorBase topHeightTransposer flatTopBuilder  runTopCenterCpointsGenerator runTopToeCpointsGenerator

runTopHeelCenterStlGenerator ::  IO ()
runTopHeelCenterStlGenerator =
  doubleStlGeneratorBase topHeightTransposer flatTopBuilder runTopHeelCpointsGenerator runTopCenterCpointsGenerator
