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
Create the <single/double> sections that include the variable height(contoured) of the scan.
-}
module Examples.ShoeLift.Pillars.ContourScan(runContourScan, SectionBuilder(..), databaseName, SectionData(..), toSectionBuilderData, topToeBuilderData,
                                             runBtmHeelCpointsGenerator, runBtmCenterCpointsGenerator, runBtmToeCpointsGenerator,
                                             runTopHeelCpointsGenerator, runTopCenterCpointsGenerator, runTopToeCpointsGenerator) where

import Examples.ShoeLift.Pillars.FullScan(FullScanBuilder(..), FullScanBuilderData(..), LayerNames(..), layerNames,
                                          fullTopBuilder, fullBtmBuilder)
import Examples.ShoeLift.Pillars.Common(LayerName(..), databaseName)
import Examples.ShoeLift.Pillars.Pillars(outerTreadRingRadius)

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
                     buildCubePointsListWithIOCpointsListBase, buildCubePointsListSingleNoPush,
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



--makeLenses ''FullScanBuilderData
makeLenses ''LayerNames

type SectionBuilder = SectionData
                      -> FullScanBuilder
                      -> ExceptStackCornerPointsBuilder 



type CylinderTransposer = (Double) -> (Double)
type Height = Double

--transpose the center of the pillar cylinders along the y axis
--to center of each section
toeCylinderTransposer :: CylinderTransposer
toeCylinderTransposer = (+80)
centerCylinderTransposer :: CylinderTransposer
centerCylinderTransposer = (+0)
heelCylinderTransposer :: CylinderTransposer
heelCylinderTransposer = (+(-70))


--export for Main to run as exe.
runContourScan = runBtmHeelCenterStlGenerator

data SectionData =
  SectionDimensions
  {_cylinderHeightTS :: Height,
   _cylinderZTransposerTS :: CylinderTransposer,
   _cylinderYTransposerTS :: CylinderTransposer
  }
  |
  SectionBuilderData
  {_originTS :: Point,
   _cylinderHeightTS :: Height,
   _cylinderZTransposerTS :: CylinderTransposer,
   _cylinderYTransposerTS :: CylinderTransposer,
   _scanAHRTS :: AnglesHeightsRadii
  }


toSectionBuilderData :: Point -> SectionData -> AnglesHeightsRadii -> SectionData
toSectionBuilderData origin (SectionDimensions cylinderHeight cylinderHorizontalTransposer cylinderYTransposer) angleHeightRadii =
  SectionBuilderData origin cylinderHeight cylinderHorizontalTransposer cylinderYTransposer angleHeightRadii
  


---------- run singleSectionStlGenerator with SectionDimensions for the <top/btm> <heel/center/toe> section req'd. ------
--topToeBuilderData = SectionDimensions  16 (+(-20)) toeCylinderTransposer
topToeBuilderData = SectionDimensions  12 (+(-20)) toeCylinderTransposer 
--topHeelBuilderData = SectionDimensions  9 (+(-20)) heelCylinderTransposer
topHeelBuilderData = SectionDimensions  6 (+(-20)) heelCylinderTransposer
--topCenterBuilderData = SectionDimensions  13 (+(-20)) centerCylinderTransposer
topCenterBuilderData = SectionDimensions  8 (+(-20)) centerCylinderTransposer

btmCenterBuilderData = SectionDimensions  10 (+10) heelCylinderTransposer
btmHeelBuilderData = SectionDimensions  13 (+7) heelCylinderTransposer
btmToeBuilderData = SectionDimensions  10 (+(10)) toeCylinderTransposer

runTopToeStlGenerator =
  singleSectionStlGenerator topToeBuilderData toeCPoints fullTopBuilder (layerNames^.topLayer)

runBtmToeStlGenerator =
  singleSectionStlGenerator btmToeBuilderData toeCPoints fullBtmBuilder (layerNames^.btmLayer)

runTopCenterStlGenerator =
  singleSectionStlGenerator topCenterBuilderData centerCPoints fullTopBuilder (layerNames^.topLayer)

runBtmCenterStlGenerator =
  singleSectionStlGenerator btmCenterBuilderData centerCPoints fullBtmBuilder (layerNames^.btmLayer)

runTopHeelStlGenerator = 
  singleSectionStlGenerator topHeelBuilderData heelCPoints fullTopBuilder (layerNames^.topLayer)

runBtmHeelStlGenerator = 
  singleSectionStlGenerator btmHeelBuilderData heelCPoints fullBtmBuilder (layerNames^.btmLayer)

----------end:  run singleSectionStlGenerator with SectionDimensions for the <top/btm> <heel/center/toe> section req'd. ------

--genearte stl by running the <heel/center/toe> cpoints builder for <top/btm> layer
singleSectionStlGenerator :: SectionData -> SectionBuilder -> FullScanBuilder -> LayerName -> IO ()
singleSectionStlGenerator sectionDimensions sectionBuilder fullScanBuilder layerName = runSqlite databaseName . PstB.asSqlBackendReader $ do
    
  layerId <- getBy $ nameUnique' $ layerName
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []

  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    (Just (Entity key layerVal)) -> do
      let
        builder = sectionBuilder (toSectionBuilderData (extractOrigin layerVal) sectionDimensions (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity)) fullScanBuilder
        
        valCpoints = ((evalState $ runExceptT builder) [])

        cpoints =  ((execState $ runExceptT builder) [])

      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "heel geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"




-- ========================================================= double section stl generators =================================================
runTopHeelCenterStlGenerator :: IO ()
runTopHeelCenterStlGenerator =
  doubleSectionStlGenerator topHeelBuilderData heelCPoints topCenterBuilderData centerCPoints  fullTopBuilder (layerNames^.topLayer)

runTopCenterToeStlGenerator :: IO ()
runTopCenterToeStlGenerator =
  doubleSectionStlGenerator topCenterBuilderData centerCPoints topToeBuilderData toeCPoints   fullTopBuilder (layerNames^.topLayer)

runBtmHeelCenterStlGenerator :: IO ()
runBtmHeelCenterStlGenerator =
  doubleSectionStlGenerator btmHeelBuilderData heelCPoints btmCenterBuilderData centerCPoints  fullBtmBuilder (layerNames^.btmLayer)

runBtmCenterToeStlGenerator :: IO ()
runBtmCenterToeStlGenerator =
  doubleSectionStlGenerator btmCenterBuilderData centerCPoints btmToeBuilderData toeCPoints  fullBtmBuilder (layerNames^.btmLayer)



doubleSectionStlGenerator :: SectionData -> SectionBuilder -> SectionData -> SectionBuilder -> FullScanBuilder -> LayerName -> IO ()
doubleSectionStlGenerator section1Dimensions section1Builder section2Dimensions section2Builder fullScanBuilder layerName = runSqlite databaseName . PstB.asSqlBackendReader $ do
  layerId <- getBy $ nameUnique' $ layerName
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []

  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    (Just (Entity key layerVal)) -> do
      let
        builder1 = section1Builder (toSectionBuilderData (extractOrigin layerVal) section1Dimensions (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity)) fullScanBuilder
        
        val1Cpoints = ((evalState $ runExceptT builder1) [])

        cpoints1 =  ((execState $ runExceptT builder1) [])

        builder2 = section2Builder (toSectionBuilderData (extractOrigin layerVal) section2Dimensions (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity)) fullScanBuilder
        
        val2Cpoints = ((evalState $ runExceptT builder2) [])

        cpoints2 =  ((execState $ runExceptT builder2) [])

      case val1Cpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder1 was good"
          case val2Cpoints of
            (Left e) -> liftIO $ print $ e
            (Right a) -> do
              liftIO $ putStrLn "output from Builder2 was good"
              liftIO $ writeStlToFile $ newStlShape "heel geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] (cpoints1 ++ cpoints2))
              liftIO $ putStrLn "stl has been written"


-- =============================================================[CornerPoints] generators =======================================================
--output the [CornerPoints] for a single section.
--Return a [CornerPointsError] if there was an error in Builder.

singleSectionCptsGenerator :: SectionData -> SectionBuilder -> FullScanBuilder -> LayerName -> IO ([CornerPoints])
singleSectionCptsGenerator sectionDimensions sectionBuilder fullScanBuilder layerName = runSqlite databaseName . PstB.asSqlBackendReader $ do
    
  layerId <- getBy $ nameUnique' $ layerName
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []

  case layerId of
    Nothing -> return [CornerPointsError "layer id was not found"]
    (Just (Entity key layerVal)) -> do
      let
        builder = sectionBuilder (toSectionBuilderData (extractOrigin layerVal) sectionDimensions (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity)) fullScanBuilder
        
        valCpoints = ((evalState $ runExceptT builder) [])

        cpoints =  ((execState $ runExceptT builder) [])

      case valCpoints of
        --(Left e) -> liftIO $ print $ e
        (Left e) -> return [CornerPointsError $ show e]
        (Right a) -> do
          return cpoints


--export these to FlatScan module for building the flattened sections.
-- ============== tops =============
runTopHeelCpointsGenerator :: IO [CornerPoints]
runTopHeelCpointsGenerator =
  singleSectionCptsGenerator topHeelBuilderData heelCPoints fullTopBuilder (layerNames^.topLayer)

runTopCenterCpointsGenerator :: IO [CornerPoints]
runTopCenterCpointsGenerator =
  singleSectionCptsGenerator topCenterBuilderData centerCPoints fullTopBuilder (layerNames^.topLayer)  

runTopToeCpointsGenerator :: IO [CornerPoints]
runTopToeCpointsGenerator =
  singleSectionCptsGenerator topToeBuilderData toeCPoints fullTopBuilder (layerNames^.topLayer)  

-- ============== bottoms============
runBtmHeelCpointsGenerator :: IO [CornerPoints]
runBtmHeelCpointsGenerator =
  singleSectionCptsGenerator btmHeelBuilderData heelCPoints fullBtmBuilder (layerNames^.btmLayer)

runBtmCenterCpointsGenerator :: IO [CornerPoints]
runBtmCenterCpointsGenerator =
  singleSectionCptsGenerator btmCenterBuilderData centerCPoints fullBtmBuilder (layerNames^.btmLayer)  

runBtmToeCpointsGenerator :: IO [CornerPoints]
runBtmToeCpointsGenerator =
  singleSectionCptsGenerator btmToeBuilderData toeCPoints fullBtmBuilder (layerNames^.btmLayer)  

-- ======================================================== toe builder ====================================================================================
toeCPoints :: SectionBuilder
toeCPoints (SectionBuilderData origin' cylinderHeight cylinderMoveHorizontally cylinderYTransposer angleHeightRadius) fullScanBuilder = do
  let  pillarCylinder = map (transposeZ (cylinderMoveHorizontally)) $ cylinder outerTreadRingRadius (transposeY cylinderYTransposer origin') [Angle a | a <- [0,10..360]] cylinderHeight
  --extract the front faces of the entire <top/bottom> scan, as loaded with fullScanBuilder.
  --fullScanBuilder passed in will dictate whether shoe or tread scan is loaded.
  toeOuterFaces
    <- buildCubePointsListSingle "toeOuterFaces"
         ( 
            --map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
            map (extractFrontFace) $ (execState $ runExceptT   (fullScanBuilder $ FullScanBuilderData angleHeightRadius origin'  ) ) []
         )

  --extract [FrontFace] of pillar, and convert to [BackFace] so they can be added to the topOuterFaces, which are [FrontFace]
  pillarFrontFacesAsBackFaces
              <- buildCubePointsListSingle "riser circle outer faces"
                 ( 
                    map (toBackFace . extractFrontFace ) pillarCylinder
                 )

  --Use the manual join system to align [<Front/Back>Face] as the pillar has different # of Faces than the toeOuterFaces.
  --This will be replaced with the delaunay joiner.
  let mainJoiners =
        --Use these Joiners as the limits to the Joiner fx.
        --Took 18 pillars
        --fst = tread
        --snd = pillar
        [(Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (HoldLeading,Take ),
         (HoldLeading, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (HoldLeading,Take ),
         (HoldLeading, Take),
         (HoldLeading,Take ),
         (HoldLeading, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (Take, Take),
         (Take, Take)
        ]


  --the main cubes that wrap the toe. 
  mainCubes <- buildCubePointsListWithAdd "mainCubes"
        (
          joiner (takeLeading) (takeTrailing) ( map (fst) mainJoiners) $ drop 25 toeOuterFaces 
        )
        (--                                          the [Joiner] limits # of pillar faces
          joiner (takeLeading) (takeTrailing) (map (snd) mainJoiners) pillarFrontFacesAsBackFaces
        )

  fillerCube <- buildCubePointsListWithAdd "fillerCube"
     ([(toFrontLeftLine . extractFrontRightLine $ head mainCubes)
       +++
       (toFrontRightLine . extractFrontLeftLine $ last mainCubes)
      ])
     ([last pillarFrontFacesAsBackFaces])

  return fillerCube

-- ================================================================= center ==============================================================
--get the scanned [CornerPoints] for the center section of the scan.
--The treadAHR passed in will be either the geox or golf tread.

centerCPoints :: SectionBuilder
--centerCPoints treadAHR origin' cylinderHeight cylinderZAdj cylinderYTransposer treadCpoints = do
centerCPoints (SectionBuilderData origin' cylinderHeight cylinderMoveHorizontally cylinderYTransposer angleHeightRadius) fullScanBuilder = do
       --cylinder did no need to be adjusted on Yaxis.
  let  pillarCylinder = map (transposeZ (cylinderMoveHorizontally)) $ cylinder outerTreadRingRadius origin' [Angle a | a <- [0,10..360]] cylinderHeight
       
 
  heelOuterFaces
    <- buildCubePointsListSingle "heelOuterFaces"
         ( 
            --map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
           map (extractFrontFace) $ (execState $ runExceptT   (fullScanBuilder $ FullScanBuilderData  angleHeightRadius origin'  ) ) []
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
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, Take),
         (Take, HoldLeading),
         (Take, HoldLeading),
         (Take, HoldLeading),
         (Take, HoldLeading),
         (Take, HoldLeading),
         (Take, Take),
         (Take, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take),
         (HoldLeading, Take)
        ]

  leadingSideCubes <- buildCubePointsListWithAdd "first cubes"
         (--                                           the [Joiner] limits the # of heelOuterFaces.
          joiner (takeLeading     ) (takeTrailing) ( map (fst) leadingSideJoiners) $ drop 9 heelOuterFaces           
         )
         (--                                          the [Joiner] limits # of pillar faces
          joiner (takeLeading       ) (takeTrailing) (map (snd) leadingSideJoiners) pillarFrontFacesAsBackFaces
         )

  let
     --Use these Joiners as the limits to the Joiner fx.
        --fst = tread
        --snd = pillar
     trailingSideJoiners =
       [(Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),

        (Take,HoldLeading),
        (Take,HoldLeading),
        (Take,HoldLeading),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (Take,Take),
        (HoldLeading,Take)
       ]

  trailingSideCubes <- buildCubePointsListWithAdd "trailing cubes"
         (--                                           the [Joiner] limits the # of heelOuterFaces.
          joiner (takeLeading) (takeTrailing) ( map (fst) trailingSideJoiners) $ drop 45 heelOuterFaces           
         )
         (--                                          the [Joiner] limits # of pillar faces
          joiner (takeLeading) (takeTrailing) (map (snd) trailingSideJoiners) $ drop 19 pillarFrontFacesAsBackFaces
         )

  frontFillerCube <- buildCubePointsListWithAdd "backFillerCube"
        (
         [--lead tread front face
          (toFrontRightLine . extractFrontLeftLine $ last leadingSideCubes)
          +++
          --trailing tread front face
          (toFrontLeftLine . extractFrontRightLine $ head trailingSideCubes)
         ]
        )
        ([head $ drop 18 pillarFrontFacesAsBackFaces])

  backFillerCube <- buildCubePointsListWithAdd "backFillerCube"
        (
         [--lead tread front face
          (toFrontLeftLine . extractFrontRightLine $ head leadingSideCubes)
          +++
          --trailing tread front face
          (toFrontRightLine . extractFrontLeftLine $ last trailingSideCubes)
         ]
        )
        ([last pillarFrontFacesAsBackFaces])

  
  return frontFillerCube

-- ===================================================================== heel ===============================================================

--builder for the heel section, can be the geox or golf tread.
--Cuts out the collar
heelCPoints :: SectionBuilder
heelCPoints (SectionBuilderData origin' cylinderHeight cylinderMoveHorizontally cylinderYTransposer angleHeightRadius) fullScanBuilder = do
  
  let  pillarCylinder = map (transposeZ (cylinderMoveHorizontally)) $ cylinder outerTreadRingRadius (transposeY cylinderYTransposer origin') [Angle a | a <- [0,10..360]] cylinderHeight
       
       

  heelOuterFaces
  --Create the entire tread cubes as scanned.
  --Extract the [FrontFace] which will be combined with the riser collar faces.
    <- buildCubePointsListSingle "heelOuterFaces"
         (
           --map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
           map (extractFrontFace) $ (execState $ runExceptT   (fullScanBuilder $ FullScanBuilderData angleHeightRadius origin'  ) ) []
         )
                    

  pillarFrontFacesAsBackFaces
              <- buildCubePointsListSingle "riser circle outer faces"
                 ( 
                   map (toBackFace . extractFrontFace ) pillarCylinder
                 )

  let leadingJoiners =
        --fst: tread front faces
        --snd: pillars back faces
        [(Take,Take),
         (Take,Take),
         (Take,Take),
         (Take,Take),
         (Take,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (Take,Take),
         (Take,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (Take,Take),
         (Take,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (HoldLeading,Take)
        ]

  leadingCubes <- buildCubePointsListWithAdd "leadingCubes"
    (
      joiner (takeLeading) (takeTrailing) ( map (fst) leadingJoiners) heelOuterFaces  
    )
    (
      joiner (takeLeading) (takeTrailing) ( map (snd) leadingJoiners) pillarFrontFacesAsBackFaces 
    )

  let trailingJoiners =
        --fst: tread front faces
        --snd: pillars back faces
        [(Take,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (Take,Take),
         (Take, Take),
         (Take,Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (Take, Take),
         (HoldLeading,Take),
         (HoldLeading,Take),
         (Take,Take),
         (Take, Take),
         (Take,Take),
         (HoldLeading,Take),
         (Take,Take)
        ]

  trailingCubes <- buildCubePointsListWithAdd "trailingCubes"
    (
      joiner (takeLeading) (takeTrailing) ( map (fst) trailingJoiners) $ drop 63 heelOuterFaces  
    )
    (
      joiner (takeLeading) (takeTrailing) ( map (snd) trailingJoiners) $ drop 19 pillarFrontFacesAsBackFaces 
    )


  fillerCube <- buildCubePointsListWithAdd "fillerCube"
    ([
      (extractFrontRightLine $ last leadingCubes)
      +++
      (toFrontLeftLine .  extractFrontRightLine $ head trailingCubes)
     ]
    )
    ([head $ drop 18 pillarFrontFacesAsBackFaces])
 
  return leadingCubes
