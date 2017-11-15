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
import  Builder.Joiner(Joiner(..),joiner, takeLeading, takeTrailing)

import Primitives.Cylindrical.Solid(cylinder)


type Height = Double
type LayerName = String

databaseName = "src/Examples/ShoeLift/geoxPillarsWithAnkleBrace.db"

--what to run in main.
--Handy to change it here, instead of going to main.
--Saves running ghci.
runGeoxPillarsWithAnkleBrace = geoxToeStlGenerator

cylinderRadius = Radius 20
type CylinderTransposer = (Double) -> (Double)
toeCylinderTransposer :: CylinderTransposer
toeCylinderTransposer = (+80)
centerCylinderTransposer :: CylinderTransposer
centerCylinderTransposer = (+0)
heelCylinderTransposer :: CylinderTransposer
heelCylinderTransposer = (+(-70))
type SectionTransposer = (Double) -> (Double)

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
Uses runSingleSectionStlGenerator

heelCPoints:
  Create a heel section by removing the riser collar section.
Uses whatever full <geox,tread> [cpoints] is passed in.

runSingleSectionStlGenerator:
-queries the db for the <geox/golf> tread.
-generates the full tread and passes it into
 heelCPoints if there were no errors.

-}

-- ================================================================ toe ===============================================================
--get the scanned [CornerPoints] for the front section of the scans.
--The treadAHR passed in will be either the geox or golf tread.
{-
origin': original origin from the scan.
treadAHR: The AngleHeightRadius datatype from the scan which gets passed to the geox/golf tread builder used to get the [FrontFace].
cylinderHeight: set the height of the cylinder above the bottom of the scan.
cylinderZAdj: used to move the origin forward to the center of the front section
treadCpoints: The Builder Fx used to create the [CornerPoints] from the [AngleHeightRadius]
-}
{-
toeCPoints ::   [AngleHeightRadius] -> Point -> Height -> Height -> CylinderTransposer -> ([AngleHeightRadius] -> Point -> (ExceptT BuilderError (State CpointsStack) CpointsList))
                 -> ExceptT BuilderError (State CpointsStack) CpointsList
-}
toeCPoints :: SectionBuilder
--toeCPoints treadAHR origin' cylinderHeight cylinderZAdj cylinderYTransposer treadCpoints = do
toeCPoints treadAHR origin' cylinderHeight cylinderZAdj cylinderYTransposer fullScanBuilder = do
  let  pillarCylinder = map (transposeZ (+ (cylinderZAdj))) $ cylinder cylinderRadius (transposeY cylinderYTransposer origin') [Angle a | a <- [0,10..360]] cylinderHeight

  toeOuterFaces
    <- buildCubePointsListSingle "toeOuterFaces"
         ( 
            --map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
            map (extractFrontFace) $ (execState $ runExceptT   (fullScanBuilder $ FullScanBuilderData treadAHR origin'  ) ) []
         )

  pillarFrontFacesAsBackFaces
              <- buildCubePointsListSingle "riser circle outer faces"
                 ( 
                    map (toBackFace . extractFrontFace ) pillarCylinder
                 )

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
centerCPoints treadAHR origin' cylinderHeight cylinderZAdj cylinderYTransposer fullScanBuilder = do
       --cylinder did no need to be adjusted on Yaxis.
  let  pillarCylinder = map (transposeZ (+ (cylinderZAdj))) $ cylinder cylinderRadius origin' [Angle a | a <- [0,10..360]] cylinderHeight
       
 
  heelOuterFaces
    <- buildCubePointsListSingle "heelOuterFaces"
         ( 
            --map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
           map (extractFrontFace) $ (execState $ runExceptT   (fullScanBuilder $ FullScanBuilderData  treadAHR origin'  ) ) []
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
--heelCPoints treadAHR origin' cylinderHeight cylinderZAdj cylinderYTransposer treadCpoints = do
heelCPoints treadAHR origin' cylinderHeight cylinderZAdj cylinderYTransposer fullScanBuilder = do
  
  let  --setOriginYaxisToCenterOfHeel origin' = (transposeY (+ (-70)) origin')
       setOriginYaxisToCenterOfHeel origin' = (transposeY cylinderYTransposer origin')
       pillarCylinder = map (transposeZ (+ (cylinderZAdj))) $ cylinder cylinderRadius (setOriginYaxisToCenterOfHeel origin') [Angle a | a <- [0,10..360]] cylinderHeight -- (7::Height)
       
       

  heelOuterFaces
  --Create the entire tread cubes as scanned.
  --Extract the [FrontFace] which will be combined with the riser collar faces.
    <- buildCubePointsListSingle "heelOuterFaces"
         (
           --map (extractFrontFace) $ (execState $ runExceptT   (treadCpoints treadAHR origin'  ) ) []
           map (extractFrontFace) $ (execState $ runExceptT   (fullScanBuilder $ FullScanBuilderData treadAHR origin'  ) ) []
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
-- =============================================== flat section builders ============================================
--Create sections with flat top/bottom. Must be able to create all single/double sections


flatGeoxSectionBuilder :: FlatSectionBuilder
flatGeoxSectionBuilder flatSectionBuilderData = do
  btmFaces <- buildCubePointsListSingle "btmFaces"
              (map (extractBottomFace) $ sectionCpoints flatSectionBuilderData)

  topFaces <- buildCubePointsListSingle "topFaces"
              (map (toTopFace . (transposeZ $ sectionTransposer flatSectionBuilderData)) btmFaces)

  cubes    <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces

  return cubes

flatGolfSectionBuilder :: FlatSectionBuilder
flatGolfSectionBuilder flatSectionBuilderData = do
  topFaces <- buildCubePointsListSingle "topFaces"
              (map (extractTopFace) $ sectionCpoints flatSectionBuilderData)

  btmFaces <- buildCubePointsListSingle "btmFaces"
              (map (toBottomFace . (transposeZ $ sectionTransposer flatSectionBuilderData)) topFaces)

  cubes    <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces

  return cubes

runSingleFlatSectionBuilder :: SingleFlatSectionBuilderRunData -> IO ()
runSingleFlatSectionBuilder singleFlatSectionBuilderRunData = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' $ layerName' $ sectionRunDataFSBRD singleFlatSectionBuilderRunData  
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []

  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "layer id was found"
  
{-
Call sectionBuilder(current section<heel,center,toe> to build) with the Builder for the entire geox or golf tread as required.
Also pass in the layerId for the database that is the scanned layer for the target tread in order to dictate golf vs geox tread

=========given:
cylinderHeight: height of the cylinder which makes up the riser collar.

cylinderZAdj: Adjust the position of the riser collar up or down in relation to the tread cpoints.

fullScanBuilder: Builder.Monad fx that supplies the full scanned shape<geox/golf>.

sectionBuilder: Builder.Monad fx that will generate the current section.
-}


runSingleSectionStlGenerator :: SectionRunData -> IO ()
runSingleSectionStlGenerator sectionData = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' $ layerName' sectionData  
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "layer id was not found"
    (Just (Entity key layerVal)) -> do
      let
        builder = ((sectionBuilder sectionData)
                   (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity)
                   (extractOrigin layerVal)
                   (cylinderHeight sectionData)
                   (cylinderAdj sectionData)
                   (cylinderYTransposer sectionData)
                   (fullScanBuilder sectionData)
                  ) 
        
        valCpoints = ((evalState $ runExceptT builder) [])

        cpoints =  ((execState $ runExceptT builder) [])

      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "heel geox" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"

runDoubleSectionStlGenerator :: SectionRunData -> SectionRunData -> IO ()
runDoubleSectionStlGenerator section1Data section2Data  = runSqlite databaseName $ do
  case ((layerName' section1Data) == (layerName' section2Data)) of
    False -> liftIO $ putStrLn "layerName of sectionData's do not match"
    True  -> do
      layerId <- getBy $ nameUnique' (layerName'  section1Data)
      angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
      case layerId of
        Nothing -> liftIO $ putStrLn "layer id was not found"
        (Just (Entity key layerVal)) -> do
          let
            builder1 = ((sectionBuilder section1Data) (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
                       (cylinderHeight section1Data) (cylinderAdj section1Data) (cylinderYTransposer section1Data) (fullScanBuilder section1Data)  ) 
          
            valCpoints1 = ((evalState $ runExceptT builder1) [])
          
            cpoints1 =  ((execState $ runExceptT builder1) [])
          
          case valCpoints1 of
            (Left e) -> liftIO $ print $ e
            (Right a) -> do
              liftIO $ putStrLn "output from section 1 Builder was good"
              let
                builder2 = ((sectionBuilder section2Data) (extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
                       (cylinderHeight section2Data) (cylinderAdj section2Data) (cylinderYTransposer section2Data) (fullScanBuilder section2Data)  ) 
          
                valCpoints2 = ((evalState $ runExceptT builder2) [])
          
                cpoints2 =  ((execState $ runExceptT builder2) [])
              
              case valCpoints2 of
                (Left e) -> liftIO $ print $ e
                (Right a) -> do
                  liftIO $ putStrLn "output from section2 Builder was good"
                  liftIO $ writeStlToFile $ newStlShape "pillars" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] $ cpoints1 ++ cpoints2)
                  liftIO $ putStrLn "stl should have been output"


runGeoxHeelCenterStlGenerator :: IO ()
runGeoxHeelCenterStlGenerator = runDoubleSectionStlGenerator geoxHeelSectionData geoxCenterSectionData

runGeoxCenterToeStlGenerator :: IO ()
runGeoxCenterToeStlGenerator = runDoubleSectionStlGenerator  geoxCenterSectionData geoxToeSectionData

          
--Holds all data needed to run a Builder for a single section.
--This keeps it DRY when generating a single section, or 2 combined sections.
data SectionRunData =
  SectionRunData
  {cylinderHeight :: Height,
   cylinderAdj :: Height,
   cylinderYTransposer :: CylinderTransposer,
   fullScanBuilder :: FullScanBuilder,
   sectionBuilder :: SectionBuilder,
   layerName' :: LayerName
  }

data SingleFlatSectionData =
  SingleFlatSectionData
   {singleFlatTransposer :: CylinderTransposer,
    sectionData :: SectionRunData,
    flatSingleSectionBuilder :: FlatSectionBuilder
   }

data DoubleFlatSectionData =
  DoulbeFlatSectionData
   {doubleFlatTransposer :: CylinderTransposer,
    section1Data :: SectionRunData,
    section2Data :: SectionRunData,
    flatDoubleSectionBuilder :: FlatSectionBuilder
   }

data FullScanBuilderData =
  FullScanBuilderData
   {angleHeighRadiusFSBD :: [AngleHeightRadius],
    originFSBD :: Point
    }


type FullScanBuilder = (FullScanBuilderData -> ExceptT BuilderError (State CpointsStack) CpointsList)

type SectionBuilder = [AngleHeightRadius] -> Point -> Height -> Height -> CylinderTransposer
                      -> FullScanBuilder
                      -> ExceptT BuilderError (State CpointsStack) CpointsList
                    
type FlatSectionBuilder = FlatSectionBuilderData -> ExceptT BuilderError (State CpointsStack) CpointsList

data FlatSectionBuilderData =
  FlatSectionBuilderData
    {sectionCpoints ::  [CornerPoints],
     sectionTransposer :: SectionTransposer
    }

data SingleFlatSectionBuilderRunData =
  SingleFlatSectionBuilderRunData
   {sectionRunDataFSBRD :: SectionRunData,
    flatSectionBuilderDataFSBRD :: FlatSectionBuilderData
   }
geoxHeelSectionData :: SectionRunData
geoxHeelSectionData = SectionRunData 9 (-20) heelCylinderTransposer (entireGeoxTreadCpoints) (heelCPoints) "top"

geoxCenterSectionData :: SectionRunData
geoxCenterSectionData = SectionRunData 13 (-20) centerCylinderTransposer (entireGeoxTreadCpoints) (centerCPoints) "top"

geoxToeSectionData :: SectionRunData
geoxToeSectionData = SectionRunData 16 (-20) toeCylinderTransposer (entireGeoxTreadCpoints) (toeCPoints) "top"

golfHeelSectionData :: SectionRunData
golfHeelSectionData = SectionRunData 12 8 heelCylinderTransposer (entireGolfTreadCpoints) (heelCPoints) "bottom"

golfCenterSectionData :: SectionRunData
golfCenterSectionData = SectionRunData 10 10 centerCylinderTransposer (entireGolfTreadCpoints) (centerCPoints) "bottom"

golfToeSectionData :: SectionRunData
golfToeSectionData = SectionRunData 10 10 toeCylinderTransposer (entireGolfTreadCpoints) (toeCPoints) "bottom"  

-------------------------------run the builders. Generate the stl -----------------------------------------
geoxHeelStlGenerator :: IO ()
geoxHeelStlGenerator = runSingleSectionStlGenerator geoxHeelSectionData

geoxCenterStlGenerator :: IO ()
geoxCenterStlGenerator = runSingleSectionStlGenerator geoxCenterSectionData

geoxToeStlGenerator :: IO ()
geoxToeStlGenerator = runSingleSectionStlGenerator geoxToeSectionData

golfHeelStlGenerator :: IO ()
golfHeelStlGenerator = runSingleSectionStlGenerator golfHeelSectionData

golfCenterStlGenerator :: IO ()
golfCenterStlGenerator = runSingleSectionStlGenerator golfCenterSectionData

golfToeStlGenerator :: IO ()
golfToeStlGenerator = runSingleSectionStlGenerator golfToeSectionData


{---------------------------------------------entire geox tead-----------------------------------------------------------
Build the entire geox tread from the database just to have a look at it.
Also gets used for taking sections out of for building the various pieces: heel, toe, heel&midddle, toe&middle,
as well as the flat geox tread that has no variable height.
-}
entireGeoxTreadCpoints :: FullScanBuilderData -> ExceptT BuilderError (State CpointsStack) CpointsList
entireGeoxTreadCpoints fullScanBuilderData = do
  let setBottomOriginToZero origin' = (transposeZ (\z -> 0) origin')
      adjustHeightOfTopOrigin origin' = (transposeZ (+ (-7)) origin')
  
  btmFaces <- buildCubePointsListSingle "bottom faces"
              ( 
                createBottomFaces (setBottomOriginToZero $ originFSBD fullScanBuilderData)
                (extractRadii $ angleHeighRadiusFSBD fullScanBuilderData )
                (extractAngles $ angleHeighRadiusFSBD fullScanBuilderData) 
              )
  
  topFaces <- buildCubePointsListSingle "top faces"
              (
               createTopFacesVariableHeight
                 (adjustHeightOfTopOrigin $ originFSBD fullScanBuilderData)
                 (extractRadii $ angleHeighRadiusFSBD fullScanBuilderData)
                 (extractAngles $ angleHeighRadiusFSBD fullScanBuilderData)
                 (map (+ (-10)) $ extractHeights $ angleHeighRadiusFSBD fullScanBuilderData)
              )
  
  cubes <- buildCubePointsListWithAdd "cubes"
              btmFaces
              topFaces
  
  return cubes

entireGolfTreadCpoints :: FullScanBuilderData -> ExceptT BuilderError (State CpointsStack) CpointsList
entireGolfTreadCpoints fullScanBuilderData = do
  let setTopOriginTo origin' = (transposeZ (\z -> 20) $ origin')
      adjustHeightOfBtmOrigin origin' = (transposeZ (+ (7)) origin')
      
  btmFaces <- buildCubePointsListSingle "bottom faces"
              (createBottomFacesVariableHeight
                (adjustHeightOfBtmOrigin $ originFSBD fullScanBuilderData)
                (extractRadii $ angleHeighRadiusFSBD fullScanBuilderData)
                (extractAngles $ angleHeighRadiusFSBD fullScanBuilderData)
                (map (+ (-10)) $ extractHeights $ angleHeighRadiusFSBD fullScanBuilderData))

  topFaces <- buildCubePointsListSingle "top faces"
              ( 
                createTopFaces
                 (setTopOriginTo $ originFSBD fullScanBuilderData)
                 (extractRadii $ angleHeighRadiusFSBD fullScanBuilderData)
                 (extractAngles $ angleHeighRadiusFSBD fullScanBuilderData) 
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
entireTreadStlGeneratorBase :: (FullScanBuilderData -> ExceptT BuilderError (State CpointsStack) CpointsList) -> LayerName -> IO ()
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

