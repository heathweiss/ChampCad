{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Build shoelift for German hiking boots

Will have a central pillar with flexible carbon fiber toe and heel.
-}

module Examples.ShoeLift.GermanHikers.GermanHikerBase() where

import Primitives.Cylindrical.Walled(cylinder)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)

import Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin, loadAndExtractedAngleHeightRadiusFromDB)

import Builder.Monad(BuilderError(..),
                     cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListSingle,
                     buildCubePointsListWithIOCpointsListBase, buildCubePointsListWithAdd,
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

import CornerPoints.Points(Point(..))
import CornerPoints.Transpose(transposeZ)
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight, createBottomFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace, toTopRightLine)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace,
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace,
                                   extractTopFace, extractB4, extractB3, extractB2, extractB1, extractF4, extractF3, extractF2, extractF1)

import Joiners.AdvanceComposable(Advancer(..), OuterAdvancerOutput(..), InnerAdvancerOutput(..),naiveAdvCpointFromInnerPerims, naiveAdvCPointFromOuterPerims, advancerRecur,
                                 advCPointFromClosestInnerOuterAdvCPoint, extractAdvCPointsFromAdvancer, advCPointFromClosestInnerOuterUsedCPoint,
                                 createAdvCPointFromInnerPerimsCheckLegalIntersection, outerAdvancerOutPutHasLegalIntersections, checkInnerAdvCPtForLegality)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Test.HUnit

import Control.Lens

makeLenses ''Advancer

--FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
--               extractFrontLeftLine, extractFrontRightLine, extractBackBottomLine, extractBackFace)
       
treadScanLayer = "bottom"
databaseName = "src/Examples/ShoeLift/GermanHikers/lineScanner.db"

--choose which builder to run to keep run/show builder dry
currentBuilder = secondAdvCpt

runTreadScanBuilder :: IO () 
runTreadScanBuilder = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' treadScanLayer
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "tread scan layer was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = currentBuilder ( extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "boot tread meets full lenght contoured riser" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"


showTreadScanBuilderValue :: IO () 
showTreadScanBuilderValue = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' treadScanLayer
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "tread scan layer was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = currentBuilder ( extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ print $ show a



topTreadBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
topTreadBuilder ahr origin = do
  topFronts <- buildCubePointsListSingle "topFronts"
             (let topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii ahr)
                    (extractAngles ahr)
                    (extractHeights ahr)
                    --level off height till figure out problem
                    --[10 | x <- [1..]]
              in
              --(extractF3 $ head topFaces) : (map (extractF2) topFaces)
              --(extractF3 $ head topFaces) : (map (extractF2) $ take 1 topFaces)
              [(extractF3 $ head topFaces)]
             )
  --let cylinder' = cylinder [Radius 20 | r <- [1..]] [Radius 20 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
                      
  --hole in center for alignment insert.
  alignmentPillar <- buildCubePointsListSingle "alignmentPillar"
                     (
                      --(extractB3 $ head cylinder') : (map (extractB2) cylinder')
                      (extractB3 $ head cylinder') : (map (extractB2) $ take 1 cylinder')
                      --[(extractB3 $ head cylinder')] -- : (map (extractB2) $ take 1 cylinder')
                     )

  
  topLeftRightLines <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String Advancer
     recurProcessor advancer = do
       advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner advCPointFromOuter  advancer
       advancerRecur recurProcessor newAdvancer
    in
    extractAdvCPointsFromAdvancer $ 
     recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing []
   )

  topFaces <- buildCubePointsListSingle "topFaces"
              ((head topLeftRightLines) +++> (tail topLeftRightLines))

  cubes <- buildCubePointsListWithAdd "cubes"
           (topFaces)
           (map (toBottomFace . (transposeZ (+ (-10)) )) topFaces) 
                 
  return topFronts

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--For initial advCPoint, naiveAdvCpointFromInnerPerims should not produce an advCPt, so return a [CornerPointsNothing]
innerPerimemetersOutputOnInitAdvCPt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
innerPerimemetersOutputOnInitAdvCPt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  headOfInnerOuterPerimsFromWhichInitAdvCPointWillBeBuilt <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String InnerAdvancerOutput
     recurProcessor advancer = do
       advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       return advCPointFromInner
    in
     case  
      recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing [] of
       Right (InnerAdvancerOutput (Just (i:innerP)) Nothing usedCPt advCPts) -> [CornerPointsNothing] --[head alignmentPillar, (head topFronts)]
       Left e -> [CornerPointsError e]
            
   )
  return headOfInnerOuterPerimsFromWhichInitAdvCPointWillBeBuilt


--For initial advCPoint, using naive builder for inner/outer output
outerAdvancerOutputOnInitAdvCPt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
outerAdvancerOutputOnInitAdvCPt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  outerPerimeterOutput <- buildCubePointsListSingle "outerPerimeterOutput"
   (let
     recurProcessor :: Advancer -> Either String OuterAdvancerOutput
     recurProcessor advancer = do
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       return advCPointFromOuter
    in
     case  
      recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing [] of
       Right (OuterAdvancerOutput _ Nothing _ _) -> [CornerPointsNothing] 
       Left e -> [CornerPointsError e]
            
   )
  return outerPerimeterOutput

--For initial advCPoint
initAdvCPt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
initAdvCPt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  initAdvCPt <- buildCubePointsListSingle "outerPerimeterOutput"
    (let
      recurProcessor :: Advancer -> Either String Advancer
      recurProcessor advancer = do
       advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       --advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
         --no need for this yet, as it will not create an adv cpoint.
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner advCPointFromOuter  advancer
       return newAdvancer
     in
      case  
       recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing [] of
        Right (Advancer _ _ _ (Just advCPt) _) -> [advCPt] 
        Left e -> [CornerPointsError e]
            
   )
  return initAdvCPt


--The next advCPoint after the initial advCPoint which is built using innerPerims:
{-
output with inner cx legal. Did not build from inner as it should have. NFGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
 -Need to test why it was illegal, because it should not have been. NFGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
[TopLeftLine {b2 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 10.0}, f2 = Point {x_axis = 4.607827919499482, y_axis = -113.73482300016958, z_axis = 18.75}}]
output with naive inner. This is the output that legal should have had.
[TopLeftLine {b2 = Point {x_axis = 1.7431148549531632, y_axis = -19.92389396183491, z_axis = 10.0}, f2 = Point {x_axis = 0.0, y_axis = -113.828125, z_axis = 18.75}}]


-}
secondAdvCpt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
secondAdvCpt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  secondAdvCPt <- buildCubePointsListSingle "outerPerimeterOutput"
    (let
      recurProcessor :: Advancer -> Either String Advancer
      recurProcessor advancer = do
       noAdvCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       noAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       initAdvCPtAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint noAdvCPointFromInner noAdvCPointFromOuter  advancer
       --secondAdvCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection initAdvCPtAdvancer
         --yeilds nothing, so prob's with intersection
       secondAdvCPointFromInner <- naiveAdvCpointFromInnerPerims initAdvCPtAdvancer 
       legalizedSecondAdvCPointFromInner <- checkInnerAdvCPtForLegality secondAdvCPointFromInner (advancer^.innerPerimetersBeforeExtraction)
       secondAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims initAdvCPtAdvancer
       --advancerWithSecondAdvCPt <- advCPointFromClosestInnerOuterUsedCPoint secondAdvCPointFromInner secondAdvCPointFromOuter initAdvCPtAdvancer
       advancerWithSecondAdvCPt <- advCPointFromClosestInnerOuterUsedCPoint legalizedSecondAdvCPointFromInner secondAdvCPointFromOuter initAdvCPtAdvancer
       return advancerWithSecondAdvCPt
     in
      case  
       recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [take 34 $ map extractBackTopLine cylinder']) --2nd advCPt from innerP's fails past 33
            (Just topFronts) Nothing [] of
        Right (Advancer _ _ _ (Just advCPt) _) -> [advCPt] 
        Left e -> [CornerPointsError e]
            
   )
  return secondAdvCPt


----------------------------------------------------- support test values-----------------------------------------------------------

cylinder' = cylinder [Radius 20 | r <- [1..]] [Radius 20 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
alignmentPillar =
  (extractB3 $ head cylinder') : (map (extractB2) cylinder')


--these would normally be built in a Builder.
topFronts' :: AnglesHeightsRadii -> Point -> [CornerPoints]
topFronts' ahr origin =
   let topFaces =
           createTopFacesVariableHeight
           origin
           (extractRadii ahr)
           (extractAngles ahr)
           (extractHeights ahr)
   in
   (extractF3 $ head topFaces) : (map (extractF2) topFaces)
              
  
             
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------testing -------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
