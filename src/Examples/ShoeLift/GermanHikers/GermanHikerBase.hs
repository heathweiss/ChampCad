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
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace,
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace,
                                   extractTopFace, extractB4, extractB3, extractB2, extractB1, extractF4, extractF3, extractF2, extractF1)

import Joiners.AdvanceComposable(Advancer(..), OuterAdvancerOutput(..), naiveAdvCpointFromInnerPerims, naiveAdvCPointFromOuterPerims, advancerRecur,
                                 advCPointFromClosestInnerOuterAdvCPoint, extractAdvCPointsFromAdvancer, advCPointFromClosestInnerOuterUsedCPoint,
                                 createAdvCPointFromInnerPerimsCheckLegalIntersection, outerAdvancerOutPutHasLegalIntersections)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

--FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
--               extractFrontLeftLine, extractFrontRightLine, extractBackBottomLine, extractBackFace)
       
treadScanLayer = "bottom"
databaseName = "src/Examples/ShoeLift/GermanHikers/lineScanner.db" 

runTreadScanBuilder :: IO () 
runTreadScanBuilder = runSqlite databaseName $ do
  layerId <- getBy $ nameUnique' treadScanLayer
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "tread scan layer was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = topTreadBuilder ( extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
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
      let builder = topTreadBuilder ( extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
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
              (extractF3 $ head topFaces) : (map (extractF2) $ take 20 topFaces)
             )
  let cylinder' = cylinder [Radius 20 | r <- [1..]] [Radius 20 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
                      
  --hole in center for alignment insert.
  alignmentPillar <- buildCubePointsListSingle "alignmentPillar"
                     (
                      --(extractB3 $ head cylinder') : (map (extractB2) cylinder')
                      (extractB3 $ head cylinder') : (map (extractB2) $ take 40 cylinder')
                      -- [(extractB3 $ head cylinder')] -- : (map (extractB2) $ take 1 cylinder')
                     )

  {-
  topLeftRightLines <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String OuterAdvancerOutput
     recurProcessor advancer = do
       advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
       --advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner advCPointFromOuter advancer

       advCPointFromInner2 <- createAdvCPointFromInnerPerimsCheckLegalIntersection newAdvancer
       advCPointFromOuter2 <- naiveAdvCPointFromOuterPerims newAdvancer
       --legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter2 (_innerPerimetersBeforeExtraction advancer)
       newAdvancer2       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner2 advCPointFromOuter2 newAdvancer

       advCPointFromInner3 <- createAdvCPointFromInnerPerimsCheckLegalIntersection newAdvancer2
       advCPointFromOuter3 <- naiveAdvCPointFromOuterPerims newAdvancer2
       legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter3 (_innerPerimetersBeforeExtraction advancer)
       return advCPointFromOuter3 --legalizedAdvCPointFromOuter
       --advancerRecur recurProcessor newAdvancer
    in
     case  
      recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing [] of
       Right (OuterAdvancerOutput _ (Just advCPoint) _ _) -> [advCPoint]
       Right (OuterAdvancerOutput _ Nothing _ _) -> [CornerPointsNothing]
       Left e -> [CornerPointsError e]
            
   )
  -}
   
  
  topLeftRightLines <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String Advancer
     recurProcessor advancer = do
       --advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
       advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       --legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter (_innerPerimetersBeforeExtraction advancer)
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner advCPointFromOuter {-legalizedAdvCPointFromOuter-} advancer
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
                  
  return topLeftRightLines

-------------------------------------------- does not work------------------------------------------------
--has pattern matches to work through just as topTreadBuilder did. Stick with using topTreadBuilder for now.
btmTreadBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
btmTreadBuilder ahr origin = do
  btmFronts <- buildCubePointsListSingle "btmFronts"
             (let btmFaces =
                    createBottomFacesVariableHeight
                    origin
                    (extractRadii ahr)
                    (extractAngles ahr)
                    (extractHeights ahr)
                    --level off height till figure out problem
                    --[10 | x <- [1..]]
              in
              (extractB4 $ head btmFaces) : (map (extractB1) btmFaces)
             )
  let cylinder' = cylinder [Radius 20 | r <- [1..]] [Radius 30 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
                      
  --hole in center for alignment insert.
  alignmentPillar <- buildCubePointsListSingle "alignmentPillar"
                     (
                      (extractB4 $ head cylinder') : (map (extractB1) cylinder')
                     )

  
  btmLeftRightLines <- buildCubePointsListSingle "btmLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String Advancer
     recurProcessor advancer = do
       advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
       --advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner advCPointFromOuter advancer
       advancerRecur recurProcessor newAdvancer
    in
    extractAdvCPointsFromAdvancer $ 
     recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just btmFronts) Nothing []
   )
  
  btmFaces <- buildCubePointsListSingle "btmFaces"
              ((head btmLeftRightLines) +++> (tail btmLeftRightLines))

  cubes <- buildCubePointsListWithAdd "cubes"
           (btmFaces)
           (map (toTopFace . (transposeZ (+ 10) )) btmFaces)
                  
  return btmFaces

