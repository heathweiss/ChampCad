{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{- |
Build shoelift for Geox running shoes.
Inside shoes with a 6cm lift that flexes at the toe.
Will use a rubber bootie for the bottom tread so it will be a match to the top tread.

-}

module Examples.ShoeLift.GeoxFlex.GeoxFlexBuilder() where


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
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight, createBottomFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace, toTopRightLine,
                                    toF2, toF3, toB2, toB3, toBackTopLine, toBackBottomLine, toBottomFrontLine)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace, 
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace, extractBottomFrontLine,
                                   extractTopFace, extractB4, extractB3, extractB2, extractB1, extractF4, extractF3, extractF2, extractF1)

import Joiners.AdvanceComposable(Advancer(..), OuterAdvancerOutput(..), InnerAdvancerOutput(..),naiveAdvCpointFromInnerPerims, naiveAdvCPointFromOuterPerims, advancerRecur,
                                 advCPointFromClosestInnerOuterAdvCPoint, extractAdvCPointsFromAdvancer, advCPointFromClosestInnerOuterUsedCPointBase,
                                 createAdvCPointFromInnerPerimsCheckLegalIntersection, outerAdvancerOutPutHasLegalIntersections, checkInnerAdvCPtForLegality)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import TypeClasses.Transposable(transpose)

import Control.Lens

makeLenses ''AngleHeightRadius


currentBuilder = topTreadBuilder 
                 --testPrintTreadToCheckScanBuilder
                 --topTreadBuilder

showBuilderValue :: IO () 
showBuilderValue = runSqlite "src/Examples/ShoeLift/GeoxFlex/lineScanner.db" $ do
  layerId <- getBy $ nameUnique' "tread"
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

--make a riser to convert from pillars to german centers
runBuilder :: IO () 
runBuilder = runSqlite "src/Examples/ShoeLift/GeoxFlex/lineScanner.db" $ do
  pillarLayerId <- getBy $ nameUnique' "tread"
  geoxAngleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId pillarLayerId)] []

  
  case pillarLayerId of
    Nothing -> liftIO $ putStrLn "geox layer was not found"
    
    (Just (Entity key geoxLayerVal)) -> do
      liftIO $ putStrLn "geox scan layer was found"
      
      let builder = currentBuilder
                            (extractAnglesHeightsRadiiFromEntity geoxAngleHeightRadiusEntity)
                            (extractOrigin geoxLayerVal)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "geox flex tread" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"
          

{-
--top of this tread will fit the shoe. Bottom will be flat for printing.
--Keep it thin so as not to mess with the flex.
Build the cubes by building Top<Front/Back>Lines.
This means processing each side of the scan (pre/post 180 degrees) into b3:[f3] and b2:[f2] which can then be joined with +++>
-}
topTreadBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
topTreadBuilder ahr origin = do

  let
    pre180Angles ang  = (ang < 178.0)
    post180Angles ang = (ang > 179.0)
    
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> pre180Angles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> post180Angles ang) ahr
    
    extractedPreRadii = extractRadii ahrPre180
    extractedPreAngles = extractAngles ahrPre180
    extractedPreHeights = extractHeights ahrPre180

    extractedPostRadii = extractRadii ahrPost180
    extractedPostAngles = extractAngles ahrPost180
    extractedPostHeights = extractHeights ahrPost180
    

  -- --------------------------------------- original system left in place to compile
  preTopFaces
    <- buildCubePointsListSingle "topOuterFaces"
              (let
                  
                  --the scan is missing 0 and 360 degrees as it is used to make up the faces by adding left and right sides together.
                  --But for a test, will build it the normal way with: createTopFacesVariableHeight
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    extractedPreRadii
                    extractedPreAngles
                    extractedPreHeights
               in
               topFaces
              )

  preTopRightVertices <- buildCubePointsListSingle "preTopRightLines"
     ((toB3 . extractF3 $ head preTopFaces) : map (toF3 . extractF2) (preTopFaces))

     
  postTopFaces
    <- buildCubePointsListSingle "topOuterFaces"
              (let
                  
                  --the scan is missing 0 and 360 degrees as it is used to make up the faces by adding left and right sides together.
                  --But for a test, will build it the normal way with: createTopFacesVariableHeight
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    extractedPostRadii
                    extractedPostAngles
                    extractedPostHeights
               in
               topFaces
              )

  postTopLeftVertices <- buildCubePointsListSingle "preTopRightLines"
     ( let
         reversed = {-reverse-} postTopFaces
       in
       (toB2 . extractF3 $ head reversed) : map (extractF2) (reversed)
     )
  
  
  frontBackLines <- buildCubePointsListWithAdd "frontBackLines"
    (preTopRightVertices)
    (postTopLeftVertices)

  topFaces <- buildCubePointsListSingle "topFaces"
    ((head frontBackLines) +++> (tail frontBackLines))
  {-
  topInnerFaces
    <- buildCubePointsListSingle "topInnerFaces"
              (let
                  
                  --the scan is missing 0 and 360 degrees as it is used to make up the faces by adding left and right sides together.
                  --But for a test, will build it the normal way with: createTopFacesVariableHeight
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (map (transpose (+ (-5))) extractedRadii)
                    extractedAngles
                    extractedHeights
               in
               topFaces
              )

  topFaces
    <- buildCubePointsListWithAdd "topFaces"
       (map (extractFrontTopLine) topOuterFaces)
       (map (toBackTopLine . extractFrontTopLine) topInnerFaces)-}

  cubes <- buildCubePointsListWithAdd "cubes"
           (topFaces)
           (map (toBottomFace . (transposeZ (\z -> 17)) ) topFaces )

  return postTopFaces



--create with:createTopFacesVariableHeight
--did not print well as just used outer faces.
testPrintTreadToCheckScanBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
testPrintTreadToCheckScanBuilder ahr origin = do

  let
    --pre180Angles ang  = (ang < 180)
    --post180Angles ang = (ang > 180)
    
    --ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> pre180Angles ang) ahr
    --ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> post180Angles ang) ahr
    --joinedAHR = ahrPre180 ++ ahrPost180
    ahrWithWrappedEnd = ahr ++ [head ahr]

    extractedRadii = extractRadii ahrWithWrappedEnd --joinedAHR
    extractedAngles = extractAngles ahrWithWrappedEnd --joinedAHR
    extractedHeights = extractHeights ahrWithWrappedEnd --joinedAHR
    

  -- --------------------------------------- original system left in place to compile
  topOuterFaces
    <- buildCubePointsListSingle "topOuterFaces"
              (let
                  
                  --the scan is missing 0 and 360 degrees as it is used to make up the faces by adding left and right sides together.
                  --But for a test, will build it the normal way with: createTopFacesVariableHeight
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    extractedRadii
                    extractedAngles
                    extractedHeights
               in
               topFaces
              )

  topInnerFaces
    <- buildCubePointsListSingle "topInnerFaces"
              (let
                  
                  --the scan is missing 0 and 360 degrees as it is used to make up the faces by adding left and right sides together.
                  --But for a test, will build it the normal way with: createTopFacesVariableHeight
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (map (transpose (+ (-5))) extractedRadii)
                    extractedAngles
                    extractedHeights
               in
               topFaces
              )

  topFaces
    <- buildCubePointsListWithAdd "topFaces"
       (map (extractFrontTopLine) topOuterFaces)
       (map (toBackTopLine . extractFrontTopLine) topInnerFaces)

  cubes <- buildCubePointsListWithAdd "cubes"
           (topFaces)
           (map (toBottomFace . (transposeZ (\z -> 17)) ) topFaces )

  return cubes

{-original
  topFaces <- buildCubePointsListSingle "topFaces"
              (let
                  ahrWithWrappedEnd = ahr ++ [head ahr]
                  --the scan is missing 0 and 360 degrees as it is used to make up the faces by adding left and right sides together.
                  --But for a test, will build it the normal way with: createTopFacesVariableHeight
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii ahrWithWrappedEnd)
                    (extractAngles ahrWithWrappedEnd)
                    (extractHeights ahrWithWrappedEnd)
               in
               topFaces
              )

-}
