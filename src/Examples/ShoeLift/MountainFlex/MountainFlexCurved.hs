{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{- |
The mountain warehouse shoes that will use the roller blades ankle brace and some padding
attached to the shoe.

The lift will be in 1 piece as the flex section in MountainFlexBase did not seem to help.
The heel and toe will be well rounded, beyond what the original shoe tread is.


-}
module Examples.ShoeLift.MountainFlex.MountainFlexCurved() where

import Joiners.RadialLines(getMinY, getMaxY, extractYaxis, createYaxisGridFromTopFrontPoints, splitOnXaxis,
                           buildLeftRightLineFromGridAndLeadingTrailingCPointsBase, createYaxisGridFromMinMaxY)

import Database.Persist 
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import qualified Persistable.Base as PstB


import Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin,
                           loadAndExtractedAngleHeightRadiusFromDB, loadAndExtractedAngleHeightRadiusFromDbT,
                           anglesHeightsRadiiToExtractedAnglesHeightsRadii, filterAngleHeightRadiusOnAngle)

import Builder.Monad(BuilderError(..),
                     cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListSingle,
                     buildCubePointsListWithIOCpointsListBase, buildCubePointsListWithAdd,
                     CpointsStack, CpointsList, ExceptStackCornerPointsBuilder)
import qualified Builder.ExceptStateIO as BIO (BuilderError(..),
                      cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListWithAdd,
                      buildCubePointsListSingle, buildCubePointsListSingleNoPush,
                      CpointsStack, CpointsList, ExceptStateIOCornerPointsBuilder) 

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
import CornerPoints.Transpose(transposeX, transposeY, transposeZ, transposeZWithList, transposeZWithList')
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight, createBottomFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace, toTopRightLine,
                                    toF2, toF3, toB2, toB3, toBackTopLine, toBackBottomLine, toBottomFrontLine)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace, 
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace, extractBottomFrontLine,
                                   extractTopFace, extractB4, extractB3, extractB2, extractB1, extractF4, extractF3, extractF2, extractF1, extractBottomFace)


import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import TypeClasses.Transposable(transpose)

import Helpers.Applicative(extractE)



--which lift builder to evaluate in runLiftBuilder
currentLiftBuilder = centerLiftBuilder

--run the currentLiftBuilder, outputting the stl.
runLiftBuilder :: IO () 
runLiftBuilder = runSqlite "src/Examples/ShoeLift/MountainFlex/lineScanner.db" . PstB.asSqlBackendReader $ do
  pillarLayerId <- getBy $ nameUnique' "tread"
  geoxAngleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId pillarLayerId)] []

  
  case pillarLayerId of
    Nothing -> liftIO $ putStrLn "geox layer was not found"
    (Just (Entity key geoxLayerVal)) -> do
      liftIO $ putStrLn "geox scan layer was found"
      
      let builder = currentLiftBuilder
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
The bottom of the full length lift onto which the rubber tread glues.
Create a flat top on it for printing.
Give heel and toe lots of curve as this lift does not flex.
Create using the grid from the Joiners.RadialLines module.

-}
btmLiftBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
btmLiftBuilder ahr origin = do

  let
    --adjust radii for use with size 5 shoes. Scan was for size 4
    transposeFactor = 1.05
    --to select each side of the ahr::AnglesHeightsRadii, divided along the x_axis.
    pre180Angles ang  = (ang < 180.0)
    post180Angles ang = (ang > 181.0)
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> pre180Angles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> post180Angles ang) ahr

    extractedPreRadii = map (transpose (* transposeFactor)) $ extractRadii ahrPre180
    extractedPreAngles = extractAngles ahrPre180
    
    extractedPostRadii = map (transpose (* transposeFactor)) $ extractRadii ahrPost180
    extractedPostAngles = extractAngles ahrPost180
    
    
    --add a curve to the heel and toe areas.
    leadingTransposerList =  [40,37.5..0] ++ [0 | z <- [1..65]] ++ [1,3..21] ++ [20 | z <- [1..]]
    trailingTransposerList =  [40,37.5..0] ++ [0 | z <- [1..70]] ++ [1,3..21] ++ [20 | z <- [1..]]
    leadingExtractedHeights =
     [ extractedHeight + transposer
       | extractedHeight <- extractHeights ahrPre180
       | transposer  <- leadingTransposerList
     ]
    trailingExtractedHeights =
     [ extractedHeight + transposer
       | extractedHeight <- extractHeights ahrPost180
       | transposer  <- trailingTransposerList
     ]
    

  frontTopPointsNotYetSplitIntoLeadingTrailingForCalculatingGrid
    <- buildCubePointsListSingle "frontTopPointsNotYetSplitIntoLeadingTrailing"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (map (transpose (* transposeFactor)) $ extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )

  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsNotYetSplitIntoLeadingTrailingForCalculatingGrid

  leadingTopFrontPoints
    <- buildCubePointsListSingle "leadingTopFrontPoints"
     (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          extractedPreRadii
          extractedPreAngles
          leadingExtractedHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
     )

  trailingTopFrontPoints
    <- buildCubePointsListSingle "trailingTopFrontPoints"
      (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          extractedPostRadii
          extractedPostAngles
          trailingExtractedHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
         map (extractF2) frontTopLines
     )

  topGridFaces <- buildCubePointsListSingle "topGridFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )

  cubes <- buildCubePointsListWithAdd "cubes"
    (map toBottomFace topGridFaces)
    (let
        transposer z = 70
     in
     map
     ( (transposeZ transposer) ) topGridFaces
    )
          
  return cubes

{-
The top of the full length lift onto which the rubber tread glues.
Create a flat btm on it for printing.
Create using the grid from Joiners.RadialLines module.

-}
topLiftBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
topLiftBuilder ahr origin = do

  let
    --to select each side of the ahr::AnglesHeightsRadii, divided along the x_axis.
    pre180Angles ang  = (ang < 180.0)
    post180Angles ang = (ang > 181.0)
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> pre180Angles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> post180Angles ang) ahr

    --adjust radii for use with size 5 shoes. Scan was for size 4
    transposeFactor = 1.05
    --extractedPreRadii = extractRadii ahrPre180
    extractedPreRadii = map (transpose (* transposeFactor)) $ extractRadii ahrPre180
    extractedPreAngles = extractAngles ahrPre180
    extractedPreHeights = extractHeights ahrPre180
    
    extractedPostRadii = map (transpose (* transposeFactor)) $ extractRadii ahrPost180
    extractedPostAngles = extractAngles ahrPost180
    extractedPostHeights = extractHeights ahrPost180
    
    

  frontTopPointsNotYetSplitIntoLeadingTrailingForCalculatingGrid
    <- buildCubePointsListSingle "frontTopPointsNotYetSplitIntoLeadingTrailing"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (map (transpose (* transposeFactor)) $ extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )

  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsNotYetSplitIntoLeadingTrailingForCalculatingGrid

  leadingTopFrontPoints
    <- buildCubePointsListSingle "leadingTopFrontPoints"
     (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          extractedPreRadii
          extractedPreAngles
          extractedPreHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
     )

  trailingTopFrontPoints
    <- buildCubePointsListSingle "trailingTopFrontPoints"
      (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          extractedPostRadii
          extractedPostAngles
          extractedPostHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
         map (extractF2) frontTopLines
     )

  topGridFaces <- buildCubePointsListSingle "topGridFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )

  cubes <- buildCubePointsListWithAdd "cubes"
    (topGridFaces)
    (let
        transposer z = 0
     in
     map
     ( (transposeZ transposer) . toBottomFace ) topGridFaces
    )
          
  return cubes


{-
The center of the full length lift.
Create a flat btm and top on it to glue to the flat top/btm sections.
Create using the grid from Joiners.RadialLines module.

-}
centerLiftBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
centerLiftBuilder ahr origin = do

  let
    --to select each side of the ahr::AnglesHeightsRadii, divided along the x_axis.
    pre180Angles ang  = (ang < 180.0)
    post180Angles ang = (ang > 181.0)
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> pre180Angles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> post180Angles ang) ahr

    --adjust radii for use with size 5 shoes. Scan was for size 4
    transposeFactor = 1.05
    extractedPreRadii =  map (transpose (* transposeFactor)) $ extractRadii ahrPre180
    extractedPreAngles = extractAngles ahrPre180
    extractedPreHeights = extractHeights ahrPre180
    
    extractedPostRadii =  map (transpose (* transposeFactor)) $ extractRadii ahrPost180
    extractedPostAngles = extractAngles ahrPost180
    extractedPostHeights = extractHeights ahrPost180
    
    

  frontTopPointsNotYetSplitIntoLeadingTrailingForCalculatingGrid
    <- buildCubePointsListSingle "frontTopPointsNotYetSplitIntoLeadingTrailing"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          ( map (transpose (* transposeFactor)) $ extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )

  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsNotYetSplitIntoLeadingTrailingForCalculatingGrid

  leadingTopFrontPoints
    <- buildCubePointsListSingle "leadingTopFrontPoints"
     (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          extractedPreRadii
          extractedPreAngles
          extractedPreHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
     )

  trailingTopFrontPoints
    <- buildCubePointsListSingle "trailingTopFrontPoints"
      (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          extractedPostRadii
          extractedPostAngles
          extractedPostHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
         map (extractF2) frontTopLines
     )

  topGridFaces <- buildCubePointsListSingle "topGridFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )

  cubes <- buildCubePointsListWithAdd "cubes"
    (let
       transposer z = 40
     in
      map
      (  transposeZ transposer                 ) 
      topGridFaces
    )
    (let
        transposer z = 0
     in
     map
     ( (transposeZ transposer) . toBottomFace ) topGridFaces
    )
          
  return cubes


