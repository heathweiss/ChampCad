{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{- |
The mountain warehouse shoes that have the ankle brace that I scanned.

Has a flexible toe by splitting the toe and gluing separately.
Was going to have a semiflex section but did not work out. That is why there are 3 sections instead of 2.
-}

module Examples.ShoeLift.MountainFlex.MountainFlexBase() where

import Joiners.RadialLines(getMinY, getMaxY, extractYaxis, createYaxisGridFromTopFrontPoints, splitOnXaxis,
                           buildLeftRightLineFromGridAndLeadingTrailingCPointsBase, createYaxisGridFromMinMaxY)

import Database.Persist 
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)


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

--import qualified Control.Lens as L
import Control.Lens 

makeLenses ''AngleHeightRadius
makeLenses ''ExtractedAngleHeightRadius

currentBuilder = entireTopTreadBuilderUsingRadialLinesGridForTestFit
                 

--The range of angles that define the heel/flex/toe sections. Reused > 1 fx.
--leading
heelLeadingAngles ang  = (ang < 130.0)
flexLeadingAngles ang  = (ang > 127.0) && (ang < 155.0)
toeLeadingAngles ang  = (ang > 152.0) && (ang < 180.0)
--trailing
heelTrailingAngles ang = (ang > 230.0)
flexTrailingAngles ang = (ang > 210.0) && (ang < 233.0)
toeTrailingAngles ang = (ang > 181.0) && (ang < 213.0)

showBuilderValue :: IO () 
showBuilderValue = runSqlite "src/Examples/ShoeLift/MountainFlex/lineScanner.db" $ do
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
          liftIO $ writeFile "src/Data/temp.txt" $ show a

--make a riser to convert from pillars to german centers
runBuilder :: IO () 
runBuilder = runSqlite "src/Examples/ShoeLift/MountainFlex/lineScanner.db" $ do
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
The full length tread that will glue onto the shoe.
Create a flat bottom on it for printing.
Divide the scan in half along the y_axis.
Create a grid using createYaxisGridFromTopFrontPoints from the Joiners.RadialLines module.
Recombine the 2 sides with zip. Should be the same length as they were made with createYaxisGridFromTopFrontPoints.

-}
entireTopTreadBuilderUsingRadialLinesGridForTestFit :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
entireTopTreadBuilderUsingRadialLinesGridForTestFit ahr origin = do

  let
    pre180Angles ang  = (ang < 180.0)
    post180Angles ang = (ang > 181.0)
    
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> pre180Angles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> post180Angles ang) ahr
    
    extractedPreRadii = extractRadii ahrPre180
    extractedPreAngles = extractAngles ahrPre180
    extractedPreHeights = extractHeights ahrPre180

    extractedPostRadii = extractRadii ahrPost180
    extractedPostAngles = extractAngles ahrPost180
    extractedPostHeights = extractHeights ahrPost180

  frontTopPointsNotYetSplitIntoLeadingTrailing
    <- buildCubePointsListSingle "frontTopPointsNotYetSplitIntoLeadingTrailing"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )

  
  
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
              
  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsNotYetSplitIntoLeadingTrailing
    
  topGridFaces <- buildCubePointsListSingle "topGridFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )
  
  cubes <- buildCubePointsListWithAdd "cubes"
    (topGridFaces)
    (let
        transposer z = 15
     in
     map
     ( (transposeZ transposer) . toBottomFace) topGridFaces
    )
  
  return trailingTopFrontPoints



{-
Able to print any of <heel/flex/toe>.
Prints top section which glues onto the shoe.
-}
topHeelFlexToeBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
topHeelFlexToeBuilder ahr origin = do

  let
       
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> flexLeadingAngles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> flexTrailingAngles ang) ahr
    
    extractedPreRadii = extractRadii ahrPre180
    extractedPreAngles = extractAngles ahrPre180
    extractedPreHeights = extractHeights ahrPre180

    extractedPostRadii = extractRadii ahrPost180
    extractedPostAngles = extractAngles ahrPost180
    extractedPostHeights = extractHeights ahrPost180

  frontTopPointsNotYetSplitIntoLeadingTrailing
    <- buildCubePointsListSingle "frontTopPointsNotYetSplitIntoLeadingTrailing"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )

  
  
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
              
  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsNotYetSplitIntoLeadingTrailing
    
  topGridFaces <- buildCubePointsListSingle "topGridFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )
  
  cubes <- buildCubePointsListWithAdd "cubes"
    (topGridFaces)
    (let
        transposer z = 15
     in
     map
     ( (transposeZ transposer) . toBottomFace) topGridFaces
    )
  
  return trailingTopFrontPoints

{-
The heel-toe center tread sections that will have flat top and bottom. Best to be flat so that if such large prints fail, can still utilized the section that did print.
All are made from this fx by adjusting the angles.
Divide the scan in half along the y_axis, create a grid using createYaxisGridFromTopFrontPoints from the Joiners.RadialLines module.
Recombine the 2 sides with zip. Should be the same length as they were made with createYaxisGridFromTopFrontPoints.
Will need to print each section separate as the flex section is done with Cheetah, while the heel-toe will be rigid.
-}
heelFlexToeFlatTopAndBtmTreadBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
heelFlexToeFlatTopAndBtmTreadBuilder ahr origin = do

  let
    --Use a series of angles that will define the <heel/toe> sections that are used to select/create the [AngleHeightRadius].
    leadingAHR  = filter (\(AngleHeightRadius ang _ _ _) -> flexLeadingAngles ang) ahr
    trailingAHR = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> flexTrailingAngles ang) ahr
    
    leadingRadii = extractRadii leadingAHR
    leadingAngles = extractAngles leadingAHR
    leadingHeights = extractHeights leadingAHR

    trailingRadii = extractRadii trailingAHR
    trailingAngles = extractAngles trailingAHR
    trailingHeights = extractHeights trailingAHR

  frontTopPointsForCreatingGrid
    <- buildCubePointsListSingle "frontTopPointsForCreatingGrid"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )
  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsForCreatingGrid
  
  leadingTopFrontPointsUseToBuildTopGridFaces
    <- buildCubePointsListSingle "leadingTopFrontPointsUseToBuildTopGridFaces"
     (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          leadingRadii
          leadingAngles
          leadingHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
     )

  
  trailingTopFrontPointsUsedToBuildTopGridFaces
    <- buildCubePointsListSingle "trailingTopFrontPointsUsedToBuildTopGridFaces"
      (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          trailingRadii
          trailingAngles
          trailingHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
         map (extractF2) frontTopLines
     )
              
  
  --Used to build topGridFacesWithFlatTop
  topGridFacesStillWithVariableHeights <- buildCubePointsListSingle "topGridFacesStillWithVariableHeights"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPointsUseToBuildTopGridFaces trailingTopFrontPointsUsedToBuildTopGridFaces of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )

  --the top of the heel/toe
  topGridFacesWithFlatTop <- buildCubePointsListSingle "topGridFacesWithFlatTop"
    (map (transposeZ (\z -> 60)) topGridFacesStillWithVariableHeights)

    
  
  cubes <- buildCubePointsListWithAdd "cubes"
    (topGridFacesWithFlatTop)
    (map
     ( (transposeZ (\z -> 0)) . toBottomFace) topGridFacesWithFlatTop
    )
  
  return trailingTopFrontPointsUsedToBuildTopGridFaces



{-
Build the heel/toe bottoms.
Give them a flat top for printing, and for attaching to center riser sections.
-}
heelFlexToeBtmBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
heelFlexToeBtmBuilder ahr origin = do

  let
    --Filter the AHR for leading/trailing sections sides of target heel/flex/toe section
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> flexLeadingAngles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> flexTrailingAngles ang) ahr

    
    leadingRadii = extractRadii ahrPre180
    leadingAngles = extractAngles ahrPre180
    leadingHeights = extractHeights ahrPre180

    trailingRadii = extractRadii ahrPost180
    trailingAngles = extractAngles ahrPost180
    trailingHeights = extractHeights ahrPost180

  frontTopPointsUsedToBuildGrid
    <- buildCubePointsListSingle "frontTopPointsUsedToBuildGrid"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )
  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsUsedToBuildGrid
  
  
  leadingTopFrontPoints
    <- buildCubePointsListSingle "leadingTopFrontPoints"
     (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          leadingRadii
          leadingAngles
          leadingHeights
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
          trailingRadii
          trailingAngles
          trailingHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
         map (extractF2) frontTopLines
     )
              
  
    
  btmFaces <- buildCubePointsListSingle "btmFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> map (toBottomFace) cpoints
       Left e          -> [CornerPointsError e]
    )
  
  cubes <- buildCubePointsListWithAdd "cubes"
    (map
     ( (transposeZ (\z -> 50)) . toTopFace) btmFaces
    )
    (btmFaces)
  
  return trailingTopFrontPoints

  

{-
The btm flex tread that will glue into the rubber sole. Has to be printed at full height due to the teeth.
Create a flat top on it for printing, will mate with the top printed sections/

Get the 2 AHR sides sides separately and then join them using the Joiners.RadialLines module.

Extend the grid sections out so that a toothed section will occurr in the toe section where flex is required.

-}
btmFlexBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
btmFlexBuilder ahr origin = do

  let
    leadingAHR  = filter (\(AngleHeightRadius ang _ _ _) -> flexLeadingAngles ang) ahr
    trailingAHR = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> flexTrailingAngles ang) ahr
    
    leadingRadii = extractRadii leadingAHR
    leadingAngles = extractAngles leadingAHR
    leadingHeights = extractHeights leadingAHR

    trailingRadii = extractRadii trailingAHR
    trailingAngles = extractAngles trailingAHR
    trailingHeights = extractHeights trailingAHR

  -------------------------------------------------------------------------------------
  ----------------------Build grid, grid TopFaces and grid BtmFaces--------------------
  -------------------------------------------------------------------------------------
  --All premliminary work to get the grid BtmFaces, form which everything else is built.
  --Note: TopFaces get built 1st as will have missing pattern matches to directly build BottomFaces from grid.
  
  frontTopPointsForBuildingGrid
    <- buildCubePointsListSingle "frontTopPointsForBuildingGrid"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )
  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsForBuildingGrid
  
  
  leadingTopFrontPoints
    <- buildCubePointsListSingle "leadingTopFrontPoints"
     (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          leadingRadii
          leadingAngles
          leadingHeights
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
          trailingRadii
          trailingAngles
          trailingHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
         map (extractF2) frontTopLines
     )
              
  topVariableHeightFacesForBuildingBtmFaces <- buildCubePointsListSingle "topVariableHeightFacesForBuildingBtmFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )
  
  btmVariableHeightFaces <- buildCubePointsListSingle "btmVariableHeightFaces"
    (map toBottomFace topVariableHeightFacesForBuildingBtmFaces)
  
  -------------------------------------------------------------------------------------
  ------------------------------Build the bottom layer---------------------------------
  -------------------------------------------------------------------------------------
  --Will have variable height BottomFace's, a flat top, and teeth that are not connected.
  let
    toothDepth = 50.5
      -- 110 :the original height that should have printed if printer had not died.
    toothDepthFx _ = toothDepth
  
  bottomToothedLayerWithVarBtmAndFlatTop <- buildCubePointsListWithAdd "bottomToothedLayerWithVarBtmAndFlatTop"
    (map
       (toTopFace) $ transposeZWithList'
                       (concat [[(toothDepthFx), (toothDepthFx), (toothDepthFx), (toothDepthFx), (toothDepthFx), (toothDepthFx), (toothDepthFx), (\z -> z + 0), (\z -> z + 0)] | _ <- [1..]])
                       btmVariableHeightFaces
    )
    (btmVariableHeightFaces)
  {-
  bottomToothedLayerWithVarBtmAndFlatTop <- buildCubePointsListWithAdd "bottomToothedLayerWithVarBtmAndFlatTop"
    (map
       (toTopFace) $ transposeZWithList
                       (concat [[toothDepth,toothDepth,toothDepth,toothDepth, 0] | _ <- [1..200]])
                       btmVariableHeightFaces
    )
    (btmVariableHeightFaces)

-}

  -------------------------------------------------------------------------------------
  ------------------------------Build the top layer------------------------------------
  -------------------------------------------------------------------------------------
  --Build this layer separte from the bottom layer, but so this new flat bottom will match
  --the top of the bottom layer, which should cancel out the faces where they meet.

  bottomOfTopLayer <- buildCubePointsListSingle "bottomOfTopLayer"
    (map
       (transposeZ (\z -> toothDepth))
       btmVariableHeightFaces
    )

  topCubes <- buildCubePointsListWithAdd "topCubes"
    (map
       --(toTopFace . (transposeZ (\_ -> toothDepth + 2)))
         --the original athat should have printed if printer had not died.
      (toTopFace . (transposeZ (\_ -> toothDepth + 0.5)))
       bottomOfTopLayer
    )
    (bottomOfTopLayer)

  {-

-}
  
  return bottomToothedLayerWithVarBtmAndFlatTop



{-------------------------------------- NFG----------------------------------------------------------------
NFG:
Way too heavy.
Need to make the teeth thicker so they can have honeycombe infill instead of filling by having 4 perims.
The top flat section should have only been 2 mm thick instead of 10. Had flex but was too stiff at 20% infill.

The btm flex tread that will glue into the rubber sole. Has to be printed at full height due to the teeth.
Create a flat top on it for printing, will mate with the top printed sections/

Get the 2 AHR sides sides separately and then join them using the Joiners.RadialLines module.

Extend the grid sections out so that a toothed section will occurr in the toe section where flex is required.

-}

btmFlexBuilderNFG :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
btmFlexBuilderNFG ahr origin = do

  let
    leadingAHR  = filter (\(AngleHeightRadius ang _ _ _) -> flexLeadingAngles ang) ahr
    trailingAHR = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> flexTrailingAngles ang) ahr
    
    leadingRadii = extractRadii leadingAHR
    leadingAngles = extractAngles leadingAHR
    leadingHeights = extractHeights leadingAHR

    trailingRadii = extractRadii trailingAHR
    trailingAngles = extractAngles trailingAHR
    trailingHeights = extractHeights trailingAHR

  -------------------------------------------------------------------------------------
  ----------------------Build grid, grid TopFaces and grid BtmFaces--------------------
  -------------------------------------------------------------------------------------
  --All premliminary work to get the grid BtmFaces, form which everything else is built.
  --Note: TopFaces get built 1st as will have missing pattern matches to directly build BottomFaces from grid.
  
  frontTopPointsForBuildingGrid
    <- buildCubePointsListSingle "frontTopPointsForBuildingGrid"
    (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          (extractRadii ahr)
          (extractAngles ahr)
          (extractHeights ahr)
        frontTopLines = map (extractFrontTopLine) topFaces
      in
        (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines
    )
  let
    grid = createYaxisGridFromTopFrontPoints frontTopPointsForBuildingGrid
  
  
  leadingTopFrontPoints
    <- buildCubePointsListSingle "leadingTopFrontPoints"
     (
      let
        topFaces =
          createTopFacesVariableHeight
          origin
          leadingRadii
          leadingAngles
          leadingHeights
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
          trailingRadii
          trailingAngles
          trailingHeights
        frontTopLines = map (extractFrontTopLine) topFaces
      in
         map (extractF2) frontTopLines
     )
              
  topVariableHeightFacesForBuildingBtmFaces <- buildCubePointsListSingle "topVariableHeightFacesForBuildingBtmFaces"
    (case buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopFrontPoints trailingTopFrontPoints of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
    )
  
  btmVariableHeightFaces <- buildCubePointsListSingle "btmVariableHeightFaces"
    (map toBottomFace topVariableHeightFacesForBuildingBtmFaces)
  
  -------------------------------------------------------------------------------------
  ------------------------------Build the bottom layer---------------------------------
  -------------------------------------------------------------------------------------
  --Will have variable height BottomFace's, a flat top, and teeth that are not connected.
  let
    toothDepth = 101
    toothDepthFx _ = toothDepth
  
  bottomToothedLayerWithVarBtmAndFlatTop <- buildCubePointsListWithAdd "bottomToothedLayerWithVarBtmAndFlatTop"
    (map
       (toTopFace) $ transposeZWithList'
                       (concat [[(toothDepthFx), (toothDepthFx), (toothDepthFx), (toothDepthFx), (\z -> z + 0)] | _ <- [1..]])
                       btmVariableHeightFaces
    )
    (btmVariableHeightFaces)
  {-
  bottomToothedLayerWithVarBtmAndFlatTop <- buildCubePointsListWithAdd "bottomToothedLayerWithVarBtmAndFlatTop"
    (map
       (toTopFace) $ transposeZWithList
                       (concat [[toothDepth,toothDepth,toothDepth,toothDepth, 0] | _ <- [1..200]])
                       btmVariableHeightFaces
    )
    (btmVariableHeightFaces)

-}

  -------------------------------------------------------------------------------------
  ------------------------------Build the top layer------------------------------------
  -------------------------------------------------------------------------------------
  --Build this layer separte from the bottom layer, but so this new flat bottom will match
  --the top of the bottom layer, which should cancel out the faces where they meet.

  bottomOfTopLayer <- buildCubePointsListSingle "bottomOfTopLayer"
    (map
       (transposeZ (\z -> toothDepth))
       btmVariableHeightFaces
    )

  topCubes <- buildCubePointsListWithAdd "topCubes"
    (map
       (toTopFace . (transposeZ (\_ -> toothDepth + 10)))
       bottomOfTopLayer
    )
    (bottomOfTopLayer)

  {-

-}
  
  return bottomToothedLayerWithVarBtmAndFlatTop

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------Ankle brace -----------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--use ExtractedAngleHeightRadius to avoid all the Persistent baggage.
{-
View all the ExtractedAngleHeightRadius to make sure the y_axis values all ascend
for working with the radial grid. View manaully, as there is no auto system to ensure
they all ascend.

Find the <min/max> y_axis value for setting up the radial grid

--Problems:
--Does not work as a Maybe.
--Need to make a verion using MaybeT

should look at the building web api example for how to do this using the same connection, or what about a connection pool?
-}
calculateYValuesForGrid :: IO (Maybe [ExtractedAngleHeightRadius] )
calculateYValuesForGrid = do
  let layer1Name = "layer1"
      layer2Name = "layer2"
      layer3Name = "layer3"
      layer4Name = "layer4"
      layer5Name = "layer5"
      layer6Name = "layer6"
      layer7Name = "layer7"
      layer8Name = "layer8"
      layer9Name = "layer9"
      layer10Name = "layer10"
      layer11Name = "layer11"
      layer12Name = "layer12"
      runMaybeExtractedAngleHeightRadius :: String ->  (Maybe [ExtractedAngleHeightRadius] ) -> IO ()
      runMaybeExtractedAngleHeightRadius layerName eahr =
        case eahr of
          Nothing -> 
            putStrLn $ layerName ++ " not found"
            --return Nothing
          Just (extractedAngleHeightRadius) -> do 
            let 
              leadingEAHR' = leadingEAHR extractedAngleHeightRadius
              trailingEAHR' = trailingEAHR extractedAngleHeightRadius
              trailingEAHR'' = wrapZeroDegreeToEndAs360Degree leadingEAHR' trailingEAHR'
      
            putStrLn $  "leading " ++  layerName
            print $ show $ extractY_axis  leadingEAHR'
            putStrLn $ "trailing " ++ layerName
            print $ show $ reverse $ extractY_axis  trailingEAHR''

      
      --get the minimum y_axis values for all the layers
      --need to make a set of base fx's so that can also calc max by passing in minimum or maximum fx. 
      minYaxis :: [Maybe [ExtractedAngleHeightRadius]] -> Double
      minYaxis ((Just ex):exs) =
        minYaxis' (minimum $ extractY_axis ex) exs
      minYaxis' :: Double -> [Maybe [ExtractedAngleHeightRadius]] -> Double
      minYaxis' currMin ((Just ex):exs)  =
        let currMin' = minimum $ extractY_axis ex
        in
        case currMin' <= currMin of
          True  -> minYaxis' currMin' exs 
          False -> minYaxis' currMin exs
      minYaxis' currMin []  = currMin
      minYaxis' _ (Nothing:[])  =
        999

      maxYaxis :: [Maybe [ExtractedAngleHeightRadius]] -> Double
      maxYaxis ((Just ex):exs) =
        maxYaxis' (maximum $ extractY_axis ex) exs
      maxYaxis' :: Double -> [Maybe [ExtractedAngleHeightRadius]] -> Double
      maxYaxis' currMax ((Just ex):exs)  =
        let currMax' = maximum $ extractY_axis ex
        in
        case currMax' >= currMax of
          True  -> maxYaxis' currMax' exs 
          False -> maxYaxis' currMax exs
      maxYaxis' currMax []  = currMax
      maxYaxis' _ (Nothing:[])  =
        999
        
  maybeLayer1ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer1Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer1Name maybeLayer1ExtractedAngleHeightRadius
  --y_axis valus ascend good
  --leading min y  -73.0078125
  --trailing min y 

  maybeLayer2ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer2Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer2Name maybeLayer2ExtractedAngleHeightRadius
  --y_axis valus ascend good

  maybeLayer3ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer3Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer3Name maybeLayer3ExtractedAngleHeightRadius
  --y_axis valus ascend good

  maybeLayer4ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer4Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer4Name maybeLayer4ExtractedAngleHeightRadius

  maybeLayer5ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer5Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer5Name maybeLayer5ExtractedAngleHeightRadius

  maybeLayer6ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer6Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer6Name maybeLayer6ExtractedAngleHeightRadius

  maybeLayer7ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer7Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer7Name maybeLayer7ExtractedAngleHeightRadius

  maybeLayer8ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer8Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer8Name maybeLayer8ExtractedAngleHeightRadius

  maybeLayer9ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer9Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer9Name maybeLayer9ExtractedAngleHeightRadius

  maybeLayer10ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer10Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer10Name maybeLayer10ExtractedAngleHeightRadius

  maybeLayer11ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer11Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer11Name maybeLayer11ExtractedAngleHeightRadius

  maybeLayer12ExtractedAngleHeightRadius <- loadAndExtractedAngleHeightRadiusFromDB layer12Name "src/Examples/ShoeLift/MountainFlex/ankleBrace.db"
  runMaybeExtractedAngleHeightRadius layer12Name maybeLayer12ExtractedAngleHeightRadius

  let
    maybeLayerExtractedAngleHeightRadiusList =
     [maybeLayer1ExtractedAngleHeightRadius,
      maybeLayer2ExtractedAngleHeightRadius,
      maybeLayer3ExtractedAngleHeightRadius,
      maybeLayer4ExtractedAngleHeightRadius,
      maybeLayer5ExtractedAngleHeightRadius,
      maybeLayer6ExtractedAngleHeightRadius,
      maybeLayer7ExtractedAngleHeightRadius,
      maybeLayer8ExtractedAngleHeightRadius,
      maybeLayer9ExtractedAngleHeightRadius,
      maybeLayer10ExtractedAngleHeightRadius,
      maybeLayer11ExtractedAngleHeightRadius,
      maybeLayer12ExtractedAngleHeightRadius
     ]
  
  putStrLn "min y_axis value"
  print $ show $ minYaxis maybeLayerExtractedAngleHeightRadiusList
  putStrLn "max y_axis value"
  print $ show $ maxYaxis maybeLayerExtractedAngleHeightRadiusList
  
  
  return maybeLayer11ExtractedAngleHeightRadius

--run calculateYValuesForGrid without view a [ExtractedAngleHeightRadius]
runCalculateYValuesForGrid' :: IO ()
runCalculateYValuesForGrid' = do
  t <- calculateYValuesForGrid
  putStrLn "finished runCalculateYValuesForGrid'"

  
--Add the 0 degree scan value to the end of the scan as 360 degrees as is required to build a closed horizontal face.
--Otherwise the scan will end at the last scanned degree of around 359.
wrapZeroDegreeToEndAs360Degree :: [ExtractedAngleHeightRadius] -> [ExtractedAngleHeightRadius] -> [ExtractedAngleHeightRadius]
wrapZeroDegreeToEndAs360Degree ((ExtractedAngleHeightRadius _ h r):leading) trailing =
  trailing ++ [(ExtractedAngleHeightRadius (Angle 360) h r) ]


--Use [ExtractedAngleHeightRadius] to create  F3:[F2] and then extract the y-axis values.
--Known uses:
---Examine the y_axis values to make sure they are all ascending for grid creation.
---Find the min/max y_axis values for creating the radial grid.
extractY_axis :: [ExtractedAngleHeightRadius] -> [Double]
extractY_axis ahr  =
        let frontTopLines =
              map
                (extractFrontTopLine)
                $ createTopFacesVariableHeight
                    (Point 0 0 0) --origin
                    (map (_radiusEAHR) ahr)
                    (map (_angleEAHR) ahr)
                    (map (_heightEAHR) ahr)
                    

        in
        --Get F3 from leading FrontTopLine, and all the F2's, then extract the y_axis values
        map (extractYaxis) $ (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines


--Filter values to find pre/post 180 deg [ExtractedAngleHeightRadius] by filtering on Angle.
--Used by <leading/trailing>AHR
leadingAnglesFilter (Angle ang)  = (ang < 180.0)
trailingAnglesFilter (Angle ang) = (ang > 181.0)

--apply leadingAnglesFilter to find the pre/post 180 deg [ExtractedAngleHeightRadius]
leadingEAHR :: [ExtractedAngleHeightRadius] -> [ExtractedAngleHeightRadius]
leadingEAHR layerEAHR  = filter (\(ExtractedAngleHeightRadius ang _ _ ) -> leadingAnglesFilter ang) layerEAHR
trailingEAHR :: [ExtractedAngleHeightRadius] -> [ExtractedAngleHeightRadius]
trailingEAHR layerEAHR = {-reverse $-} filter (\(ExtractedAngleHeightRadius ang _ _ ) -> trailingAnglesFilter ang) layerEAHR







{-
Given:
layer<1/2...12> :: AnglesHeightsRadii
  each layer of the scan from the db.

Task:
Split each layer along the 0 180 degree axis so that each side can be build separately as leading/trailing sections.
Transpose the radius of each layer section of AnglesHeightsRadii to give the wall thickness.

Once each individual layer/section is build as F3:[F2], combine them all to calculate <min/max>Y_axis values
in order to build the grid as used by Radial and RadialLines modules.

Now build each layer/section with the radial grid system.
The layer1 leading/trailing sections are Bottom Faces while are the following layers
are created as TopFaces and added to the layer below.
-}
ankleBraceBuilder :: AnglesHeightsRadii ->
                     AnglesHeightsRadii ->  ExceptStackCornerPointsBuilder
ankleBraceBuilder layer1 layer2 = do
  let --filter to select leading (pre 180 degree) and trailing sections of scan.
      leadingSelector = (\ang -> ang <= 180)
      trailingSelector = (\ang -> ang > 180)
      
  layer1LeadingInner <- buildCubePointsListSingle "layer1LeadingInner"
    (buildLeadingTopFrontLinesFromAHR layer1 leadingSelector)

  --Transpose the Radius to give a perimeter wall of 1 mm.
  --Could be thicker at bottom then taper off as it rises.
  layer1LeadingOuter <- buildCubePointsListSingle "layer1LeadingOuter"
    (buildLeadingTopFrontLinesFromAHR (map (transpose (+2)) layer1) leadingSelector )

  layer2LeadingInner <- buildCubePointsListSingle "layer2LeadingInner"
    (buildLeadingTopFrontLinesFromAHR layer2 leadingSelector)

  --Transpose the Radius to give a perimeter wall of 1 mm.
  --Could be thicker at bottom then taper off as it rises.
  layer2LeadingOuter <- buildCubePointsListSingle "layer2LeadingOuter"
    (buildLeadingTopFrontLinesFromAHR (map (transpose (+2)) layer2) leadingSelector)
    
  --have to build the maxYaxis values first
  let grid =
        let allLayers = layer1LeadingInner ++ layer1LeadingOuter ++ layer2LeadingInner ++ layer2LeadingOuter
            minY = minYaxis allLayers
            maxY = maxYaxis allLayers
        in
          createYaxisGridFromMinMaxY <$> minY <*> maxY

  layer1LeadingBtmFaces <- buildCubePointsListSingle "layer1LeadingBtmFaces"
    (case grid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer1LeadingOuter
           layer1LeadingInner of
          Right val -> map toBottomFace val
          Left e -> [CornerPointsError e]
    )

  layer2LeadingTopFaces <- buildCubePointsListSingle "layerLeadingTopFaces"
    (case grid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer2LeadingOuter
           layer2LeadingInner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer1_2LeadingCubes <- buildCubePointsListWithAdd "layer1_2Cubes"
    layer1LeadingBtmFaces layer2LeadingTopFaces
  {-Do this later, once I get layer<1/2> leading to work.
  layer1TrailingInitial <- buildCubePointsListSingle "layer1trailing"
    (buildTrailingFrontTopLinesFromAHR layer1 (\ang -> ang > 180)
    )
  -}
  --figure out min/max y
  --let minY = minimum $ map minYaxis [layer1LeadingInitial, layer1Outer]

  {-
(let
       leadingTopPoints = buildLeadingTopFrontLinesFromAHR layer1 (\ang -> ang <= 180)
       trailingTopPoints = buildTrailingFrontTopLinesFromAHR (map (transpose (+ 1))  layer1) (\ang -> ang <= 180)
       grid = createYaxisGridFromTopFrontPoints $ leadingTopPoints ++ trailingTopPoints
       lines = buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingTopPoints  trailingTopPoints
     in
     (case lines of
       (Right cpoints) -> cpoints
       Left e          -> [CornerPointsError e]
     )
    )

-}
  
  --temp <- buildCubePointsListSingle "make it compile" []
  return layer1_2LeadingCubes

{-
Given: List of CornerPoints
Task:
  Find the value of the smallest y_axis of all the points in the [CornerPoints]
  See extractMinYaxisFromCpoint for rules on CornerPoints with > 1 Point.
  If CornerPoint such as FrontTopLine has > 1 points, work through the
  points to get the smallest.
Return: The smallest y_axis value
-}
minYaxis :: [CornerPoints] -> Either String Double
minYaxis (c:cs) =
  case extractMinYaxisFromCpoint c of
    Left e -> Left e
    Right currMin -> minYaxis' currMin cs
minYaxis' :: Double -> [CornerPoints] -> Either String Double
minYaxis' prevMin (c:cs)  =
        case extractMinYaxisFromCpoint c of
          Left e -> Left e
          Right currMin' -> 
            case currMin' <= prevMin of
              True  -> minYaxis' currMin' cs 
              False -> minYaxis' prevMin cs
minYaxis' currMin []  = Right currMin



{-
Given: CornerPoints
Task:
  Value of the smallest y_axis.
  If CornerPoint such as FrontTopLine has > 1 Point, work through the
  points to get the smallest.
Return: Double which is the smallest y_axis value
-}
extractMinYaxisFromCpoint :: CornerPoints -> Either String Double
extractMinYaxisFromCpoint (F2 (Point _ y _))  = Right y
extractMinYaxisFromCpoint (F3 (Point _ y _))  = Right y
extractMinYaxisFromCpoint (B3 (Point _ y _))  = Right y
extractMinYaxisFromCpoint (B2 (Point _ y _))  = Right y
extractMinYaxisFromCpoint unHandledCPoint = Left $ "extractMinYaxisFromCpoint: unhandled CornerPoints: " ++ (show unHandledCPoint)


{-
Given: List of CornerPoints
Task:
  Find the value of the largest y_axis of all the points in the [CornerPoints]
  See extractMaxYaxisFromCpoint for rules on CornerPoints with > 1 Point.
  If CornerPoint such as FrontTopLine has > 1 points, work through the
  points to get the largest.
Return: The largest y_axis value
-}
maxYaxis :: [CornerPoints] -> Either String Double
maxYaxis (c:cs) =
  case extractMaxYaxisFromCpoint c of
    Left e -> Left e
    Right currMax -> maxYaxis' currMax cs
maxYaxis' :: Double -> [CornerPoints] -> Either String Double
maxYaxis' prevMax (c:cs)  =
        case extractMaxYaxisFromCpoint c of
          Left e -> Left e
          Right currMax' -> 
            case currMax' >= prevMax of
              True  -> maxYaxis' currMax' cs 
              False -> maxYaxis' prevMax cs
maxYaxis' currMax []  = Right currMax

{-
Given: CornerPoints
Task:
  Value of the largest y_axis.
  If CornerPoint such as FrontTopLine has > 1 Point, work through the
  points to get the largest.
Return: Double which is the largest y_axis value
-}
extractMaxYaxisFromCpoint :: CornerPoints -> Either String Double
extractMaxYaxisFromCpoint (F2 (Point _ y _))  = Right y
extractMaxYaxisFromCpoint (F3 (Point _ y _))  = Right y
extractMaxYaxisFromCpoint (B3 (Point _ y _))  = Right y
extractMaxYaxisFromCpoint (B2 (Point _ y _))  = Right y
extractMaxYaxisFromCpoint unHandledCPoint = Left $ "extractMaxYaxisFromCpoint: unhandled CornerPoints: " ++ (show unHandledCPoint)
   


{-
topVariableHeightFacesForBuildingBtmFaces <- buildCubePointsListSingle "topVariableHeightFacesForBuildingBtmFaces"
    
-}

{-
Given:
  ahr:: AnglesHeightsRadii as loaded from the database by Persistent, but removed from Entity
  splitter :: (Double -> Bool). Used to select a section of the ahr.
Task:
  Select a section of the ahr, and create TopFrontLine, which are then extracted as (F3:[F2]).
  
Return:
  The created F3:[F2]
Known uses:
  To split a scan along the not quite symmetrical halves, to build them using the Joiners.RadialLines grid system.
  So should this be in the Joiners.RadialLines module?
-}
buildLeadingTopFrontLinesFromAHR :: AnglesHeightsRadii -> (Double -> Bool) -> [CornerPoints]
buildLeadingTopFrontLinesFromAHR ahr splitter =
  let
    origin = Point 0 0 0
    filteredAHR  = filterAngleHeightRadiusOnAngle splitter ahr
    
    topFaces =
          createTopFacesVariableHeight
          origin
          (extractRadii filteredAHR)
          (extractAngles filteredAHR)
          (extractHeights filteredAHR)
    frontTopLines = map (extractFrontTopLine) topFaces
  in
  (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines

buildTrailingFrontTopLinesFromAHR :: AnglesHeightsRadii -> (Double -> Bool) -> [CornerPoints]
buildTrailingFrontTopLinesFromAHR ahr splitter =
  let
    temp = buildLeadingTopFrontLinesFromAHR ahr splitter
  in
  (toF2 $ head temp) : (tail temp)

showAnkleBraceBuilderValue :: IO () 
showAnkleBraceBuilderValue = runSqlite "src/Examples/ShoeLift/MountainFlex/ankleBrace.db" $ do
  layer1Id <- getBy $ nameUnique' "layer1"
  ahrEntity1 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer1Id)] []

  layer2Id <- getBy $ nameUnique' "layer2"
  ahrEntity2 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer2Id)] []
  
  case layer1Id of
    Nothing -> liftIO $ putStrLn "tread scan layer1 was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = ankleBraceBuilder ( extractAnglesHeightsRadiiFromEntity ahrEntity1)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity2) 
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ print $ show a
          liftIO $ writeFile "src/Data/temp.txt" $ show a


runAnkleBraceBuilder :: IO () 
runAnkleBraceBuilder = runSqlite "src/Examples/ShoeLift/MountainFlex/ankleBrace.db" $ do
  layer1Id <- getBy $ nameUnique' "layer1"
  ahrEntity1 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer1Id)] []

  layer2Id <- getBy $ nameUnique' "layer2"
  ahrEntity2 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer2Id)] []
  
  case layer1Id of
    Nothing -> liftIO $ putStrLn "tread scan layer1 was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = ankleBraceBuilder ( extractAnglesHeightsRadiiFromEntity ahrEntity1)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity2) 
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          --liftIO $ print $ show a
          --liftIO $ writeFile "src/Data/temp.txt" $ show a
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "mountain ankle brace" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"


--use the grid, but only layer1.
--Same error
ankleBraceBuilderLayer1Only :: AnglesHeightsRadii ->
                               AnglesHeightsRadii ->  ExceptStackCornerPointsBuilder
ankleBraceBuilderLayer1Only layer1 layer2 = do
  
  layer1LeadingInner <- buildCubePointsListSingle "layer1LeadingInner"
    (
        buildLeadingTopFrontLinesFromAHR layer1 (\ang -> ang <= 180)
        
    )

  --Transpose the Radius to give a perimeter wall of 1 mm.
  --Could be thicker at bottom then taper off as it rises.
  layer1LeadingOuter <- buildCubePointsListSingle "layer1LeadingOuter"
    (
        buildLeadingTopFrontLinesFromAHR (map (transpose (+10)) layer1) (\ang -> ang <= 180)
        
    )

  --have to build the maxYaxis values first
  let grid =
        let allLayers = layer1LeadingInner ++ layer1LeadingOuter 
            minY = minYaxis allLayers
            maxY = maxYaxis allLayers
        in
          createYaxisGridFromMinMaxY <$> minY <*> maxY

  layer1LeadingTopFaces <- buildCubePointsListSingle "layer1LeadingBtmFaces"
    (case grid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer1LeadingOuter
           layer1LeadingInner of
          Right val ->  val
          Left e -> [CornerPointsError e]
    )

--map toBottomFace
  layer1LeadingBtmFaces <- buildCubePointsListSingle "layer1LeadingBtmFaces"
    (map ( (transposeZ (+ (-10)) ) . toBottomFace  ) layer1LeadingTopFaces)
  
  
  layer1Cubes <- buildCubePointsListWithAdd "layer1_2Cubes"
    layer1LeadingTopFaces layer1LeadingBtmFaces
  return layer1Cubes


--use the grid, but only layer1.
--Same error
ankleBraceBuilderLayer1OnlyNoGrid :: AnglesHeightsRadii ->
                               AnglesHeightsRadii ->  ExceptStackCornerPointsBuilder
ankleBraceBuilderLayer1OnlyNoGrid layer1 layer2 = do
  --F3:[F2]
  layer1LeadingInnerTop <- buildCubePointsListSingle "layer1LeadingInnerTop"
    (let
        t = buildLeadingTopFrontLinesFromAHR layer1 (\ang -> ang <= 180)
     in
     (head t) +++> (tail t)
    )

  --Transpose the Radius to give a perimeter wall of 1 mm.
  ----F3:[F2]
  layer1LeadingOuterTop <- buildCubePointsListSingle "layer1LeadingOuterTop"
    (let
        t = buildLeadingTopFrontLinesFromAHR (map (transpose (+10)) layer1) (\ang -> ang <= 180)
     in
     (toB3 $ head t) +++> (map toB2 $ tail t)
    )

  layer1TopFaces <- buildCubePointsListWithAdd "layer1TopFaces"
    layer1LeadingInnerTop layer1LeadingOuterTop

  layer1Cubes <- buildCubePointsListWithAdd "layer1Cubes"
    (layer1TopFaces)
    (map (toBottomFace . (transposeZ (+ (-10)))) layer1TopFaces)
  
  return layer1Cubes
