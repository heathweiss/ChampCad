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

--which lift evaluate in <show/run>LiftBuilderValue
currentLiftBuilder = entireTopTreadBuilderUsingRadialLinesGridForTestFit
                 

--The range of angles that define the heel/flex/toe sections. Reused > 1 fx.
--leading
heelLeadingAngles ang  = (ang < 130.0)
flexLeadingAngles ang  = (ang > 127.0) && (ang < 155.0)
toeLeadingAngles ang  = (ang > 152.0) && (ang < 180.0)
--trailing
heelTrailingAngles ang = (ang > 230.0)
flexTrailingAngles ang = (ang > 210.0) && (ang < 233.0)
toeTrailingAngles ang = (ang > 181.0) && (ang < 213.0)

--show return value from currentLiftBuilder
showLiftBuilderValue :: IO () 
showLiftBuilderValue = runSqlite "src/Examples/ShoeLift/MountainFlex/lineScanner.db" $ do
  layerId <- getBy $ nameUnique' "tread"
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "tread scan layer was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = currentLiftBuilder ( extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ print $ show a
          liftIO $ writeFile "src/Data/temp.txt" $ show a

--run the currentLiftBuilder, outputting the stl.
runLiftBuilder :: IO () 
runLiftBuilder = runSqlite "src/Examples/ShoeLift/MountainFlex/lineScanner.db" $ do
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
The full length tread that will glue onto the shoe.
Create a flat bottom on it for printing.
Divide the scan in half along the y_axis.
Create a grid using createYaxisGridFromTopFrontPoints from the Joiners.RadialLines module.
Recombine the 2 sides with zip. Should be the same length as they were made with createYaxisGridFromTopFrontPoints.
Used just to check the fit of the scan. Not used in final lift as it is a single non-flexible piece.
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

ToDo:
Create a version of this for AngleHeightsRadius, as using ExtractedAngleHeightRadius is a poor design choice.
In addition to having a version like this that show the y_axis values, have a fx that returns a Bool, or an Either
to say it all y_axis values are ascending in the list.

If an Either was used in conjuction with an error msg, then applicative could be used to see which layer is
not ascending. That layer could then be independently viewed to decide on what correction are required
to ensure ascending y_axis values

extractY_axis: need a AngleHeightsRadius version as well.
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
addZeroDegreeToEndAs360Degree :: AngleHeightRadius -> [AngleHeightRadius]-> [AngleHeightRadius]
addZeroDegreeToEndAs360Degree (AngleHeightRadius a h r i) ahr =
  ahr ++ [(AngleHeightRadius 360 h r i)]


--Add a AngleHeightRadius to the end of the AngleHeightRadii scan as 360 degrees as is required to build a closed horizontal face.
--Otherwise the scan will end at the last scanned degree of around 359.
wrapZeroDegreeToEndAs360Degree :: [ExtractedAngleHeightRadius] -> [ExtractedAngleHeightRadius] -> [ExtractedAngleHeightRadius]
wrapZeroDegreeToEndAs360Degree ((ExtractedAngleHeightRadius _ h r):leading) trailing =
  trailing ++ [(ExtractedAngleHeightRadius (Angle 360) h r) ]


--Use [ExtractedAngleHeightRadius] to create  F3:[F2] and then extract the y-axis values.
--Known uses:
---Examine the y_axis values to make sure they are all ascending for grid creation.
---Find the min/max y_axis values for creating the radial grid.

--ToDo:
--Make a version for AngleHeightsRadius as ExtractedAngleHeightRadius is NFG design decision.


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
--rename: buildF3appendedToF2s
buildLeadingFrontTopLinesFromAHR :: AnglesHeightsRadii -> (Double -> Bool) -> [CornerPoints]
buildLeadingFrontTopLinesFromAHR ahr splitter =
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


--load all 12 layers of the scan.
--show the current val of 1 of the 3 ankle brace builders
showAnkleBraceBuilderValue :: IO () 
showAnkleBraceBuilderValue = runSqlite "src/Examples/ShoeLift/MountainFlex/ankleBrace.db" $ do
  layer1Id <- getBy $ nameUnique' "layer1"
  ahrEntity1 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer1Id)] []

  layer2Id <- getBy $ nameUnique' "layer2"
  ahrEntity2 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer2Id)] []

  layer3Id <- getBy $ nameUnique' "layer3"
  ahrEntity3 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer3Id)] []

  layer4Id <- getBy $ nameUnique' "layer4"
  ahrEntity4 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer4Id)] []

  layer5Id <- getBy $ nameUnique' "layer5"
  ahrEntity5 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer5Id)] []

  layer6Id <- getBy $ nameUnique' "layer6"
  ahrEntity6 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer6Id)] []

  layer7Id <- getBy $ nameUnique' "layer7"
  ahrEntity7 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer7Id)] []

  layer8Id <- getBy $ nameUnique' "layer8"
  ahrEntity8 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer8Id)] []

  layer9Id <- getBy $ nameUnique' "layer9"
  ahrEntity9 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer9Id)] []

  layer10Id <- getBy $ nameUnique' "layer10"
  ahrEntity10 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer10Id)] []

  layer11Id <- getBy $ nameUnique' "layer11"
  ahrEntity11 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer11Id)] []

  layer12Id <- getBy $ nameUnique' "layer12"
  ahrEntity12 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer12Id)] []
  
  
  case layer1Id of
    Nothing -> liftIO $ putStrLn "tread scan layer1 was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = ankleBraceBuilder
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity1)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity2)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity3)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity4)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity5)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity6)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity7)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity8)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity9)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity10)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity11)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity12)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ print $ show a
          liftIO $ writeFile "src/Data/temp.txt" $ show a

--load all 12 layers of the scan from db.
--call 1 of 3 ankle brace builders.
runAnkleBraceBuilder :: IO () 
runAnkleBraceBuilder = runSqlite "src/Examples/ShoeLift/MountainFlex/ankleBrace.db" $ do
  
  layer1Id <- getBy $ nameUnique' "layer1"
  ahrEntity1 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer1Id)] []

  layer2Id <- getBy $ nameUnique' "layer2"
  ahrEntity2 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer2Id)] []

  layer3Id <- getBy $ nameUnique' "layer3"
  ahrEntity3 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer3Id)] []

  layer4Id <- getBy $ nameUnique' "layer4"
  ahrEntity4 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer4Id)] []

  layer5Id <- getBy $ nameUnique' "layer5"
  ahrEntity5 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer5Id)] []

  layer6Id <- getBy $ nameUnique' "layer6"
  ahrEntity6 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer6Id)] []

  layer7Id <- getBy $ nameUnique' "layer7"
  ahrEntity7 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer7Id)] []

  layer8Id <- getBy $ nameUnique' "layer8"
  ahrEntity8 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer8Id)] []

  layer9Id <- getBy $ nameUnique' "layer9"
  ahrEntity9 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer9Id)] []

  layer10Id <- getBy $ nameUnique' "layer10"
  ahrEntity10 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer10Id)] []

  layer11Id <- getBy $ nameUnique' "layer11"
  ahrEntity11 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer11Id)] []

  layer12Id <- getBy $ nameUnique' "layer12"
  ahrEntity12 <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layer12Id)] []
  
  
  case layer1Id of
    Nothing -> liftIO $ putStrLn "tread scan layer1 was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = ankleBraceBuilder
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity1)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity2)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity3)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity4)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity5)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity6)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity7)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity8)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity9)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity10)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity11)
                                      ( extractAnglesHeightsRadiiFromEntity ahrEntity12)
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


{-
Given:
layer<1,2...12> :: AnglesHeightsRadii
  each layer of the scan from the db.

Task:
Read each scan layer and split into leading/trailing angles to build the leading/trailing sections.
Transpose the radius of each layer section of AnglesHeightsRadii to give the wall thickness.

Once each individual layer/section is build as F3:[F2], combine them all to calculate <min/max>Y_axis values
in order to build the grid as used by Radial and RadialLines modules.

Now build each layer/section with the radial grid system.
The layer1 leading/trailing sections are Bottom Faces while are the following layers
are created as TopFaces and added to the layer below.
-}

ankleBraceBuilder ::
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->
                     AnglesHeightsRadii ->  ExceptStackCornerPointsBuilder
ankleBraceBuilder layer1 layer2 layer3 layer4 layer5 layer6 layer7 layer8 layer9 layer10 layer11 layer12 = do
  let --filter each layer to select leading (pre 180 degree) section of scan.
      leadingSelector = (\ang -> ang <= 180)
      
  --leadingLayer<1..12>Inner: the leadling F2:[F3] Cpoints created from scan. Make up the inner wall of brace
  --leadingLayer<1..12>OUter: Transpose the Radius of the inner wall to create the outer wall of brace.   
  leadingLayer1Inner <- buildCubePointsListSingle "leadingLayer1Inner"
    (buildLeadingFrontTopLinesFromAHR layer1 leadingSelector)
  
  leadingLayer1Outer <- buildCubePointsListSingle "leadingLayer1Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer1) leadingSelector )
  
  leadingLayer2Inner <- buildCubePointsListSingle "leadingLayer2Inner"
    (buildLeadingFrontTopLinesFromAHR layer2 leadingSelector)

  leadingLayer2Outer <- buildCubePointsListSingle "leadingLayer2Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer2) leadingSelector)

  leadingLayer3Inner <- buildCubePointsListSingle "leadingLayer3Inner"
    (buildLeadingFrontTopLinesFromAHR layer3 leadingSelector)

  leadingLayer3Outer <- buildCubePointsListSingle "leadingLayer3Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer3) leadingSelector)

  leadingLayer4Inner <- buildCubePointsListSingle "leadingLayer4Inner"
    (buildLeadingFrontTopLinesFromAHR layer4 leadingSelector)

  leadingLayer4Outer <- buildCubePointsListSingle "leadingLayer4Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer4) leadingSelector)

  leadingLayer5Inner <- buildCubePointsListSingle "leadingLayer5Inner"
    (buildLeadingFrontTopLinesFromAHR layer5 leadingSelector)

  leadingLayer5Outer <- buildCubePointsListSingle "leadingLayer5Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer5) leadingSelector)

  leadingLayer6Inner <- buildCubePointsListSingle "leadingLayer6Inner"
    (buildLeadingFrontTopLinesFromAHR layer6 leadingSelector)

  leadingLayer6Outer <- buildCubePointsListSingle "leadingLayer6Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer6) leadingSelector)

  leadingLayer7Inner <- buildCubePointsListSingle "leadingLayer7Inner"
    (buildLeadingFrontTopLinesFromAHR layer7 leadingSelector)

  leadingLayer7Outer <- buildCubePointsListSingle "leadingLayer7Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer7) leadingSelector)

  leadingLayer8Inner <- buildCubePointsListSingle "leadingLayer8Inner"
    (buildLeadingFrontTopLinesFromAHR layer8 leadingSelector)

  leadingLayer8Outer <- buildCubePointsListSingle "leadingLayer8Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer8) leadingSelector)

  leadingLayer9Inner <- buildCubePointsListSingle "leadingLayer9Inner"
    (buildLeadingFrontTopLinesFromAHR layer9 leadingSelector)

  leadingLayer9Outer <- buildCubePointsListSingle "leadingLayer9Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer9) leadingSelector)

  leadingLayer10Inner <- buildCubePointsListSingle "leadingLayer10Inner"
    (buildLeadingFrontTopLinesFromAHR layer10 leadingSelector)

  leadingLayer10Outer <- buildCubePointsListSingle "leadingLayer10Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer10) leadingSelector)

  leadingLayer11Inner <- buildCubePointsListSingle "leadingLayer11Inner"
    (buildLeadingFrontTopLinesFromAHR layer11 leadingSelector)

  leadingLayer11Outer <- buildCubePointsListSingle "leadingLayer11Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer11) leadingSelector)

  leadingLayer12Inner <- buildCubePointsListSingle "leadingLayer12Inner"
    (buildLeadingFrontTopLinesFromAHR layer12 leadingSelector)

  leadingLayer12Outer <- buildCubePointsListSingle "leadingLayer12Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer12) leadingSelector)

  
  --RadialLines grid for leading wall
  let leadingGrid =
        let allLayers = leadingLayer1Inner ++ leadingLayer1Outer ++ leadingLayer2Inner ++ leadingLayer2Outer
                        ++ leadingLayer3Inner ++ leadingLayer3Outer
                        ++ leadingLayer4Inner ++ leadingLayer4Outer
                        ++ leadingLayer5Inner ++ leadingLayer5Outer
                        ++ leadingLayer6Inner ++ leadingLayer6Outer
                        ++ leadingLayer7Inner ++ leadingLayer7Outer
                        ++ leadingLayer8Inner ++ leadingLayer8Outer
                        ++ leadingLayer9Inner ++ leadingLayer9Outer
                        ++ leadingLayer10Inner ++ leadingLayer10Outer
                        ++ leadingLayer11Inner ++ leadingLayer11Outer
                        ++ leadingLayer12Inner ++ leadingLayer12Outer 
                        
            minY = minYaxis allLayers
            maxY = maxYaxis allLayers
        in
          createYaxisGridFromMinMaxY <$> minY <*> maxY

  --create layer 1 as [BottomFace] to start off the leading wall.
  leadingLayer1BtmFaces <- buildCubePointsListSingle "leadingLayer1BtmFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "leadingGrid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer1Outer
           leadingLayer1Inner of
          Right val -> map toBottomFace val
          Left e -> [CornerPointsError e]
    )
  --leadingLayer<2..12>TopFaces: All subsequent layers are created as [TopFace] which will later be added to the preceding [CornerPoints].
  --to create the current [CubePoints]
  leadingLayer2TopFaces <- buildCubePointsListSingle "leadingLayer2TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "leadingGrid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer2Outer
           leadingLayer2Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer3TopFaces <- buildCubePointsListSingle "leadingLayer3TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer3Outer
           leadingLayer3Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer4TopFaces <- buildCubePointsListSingle "leadingLayer4TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer4Outer
           leadingLayer4Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer5TopFaces <- buildCubePointsListSingle "leadingLayer5TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer5Outer
           leadingLayer5Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer6TopFaces <- buildCubePointsListSingle "leadingLayer6TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer6Outer
           leadingLayer6Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer7TopFaces <- buildCubePointsListSingle "leadingLayer7TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer7Outer
           leadingLayer7Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer8TopFaces <- buildCubePointsListSingle "leadingLayer8TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer8Outer
           leadingLayer8Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer9TopFaces <- buildCubePointsListSingle "leadingLayer9TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer9Outer
           leadingLayer9Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer10TopFaces <- buildCubePointsListSingle "leadingLayer10TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer10Outer
           leadingLayer10Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer11TopFaces <- buildCubePointsListSingle "leadingLayer11TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer11Outer
           leadingLayer11Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  leadingLayer12TopFaces <- buildCubePointsListSingle "leadingLayer12TopFaces"
    (case leadingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           leadingLayer12Outer
           leadingLayer12Inner of
          Right val -> val
          Left e -> [CornerPointsError e]
    )
  
  --leadingLayer<..>Cubes: Now add up all the layers of <Bottom/Top> to create CubePoints.
  leadingLayer1_2Cubes <- buildCubePointsListWithAdd "leadingLayer1_2Cubes"
    leadingLayer1BtmFaces leadingLayer2TopFaces

  leadingLayer2_3Cubes <- buildCubePointsListWithAdd "leadingLayer2_3Cubes"
    leadingLayer3TopFaces leadingLayer1_2Cubes

  leadingLayer3_4Cubes <- buildCubePointsListWithAdd "leadingLayer3_4Cubes"
    leadingLayer4TopFaces leadingLayer2_3Cubes

  leadingLayer4_5Cubes <- buildCubePointsListWithAdd "leadingLayer4_5Cubes"
    leadingLayer5TopFaces leadingLayer3_4Cubes

  leadingLayer5_6Cubes <- buildCubePointsListWithAdd "leadingLayer5_6Cubes"
    leadingLayer6TopFaces leadingLayer4_5Cubes

  leadingLayer6_7Cubes <- buildCubePointsListWithAdd "leadingLayer6_7Cubes"
    leadingLayer7TopFaces leadingLayer5_6Cubes

  leadingLayer7_8Cubes <- buildCubePointsListWithAdd "leadingLayer7_8Cubes"
    leadingLayer8TopFaces leadingLayer6_7Cubes

  leadingLayer8_9Cubes <- buildCubePointsListWithAdd "leadingLayer8_9Cubes"
    leadingLayer9TopFaces leadingLayer7_8Cubes

  leadingLayer9_10Cubes <- buildCubePointsListWithAdd "leadingLayer9_10Cubes"
    leadingLayer10TopFaces leadingLayer8_9Cubes

  leadingLayer10_11Cubes <- buildCubePointsListWithAdd "leadingLayer10_11Cubes"
    leadingLayer11TopFaces leadingLayer9_10Cubes
  
  leadingLayer11_12Cubes <- buildCubePointsListWithAdd "leadingLayer11_12Cubes"
    leadingLayer12TopFaces leadingLayer10_11Cubes

  --------------------------------------------- trailing (0 and > 180 degrees) faces ----------------------------------------------------------
  ---------------------------------------------------------------------------------------------------------------------------------------------
  let
      --include the 0 degree angle in order to bring the leading/trailing sides of scan
      --together at the 0/360 angle. Select all trailing angles.
      selectZeroAndTrailingAngles = (\ang -> (ang > 180) || (ang == 0))

      --need to reverse the trailing lists in order to avoid AppendE error.
      --Not sure what the error is about, but easier just to reverse the list,
      --as it is more logical to build in ascending y_axis direction like the leading list.
      --Each 1 is used twice, so do it here for efficiency.
      layer1ReversedToAvoidAppendEerror = reverse layer1

      layer2ReversedToAvoidAppendEerror = reverse layer2

      layer3ReversedToAvoidAppendEerror = reverse layer3

      layer4ReversedToAvoidAppendEerror = reverse layer4

      layer5ReversedToAvoidAppendEerror = reverse layer5

      layer6ReversedToAvoidAppendEerror = reverse layer6

      layer7ReversedToAvoidAppendEerror = reverse layer7

      layer8ReversedToAvoidAppendEerror = reverse layer8

      layer9ReversedToAvoidAppendEerror = reverse layer9

      layer10ReversedToAvoidAppendEerror = reverse layer10

      layer11ReversedToAvoidAppendEerror = reverse layer11

      layer12ReversedToAvoidAppendEerror = reverse layer12 
  

  --layer<1..12>Inner: the trailing F2:[F3] Cpoints created from scan. Make up the inner wall of brace
  --layer<1..12>OUter: Transpose the Radius of the inner wall to create the outer wall of brace.
  layer1Inner <- buildCubePointsListSingle "layer1Inner"
    (buildLeadingFrontTopLinesFromAHR layer1ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)
  
  
  layer1Outer <- buildCubePointsListSingle "layer1Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer1ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles )

  
  layer2Inner <- buildCubePointsListSingle "layer2Inner"
    (buildLeadingFrontTopLinesFromAHR layer2ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer2Outer <- buildCubePointsListSingle "layer2Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer2ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer3Inner <- buildCubePointsListSingle "layer3Inner"
    (buildLeadingFrontTopLinesFromAHR layer3ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer3Outer <- buildCubePointsListSingle "layer3Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer3ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer4Inner <- buildCubePointsListSingle "layer4Inner"
    (buildLeadingFrontTopLinesFromAHR layer4ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer4Outer <- buildCubePointsListSingle "layer4Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer4ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer5Inner <- buildCubePointsListSingle "layer5Inner"
    (buildLeadingFrontTopLinesFromAHR layer5ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer5Outer <- buildCubePointsListSingle "layer5Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer5ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer6Inner <- buildCubePointsListSingle "layer6Inner"
    (buildLeadingFrontTopLinesFromAHR layer6ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer6Outer <- buildCubePointsListSingle "layer6Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer6ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer7Inner <- buildCubePointsListSingle "layer7Inner"
    (buildLeadingFrontTopLinesFromAHR layer7ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer7Outer <- buildCubePointsListSingle "layer7Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer7ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer8Inner <- buildCubePointsListSingle "layer8Inner"
    (buildLeadingFrontTopLinesFromAHR layer8ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer8Outer <- buildCubePointsListSingle "layer8Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer8ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer9Inner <- buildCubePointsListSingle "layer9Inner"
    (buildLeadingFrontTopLinesFromAHR layer9ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer9Outer <- buildCubePointsListSingle "layer9Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer9ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer10Inner <- buildCubePointsListSingle "layer10Inner"
    (buildLeadingFrontTopLinesFromAHR layer10ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer10Outer <- buildCubePointsListSingle "layer10Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer10ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer11Inner <- buildCubePointsListSingle "layer11Inner"
    (buildLeadingFrontTopLinesFromAHR layer11ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer11Outer <- buildCubePointsListSingle "layer11Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer11ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  layer12Inner <- buildCubePointsListSingle "layer12Inner"
    (buildLeadingFrontTopLinesFromAHR layer12ReversedToAvoidAppendEerror selectZeroAndTrailingAngles)

  layer12Outer <- buildCubePointsListSingle "layer12Outer"
    (buildLeadingFrontTopLinesFromAHR (map (transpose (+2)) layer12ReversedToAvoidAppendEerror) selectZeroAndTrailingAngles)

  
  --RadialLines grid for trailing wall
  let trailingGrid =
        let allLayers = layer1Inner ++ layer1Outer ++ layer2Inner ++ layer2Outer
                        ++ layer3Inner ++ layer3Outer
                        ++ layer4Inner ++ layer4Outer
                        ++ layer5Inner ++ layer5Outer
                        ++ layer6Inner ++ layer6Outer
                        ++ layer7Inner ++ layer7Outer
                        ++ layer8Inner ++ layer8Outer
                        ++ layer9Inner ++ layer9Outer
                        ++ layer10Inner ++ layer10Outer
                        ++ layer11Inner ++ layer11Outer
                        ++ layer12Inner ++ layer12Outer 
                        
            minY = minYaxis allLayers
            maxY = maxYaxis allLayers
        in
          createYaxisGridFromMinMaxY <$> minY <*> maxY
  --create layer 1 as [BottomFace] to start off the trailing wall.
  layer1BtmFaces <- buildCubePointsListSingle "layer1BtmFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer1Inner
           layer1Outer of
          Right val -> map toBottomFace val
          Left e -> [CornerPointsError e]
    )
  --layer<2..12>TopFaces: All subsequent layers are created as [TopFace] and added to the preceding [CornerPoints]
  --to create the current [CubePoints]
  layer2TopFaces <- buildCubePointsListSingle "layer2TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer2Inner
           layer2Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer3TopFaces <- buildCubePointsListSingle "layer3TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer3Inner
           layer3Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer4TopFaces <- buildCubePointsListSingle "layer4TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer4Inner
           layer4Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer5TopFaces <- buildCubePointsListSingle "layer5TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer5Inner
           layer5Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer6TopFaces <- buildCubePointsListSingle "layer6TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer6Inner
           layer6Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer7TopFaces <- buildCubePointsListSingle "layer7TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer7Inner
           layer7Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer8TopFaces <- buildCubePointsListSingle "layer8TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer8Inner
           layer8Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer9TopFaces <- buildCubePointsListSingle "layer9TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer9Inner
           layer9Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer10TopFaces <- buildCubePointsListSingle "layer10TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer10Inner
           layer10Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer11TopFaces <- buildCubePointsListSingle "layer11TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer11Inner
           layer11Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  layer12TopFaces <- buildCubePointsListSingle "layer12TopFaces"
    (case trailingGrid of
       Left e -> [CornerPointsError $ "grid error: " ++ e]
       Right grid' ->
        case
         buildLeftRightLineFromGridAndLeadingTrailingCPointsBase
           grid'
           layer12Inner
           layer12Outer of
          Right val -> val
          Left e -> [CornerPointsError e]
    )

  --now combine all the layers of <Bottom/Top>Face to create the CubePoints.
  layer1_2Cubes <- buildCubePointsListWithAdd "layer1_2Cubes"
    layer1BtmFaces layer2TopFaces

  layer2_3Cubes <- buildCubePointsListWithAdd "layer2_3Cubes"
    layer1_2Cubes layer3TopFaces

  layer3_4Cubes <- buildCubePointsListWithAdd "layer3_4Cubes"
    layer2_3Cubes layer4TopFaces

  layer4_5Cubes <- buildCubePointsListWithAdd "layer4_5Cubes"
    layer3_4Cubes layer5TopFaces

  layer5_6Cubes <- buildCubePointsListWithAdd "layer5_6Cubes"
    layer4_5Cubes layer6TopFaces

  layer6_7Cubes <- buildCubePointsListWithAdd "layer6_7Cubes"
    layer5_6Cubes layer7TopFaces

  layer7_8Cubes <- buildCubePointsListWithAdd "layer7_8Cubes"
    layer6_7Cubes layer8TopFaces

  layer8_9Cubes <- buildCubePointsListWithAdd "layer8_9Cubes"
    layer7_8Cubes layer9TopFaces

  layer9_10Cubes <- buildCubePointsListWithAdd "layer9_10Cubes"
    layer8_9Cubes layer10TopFaces

  layer10_11Cubes <- buildCubePointsListWithAdd "layer10_11Cubes"
    layer9_10Cubes layer12TopFaces

  layer11_12Cubes <- buildCubePointsListWithAdd "layer11_12Cubes"
    layer10_11Cubes layer12TopFaces
  
  return layer1_2Cubes

