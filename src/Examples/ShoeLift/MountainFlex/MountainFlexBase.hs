{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{- |
The mountain warehouse shoes that have the ankle brace that I scanned.

Has a flexible toe by splitting the toe and gluing separately.
Was going to have a semiflex section but did not work out. That is why there are 3 sections instead of 2.
-}

module Examples.ShoeLift.MountainFlex.MountainFlexBase() where

import Joiners.RadialLines(getMinY, getMaxY, extractYaxis, createYaxisGrid, splitOnXaxis,
                           buildLeftRightLineFromGridAndLeadingTrailingCPointsBase)

import Database.Persist 
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)


import Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin,
                           loadAndExtractedAngleHeightRadiusFromDB, loadAndExtractedAngleHeightRadiusFromDbT)

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

--import qualified Control.Lens as L
import Control.Lens 

makeLenses ''AngleHeightRadius
makeLenses ''ExtractedAngleHeightRadius

currentBuilder = heelFlexToeFlatTopAndBtmTreadBuilder
                 

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
Create a grid using createYaxisGrid from the Joiners.RadialLines module.
Recombine the 2 sides with zip. Should be the same length as they were made with createYaxisGrid.

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
    grid = createYaxisGrid frontTopPointsNotYetSplitIntoLeadingTrailing
    
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
    grid = createYaxisGrid frontTopPointsNotYetSplitIntoLeadingTrailing
    
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
Divide the scan in half along the y_axis, create a grid using createYaxisGrid from the Joiners.RadialLines module.
Recombine the 2 sides with zip. Should be the same length as they were made with createYaxisGrid.
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
    grid = createYaxisGrid frontTopPointsForCreatingGrid
  
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
    grid = createYaxisGrid frontTopPointsUsedToBuildGrid
  
  
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
    grid = createYaxisGrid frontTopPointsForBuildingGrid
  
  
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
    grid = createYaxisGrid frontTopPointsForBuildingGrid
  
  
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

--leftOff
        {-need to adjust Radius of layer1 angle 0 as it's yval is less than the next angle.
          Should wait till I see all the values.

         Should I create a fx to get the min/max y values or just look them over and pick them manually.
         If I do want to get min/max values, will need to make this a Monad transformer so the Maybe part works
         when a layer fails to load.
         -}



