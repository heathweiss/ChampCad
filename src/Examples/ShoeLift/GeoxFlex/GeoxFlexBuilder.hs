{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{- |
Build shoelift for Geox running shoes.
Inside shoes with a 6cm lift that flexes at the toe.
Will use a rubber bootie for the bottom tread so it will be a match to the top tread.

-}

module Examples.ShoeLift.GeoxFlex.GeoxFlexBuilder() where

import RIO
import qualified RIO.Text as T

import OpenSCad.ScriptBase(ToOpenScript(toScript), Name(..), Script(..))
import OpenSCad.Polyhedron(Polyhedron(..))

import Joiners.RadialLines(getMinY, getMaxY, extractYaxis, createYaxisGridFromTopFrontPoints, splitOnXaxis, buildLeftRightLineFromGridAndLeadingTrailingCPointsBase)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import qualified Persistable.Base as PstB

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
import CornerPoints.Transpose(transposeX, transposeY, transposeZ, transposeZWithList)
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

import Control.Lens

makeLenses ''AngleHeightRadius


currentBuilder = btmTreadBuilderUsingRadialLinesGrid
                 --btmTreadBuilderUsingRadialLinesGrid
                 --topTreadBuilderUsingRadialLinesGrid
                 --testPrintTreadToCheckScanBuilder
                 --topTreadBuilderUsingLeftRightSidesButNoGrid

showBuilderValue :: IO () 
showBuilderValue = runSqlite "src/Examples/ShoeLift/GeoxFlex/lineScanner.db" . PstB.asSqlBackendReader $ do
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
          --print to console
          --liftIO $ print $ show a

          --this is where I need to convert to script
          --below is the original.
          --liftIO $ writeFile "src/Data/temp.txt" $ show a
          --now I convert the CubePoints to PolyhedronScript
          let
            cubeScripts =  [PolyhedronScript $ PolyCPoints (Name ("cube" <> (display $ textDisplay x)) ) cube' 
                          | x <- ([1..] :: [Int])
                          | cube' <- a
                    ]        
            --cut out a central cylinder to showcase the use of OpenSCad difference command. Use a temporary cylinder script, as have not created a cylinder.
            cutCylinder = map (Difference (RawUtf8 "cylinder(r=10,h=200, $fn=10);")) {-$ take 10 $ drop 120-} cubeScripts
            --diff = Difference 
          writeFileUtf8Builder 
           "temp.txt" $ 
            mconcat $ map toScript cutCylinder -- cubeScripts
            


--make a riser to convert from pillars to german centers
runBuilder :: IO () 
runBuilder = runSqlite "src/Examples/ShoeLift/GeoxFlex/lineScanner.db" . PstB.asSqlBackendReader $ do
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
The full length tread that will glue into the rubber sole.
Create a flat top on it for printing, will mate with the top printed sections/
Will ratate it with slic3r for printing.

Divide the scan in half along the y_axis.
Recombine the 2 sides using the Joiners.RadialLines module.

Extend the grid sections out so that a toothed section will occurr in the toe section where flex is required.

-}
btmTreadBuilderUsingRadialLinesGrid :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
btmTreadBuilderUsingRadialLinesGrid ahr origin = do

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

  btmGridFaces <- buildCubePointsListSingle "btmGridFaces"
    (map toBottomFace topGridFaces)
  
  cubesLayer1 <- buildCubePointsListWithAdd "cubesLayer1"
    (btmGridFaces)
    (let
        transposer z = 50
     in
     map
     ( (transposeZ transposer) . toTopFace) btmGridFaces
    )

  cubesLayer2 <- buildCubePointsListWithAdd "cubesLayer2"
    (let
        btmFaces = map (extractBottomFace) cubesLayer1
        fullDepth = -25
        fullSpace = -2
        transposeList = [fullDepth | _ <- [1..170]] ++
                        (concat [[fullSpace - 10 ,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..2]]) ++
                        (concat [[fullSpace - 5,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..2]]) ++
                        (concat [[fullSpace,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..5]]) ++
                        (concat [[fullSpace - 8 ,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..3]]) ++
                        (concat [[fullSpace - 13,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..4]]) ++
                        [fullDepth | _  <- [1..]]
                        {-[fullDepth | _ <- [1..170]] ++
                        (concat [[-15,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..2]]) ++
                        (concat [[-10,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..2]]) ++
                        (concat [[fullSpace,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..5]]) ++
                        (concat [[-10,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..3]]) ++
                        (concat [[-15,fullDepth,fullDepth,fullDepth,fullDepth] | _ <- [1..4]]) ++
                        [fullDepth | _  <- [1..]]-}
      in
       transposeZWithList transposeList btmFaces
    )
    (map (toTopFace . extractBottomFace) cubesLayer1)
  
  return cubesLayer2

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
The full length tread that will glue onto the shoe.
Create a flat bottom on it for printing.
Divide the scan in half along the y_axis.
Recombine the 2 sides using the Joiners.RadialLines module.

-}
topTreadBuilderUsingRadialLinesGrid :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
topTreadBuilderUsingRadialLinesGrid ahr origin = do

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
        transposer z = 18
     in
     map
     ( (transposeZ transposer) . toBottomFace) topGridFaces
    )
  
  return trailingTopFrontPoints
{-
--top of this tread will fit the shoe. Bottom will be flat for printing.
--Keep it thin so as not to mess with the flex.
Build the cubes by building Top<Front/Back>Lines.
This means processing each side of the scan (pre/post 180 degrees) into b3:[f3] and b2:[f2] which can then be joined with +++>
This does not use the RadialLines grid system, but does divide the scan into leading trailing sides.
Should delete it once I get the grid working.
-}
topTreadBuilderUsingLeftRightSidesButNoGrid :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
topTreadBuilderUsingLeftRightSidesButNoGrid ahr origin = do

  let
    pre180Angles ang  = (ang < 180.0)
    post180Angles ang = (ang > 180.0)
    
    ahrPre180  = filter (\(AngleHeightRadius ang _ _ _) -> pre180Angles ang) ahr
    ahrPost180 = reverse $ filter (\(AngleHeightRadius ang _ _ _) -> post180Angles ang) ahr
    
    extractedPreRadii = extractRadii ahrPre180
    extractedPreAngles = extractAngles ahrPre180
    extractedPreHeights = extractHeights ahrPre180

    extractedPostRadii = extractRadii ahrPost180
    extractedPostAngles = extractAngles ahrPost180
    extractedPostHeights = extractHeights ahrPost180

  --build the top faces of the entire tread to be able to find <min/max>Y to build the grid list
  --Could do this by creating the topFrontPoints of each side, finding <min/max>Y of each, and deriving them from that
  topFacesEntireTread
    <- buildCubePointsListSingle "topInnerFaces"
              (let
                  
                  --the scan is missing 0 and 360 degrees as it is used to make up the faces by adding left and right sides together.
                  --But for a test, will build it the normal way with: createTopFacesVariableHeight
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii ahr)
                    (extractAngles ahr)
                    (extractHeights ahr)
               in
               topFaces
              )

  
  frontTopLinesEntireTread <- buildCubePointsListSingle "frontTopLines"
    (map (extractFrontTopLine) topFacesEntireTread)

  topFrontPointsEntireTread <- buildCubePointsListSingle "entireTreadAsTopFrontPoints"
    ((extractF3 $ head frontTopLinesEntireTread) :  map (extractF2) frontTopLinesEntireTread)

  
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

  let minY =
        let preMinY  = extractYaxis $ getMinY preTopFaces
            postMinY = extractYaxis $ getMinY postTopFaces
        in
          case preMinY < postMinY of
            True -> preMinY
            False -> postMinY

      mayY =
        let preMaxY  = extractYaxis $ getMaxY preTopFaces
            postMaxY = extractYaxis $ getMaxY postTopFaces
        in
          case preMaxY < postMaxY of
            True -> preMaxY
            False -> postMaxY
       
      grid = createYaxisGridFromTopFrontPoints topFrontPointsEntireTread

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



--create with:createTopFacesVariableHeight. Does not yet use a grid.
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
