{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

{- |
Create parts for the lineScanner.
-Top part to mount lasers.

Print calibration pieces.
-}
module Examples.LineScanner.LineScanner() where

import Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin, loadAndExtractedAngleHeightRadiusFromDB)
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFacesVariableHeight)
import CornerPoints.Points(Point(..))
import Builder.Monad(ExceptStackCornerPointsBuilder, buildCubePointsListSingle, buildCubePointsListWithAdd)
import CornerPoints.Radius(Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.FaceExtraction(extractBackBottomLine, extractBottomFrontLine, extractFrontFace)
import CornerPoints.Transpose(transposeZ, transposeY)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace, toTopRightLine,
                                    toF2, toF3, toB2, toB3, toBackTopLine, toBackBottomLine, toBottomFrontLine)
import Primitives.Cubical(rectangularCubeNonRadial)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)

import Control.Monad.State
import Control.Monad.Except

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Persistable.Base as PstB
import Control.Monad.IO.Class (liftIO)

currentBuilder = rectangularBlockAttachingToAluminumPlaterBuilder


{-
Create the arms on which lasers mount.
Will need to print in 2 sections to fit onto the printer.
2 arcs will make up 120 degrees. A third will be printed, which will be used to attach them.
-}
simpleFlatArmBuilder :: ExceptStackCornerPointsBuilder
simpleFlatArmBuilder = do
  --A section which is 1/6 circumference.
  --2 of these will be joined via a joiner to make the section on which the lasers are mounted.
  let arcDegrees = [353..359] ++ [0..7]
      --[0..15]
      --the section for extending back to the lift.
      
      --[0..60]
      --gives 1/6th of a circle, which will fit on the printer,
      --and then 2 parts can be joined to give 120 degrees
  
      height = 25
      --25: for the arm that reaches back to the lift
      --5: the main arcs that hold the lasers
      --keep it nice and thin.
      --Can always laminate another layer on if not strong enough

      innerArcRadius = 180
      --radius of the arc that makes up the laser mount arms, in mm
      outerArcRadius = innerArcRadius + 70
      --the outer radius of the arms. 50 was 20 too narrow to bolt front of mount and clamp backside.
      
  buildCurvedSectionBackBtmLines
    <- buildCubePointsListSingle "buildCurvedSectionBtmFaces"
    (
      let
        radii = [Radius innerArcRadius | _ <- [1..]]
        angles = [Angle a | a <- arcDegrees]
        btmFaces = createBottomFaces (Point 0 0 0) radii angles
      in
       map (toBackBottomLine . extractBottomFrontLine)  btmFaces
    )

  buildCurvedSectionBtmFrontline
    <- buildCubePointsListSingle "buildCurvedSectionBtmFrontFaces"
    (
      let
        radii = [Radius outerArcRadius  | _ <- [1..]]
        angles = [Angle a | a <- arcDegrees]
      in
       map (extractBottomFrontLine)  $ createBottomFaces (Point 0 0 0) radii angles 
    )

  btmFaces <- buildCubePointsListWithAdd "btmFaces"
    (buildCurvedSectionBackBtmLines)
    (buildCurvedSectionBtmFrontline)

    
  --the section of arc on which the lasers mount
  arcCubes <- buildCubePointsListWithAdd "cubes"
    (btmFaces)
    (map (toTopFace . (transposeZ (+ height))) btmFaces)

  --the arm that reaches back to the lift system
  --it gets commented out for printing the arms.
  arm <- buildCubePointsListWithAdd "cubes"
    (arcCubes)
    (map (extractFrontFace . (transposeY (+ (- 150)))) arcCubes)
    
  return arcCubes


{-
Create the riser block that attached to the top of the aluminum plate of the c-frame.
The extension arm will attach to the top of this block.
-}
rectangularBlockAttachingToAluminumPlaterBuilder :: ExceptStackCornerPointsBuilder
rectangularBlockAttachingToAluminumPlaterBuilder = do
  let   
      height = 60
      width = 125
      length = 87
  buildRectangle
    <- buildCubePointsListSingle "buildRectangle"
    (
      [rectangularCubeNonRadial height width length]
    )

  return buildRectangle



    
showBuilder :: IO ()  
showBuilder =
  let
    valCpoints = ((evalState $ runExceptT currentBuilder) [])

  in
    case valCpoints of
      (Left e) -> liftIO $ print $ e
      (Right a) -> do
        print $ show a

runBuilder :: IO ()
runBuilder =
  let
    runBuilder = (runExceptT currentBuilder) 
    valCpoints = ((evalState runBuilder) [])
    cpoints = ((execState runBuilder) [])
  in
    case valCpoints of
      (Left e) -> liftIO $ print $ e
      (Right a) -> do
        liftIO $ putStrLn "output from Builder was good"
        liftIO $ writeStlToFile $ newStlShape "lineScanner laser mount" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
  
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------calibration ----------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--will require it's own <show/run>Builder as db connection is required.


currentCalibrationBuilder = rectangularBlockOnTurntableBuilder
{-
Scan the 4x4 block in the turntable, to print it out. This should check for calibration.
-}

rectangularBlockOnTurntableBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
rectangularBlockOnTurntableBuilder ahr origin = do
  outerBtmLines <- buildCubePointsListSingle "outerBtmLines"
    (createTopFacesVariableHeight origin (extractRadii ahr) (extractAngles ahr) (extractHeights ahr))

  cubes <- buildCubePointsListWithAdd "cubes"
    (outerBtmLines)
    ( let
        transposer z = z - 10
      in
      map (toBottomFace . (transposeZ transposer) ) outerBtmLines
    )
    
  return outerBtmLines


runCalibrationBuilder :: IO () 
runCalibrationBuilder = runSqlite "src/Examples/LineScanner/lineScanner.db" . PstB.asSqlBackendReader $ do
  pillarLayerId <- getBy $ nameUnique' "layer1"
  geoxAngleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId pillarLayerId)] []

  
  case pillarLayerId of
    Nothing -> liftIO $ putStrLn "calibration layer1 was not found"
    (Just (Entity key geoxLayerVal)) -> do
      liftIO $ putStrLn "calibration layer1 was found"
      
      let builder = currentCalibrationBuilder
                            (extractAnglesHeightsRadiiFromEntity geoxAngleHeightRadiusEntity)
                            (extractOrigin geoxLayerVal)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "calibration" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"

