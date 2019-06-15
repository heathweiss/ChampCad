{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GMSH.Writer({-gmshPointString, pntsBldrDataScriptStr,-} toGmshPoints, toGmshPoint) where
{- |
Convert ChampCad Points/Lines/etc to gmsh scripts and print to .geo file.
-}

import CornerPoints.Points(Point(..))
import qualified GMSH.Common as GC
import qualified GMSH.Points as GP

import Control.Lens
import qualified Data.HashMap.Strict as HM

makeLenses ''GC.PointsBuilderData
makeLenses ''GC.BuilderStateData


{- |
------------------------------- old version ----------------------------------------
Should be abel to get rid of this once new system to write to file during the Builder monad, is done.
------------------------------------------------------------------------------------

Task:
Create GMSH Points script from the hashmap of GC.PointsBuilderData contained in a GC.BuilderData.

Return:
A single String with each GC.PointsBuilderData output as a gmsh script. eg: \nPoint(3) = {3.0,3.0,3.0};

Known uses:
Once a GMESH Builder is run, extract the GC.BuilderData from State, and write gmsh Points string to a .geo file.
-}
toGmshPoints :: GC.BuilderStateData -> String
toGmshPoints builderData =
  --Needs to traverse the hashmap, creating a string containing all GC.PointsBuilderData values as gmesh Points.
  --All Points are preceded with a \n to make it more readable in the gmsh .geo file.

  let
    --create a gmsh string from a Point and Id in the GC.PointsBuilderData
    buildGmshString :: GC.PointsBuilderData -> String
    buildGmshString pointsBuilderData =
      "\nPoint(" ++
      (show (pointsBuilderData ^. pointsId)) ++ ") = {"  ++
      (show (x_axis (pointsBuilderData ^. point))) ++ "," ++
      (show (y_axis (pointsBuilderData ^. point))) ++ "," ++
      (show (z_axis (pointsBuilderData ^. point))) ++ "};"  
    
  in
  concat $ map (buildGmshString) $ HM.elems (builderData ^. pointsMap)

{- |
Task::
Get the GPointId from the GC.PointsBuilderData, of the CornerPoints.Point.
Build a gmsh script string from it.

Given::
Point: The CornerPoints.Point that the GPoint represents.

Return::
The gmsh script string for the points.
If the point did not exist, give an error string.
-}
toGmshPoint :: GC.BuilderStateData -> Point -> String
toGmshPoint bldrStateData point =
  let
    toGmshPoint' :: GC.PointsBuilderData -> Point -> String
    toGmshPoint' builderData (Point x y z) =
      "\nPoint(" ++
      (show (builderData ^. pointsId)) ++ ") = {"  ++
      (show x) ++ "," ++
      (show y) ++ "," ++
      (show z) ++ "};"
      
    maybe_pointsBuilderData = GP.retrieve bldrStateData point
  in
  case maybe_pointsBuilderData of
    Nothing -> "No GPoint exists for: " ++ (show point)
    Just pointsBuilderData -> toGmshPoint' pointsBuilderData point
       

