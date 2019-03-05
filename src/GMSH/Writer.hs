{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GMSH.Writer({-gmshPointString, pntsBldrDataScriptStr,-} toGmshPoints) where
{- |
Convert ChampCad Points/Lines/etc to gmsh scripts and print to .geo file.
-}

import CornerPoints.Points(Point(..))
import qualified GMSH.Common as GC

import Control.Lens
import qualified Data.HashMap.Strict as HM

makeLenses ''GC.PointsBuilderData
makeLenses ''GC.BuilderData


{- |
Task:
Create GMSH Points script from the hashmap of GC.PointsBuilderData contained in a GC.BuilderData.



Return:
A single String with each GC.PointsBuilderData output as a gmsh script. eg: \nPoint(3) = {3.0,3.0,3.0};

Known uses:
Once a GMESH Builder is run, extract the GC.BuilderData from State, and write gmsh Points string to a .geo file.
-}
toGmshPoints :: GC.BuilderData -> String
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
