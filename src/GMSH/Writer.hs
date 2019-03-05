{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GMSH.Writer(gmshPointString, pntsBldrDataScriptStr, pntsBldrDataScriptFromBlderData) where
{- |
Print out gmsh script to console or to file.
-}

import CornerPoints.Points(Point(..))
import qualified GMSH.Common as GC

import Control.Lens
import qualified Data.HashMap.Strict as HM

makeLenses ''GC.PointsBuilderData
makeLenses ''GC.BuilderData

{- |
Create a GMSH script string from a GC.PointsBuilderData
-}
pntsBldrDataScriptStr :: GC.PointsBuilderData -> String
pntsBldrDataScriptStr pointsBuilderData =
  gmshPointString (pointsBuilderData ^. point) (pointsBuilderData ^. pointsId)

{- |
Create GMSH script strings from a hashmap of GC.PointsBuilderData contained in a GC.BuilderData
-}
pntsBldrDataScriptFromBlderData :: GC.BuilderData -> String
pntsBldrDataScriptFromBlderData builderData = 
  let
    a = ""
    --traverse = concat $ map (pntsBldrDataScriptStr . snd) $ HM.toList (builderData ^. pointsMap)
    traverse = map (pntsBldrDataScriptStr . snd) $ HM.toList (builderData ^. pointsMap)
    traverse' = concat $ map (\s -> "\n" ++ s) traverse
  in
  traverse'
  


{-
Create the gmsh script Point fx string for a Point.
Used by gmshCPointString, to create string for each Point contained in current CornerPoint

This is the original I used before the Builder. Keep for now as it is used by examples/gmsh/gate
-}
gmshPointString :: Point -> Int -> String
gmshPointString (Point x y z) num =
  --"\nPoint(" ++ (show num) ++ ") = {"  ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "};"
  "Point(" ++ (show num) ++ ") = {"  ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "};"

