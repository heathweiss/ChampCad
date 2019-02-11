module GMSH.Writer(gmshPointString) where
{- |
Print out gmsh script to console or to file.
-}

import CornerPoints.Points(Point(..))

{-
Create the gmsh script Point fx string for a Point.
Used by gmshCPointString, to create string for each Point contained in current CornerPoint
-}
gmshPointString :: Point -> Int -> String
gmshPointString (Point x y z) num =
  --"\nPoint(" ++ (show num) ++ ") = {"  ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "};"
  "Point(" ++ (show num) ++ ") = {"  ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "};"

