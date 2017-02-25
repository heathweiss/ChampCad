
{- |
Supplies cylindrical shapes.

-}

module Primitives.Cylindrical.Walled(
  squaredCylinder,
  cylinder,
  squaredYLengthenedCylinder, 
  slopedCylinder
  ) where

import CornerPoints.Create(slopeAdjustedForVerticalAngle, Slope(..), Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.HorizontalFaces(createTopFaces, createBottomFaces,
                                    createTopFacesWithVariableSlope, createBottomFacesWithVariableSlope,
                                    createTopFaces, createBottomFacesSquaredOff,
                                   createBottomFacesWithVariableSlope,createBottomFacesSquaredOffLengthenY,
                                   createBottomFacesSquaredOffLengthenYSeparately)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.FaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace )
import CornerPoints.FaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine,
                                    upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine, backFaceFromFrontFace)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Transpose (transposeZ)
import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose)

type Thickness = Double
type Height = Double
type Power = Double
type LengthenFactor = Double


{- |
Create a walled cylinder.
-}
cylinder :: [Radius] ->     [Radius] ->     [Angle] -> Origin -> Height -> [CornerPoints]
cylinder    innerWallRadii  outerWallRadii  angles     origin    height  =
   ((--bottom faces
      (map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces origin innerWallRadii angles )) 
      |+++|
      (map (extractBottomFrontLine) (createBottomFaces origin outerWallRadii angles )) 
    )
    |@+++#@|
    ((transposeZ (+ height)) . upperFaceFromLowerFace)
   )

{- |
Create a walled cylinder with sloped top
-}   
slopedCylinder :: [Radius] -> [Radius] -> [Angle] -> Origin -> Slope -> Slope -> Height -> [CornerPoints]
slopedCylinder    innerRadii  outerRadii  angles     origin      xSlope   ySlope   height  =
  (  --top Sloped faces
    (map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces origin innerRadii angles {-xSlope ySlope-}))
    |+++|
    (map extractFrontTopLine (createTopFaces origin outerRadii angles {-xSlope ySlope-}))
  )
  |+++|
  --flat bottom faces.
  ( map extractBottomFace
    (cylinder innerRadii outerRadii angles origin (0::Height)  )
  )

{- |
Create a walled cylinder which is squared off by modifying the radii with the formula:
radius**2)/ (((x**power) + (y**power)) **(1/power))
The higher the power, the more the shape comes to a square, though the corners will always remain somewhat rounded.
-}
squaredCylinder :: [Radius] -> Thickness ->   Origin -> [Angle] -> Height ->   Power -> [CornerPoints]
squaredCylinder    innerWallRadii wallThickness  origin    angles     height     power  =
        let  innerCubes = createBottomFacesSquaredOff origin innerWallRadii angles  power
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
         
             outerCubes = createBottomFacesSquaredOff origin [Radius ((radius x) + wallThickness) |x <- innerWallRadii] angles  power
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
             
        in   cylinderCubes


squaredYLengthenedCylinder :: Radius ->     Origin -> [Angle] -> Height -> Thickness -> Power -> LengthenFactor -> [CornerPoints]
squaredYLengthenedCylinder    innerRadius  origin     angles     height   wallThickness power  lengthenFactor =
        let  innerCubes = createBottomFacesSquaredOffLengthenY origin [innerRadius | x <-  [1..]] angles power lengthenFactor
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
         
             outerCubes = createBottomFacesSquaredOffLengthenY origin [Radius ((radius innerRadius) + wallThickness) |x <- [1..]] angles power lengthenFactor
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
             
        in   cylinderCubes











  
