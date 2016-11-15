-- {-# LANGUAGE ParallelListComp #-}
{---------------- intro -----------------------------
Supplies basic cylindrical shapes as cad building blocks.
Includes:
  ringBase
  cylinder
-}

module Primitives.Cylindrical(
  cylinderWallsNoSlopeSquaredOff,
  cylinder,
  cylinderWallsNoSlopeSquaredOffLengthenY,
  slopedCylinder,
  {-cylinderWallsVariableThicknessNoSlope-}) where

import CornerPoints.Create(slopeAdjustedForVerticalAngle, Slope(..), Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.HorizontalFaces(createTopFaces, createBottomFaces,
                                    createTopFacesWithVariableSlope, createBottomFacesWithVariableSlope,
                                    createTopFaces,createBottomFacesSquaredOff,
                                   createBottomFacesWithVariableSlope,createBottomFacesSquaredOffLengthenY,createBottomFacesLengthenY,
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
      (map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces origin innerWallRadii angles flatXSlope flatYSlope)) 
      |+++|
      (map (extractBottomFrontLine) (createBottomFaces origin outerWallRadii angles flatXSlope flatYSlope)) 
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
    (map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces {-(Point 0 0 height)-}origin innerRadii angles xSlope ySlope))
    |+++|
    (map extractFrontTopLine (createTopFaces origin outerRadii angles xSlope ySlope))
  )
  |+++|
  --flat bottom faces.
  ( map extractBottomFace
    (cylinder innerRadii outerRadii angles origin (0::Height)  )
  )


--cylinderWallsNoSlopeSquaredOff :: Radius -> Thickness ->   Origin -> [Angle] -> Height ->   Power -> [CornerPoints]
cylinderWallsNoSlopeSquaredOff :: [Radius] -> Thickness ->   Origin -> [Angle] -> Height ->   Power -> [CornerPoints]
cylinderWallsNoSlopeSquaredOff    innerWallRadii wallThickness  origin    angles     height     power  =
        let  --innerCubes = createBottomFacesSquaredOff origin [innerWallRadii | x <-  [1..]] angles flatXSlope flatYSlope power
             innerCubes = createBottomFacesSquaredOff origin innerWallRadii angles flatXSlope flatYSlope power
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
         
             --outerCubes = createBottomFacesSquaredOff origin [Radius ((radius innerWallRadii) + wallThickness) |x <- [1..]] angles flatXSlope flatYSlope power
             outerCubes = createBottomFacesSquaredOff origin [Radius ((radius x) + wallThickness) |x <- innerWallRadii] angles flatXSlope flatYSlope power
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
             
        in   cylinderCubes


cylinderWallsNoSlopeSquaredOffLengthenY :: Radius ->     Origin -> [Angle] -> Height -> Thickness -> Power -> LengthenFactor -> [CornerPoints]
cylinderWallsNoSlopeSquaredOffLengthenY    innerRadius  origin     angles     height   wallThickness power  lengthenFactor =
        let  innerCubes = createBottomFacesSquaredOffLengthenY origin [innerRadius | x <-  [1..]] angles flatXSlope flatYSlope power lengthenFactor
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
         
             outerCubes = createBottomFacesSquaredOffLengthenY origin [Radius ((radius innerRadius) + wallThickness) |x <- [1..]] angles flatXSlope flatYSlope power lengthenFactor
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
             
        in   cylinderCubes











  
