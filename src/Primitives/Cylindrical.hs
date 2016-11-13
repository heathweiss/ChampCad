-- {-# LANGUAGE ParallelListComp #-}
{---------------- intro -----------------------------
Supplies basic cylindrical shapes as cad building blocks.
Includes:
  ringBase
  cylinder
-}

module Primitives.Cylindrical(cylinderWallsNoSlope,cylinderWallsNoSlopeSquaredOff,
  cylinderSolidNoSlope,
  cylinderWallsVariableRadiusNoSlope,
  cylinderSolidNoSlopeSquaredOff,
  cylinderSolidNoSlopeLengthenY,
  cylinderSolidNoSlopeSquaredOffLengthenY,
  cylinderWallsNoSlopeSquaredOffLengthenY,
  cylinderWallsVariableThicknessSloped,
  cylinderWallsVariableThicknessNoSlope,
  cylinderSolidNoSlopeSquaredOffLengthenYSeparately,) where

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
Create a no slope walled cylinder with different inner and outer radius.
-}
cylinderWallsVariableThicknessNoSlope :: [Radius] -> [Radius] -> [Angle] -> Origin -> Height -> [CornerPoints]
cylinderWallsVariableThicknessNoSlope    innerRadii  outerRadii  angles     origin    height  =
   ((--bottom faces
      (map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces {-(Point 0 0 0)-}origin innerRadii angles flatXSlope flatYSlope)) 
      |+++|
      (map (extractBottomFrontLine) (createBottomFaces {-(Point 0 0 0)-}origin outerRadii angles flatXSlope flatYSlope)) 
    )
    |@+++#@|
    ((transposeZ (+ height)) . upperFaceFromLowerFace)
   )
   

cylinderWallsVariableThicknessSloped :: [Radius] -> [Radius] -> [Angle] -> Origin -> Slope -> Slope -> Height -> [CornerPoints]
cylinderWallsVariableThicknessSloped    innerRadii  outerRadii  angles     origin      xSlope   ySlope   height  =
  (  --top Sloped faces
    (map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces {-(Point 0 0 height)-}origin innerRadii angles xSlope ySlope))
    |+++|
    (map extractFrontTopLine (createTopFaces {-(Point 0 0 height)-}origin outerRadii angles xSlope ySlope))
  )
  |+++|
  ( map extractBottomFace
    (cylinderWallsVariableThicknessNoSlope innerRadii outerRadii angles origin (0::Height)  )
  )

{- |Create a cylindrical wall, based on a single radius.
It is a convenience wrapper around cylinderWallsVariableRadiusNoSlope.-}
cylinderWallsNoSlope :: Radius -> Thickness ->  Origin -> [Angle] -> Height -> [CornerPoints]
cylinderWallsNoSlope    innerRadius    wallThickness origin    angles     height =
  cylinderWallsVariableRadiusNoSlope  [innerRadius | x<- [1..] ]    wallThickness origin    angles     height

{- |Create a cylindrical wall, based on an [Radius].-}
cylinderWallsVariableRadiusNoSlope :: [Radius] -> Thickness ->  Origin -> [Angle] -> Height -> [CornerPoints]
cylinderWallsVariableRadiusNoSlope    innerRadii    wallThickness origin    angles     height =
        let  innerCubes = createBottomFaces origin innerRadii angles flatXSlope flatYSlope
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))

             outerCubes = createBottomFaces origin (map (transpose (+wallThickness )) innerRadii) angles flatXSlope flatYSlope
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
               
        in  cylinderCubes



cylinderWallsNoSlopeSquaredOff :: Radius ->    Origin -> [Angle] -> Height -> Thickness ->  Power -> [CornerPoints]
cylinderWallsNoSlopeSquaredOff    innerRadius  origin    angles     height wallThickness    power  =
        let  innerCubes = createBottomFacesSquaredOff origin [innerRadius | x <-  [1..]] angles flatXSlope flatYSlope power
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
         
             outerCubes = createBottomFacesSquaredOff origin [Radius ((radius innerRadius) + wallThickness) |x <- [1..]] angles flatXSlope flatYSlope power
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

cylinderSolidNoSlope :: Radius -> Origin -> [Angle] -> Height -> [CornerPoints]
cylinderSolidNoSlope    radius    origin    angles     height  =
  createBottomFaces origin [radius | x <- [1..]] angles flatXSlope flatYSlope
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))

cylinderSolidNoSlopeLengthenY :: Radius -> Origin -> [Angle] -> Height -> LengthenFactor -> [CornerPoints]
cylinderSolidNoSlopeLengthenY    radius    origin    angles     height    lengthenFactor  =
  createBottomFacesLengthenY origin [radius | x <- [1..]] angles flatXSlope flatYSlope lengthenFactor
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))


cylinderSolidNoSlopeSquaredOffLengthenY :: Radius -> Origin -> [Angle] -> Height  -> Power -> LengthenFactor -> [CornerPoints]
cylinderSolidNoSlopeSquaredOffLengthenY    radius    origin    angles     height     power    lengthenFactor      =
--createBottomFacesSquaredOffLengthenY :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> Power -> LengthenFactor  -> [CornerPoints]
  createBottomFacesSquaredOffLengthenY origin [radius | x <- [1..]] angles flatXSlope flatYSlope power lengthenFactor 
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))

{- |
Create a solid cylinder, squared off, lengthened along the Y axis. The amount lengthened in the pos/neg directions, can be set separately.
-}  
cylinderSolidNoSlopeSquaredOffLengthenYSeparately :: Radius -> Origin -> [Angle] -> Height  -> Power -> LengthenFactor -> LengthenFactor -> [CornerPoints]
cylinderSolidNoSlopeSquaredOffLengthenYSeparately    radius    origin    angles     height     power    lengthenNegYFactor lengthenPosYFactor      =
  createBottomFacesSquaredOffLengthenYSeparately origin [radius | x <- [1..]] angles flatXSlope flatYSlope power lengthenNegYFactor lengthenPosYFactor
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))

--
cylinderSolidNoSlopeSquaredOff :: Radius -> Origin -> [Angle] -> Height -> Power -> [CornerPoints]
cylinderSolidNoSlopeSquaredOff    radius    origin    angles     height    power  =
  createBottomFacesSquaredOff origin [radius | x <- [1..]] angles flatXSlope flatYSlope power
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))




  
