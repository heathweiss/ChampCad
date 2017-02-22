module Primitives.Cylindrical.Solid(cylinder,
                                    slopedBottomCylinder, slopedTopCylinder,
                                    yLengthenedCylinder, squaredOffYLengthenedCylinder,
                                    squaredOffCylinder) where

import CornerPoints.Create(Slope(..), Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.HorizontalFaces(createTopFaces, createTopFacesWithVariableSlope,
                                    createBottomFaces, createBottomFacesWithVariableSlope, createBottomFacesLengthenY,
                                    createBottomFacesSquaredOff, createBottomFacesSquaredOffLengthenY,
                                    createBottomFacesSquaredOffLengthenYSeparately
                                   )
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Radius(Radius(..))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.FaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine,
                                    frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine,
                                    backFaceFromFrontFace)



import TypeClasses.Transposable(transposeZ)

{- |
Solid cylinder which is stretched out along the y-axis by the LengthenFactor.
-}

yLengthenedCylinder :: Radius -> Origin -> [Angle] -> Height -> LengthenFactor -> [CornerPoints]
yLengthenedCylinder    radius    origin    angles     height    lengthenFactor  =
  createBottomFacesLengthenY origin [radius | x <- [1..]] angles flatXSlope flatYSlope lengthenFactor
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))


{- |
Solid cylinder based only on a list of angles and a height.
-}
cylinder :: Radius -> Origin -> [Angle] -> Height -> [CornerPoints]
cylinder    radius    origin    angles     height  =
  createBottomFaces origin [radius | x <- [1..]] angles flatXSlope flatYSlope
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))



{- |
Create a solid cylinder with
-variable Radius
-variable bottom slope
-flat bottom
-}
slopedBottomCylinder :: [Radius] -> Origin -> [Angle] -> [Slope] -> [Slope] -> Height -> [CornerPoints]
slopedBottomCylinder    radii       origin     angles     xSlopes    ySlopes    height  =
  --bottom faces
  (
   createBottomFacesWithVariableSlope origin  radii angles xSlopes ySlopes
   
  )
  |+++|
  --top faces
  (
    createTopFaces (transposeZ (+ height) origin ) radii  angles flatXSlope flatYSlope 
   
  )

{- |
Create a solid cylinder with
-variable Radius
-variable top slope
-flat bottom
-}
slopedTopCylinder :: [Radius] -> Origin -> [Angle] -> [Slope] -> [Slope] -> Height -> [CornerPoints]
slopedTopCylinder    radii       origin     angles     xSlopes    ySlopes    height  =
  --top faces
  (
   createTopFacesWithVariableSlope (transposeZ (+ height) origin ) radii angles xSlopes ySlopes
   
  )
  |+++|
  --bottom faces
  (
    createBottomFaces origin radii  angles flatXSlope flatYSlope 
   
  )


{- |
A cylinder that has flattened sides. The degree of flatness is based on the 'power' parameter.
-}
squaredOffCylinder :: Radius -> Origin -> [Angle] -> Height -> Power -> [CornerPoints]
squaredOffCylinder    radius    origin    angles     height    power  =
  createBottomFacesSquaredOff origin [radius | x <- [1..]] angles flatXSlope flatYSlope power
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))

{-
squaredOffCylinder' :: Radius -> Origin -> [Angle] -> Height -> Power -> [CornerPoints]
squaredOffCylinder'    radius    origin    angles     height    power  =
  let btmFaces = 
-}

squaredOffYLengthenedCylinder :: Radius -> Origin -> [Angle] -> Height  -> Power -> LengthenFactor -> [CornerPoints]
squaredOffYLengthenedCylinder    radius    origin    angles     height     power    lengthenFactor      =
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



type Height = Double
type LengthenFactor = Double
type Power = Double
