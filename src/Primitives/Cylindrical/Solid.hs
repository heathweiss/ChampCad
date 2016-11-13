module Primitives.Cylindrical.Solid(slopedBottomCylinder, slopedTopCylinder) where

import CornerPoints.Create(Slope(..), Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.HorizontalFaces(createTopFaces, createTopFacesWithVariableSlope,
                                    createBottomFaces, createBottomFacesWithVariableSlope)
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|))
import CornerPoints.Radius(Radius(..))
import CornerPoints.Transpose (transposeZ)
import TypeClasses.Transposable(transposeZ)

{-
type Thickness = Double

type Power = Double
type LengthenFactor = Double
-}
type Height = Double

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
