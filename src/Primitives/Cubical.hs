{-
For creating hexahedrons such as cubes, rectangular cubes, etc.
-}

module Primitives.Cubical(rectangularCubeNonRadial, rectangularSolidNoSlope) where
import CornerPoints.CornerPoints(CornerPoints(..),(+++),(+++-),(@+++#@),(|@+++#@|))
import CornerPoints.Points (Point(..))
import CornerPoints.Transpose (transposeZ,transposeX,transposeY)
import CornerPoints.FaceConversions(
  upperFaceFromLowerFace,
  backFaceFromFrontFace,
  f23LineFromF14Line,
  frontBottomLineFromFrontTopLine,
  backTopLineFromFrontTopLine,
  lowerFaceFromUpperFace,
  backBottomLineFromBottomFrontLine,
  frontTopLineFromBackTopLine,
  bottomFrontLineFromBackBottomLine)
import  Math.Trigonometry(atanDegrees)
import CornerPoints.Create(Origin(..),  createCornerPointSquaredOff)
import CornerPoints.Radius(Radius(..))
import CornerPoints.HorizontalFaces(createTopFaces, createBottomFaces, createTopFacesWithVariableSlope,  createBottomFacesWithVariableSlope)

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

type ZHeight = Double
type XWidth = Double
type YLength = Double
type Thickness = Double
type Height = Double
type Power = Double
type LengthenFactor = Double

-- | Create a 4 sided shape radially from an origin.
--   See Examples.Primitives.Cubes for an example.
-- Should be named something else, as it could make any 4 sided shape, just by varying the
-- angles and radii.
rectangularSolidNoSlope :: [Radius] -> Origin -> [Angle] -> Height -> [CornerPoints]
rectangularSolidNoSlope    radii    origin    angles     height   =
  createBottomFaces origin radii   angles 
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))

{-
Create a rectangle with a non-radial system.
Should I get rid of this, and go to the strictly radial system.
-}
rectangularCubeNonRadial :: ZHeight -> XWidth -> YLength -> CornerPoints
rectangularCubeNonRadial height width length =
  
 let
    b1Point =  (Point 0 0 0) --the origin of the cube is the B1 corner
  
  in
     -- btmFace
    (transposeX (+ width) (B4 b1Point))
    +++
    (B1 b1Point)
    +++-  
    (transposeY (+ length) ( transposeX (+ width) (F4 b1Point)))
    +++   
    (transposeY (+ length) (F1 b1Point))
    @+++#@
    --top face
    (\btmFace -> (upperFaceFromLowerFace  ( transposeZ (+ height) btmFace)))
  
   
---------------------------------------------------------------- polar rectanlge -------------------------------------------------------------------
{-
Create a rectangular cube, using the polar coordinate system used by TriCad.MathPolar module.
This will allow the recangle to be created from a center point, thus allowing for nested rectangles, as would be used
when a perimeter is desired.

Known use:
Create a rectangle with an outer wall, and inner wall, so that the center could be drilled out and tapped for a bolt.


polarRectangularCube
-}

--quad1Angle length width = atan (length/2) (width /s) 
