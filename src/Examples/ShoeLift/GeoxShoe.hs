module Examples.ShoeLift.GeoxShoe where

import CornerPoints.Radius(Radius(..), buildSymmetricalRadius)
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Slope(..))
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++))
import CornerPoints.Transpose(transposeZ, transposeY)
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace, extractFrontFace)
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.Degree(Degree(..))
import CornerPoints.Transposable(transpose)

import Control.Lens


import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..), (+++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))

{-
Shoe lift for the geox running shoes. Refering to the shoe tread will be done with 'geox'
The bottom tread is of a Cougar shoe. Refering to the bottom tread from the Cougar shoe is done with 'cougar'.

The lift has no gaps between the heel and the toe. It is done in 3 sections:
1: The heel, which goes from the back of the shoe, to about the center, which is the back of the flexible section requied when walking.
   Made of solid filament such as pla.
2: The center section which needs to flex while walking. This is made of ninjaflex filament.
3: The toe, which is made of solid filament such as pla.

Lift doesn't use the radial input system because it has to have a variable z-slope, so that the resulting lift requires no vertical shaping to fit.
Is input as a series of cubes running from back to front of the lift.
These cubes are split into 3 groups to correspond with the 3 printable section: solid heel, flexible middle, solid toe

Input for the cubes requires a new datatype that takes a single point and a width. There is 2 arrays of these, 1 for top faces(geox), 1 for bottom faces(cougar).
Much like the radial system, starts with a single back(top/bottom) face, which is then merged into remainder of list for front(top/bottom) faces.
Then zip together the top and bottom to create the lift.
-}


{--}
