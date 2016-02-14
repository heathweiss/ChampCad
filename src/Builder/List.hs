{- |Build up a shape using lists. Should be deleted once Builder.Sequence is done?-}
module Builder.List((&@~+++@), (&@~+++#@), (||@~+++^||), newCornerPointsWith10DegreesBuilder) where


import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..),  (@~+++#@), (|@~+++#@|), (|@~+++@|),
                                           newCornerPointsWith10DegreesList)
import CornerPoints.CornerPoints(CornerPoints(..))

import Stl.StlBase(Triangle(..))
import Stl.StlCornerPoints((+++^), Faces(..))
import Stl.StlCornerPointsWithDegrees( (|@~?+++^|), FacesWithRange(..),  {-(||@~?+++^||)-}cornerPointsWithDegreesToTriangles)

--add a CornerPoint to the head of the [[CornerPointsWithDegrees]] in list fashion
(&@~+++@) :: [[CornerPointsWithDegrees]] -> [CornerPoints] -> [[CornerPointsWithDegrees]]
cornerPointsWithDegreesListList &@~+++@ cornerPointsList =
   ((head cornerPointsWithDegreesListList )  |@~+++@| cornerPointsList) : cornerPointsWithDegreesListList


(&@~+++#@) :: [[CornerPointsWithDegrees]] -> (CornerPoints -> CornerPoints) -> [[CornerPointsWithDegrees]]
cornerPointsWithDegreesListList &@~+++#@ f =
  ((head cornerPointsWithDegreesListList ) |@~+++#@| f)  : cornerPointsWithDegreesListList




{- | Process a shape made up of [[CornerPointsWithDegrees]] into stl [Triangle]'s for output to stl file. -}
--ToDo: create module Stl.StlCornerPointsWithDegreesList and move this there.
(||@~+++^||) :: [[CornerPointsWithDegrees]] -> [[FacesWithRange]] -> [Triangle]
cornerPointsWithDegreesList ||@~+++^|| facesWithRangeList = concat $
  zipWith cornerPointsWithDegreesToTriangles cornerPointsWithDegreesList facesWithRangeList

{- |
Used by numerous infix functions such as (&@~+++#@) for building up a [[CornerPointsWithDegrees]].
Each layer of a stl shape is made up of [CornerPointsWithDegrees].
This Builder allows these layer to be built up, by adding another top/bottome face to the top of the
Builder list.

The 10 indicates it is based on a 10 degree spread of the radial shape.
Eg: A scan that is taken at 10 degree intervals such as 0,10..360
-}
newCornerPointsWith10DegreesBuilder :: [CornerPoints] -> [[CornerPointsWithDegrees]]
newCornerPointsWith10DegreesBuilder    cornerPoints   = [newCornerPointsWith10DegreesList cornerPoints]
