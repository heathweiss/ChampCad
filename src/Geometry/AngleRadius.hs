{- |
Combine an Angle and a Radius in a single datatype.
Makes it easier to manipulate them, as opposed to when they are in separate lists.
Afterwards the [Angle] [Radius] can be extracted using extract<Angle><Radius> functions,
so that they can be used in the normal manner to generate [CornerPoints] through typical fucntions
such as createBottomFaces or cylinder.

Known uses:
Expand shorter lists of [Radius] [Angle] so they can be matched up with a longer set of lists.
For instance, adding the radii from an octagon, to the 36 angles from a typical scan, would
require using filler values in the octagon list.
An example of this is in: Examples.Scan.OpenBionicsDotComDesignWork

-}
module Geometry.AngleRadius(AngleRadius(..), extractAngles, extractRadii,) where

import Geometry.Angle(Angle(..))
import CornerPoints.Radius(Radius(..))

data AngleRadius = AngleRadius
                       { posDegree :: Angle,
                         posRadius :: Radius
                       }

instance Show AngleRadius where
  show (AngleRadius posDegree' posRadius') = (show posDegree') ++ " : " ++ (show posRadius')

{- | Extract the [Angle] from a [AngleRadius]-}
extractAngles :: [AngleRadius] -> [Angle]
extractAngles anglesRadii =
  extractAngles' (reverse anglesRadii) []

extractAngles' :: [AngleRadius] -> [Angle] -> [Angle]
extractAngles' (x:[]) angles =
  posDegree x : angles

extractAngles' (x:xs) angles =
  extractAngles' xs (posDegree x : angles)

{- | Extract the [Radius] from a [AngleRadius]-}
extractRadii :: [AngleRadius] -> [Radius]
extractRadii anglesRadii =
  extractRadii' (reverse anglesRadii) []

extractRadii' :: [AngleRadius] -> [Radius] -> [Radius]
extractRadii'(x:[]) radii =
  posRadius x : radii

extractRadii' (x:xs) radii =
  extractRadii' xs (posRadius x : radii)

