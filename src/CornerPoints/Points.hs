
module CornerPoints.Points (Point(..), transposeZ, calculateDistance, calculateXYDistance) where
import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, )

import Math.Doubles(equal, Distance(..))
{-------------------------- Point------------------------------
Points in 3D geometry.

Know uses:
Make up the corners of Cubes and Faces.
-}
data Point =  Point { x_axis :: Double, y_axis :: Double, z_axis :: Double } 
              deriving (Show)

{-
	Front view of cube:
	     2 ---------- 3
	       |		|	
	       |		|	x-axis ---->
	     1 ---------- 4 y-axis: coming toward you.
	     				z-axis: ^
	     						|
	     f: front of cube
	     b: back of cube
	Now each corner can be named with corner # and f (front) or b (back)
-}

{----------------       instance of equal ---------------
In order to avoid double rounding errors and  trig errors which cause
the same point, and thus CornerPoints, to be != due to tiny differences,
give it a range of .01, and still allow the points to be equal.

All the type restrictions are to get it to compile.
-}
{-
axisEqual :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
axisEqual  a b
  
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False
-}
instance Eq Point where
    Point x y z == Point xa ya za
      |  (equal x xa) && (equal y ya)  && (equal z za) = True 
      | otherwise = False


instance TransposePoint Point where
  transposeZ f (Point x y z) = Point x y (f z)
  transposeX f (Point x y z) = Point (f x) y z
  transposeY f (Point x y z) = Point x (f y) z

   
-- | Given a 2 points, caluclate the distance between the points.
calculateDistance :: Point -> Point -> Distance
calculateDistance    point1   point2  =
  let
      distance :: Point -> Point -> Point
      distance    (Point x y z)    (Point x1 y1 z1) =
        --Point (abs $ x - x1) (abs $ y - y1) (abs $ z - z1)
        Point (x - x1) (y - y1) (z - z1)
      p = distance point1 point2
      x = x_axis p
      y = y_axis p
      z = z_axis p
  in 
      Distance $ sqrt (x**2 + y**2 + z**2)

calculateXYDistance :: Point -> Point -> Distance
calculateXYDistance    point1   point2  =
  let
      distance :: Point -> Point -> Point
      distance    (Point x y z)    (Point x1 y1 z1) =
        Point (x - x1) (y - y1) (z - z1)
      p = distance point1 point2
      x = x_axis p
      y = y_axis p
      z = z_axis p
  in
      Distance $ sqrt (x**2 + y**2)  
