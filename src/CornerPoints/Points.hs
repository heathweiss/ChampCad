{-# LANGUAGE DeriveDataTypeable #-}
module CornerPoints.Points (Point(..), transposeZ, calculateDistance, calculateXYDistance, Center, center ,(<-|->)) where
import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, )

import Data.Data
import Data.Typeable

import Math.Distance(Distance(..),Distant, calculateDistance)
import Math.Equal(equal)

{-------------------------- Point------------------------------
Points in 3D geometry.

Know uses:
Make up the corners of Cubes and Faces.
-}
data Point =  Point { x_axis :: Double, y_axis :: Double, z_axis :: Double } 
              deriving (Show, Typeable, Data)

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
instance Distant Point where
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

--ToDo: this is to be a fx of Distant
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

-- | Of a Point(s) or CornerPoint(s):
-- Find the center. For a line such as FrontLeftLine, this would be the middle of the line.
class Center a where
  (<-|->) :: a -> a -> Point -- ^ Takes 2 <CornerPoints/Points> and returns the center Point between them
  center :: a -> Point -- ^ Return the center of the <CornerPoint/Point>. 

instance Center Point where
  center p = p
  (Point x1 y1 z1) <-|-> (Point x2 y2 z2) =
    let
      x = (x1 + x2)/2
      y = (y1 + y2)/2
      z = (z1 + z2)/2
    in
      Point x y z
