module CornerPoints.Points (Point(..), transposeZ) where
import CornerPoints.Transposable(TransposePoint, transposeX, transposeY, transposeZ)
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
axisEqual :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
axisEqual  a b
  
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False

instance Eq Point where
    Point x y z == Point xa ya za
      |  (axisEqual x xa) && (axisEqual y ya)  &&(axisEqual z za) = True 
      | otherwise = False


instance TransposePoint Point where
  transposeZ f (Point x y z) = Point x y (f z)
  transposeX f (Point x y z) = Point (f x) y z
  transposeY f (Point x y z) = Point x (f y) z

   
