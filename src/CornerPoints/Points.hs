{-# LANGUAGE DeriveDataTypeable #-}
module CornerPoints.Points (Point(..), transposeZ) where
import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, )

import Data.Data
import Data.Typeable

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H

--import Math.Distance(Distance(..),Distant, calculateDistance, DistanceA(..),DistantA, calculateDistanceA)
import Math.Equal(equal)

{-------------------------- Point------------------------------
Points in 3D geometry.

Know uses:
Make up the corners of Cubes and Faces.
-}
data Point =  Point { x_axis :: Double, y_axis :: Double, z_axis :: Double }
           |  Line {_p1 :: Point, _p2 :: Point}
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
    Line p1 p2 == Line q1 q2
      | (p1 == q1) && (p2 == q2) = True
      | (p1 == q2) && (p2 == q2) = True
      | otherwise = False
    Line _ _ == Point _ _ _ = False
    Point _ _ _ == Line _ _ = False


instance TransposePoint Point where
  transposeZ f (Point x y z) = Point x y (f z)
  transposeX f (Point x y z) = Point (f x) y z
  transposeY f (Point x y z) = Point x (f y) z



-- | Make CornerPoints.Point an instance of Hashable so it can be  used with Data.HashMap.Strict
-- Known uses:
-- Associate CornerPoints.Point with an Id::Int for generating GMSH script.
-- Only 'hash' is being used so far.
instance H.Hashable Point where
    hashWithSalt s (Point x y z) =
        s `H.hashWithSalt`
        x `H.hashWithSalt`
        y `H.hashWithSalt` z
    hash point =
      1 `H.hashWithSalt` point
