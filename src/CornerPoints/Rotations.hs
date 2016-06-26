module CornerPoints.Rotations(rotateAlongYaxis90, rotateAlongYaxis180) where

import CornerPoints.CornerPoints(CornerPoints(..))

{-
Does not work.

Nothing changes when the stl is viewed. Need to re-think the logic.
May be considerably more complicated than rotating the points.
-}

rotateAlongYaxis90 :: CornerPoints -> CornerPoints
rotateAlongYaxis90 (CubePoints f1' f2' f3' f4' b1' b2' b3' b4')  =
  CubePoints f4' f1' f2' f3' b4' b1' b2' b3'

rotateAlongYaxis180 :: CornerPoints -> CornerPoints
rotateAlongYaxis180 (CubePoints f1' f2' f3' f4' b1' b2' b3' b4')  =
 rotateAlongYaxis90 $ rotateAlongYaxis90 (CubePoints f1' f2' f3' f4' b1' b2' b3' b4')
