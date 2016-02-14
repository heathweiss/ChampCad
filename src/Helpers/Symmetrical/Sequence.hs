{-# LANGUAGE ViewPatterns #-}
{- |
Used for building symmetrical sequences from one half of the symmetrical shape.
Eg: An oval can be described by giving the radius of the first 180 degrees, then reversing these first 180 degrees, and adding them on to the tail.
Note though, that the 180th degree would be there twice, which has to be handled. 
-}
module Helpers.Symmetrical.Sequence (mirror, mirrorPlusMidPoint, mirrorMinusMidPoint) where
import qualified Data.Sequence as S
import qualified Data.Foldable as F

{- |
Reverse the first half and add to tail. This will create a second copy of the last item.
-}
mirror :: (S.Seq a) -> (S.Seq a)
mirror seq = seq S.>< (S.reverse seq)

{- |
Reverse the first half and add to tail. Add a midpoint between the 2 halves.
-}
mirrorPlusMidPoint :: (S.Seq a) -> a -> (S.Seq a)
mirrorPlusMidPoint    seq          midPoint =
  (seq S.|> midPoint) S.>< (S.reverse seq)

{- |
Reverse the first half and add to tail. Remove the midpoint from the second half so it is not repeated.
-}
mirrorMinusMidPoint :: (S.Seq a) -> (S.Seq a)
mirrorMinusMidPoint    seq        =
  seq S.>< (S.reverse(xs seq))

xs :: (S.Seq a) -> (S.Seq a)
xs (S.viewr -> xs' S.:> x) = xs'
