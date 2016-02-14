module Helpers.Symmetrical.List  (mirror, mirrorPlusMidPoint, mirrorMinusMidPoint) where
import qualified Helpers.Symmetrical.Sequence as SS (mirror, mirrorPlusMidPoint, mirrorMinusMidPoint)
import qualified Data.Sequence as S
import qualified Data.Foldable as F

mirror :: [a] -> [a]
mirror list = F.toList $ SS.mirror $ S.fromList list

{- |
Reverse the first half and add to tail. Add a midpoint between the 2 halves.
-}
mirrorPlusMidPoint :: [a] -> a -> [a]
mirrorPlusMidPoint    list        midPoint =
  F.toList $ SS.mirrorPlusMidPoint (S.fromList list) midPoint


{- |
Reverse the first half and add to tail. Remove the midpoint from the second half so it is not repeated.
-}
mirrorMinusMidPoint :: [a] -> [a]
mirrorMinusMidPoint    list        =
  F.toList $ SS.mirrorMinusMidPoint (S.fromList list) 
