module CornerPoints.Collections(CPointsCollection(..), (|+++|), (+++>)) where

{- |
Collections of CornerPoints.
Such as List, Sequence, Map. Possibly more coming.

Allows working together of the various types.
Eg: Sequence CornerPoints |+++| [CornerPoints] will result in a Sequence CornerPoints

Used anywhere that [CornerPoints] is currently used.
[CornerPoints] will be depracated once this is done.
-}

import CornerPoints.CornerPoints(CornerPoints(..), (+++))

import qualified Data.Sequence as S (Seq(..), scanl, drop, zipWith, fromList)

-- | Any type of collection containing CornerPoints.
data CPointsCollection = CPointsList {cList :: [CornerPoints]}
                       | CPointsSeq {cSeq :: (S.Seq CornerPoints)}
                       | CPointsErrors {errMessage :: String}
  deriving (Eq, Show)

{- |Add CPointsCollection to CPointsCollection.
-}
(|+++|) :: CPointsCollection -> CPointsCollection -> CPointsCollection

(CPointsList c1) |+++| (CPointsList c2) = CPointsList $ zipWith (+++) c1 c2

(CPointsSeq c1) |+++| (CPointsSeq c2) = CPointsSeq $ S.zipWith (+++) c1 c2

(CPointsSeq c1) |+++| (CPointsList c2) = (CPointsSeq c1) |+++| (CPointsSeq $ S.fromList c2)

(CPointsList c1) |+++| (CPointsSeq c2) = (CPointsSeq $ S.fromList c1) |+++| (CPointsSeq c2)


-- |Add each face to the next face, left -> right, resulting in CubePoints.
-- Ex: pass a RightFace into a CPointsCollection of LeftFaces, resulting in a CPointsCollection of CubePoints
(+++>) :: CornerPoints -> CPointsCollection -> CPointsCollection

a +++> (CPointsList bs) =
     CPointsList $ tail $ scanl (+++) a bs

a +++> (CPointsSeq bs) =
     CPointsSeq $ S.drop 1 $ S.scanl (+++) a bs
