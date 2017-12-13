module Math.Distance(Distance(..), Distant, calculateDistance, getOrdering) where

import Math.Equal(equal)



--ToDo: Decide if this should have it's own module.
-- | The disance between 2 Points, or 2 CornerPoints.
-- Known uses: Joiners.Delaunay
data Distance = Distance {_distance :: Double}
 deriving (Show)

instance Eq Distance where
    Distance d == Distance d'
      |  (equal d d')  = True 
      | otherwise = False


class Distant a where
  calculateDistance :: a -> a -> Distance


getOrdering :: Distance ->  Distance -> Ordering
getOrdering    (Distance d) (Distance d')
  | d > d' = GT
  | d < d' = LT
  | d == d' = EQ
