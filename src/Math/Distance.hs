module Math.Distance(Distance(..), Distant, calculateDistance, getOrdering, fromDistance) where

import Math.Equal(equal)



-- | The disance between 2 Points, or 2 CornerPoints.
-- Known uses: Joiners.Delaunay
data Distance =
  Distance {_distance :: Double}
  |
  NoDistance
 deriving (Show)

fromDistance :: Distance -> Double
fromDistance (Distance d) = d
fromDistance NoDistance = 0.0

instance Eq Distance where
    Distance d == Distance d'
      |  (equal d d')  = True 
      | otherwise = False


-- | Caluculate the Distance between to objects.
-- Currently used by CornerPoints and Points.
class Distant a where
  calculateDistance :: a -> a -> Distance

-- | Compare 2 Distance for an Ordering base on underlying distance.
getOrdering :: Distance ->  Distance -> Ordering
getOrdering    (Distance d) (Distance d')
  | d > d' = GT
  | d < d' = LT
  | d == d' = EQ
