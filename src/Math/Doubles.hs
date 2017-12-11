module Math.Doubles(equal, Distance(..)) where

equal :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
equal  a b
  
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False

--ToDo: Decide if this should have it's own module.
-- | The disance between 2 Points, or 2 CornerPoints.
-- Known uses: Joiners.Delaunay
data Distance = Distance {_distance :: Double}
 deriving (Show)

instance Eq Distance where
    Distance d == Distance d'
      |  (equal d d')  = True 
      | otherwise = False

