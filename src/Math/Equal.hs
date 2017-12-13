module Math.Equal(equal) where

equal :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
equal  a b
  
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False
