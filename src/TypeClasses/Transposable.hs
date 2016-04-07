{-
Typeclass for changing x y and/or z values of a Point or CubePoints.
-}

module TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose,
                                TransposeWithList, transposeWithList) where



class TransposePoint a where
  transposeX :: (Double -> Double) -> a -> a
  transposeY :: (Double -> Double) -> a -> a
  transposeZ :: (Double -> Double) -> a -> a

{- |
Used to transpose a single type using a single function.
Eg: Transpose a Radius with (+3)
Eg: Transpose [Radius] in a SingleDegreeRadii by using map.
-}
class TransposeLength a where
  transpose :: (Double -> Double) -> a -> a

{- |
Transpose a [a] using a list of functions, so that each a will have it's own function.

Eg: Transpose [Radius] with [transpose(+(x))|x <- [1,2..]]
-}
class TransposeWithList a where
  transposeWithList :: [(Double -> Double)] -> [a] -> [a]
  
