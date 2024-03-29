module Builder.Map({-cornerPointsMap-} extractMaybeCube, cornerPointsWithDegreesMap) where
--import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..), newCornerPointsWithDegreesList )
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..))
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..))

import qualified Data.Map as M
{- |
Build up complex shapes using Kleisli arrows, maps, and CornerPointsWithDegrees.
-}



{-Extract the CornerPoint from the map Maybe result. Gives CornerPointsError on Nothing -}
extractMaybeCube :: Maybe CornerPoints ->  CornerPoints
extractMaybeCube (Just a) = a
extractMaybeCube Nothing = CornerPointsError "error" -- F1 (Point 1 2 3)

{- |
Create a map with:
key:: DegreeRange
value:: CornerPoints
-}
cornerPointsWithDegreesMap :: [CornerPointsWithDegrees] -> M.Map DegreeRange CornerPoints
cornerPointsWithDegreesMap cornerPointsWithDegrees =
  let {-create a tuple  from [CornerPointsWithDegrees], required to make a map with:
        key: DegreeRange
        value: CornerPoints-}
        getKeyValueTuplesFrom :: [CornerPointsWithDegrees] -> [(CornerPoints.CornerPointsWithDegrees.DegreeRange, CornerPoints)]
        getKeyValueTuplesFrom = map (\(CubesWithStartEndDegrees cube degreeRange) -> (degreeRange,cube))

  in  M.fromList $ getKeyValueTuplesFrom  cornerPointsWithDegrees

