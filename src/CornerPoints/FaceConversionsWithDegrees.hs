{- |Wrapper around CornerPoints.FaceConversions, to work on CornerPointsWithDegrees.-}
module CornerPoints.FaceConversionsWithDegrees (backFaceFromFrontFace, upperFaceFromLowerFace) where 
import qualified CornerPoints.FaceConversions as FaceEx (backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), DegreeRange(..))

{- |Convert a FrontFaceWithStartEndDegrees to a BackFaceWithStartEndDegrees -}
--not tested
backFaceFromFrontFace :: CornerPointsWithDegrees -> CornerPointsWithDegrees
backFaceFromFrontFace (FrontFaceWithStartEndDegrees cube' (DegreeRange startDegree' endDegree')) =
  BackFaceWithStartEndDegrees (FaceEx.backFaceFromFrontFace cube')  (DegreeRange startDegree' endDegree')

--not tested
upperFaceFromLowerFace :: CornerPointsWithDegrees -> CornerPointsWithDegrees
upperFaceFromLowerFace (BottomFacesWithDegrees cube' (DegreeRange startDegree' endDegree')) =
  TopFacesWithDegrees (FaceEx.upperFaceFromLowerFace cube')  (DegreeRange startDegree' endDegree')
