{- |Wrapper around CornerPoints.FaceExtractions, to work on CornerPointsWithDegrees.-}
module CornerPoints.FaceExtractionWithDegrees (extractFrontFace) where

import qualified CornerPoints.FaceExtraction as FaceEx (extractFrontFace)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), DegreeRange(..))

{- |Extract a FrontFace from a CubesWithStartEndDegrees -}
extractFrontFace :: CornerPointsWithDegrees -> CornerPointsWithDegrees
extractFrontFace (CubesWithStartEndDegrees cube' (DegreeRange startDegree' endDegree')) =
   FrontFaceWithStartEndDegrees  (FaceEx.extractFrontFace cube') (DegreeRange startDegree' endDegree')




