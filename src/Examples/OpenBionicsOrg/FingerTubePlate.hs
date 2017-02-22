module Examples.OpenBionicsOrg.FingerTubePlate() where



import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++>))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.MeshGeneration(autoGenerateEachCube)


import Stl.StlCornerPoints((|+++^|), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Control.Lens

import Control.Monad.State.Lazy
import Control.Monad.Except

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList)


{- |
Recreate the FingerTubePlate from openBionics.org as they did not generate the stl for them.

-}
