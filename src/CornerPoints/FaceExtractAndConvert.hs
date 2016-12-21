module CornerPoints.FaceExtractAndConvert(getFrontFaceAsBackFace, getFrontLeftLineAsBackFace, getLeftFaceAsBackFace,
                                         getFrontRightLineAsBackFace, getRightFaceAsBackFace, getBackRightLineAsBackFace,
                                         getLeftFaceAsFrontFace, getRightFaceAsFrontFace) where
import CornerPoints.FaceConversions
import CornerPoints.FaceExtraction

{- | Convenience functions combining FaceConversions and FaceExtraction. These combinations are used when doing unions and differences of shapes.
Tests are in Tests.FaceExtractAndConvertTest-}



getFrontFaceAsBackFace cornerPoint = toBackFace $ extractFrontFace cornerPoint

getFrontLeftLineAsBackFace cornerPoint = toBackFace $ extractFrontLeftLine cornerPoint

getLeftFaceAsBackFace cornerPoint = toBackFace $ extractLeftFace cornerPoint

getLeftFaceAsFrontFace cornerPoint = toFrontFace $ extractLeftFace cornerPoint

getFrontRightLineAsBackFace cornerPoint = toBackFace $ extractFrontRightLine cornerPoint

getRightFaceAsBackFace cornerPoint = toBackFace $ extractRightFace cornerPoint

getRightFaceAsFrontFace cornerPoint = toFrontFace $ extractRightFace cornerPoint

getBackRightLineAsBackFace cornerPoint = toBackFace $ extractBackRightLine cornerPoint
