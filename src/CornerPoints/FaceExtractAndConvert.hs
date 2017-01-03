module CornerPoints.FaceExtractAndConvert(getFrontFaceAsBackFace, getFrontLeftLineAsBackFace, getLeftFaceAsBackFace,
                                         getFrontRightLineAsBackFace, getRightFaceAsBackFace, getBackRightLineAsBackFace,
                                         getLeftFaceAsFrontFace, getRightFaceAsFrontFace, faceFromF12Line, faceFromF34Line) where
import CornerPoints.FaceConversions
import CornerPoints.FaceExtraction
import CornerPoints.CornerPoints(CornerPoints(..), (+++))

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

faceFromF12Line :: CornerPoints-> CornerPoints
faceFromF12Line (FrontFace f1 f2 f3 f4) =
  let frontFace = FrontFace f1 f2 f3 f4
  in  (extractFrontLeftLine  frontFace)
        +++
      (f34LineFromF12Line $ extractFrontLeftLine frontFace )
faceFromF12Line (LeftFace b1 b2 f3 f4) =
  let leftFace = LeftFace b1 b2 f3 f4
      frontLeftLine = extractFrontLeftLine  leftFace
  in  frontLeftLine
      +++
      (b12LineFromF12Line frontLeftLine)
faceFromF12Line invalidFace =      
      CornerPointsError "invalid cube supplied to faceFromF12Line"


faceFromF34Line :: CornerPoints-> CornerPoints
faceFromF34Line (FrontFace f1 f2 f3 f4) =
  let frontFace = FrontFace f1 f2 f3 f4
      frontRightLine = extractFrontRightLine frontFace
  in  (frontRightLine)
      +++
      (f12LineFromF34Line frontRightLine)
faceFromF34Line (RightFace b3 b4 f3 f4) =
  let rightFace = RightFace b3 b4 f3 f4
      frontRightLine = extractFrontRightLine rightFace
  in  frontRightLine
      +++
      (b34LineFromF34Line frontRightLine )
faceFromF34Line invalidFace =      
      CornerPointsError "invalid cube supplied to faceFromF34Line"
