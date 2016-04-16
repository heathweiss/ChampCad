module CornerPoints.FaceConversions(
  upperFaceFromLowerFace,
  f23LineFromF14Line,
  frontBottomLineFromFrontTopLine,
  backTopLineFromFrontTopLine,
  lowerFaceFromUpperFace,
  backBottomLineFromBottomFrontLine,
  frontTopLineFromBackTopLine,
  bottomFrontLineFromBackBottomLine,
    backFaceFromFrontFace,
  toBackFace,
  toFrontFace,
  rigthtFaceFromLeftFace,
  leftFaceFromRigthFace,
  reverseNormal,
  frontFaceFromBackFace,
  
) where
import CornerPoints.CornerPoints (CornerPoints(..))

{- | Convert faces of cube. Tests are in Tests.FaceConversionsTest and Tests.FaceExtractAndConvertTest.
     Example in Examples.Diffs.FirstDiff (if it still exits).

     Used for adding cubes together.
     Used to embed a cube inside 1 or more other cubes.
-}


{- |Converts BottomFace to TopFace. Reverses normal so it no longer faces outwards from the original cube.
Used to add cubes together.
EG: To extend a cube by adding another FrontFace to it. First get the existing FrontFace, turn it into a BackFace
with the normals inverted, the add the new FrontFace to it.-}
upperFaceFromLowerFace:: CornerPoints -> CornerPoints
upperFaceFromLowerFace (BottomFace b1 f1 b4 f4) = TopFace b1 f1 b4 f4

{- |Converts BackFace to FrontFace. Reverses normal so it no longer faces outwards from the original cube.
Used to add cubes together.
EG: To extend a cube by adding another BackFace to it. First get the existing BackFace, turn it into a FrontFace
with the normals inverted, the add the new BackFace to it.-}
backFaceFromFrontFace :: CornerPoints -> CornerPoints
backFaceFromFrontFace (FrontFace f1 f2 f3 f4) = BackFace f1 f2 f3 f4

leftFaceFromRigthFace :: CornerPoints -> CornerPoints
leftFaceFromRigthFace (RightFace b3 b4 f3 f4)  =  LeftFace b4 b3 f4 f3

rigthtFaceFromLeftFace :: CornerPoints -> CornerPoints
rigthtFaceFromLeftFace (LeftFace b1 b2 f1 f2) = RightFace b2 b1 f2 f1

{- |
Rotate the face or line to the back of cube. Maintain normals so they still face outwards from the original cube..
Lines get rotated, then copied to form the 2 lines required to make the back face.

Lines will result in a backFace with either no width or no height. So it is a BackFace squeezed into a single line.
Typically used for the back face of a radial shape, where all back faces represent a single point at the origin
of the shape.
-}
toBackFace :: CornerPoints -> CornerPoints
toBackFace (RightFace b3 b4 f3 f4) = BackFace f4 f3 b3 b4
toBackFace (LeftFace b1 b2 f1 f2) = BackFace f1 f2 b2 b1
toBackFace (FrontRightLine f3 f4) = BackFace f4 f3 f3 f4
toBackFace (FrontLeftLine f1 f2) = BackFace f1 f2 f2 f1
toBackFace (FrontFace f1 f2 f3 f4) = BackFace f4 f3 f2 f1
toBackFace (BackRightLine b3 b4) = BackFace b4 b3 b3 b4


toFrontFace(RightFace b3 b4 f3 f4) = FrontFace b4 f3 b3 b4

f23LineFromF14Line :: CornerPoints -> CornerPoints
f23LineFromF14Line (BottomFrontLine f1 f4) = FrontTopLine f1 f4

frontBottomLineFromFrontTopLine :: CornerPoints -> CornerPoints
frontBottomLineFromFrontTopLine (FrontTopLine f2 f3) = BottomFrontLine f2 f3

backTopLineFromFrontTopLine :: CornerPoints -> CornerPoints
backTopLineFromFrontTopLine (FrontTopLine f2 f3) = BackTopLine f2 f3

lowerFaceFromUpperFace :: CornerPoints -> CornerPoints
lowerFaceFromUpperFace (TopFace b2 f2 b3 f3) = BottomFace b2 f2 b3 f3

frontFaceFromBackFace :: CornerPoints -> CornerPoints
frontFaceFromBackFace (BackFace b1 b2 b3 b4) = FrontFace b1 b2 b3 b4

backBottomLineFromBottomFrontLine :: CornerPoints -> CornerPoints
backBottomLineFromBottomFrontLine (BottomFrontLine f1 f4) = BackBottomLine f1 f4

frontTopLineFromBackTopLine :: CornerPoints -> CornerPoints
frontTopLineFromBackTopLine (BackTopLine b2 b3) = FrontTopLine b2 b3

bottomFrontLineFromBackBottomLine :: CornerPoints -> CornerPoints
bottomFrontLineFromBackBottomLine (BackBottomLine b1 b4) = BottomFrontLine b1 b4

{- |Reverse the corners so that it faces the opposite way.-}
reverseNormal :: CornerPoints -> CornerPoints
reverseNormal (FrontFace f1 f2 f3 f4) = FrontFace f4 f3 f2 f1
reverseNormal (BackFace b1 b2 b3 b4)  = BackFace b4 b3 b2 b1

