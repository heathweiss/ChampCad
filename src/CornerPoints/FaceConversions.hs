module CornerPoints.FaceConversions(
  upperFaceFromLowerFace,
  f23LineFromF14Line,
  --f12LineFromF34Line,
  f34LineFromF12Line,
  b12LineFromF12Line,
  b34LineFromF34Line,
  frontBottomLineFromFrontTopLine,
  backTopLineFromFrontTopLine,
  lowerFaceFromUpperFace,
  backBottomLineFromBottomFrontLine,
  frontTopLineFromBackTopLine,
  bottomFrontLineFromBackBottomLine,
  backFaceFromFrontFace,
  toBackFace,
  toFrontFace,
  toFrontTopLine,
  toBottomFrontLine,
  toFrontLeftLine,
  toFrontRightLine,
  rigthtFaceFromLeftFace,
  leftFaceFromRigthFace,
  reverseNormal,
  frontFaceFromBackFace,
  
) where
import CornerPoints.CornerPoints (CornerPoints(..), (+++))


{- |
Convert faces of cube. Tests are in Tests.FaceConversionsTest and Tests.FaceExtractAndConvertTest.
Example in Examples.Diffs.FirstDiff (if it still exits).

Rules for conversions:

Conversions fall into 2 categories, adjacent and non-adjacent.

Adjacent: The original line/face is connected to the target line/face.
Eg: TopFace and FrontFace are joined through the FrontTopLine
Eg: FrontTopLine and FrontLeftLine are joined through F2

Non-adjacent: Points/Lines/Faces which are not connected
Eg: All points are non-adjacent
Eg: FrontFace and BackFace
Eg: TopFrontLine and BackTopLine

Adjacent joins:
The line/face gets rotated through the commont point/line
Eg: FrontFace to a TopFace:
    FrontFace:TopFrontLine gets rotated to the TopFace:BackTopLine
    FrontFace:BottomFrontLine gets rotated to the TopFace:FrontTopLine
Eg: TopFrontLine to FrontLeftLine:
    TopFrontLine:F2 to FrontLeftLine:F1
    TopFrontLine:F3 to FrontLeftLine:F2

Non-adjacent joins:
The line/face gets pushed straight accross.
At this point it will be:
1: done
Eg: All points are non-adjecent and simply get re-assigned. Such as F1 becomes F2.
Eg: FrontFace to a BackFace. F1 to B1; F2 to B2 ...

2: adjacent; Follow adjacent rules.

3: still non-adjacent
Eg: TopFrontLine to BackBottomLine;
    1st push through to BackTopLine. F2 to B2; F3 to B3
    Then push to BackBottomLine; B2 to B1; B3 to B4

Eg: TopFrontLine to BackLeftLine:
    Push through to BackTopLine; F2 to B2; F3 to B3
    BackTopLine is adjacent to BackLeftLine, so follow adjacent rules.


That being said: Many of these functions were created before these rules and may not obey.
They will be changed in the future.

           

-}
{-alternative conversions system considered.
Adjacent joins:
The common point/line which connects them, will remain unchanged.
Eg: TopFace and FrontFace are connected through the FrontTopLine.
    The FrontTopLine will remain the same. The FrontFace would have it's BottomFrontLine
    set to the BackTopLine of the TopFace
Eg: FrontTopLine and LeftFrontLine are connected with through F2.
    F2 will remain the same with the FrontTopLine F3 becoming the F1 fo the LeftFrontLine

-}
-- ToDo: Write more tests.
-- ToDo: Make sure all comply with conversion rules.

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
--ToDo: Do not comply with rules. Used in MTLDiff example.
toBackFace :: CornerPoints -> CornerPoints
toBackFace (RightFace b3 b4 f3 f4) = BackFace b4 b3  f3 f4 
toBackFace (LeftFace b1 b2 f1 f2) = BackFace f1 f2 b2 b1
toBackFace (FrontRightLine f3 f4) = BackFace f4 f3 f3 f4
toBackFace (FrontLeftLine f1 f2) = BackFace f1 f2 f2 f1
--toBackFace (FrontFace f1 f2 f3 f4) = BackFace f4 f3 f2 f1
toBackFace (FrontFace f1 f2 f3 f4) = BackFace f1 f2 f3 f4
toBackFace (BackRightLine b3 b4) = BackFace b4 b3 b3 b4
toBackFace (BackLeftLine b1 b2) = BackFace b1 b2 b2 b1

{- Before changing to comply with rules.
toBackFace :: CornerPoints -> CornerPoints
toBackFace (RightFace b3 b4 f3 f4) = BackFace b4 b3  f3 f4 
toBackFace (LeftFace b1 b2 f1 f2) = BackFace f1 f2 b2 b1
toBackFace (FrontRightLine f3 f4) = BackFace f4 f3 f3 f4
toBackFace (FrontLeftLine f1 f2) = BackFace f1 f2 f2 f1
toBackFace (FrontFace f1 f2 f3 f4) = BackFace f4 f3 f2 f1
toBackFace (BackRightLine b3 b4) = BackFace b4 b3 b3 b4
toBackFace (BackLeftLine b1 b2) = BackFace b1 b2 b2 b1
-}

-- ToDo: Finish pattern matches. Test

toBottomFrontLine :: CornerPoints -> CornerPoints
toBottomFrontLine (F2 f2) = BottomFrontLine f2 f2
--before complying to rules
--toBottomFrontLine (FrontTopLine f2 f3) = BottomFrontLine  f3 f2
--now complies
toBottomFrontLine (FrontTopLine f2 f3)   = BottomFrontLine  f2 f3
toBottomFrontLine (FrontRightLine f3 f4) = BottomFrontLine f4 f3
toBottomFrontLine (F4 f4)                = BottomFrontLine f4 f4
toBottomFrontLine (F3 f3)                = BottomFrontLine f3 f3
toBottomFrontLine (F1 f1)                = BottomFrontLine f1 f1
toBottomFrontLine (FrontLeftLine f1 f2)  = BottomFrontLine f2 f1

-- ToDo: Finish pattern matches. Test
toFrontTopLine :: CornerPoints -> CornerPoints
toFrontTopLine (F4 f4) = FrontTopLine f4 f4
toFrontTopLine (F3 f3) = FrontTopLine f3 f3
toFrontTopLine (FrontRightLine f3 f4) = FrontTopLine f3 f4
toFrontTopLine (F1 f1) = FrontTopLine f1 f1
toFrontTopLine (F2 f2) = FrontTopLine f2 f2
toFrontTopLine (FrontLeftLine f1 f2) = FrontTopLine f1 f2


--before rules
--toFrontTopLine (BottomFrontLine f1 f4) = FrontTopLine  f4 f1
--complies to rules
toFrontTopLine (BottomFrontLine f1 f4) = FrontTopLine  f1 f4

toFrontFace :: CornerPoints -> CornerPoints
toFrontFace(RightFace b3 b4 f3 f4) = FrontFace f4 f3 b3 b4
toFrontFace(LeftFace b1 b2 f1 f2) = FrontFace b1 b2 f2 f1
toFrontFace (FrontLeftLine f1 f2) = FrontFace f1 f2 f2 f1
toFrontFace (FrontRightLine f3 f4) = FrontFace f4 f3 f3 f4

toFrontLeftLine :: CornerPoints -> CornerPoints
toFrontLeftLine (FrontRightLine f3 f4) = FrontLeftLine f4 f3

toFrontRightLine :: CornerPoints -> CornerPoints
toFrontRightLine (F4 f4) = FrontRightLine f4 f4

f23LineFromF14Line :: CornerPoints -> CornerPoints
f23LineFromF14Line (BottomFrontLine f1 f4) = FrontTopLine f1 f4

{- replaced by toFrontLeftLine
f12LineFromF34Line :: CornerPoints -> CornerPoints
f12LineFromF34Line (FrontRightLine f3 f4) = FrontLeftLine f4 f3
-}
f34LineFromF12Line :: CornerPoints -> CornerPoints
f34LineFromF12Line (FrontLeftLine f1 f2) = FrontRightLine f2 f1

b12LineFromF12Line :: CornerPoints -> CornerPoints
b12LineFromF12Line (FrontLeftLine f1 f2) = BackLeftLine f1 f2

b34LineFromF34Line :: CornerPoints -> CornerPoints
b34LineFromF34Line (FrontRightLine f3 f4) = BackRightLine f3 f4

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
reverseNormal (BottomFrontLine f1 f4) = (BottomFrontLine f4 f1)
reverseNormal (FrontRightLine f3 f4)  = (FrontRightLine f4 f3)
reverseNormal (FrontLeftLine f1 f2)   = FrontLeftLine f2 f1
reverseNormal (FrontTopLine f2 f3)    = FrontTopLine f3 f2
