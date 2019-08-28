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
  toB2, toB3, toF2, toF3,
  toBackFace,
  toFrontFace,
  toFrontTopLine,
  toBottomFrontLine,
  toFrontLeftLine,
  toFrontRightLine,
  toBackBottomLine,
  toBackTopLine,
  rigthtFaceFromLeftFace,
  leftFaceFromRigthFace,
  reverseNormal,
  frontFaceFromBackFace,
  toBottomFace,
  toBackRightLine,
  toTopFace,
  toLeftFace,
  toBottomLeftLine,
  toTopRightLine,
  raisedTo,
) where
import CornerPoints.CornerPoints (CornerPoints(..), (+++))
import qualified TypeClasses.Showable as TS

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
toB3 :: CornerPoints -> CornerPoints
toB3 (F3 f3) = B3 f3
toB3 (F2 f2) = B3 f2
toB3 (B2 b2) = B3 b2

toB2 :: CornerPoints -> CornerPoints
toB2 (F2 f2) = B2 f2
toB2 (B3 b3) = B2 b3
toB2 (F3 f3) = B2 f3

toF2 :: CornerPoints -> CornerPoints
toF2 (F3 f3) = F2 f3

toF3 (F2 f2) = F3 f2

toBackFace :: CornerPoints -> CornerPoints
toBackFace (RightFace b3 b4 f3 f4) = BackFace b4 b3  f3 f4 
toBackFace (LeftFace b1 b2 f1 f2) = BackFace f1 f2 b2 b1
toBackFace (FrontRightLine f3 f4) = BackFace f4 f3 f3 f4
toBackFace (FrontLeftLine f1 f2) = BackFace f1 f2 f2 f1
--toBackFace (FrontFace f1 f2 f3 f4) = BackFace f4 f3 f2 f1
toBackFace (FrontFace f1 f2 f3 f4) = BackFace f1 f2 f3 f4
toBackFace (BackRightLine b3 b4) = BackFace b4 b3 b3 b4
toBackFace (BackLeftLine b1 b2) = BackFace b1 b2 b2 b1

toLeftFace :: CornerPoints -> CornerPoints
toLeftFace (RightFace b3 b4 f3 f4) = LeftFace b4 b3 f4 f3

toTopFace :: CornerPoints -> CornerPoints
toTopFace (BottomFace b1 f1 b4 f4) = TopFace b1 f1 b4 f4

toBottomFace :: CornerPoints -> CornerPoints
toBottomFace (TopFace b2 f2 b3 f3) = BottomFace b2 f2 b3 f3

toBackBottomLine :: CornerPoints -> CornerPoints
toBackBottomLine (BackBottomLine b1 b4) = (BackBottomLine b1 b4)
toBackBottomLine (B2 b2) = BackBottomLine b2 b2
toBackBottomLine (BackTopLine b2 b3) = BackBottomLine b2 b3
toBackBottomLine (BackRightLine b3 b4) = BackBottomLine b4 b3
toBackBottomLine (B3 b3) = BackBottomLine b3 b3
toBackBottomLine (B4 b4) = BackBottomLine b4 b4
toBackBottomLine (B1 b1) = BackBottomLine b1 b1
toBackBottomLine (BackLeftLine b1 b2) = BackBottomLine b2 b1
toBackBottomLine (BottomFrontLine f1 f4) = BackBottomLine f1 f4

toBackTopLine :: CornerPoints -> CornerPoints
toBackTopLine (BackTopLine b2 b3) = BackTopLine b2 b3
toBackTopLine (B3 b3) = BackTopLine b3 b3
toBackTopLine (BackRightLine b3 b4) = BackTopLine b3 b4
toBackTopLine (B4 b4) = BackTopLine b4 b4
toBackTopLine (BackBottomLine b1 b4) = BackTopLine b1 b4
toBackTopLine (B1 b1) = BackTopLine b1 b1
toBackTopLine (BackLeftLine b1 b2) = BackTopLine b1 b2
toBackTopLine (B2 b2) = BackTopLine b2 b2
toBackTopLine (FrontTopLine f2 f3) = BackTopLine f2 f3

toBackRightLine :: CornerPoints -> CornerPoints
toBackRightLine (BackLeftLine b1 b2) = BackRightLine b2 b1

-- ToDo: Finish pattern matches. Test
toBottomFrontLine :: CornerPoints -> CornerPoints
toBottomFrontLine (F2 f2) = BottomFrontLine f2 f2
--before complying to rules
--toBottomFrontLine (FrontTopLine f2 f3) = BottomFrontLine  f3 f2
--now complies
toBottomFrontLine (FrontTopLine f2 f3)    = BottomFrontLine  f2 f3
toBottomFrontLine (FrontRightLine f3 f4)  = BottomFrontLine f4 f3
toBottomFrontLine (F4 f4)                 = BottomFrontLine f4 f4
toBottomFrontLine (F3 f3)                 = BottomFrontLine f3 f3
toBottomFrontLine (F1 f1)                 = BottomFrontLine f1 f1
toBottomFrontLine (FrontLeftLine f1 f2)   = BottomFrontLine f2 f1
toBottomFrontLine (BottomFrontLine f1 f4) = BottomFrontLine f1 f4
toBottomFrontLine (B2 b2)                 = BottomFrontLine b2 b2
toBottomFrontLine (BackBottomLine b1 b4)  = BottomFrontLine b1 b4
toBottomFrontLine cpoint = CornerPointsError $ "FaceConversions.toBottomFrontLine: unhandled or illegal pattern match for " ++ (TS.showConstructor cpoint)

toBottomLeftLine :: CornerPoints -> CornerPoints
toBottomLeftLine (BottomRightLine b4 f4) = BottomLeftLine b4 f4
toBottomLeftLine cpoint = CornerPointsError $ "FaceConversions.toBottomLeftLine: unhandled or illegal pattern match for "  ++ (TS.showConstructor cpoint)


-- ToDo: Finish pattern matches. Test
toFrontTopLine :: CornerPoints -> CornerPoints
toFrontTopLine (F4 f4) = FrontTopLine f4 f4
toFrontTopLine (F3 f3) = FrontTopLine f3 f3
toFrontTopLine (FrontRightLine f3 f4) = FrontTopLine f3 f4
toFrontTopLine (F1 f1) = FrontTopLine f1 f1
toFrontTopLine (F2 f2) = FrontTopLine f2 f2
toFrontTopLine (FrontLeftLine f1 f2) = FrontTopLine f1 f2
toFrontTopLine (FrontTopLine f2 f3)  = FrontTopLine f2 f3
toFrontTopLine (BackTopLine b2 b3) = FrontTopLine b2 b3


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
toFrontRightLine (FrontLeftLine f1 f2) = FrontRightLine f2 f1

toTopRightLine :: CornerPoints -> CornerPoints
toTopRightLine (TopLeftLine b2 f2) = TopRightLine b2 f2
toTopRightLine missingPatternMatch = CornerPointsError "FaceConversions.toTopRightLine: missing pattern match"

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
reverseNormal (BackBottomLine b1 b4)  = (BackBottomLine b4 b1) 
reverseNormal (BackTopLine b2 b3)     = (BackTopLine b3 b2)
reverseNormal (BottomLeftLine b1 f1)  = (BottomLeftLine f1 b1)
reverseNormal (TopLeftLine b2 f2)     = (TopLeftLine f2 b2)


{------------------------------------------------------rasiedTo-----------------------------------------------------------------
Used by Joiners.Delaunay to create the advancing Cpoints.

Type of non-standared +++ in that it builds Cpoints in unexpected way.
EG:
Adding a F1 to a BottomLeftLine(BLL) will create a new BLL using the new F1 and the B1 from the
original BLL.
Compare this to +++ which would have created new BLL by adding the F1 to the F1(used as a B1) from the BLL.

The 1st parameter(raisee) is the Cpoint which will be raised.
The 2nd param(raiser) is the Cpoint which is raising.
So if raiser is a BottomLeftLine, it will raise the raiser to a BottomLeftLine.

The logic is based on standard radial system of moving clockwise, eg: B4 +++> [B1,B1]

Will return a Left is it is not a valid raise, such a raising a B1 to a CubePoints.
-}
raisedTo :: CornerPoints -> CornerPoints -> Either String CornerPoints

raisedTo (FrontLeftLine f1' f2') ( LeftFace b1 b2 f1 f2) = Right $ LeftFace b1 b2 f1' f2'

raisedTo (BackLeftLine b1' b2') ( LeftFace b1 b2 f1 f2) = Right $ LeftFace b1' b2' f1 f2

raisedTo (B1 b1') (BottomLeftLine b1 f1) = Right $ BottomLeftLine b1' f1

raisedTo (B1 b1) (BottomRightLine b4 f4) = Right $ BottomLeftLine b1 f4

raisedTo (B2 b2) (TopRightLine b3 f3) = Right $ TopLeftLine b2 f3

raisedTo (B2 b2') (TopLeftLine b2 f2) = Right $ TopLeftLine b2' f2

raisedTo (B4 b4) (BottomLeftLine b1 f1) = Right $ BottomLeftLine b4 f1

raisedTo (F1 f1') (BottomLeftLine b1 f1) = Right $ BottomLeftLine b1 f1'

raisedTo (F2 f2') (TopLeftLine b2 f2) = Right $ TopLeftLine b2 f2'

--is this what i need
raisedTo (B1 b1) (F1 f1) = Right $ BottomLeftLine b1 f1


raisedTo (F1 f1) (BottomRightLine b4 f4) = Right $ BottomLeftLine b4 f1

raisedTo (F2 f2) (TopRightLine b3 f3) = Right $ TopLeftLine b3 f2

raisedTo (B4 b4) (F4 f4) = Right $  BottomRightLine b4 f4

raisedTo (BackLeftLine b1 b2) (RightFace b3 b4 f3 f4) = Right $ LeftFace b1 b2 f4 f3
                                                                        
raisedTo (FrontLeftLine f1 f2) (RightFace b3 b4 f3 f4) = Right $ LeftFace b4 b3 f1 f2

raisedTo (BackRightLine b3 b4) ( LeftFace b1 b2 f1 f2) = Right $ LeftFace b4 b3 f1 f2

raisedTo (B4 b4') (BottomRightLine b4 f4) =
  Right $ BottomLeftLine b4' f4

--raisedTo (F1 f1) (FrontRightLine f3 f4)

--not sure about this!!!!!!!!!!!!!!
--It must be for the initial line, and so gives a RightFace
raisedTo (BackRightLine b3 b4) (FrontRightLine f3 f4) = Right $ RightFace b3 b4 f3 f4

raisedTo a (CornerPointsError msg) = Left $ "FaceConversions.raisedTo: illegal or missing pattern match for " ++ (TS.showConstructor a) ++ " and CornerPointsError msg: " ++ msg
  
raisedTo a b =
  Left $ "FaceConversions.raisedTo: illegal or missing pattern match for " ++ (TS.showConstructor a) ++ " and " ++ (TS.showConstructor b)
 
