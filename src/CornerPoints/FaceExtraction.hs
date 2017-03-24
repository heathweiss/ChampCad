module CornerPoints.FaceExtraction (
 extractFrontFace,
 extractBottomFrontLine,
 extractFrontLeftLine,
 extractFrontRightLine,
 extractFrontTopLine,
 extractTopFace,
 extractRightFace,
 extractBackTopLine,
 extractBottomFace,
 extractBackBottomLine,
 extractBackFace,
 extractLeftFace,
 extractBackRightLine,
 extractBackLeftLine,
 extractF1, extractF2, extractF3, extractF4,
 extractB1, extractB2, extractB3, extractB4
 ) where
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..))

extractBottomFace :: CornerPoints -> CornerPoints
extractBottomFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BottomFace b1 f1 b4 f4

extractFrontFace :: CornerPoints -> CornerPoints
extractFrontFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontFace f1 f2 f3 f4
extractFrontFace (CornerPointsError _) = FrontFace (Point 0 0 0) (Point 0 0 0) (Point 0 0 0) (Point 0 0 0)

extractRightFace :: CornerPoints -> CornerPoints
extractRightFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = RightFace b3 b4 f3 f4

extractTopFace :: CornerPoints -> CornerPoints
extractTopFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = TopFace b2 f2 b3 f3
extractTopFace CornerPointsNothing = CornerPointsNothing


extractBottomFrontLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BottomFrontLine f1 f4
extractBottomFrontLine (BottomFace b1 f1 b4 f4) = BottomFrontLine f1 f4
extractBottomFrontLine (FrontFace f1 f2 f3 f4) = BottomFrontLine f1 f4 


extractFrontTopLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontTopLine f2 f3
extractFrontTopLine (TopFace b2 f2 b3 f3) = FrontTopLine f2 f3
extractFrontTopLine (FrontFace f1 f2 f3 f4) = FrontTopLine f2 f3



extractBackTopLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackTopLine b2 b3
extractBackTopLine (TopFace b2 f2 b3 f3) = BackTopLine b2 b3
extractBackTopLine (BackFace b1 b2 b3 b4) = BackTopLine b2 b3

extractBackBottomLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackBottomLine b1 b4
extractBackBottomLine (BottomFace b1 f1 b4 f4) = BackBottomLine b1 b4

extractBackLeftLine (BackFace b1 b2 b3 b4) = BackLeftLine b1 b2 

extractBackFace :: CornerPoints -> CornerPoints
extractBackFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackFace b1 b2 b3 b4

extractFrontLeftLine :: CornerPoints -> CornerPoints
extractFrontLeftLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontLeftLine f1 f2
extractFrontLeftLine (FrontFace f1 f2 f3 f4) = FrontLeftLine f1 f2

extractBackRightLine :: CornerPoints -> CornerPoints
extractBackRightLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackRightLine b3 b4
extractBackRightLine (BackFace b1 b2 b3 b4) = BackRightLine b3 b4

extractFrontRightLine :: CornerPoints -> CornerPoints
extractFrontRightLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontRightLine f3 f4
extractFrontRightLine (FrontFace f1 f2 f3 f4) = FrontRightLine f3 f4

extractLeftFace :: CornerPoints -> CornerPoints
extractLeftFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = LeftFace b1 b2 f1 f2

extractF1 :: CornerPoints -> CornerPoints
extractF1 (BottomFrontLine f1 f4) = F1 f1
extractF1 (FrontLeftLine f1 f2) = F1 f1
extractF1 (CubePoints f1 _ _ _ _ _ _ _) = F1 f1

extractF2 :: CornerPoints -> CornerPoints
extractF2 (FrontTopLine f2 f3) = F2 f2
extractF2 (FrontLeftLine f1 f2) = F2 f2
extractF2 (CubePoints _ f2 _ _ _ _ _ _) = F2 f2
extractF2 (TopFace _ f2 _ _) = F2 f2

extractF3 :: CornerPoints -> CornerPoints
extractF3 (FrontTopLine f2 f3) = F3 f3
extractF3 (FrontRightLine f3 f4) = F3 f3
extractF3 (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = F3 f3

extractF4 :: CornerPoints -> CornerPoints
extractF4 (BottomFrontLine f1 f4) = F4 f4
extractF4 (FrontRightLine f3 f4) = F4 f4
extractF4 (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = F4 f4

extractB1 :: CornerPoints -> CornerPoints
extractB1 (CubePoints _ _ _ _ b1 _ _ _) = B1 b1

extractB2 :: CornerPoints -> CornerPoints
extractB2 (CubePoints _ _ _ _ _ b2 _ _) = B2 b2

extractB3 :: CornerPoints -> CornerPoints
extractB3 (CubePoints _ _ _ _ _ _ b3 _ ) = B3 b3

extractB4 :: CornerPoints -> CornerPoints
extractB4 (CubePoints _ _ _ _ _ _ _ b4 ) = B4 b4
