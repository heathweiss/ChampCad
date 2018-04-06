{-# LANGUAGE ParallelListComp #-}

module CornerPoints.Transpose (
transposeZ,
transposeX,
transposeY,
transposeZWithList,
transposeZWithList'
) where
import CornerPoints.CornerPoints
import CornerPoints.Points(Point(..), transposeZ)
import TypeClasses.Transposable(TransposePoint, transposeX, transposeY, transposeZ,)

------------------------------------- transposing cubes/points ----------------------------------------------
{-
Used for: changing points by adding values, as opposed to mulipling with the scalePoints
-}
instance TransposePoint CornerPoints where
 
  
  --------------- x-axis ---------------------
  transposeX _ (CornerPointsError err) = CornerPointsError err
  
  transposeX f (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposeX f f1),
                f2=(transposeX f f2),
                f3=(transposeX f f3),
                f4=(transposeX f f4),
                b1=(transposeX f b1),
                b2=(transposeX f b2),
                b3=(transposeX f b3),
                b4=(transposeX f b4)}

  transposeX f  (BackFace b1 b2 b3 b4 ) = 
    BackFace {  b1=(transposeX f b1),
                b2=(transposeX f b2),
                b3=(transposeX f b3),
                b4=(transposeX f b4)}

  transposeX f (BottomFace b1 f1 b4 f4) = 
    BottomFace {f1=(transposeX f f1),
                f4=(transposeX f f4 ),
                b1=(transposeX f b1 ),
                b4=(transposeX f b4 )}

  transposeX f (FrontFace f1 f2 f3 f4) = 
    FrontFace  {f1=(transposeX f f1),
                f2=(transposeX f f2 ),
                f3=(transposeX f f3 ),
                f4=(transposeX f f4 )}

  transposeX f (LeftFace b1 b2 f1 f2)=
    LeftFace   {b1=(transposeX f b1),
                b2=(transposeX f b2),
                f1=(transposeX f f1),
                f2=(transposeX f f2)}

  transposeX f (RightFace b3 b4 f3 f4) =
    RightFace  {b3=(transposeX f b3),
                b4=(transposeX f b4),
                f3=(transposeX f f3),
                f4=(transposeX f f4)}

  transposeX f  (TopFace b2 f2 b3 f3 ) = 
    TopFace {   f2=(transposeX f f2),
                f3=(transposeX f f3),
                b2=(transposeX f b2),
                b3=(transposeX f b3)}


  transposeX f (BackBottomLine b1 b4) = 
    BackBottomLine {b1=(transposeX f b1),
                    b4=(transposeX f b4)}

  transposeX f (BackTopLine b2 b3) = 
    BackTopLine {b2=(transposeX f b2),
                 b3=(transposeX f b3)}

  transposeX f (BottomFrontLine f1 f4) = 
    BottomFrontLine {f1=(transposeX f f1),
                     f4=(transposeX f f4)}

  transposeX f (BottomLeftLine b1 f1) =
    BottomLeftLine {b1=(transposeX f b1),
                    f1=(transposeX f f1)}

  transposeX f (BackRightLine b3 b4) =
    BackRightLine {b3=(transposeX f b3),
                   b4=(transposeX f b4)}

  transposeX f (BottomRightLine b4 f4) =
    BottomRightLine {b4=(transposeX f b4),
                     f4=(transposeX f f4)}

  transposeX f  (FrontLeftLine f1 f2) = 
    FrontLeftLine {f1=(transposeX f f1),
                   f2=(transposeX f f2)}

  transposeX f  (FrontRightLine f3 f4) = 
    FrontRightLine {f3=(transposeX f f3),
                    f4=(transposeX f f4)}

  transposeX f  (FrontTopLine f2 f3) = 
    FrontTopLine {f2=(transposeX f f2),
                  f3=(transposeX f f3)}

  transposeX f  (TopLeftLine b2 f2) = 
    TopLeftLine {b2=(transposeX f b2),
                 f2=(transposeX f f2)}

  transposeX f  (TopRightLine b3 f3) = 
    TopRightLine {b3=(transposeX f b3),
                  f3=(transposeX f f3)}
  
  
  transposeX f (F1 f1 ) =
    F1 {f1=(transposeX f f1)}

  transposeX f (F2 f2 ) =
    F2 {f2=(transposeX f f2)}

  transposeX f (F3 f3 ) =
    F3 {f3=(transposeX f f3)}

  transposeX f (F4 f4 ) =
    F4 {f4=(transposeX f f4)}

  transposeX f (B1 b1 ) =
    B1 {b1=(transposeX f b1)}

  transposeX f (B2 b2 ) =
    B2 {b2=(transposeX f b2)}

  transposeX f (B3 b3 ) =
    B3 {b3=(transposeX f b3)}

  transposeX f (B4 b4 ) =
    B4 {b4=(transposeX f b4)}

  transposeX f CornerPointsNothing = CornerPointsNothing
  ------------- y-axis -----------------

  transposeY _ (CornerPointsError err) = CornerPointsError err
  
  transposeY f (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposeY f f1),
                f2=(transposeY f f2),
                f3=(transposeY f f3),
                f4=(transposeY f f4),
                b1=(transposeY f b1),
                b2=(transposeY f b2),
                b3=(transposeY f b3),
                b4=(transposeY f b4)}

  transposeY f  (BackFace b1 b2 b3 b4 ) = 
    BackFace {  b1=(transposeY f b1),
                b2=(transposeY f b2),
                b3=(transposeY f b3),
                b4=(transposeY f b4)}

  transposeY f (BottomFace b1 f1 b4 f4) = 
    BottomFace {f1=(transposeY f f1),
                f4=(transposeY f f4 ),
                b1=(transposeY f b1 ),
                b4=(transposeY f b4 )}

  transposeY f (FrontFace f1 f2 f3 f4) = 
    FrontFace  {f1=(transposeY f f1),
                f2=(transposeY f f2 ),
                f3=(transposeY f f3 ),
                f4=(transposeY f f4 )}

  transposeY f (LeftFace b1 b2 f1 f2)=
    LeftFace   {b1=(transposeY f b1),
                b2=(transposeY f b2),
                f1=(transposeY f f1),
                f2=(transposeY f f2)}

  transposeY f (RightFace b3 b4 f3 f4) =
    RightFace  {b3=(transposeY f b3),
                b4=(transposeY f b4),
                f3=(transposeY f f3),
                f4=(transposeY f f4)}

  transposeY f  (TopFace b2 f2 b3 f3 ) = 
    TopFace {   f2=(transposeY f f2),
                f3=(transposeY f f3),
                b2=(transposeY f b2),
                b3=(transposeY f b3)}


  transposeY f (BackBottomLine b1 b4) = 
    BackBottomLine {b1=(transposeY f b1),
                    b4=(transposeY f b4)}

  transposeY f (BackTopLine b2 b3) = 
    BackTopLine {b2=(transposeY f b2),
                 b3=(transposeY f b3)}

  transposeY f (BottomFrontLine f1 f4) = 
    BottomFrontLine {f1=(transposeY f f1),
                     f4=(transposeY f f4)}

  transposeY f (BottomLeftLine b1 f1) =
    BottomLeftLine {b1=(transposeY f b1),
                    f1=(transposeY f f1)}

  transposeY f (BackRightLine b3 b4) =
    BackRightLine {b3=(transposeY f b3),
                   b4=(transposeY f b4)}

  transposeY f (BottomRightLine b4 f4) =
    BottomRightLine {b4=(transposeY f b4),
                     f4=(transposeY f f4)}

  transposeY f  (FrontLeftLine f1 f2) = 
    FrontLeftLine {f1=(transposeY f f1),
                   f2=(transposeY f f2)}

  transposeY f  (FrontRightLine f3 f4) = 
    FrontRightLine {f3=(transposeY f f3),
                    f4=(transposeY f f4)}

  transposeY f  (FrontTopLine f2 f3) = 
    FrontTopLine {f2=(transposeY f f2),
                  f3=(transposeY f f3)}

  transposeY f  (TopLeftLine b2 f2) = 
    TopLeftLine {b2=(transposeY f b2),
                 f2=(transposeY f f2)}

  transposeY f  (TopRightLine b3 f3) = 
    TopRightLine {b3=(transposeY f b3),
                  f3=(transposeY f f3)}




  transposeY f (F1 f1 ) =
    F1 {f1=(transposeY f f1)}

  transposeY f (F2 f2 ) =
    F2 {f2=(transposeY f f2)}

  transposeY f (F3 f3 ) =
    F3 {f3=(transposeY f f3)}

  transposeY f (F4 f4 ) =
    F4 {f4=(transposeY f f4)}

  transposeY f (B1 b1 ) =
    B1 {b1=(transposeY f b1)}

  transposeY f (B2 b2 ) =
    B2 {b2=(transposeY f b2)}

  transposeY f (B3 b3 ) =
    B3 {b3=(transposeY f b3)}

  transposeY f (B4 b4 ) =
    B4 {b4=(transposeY f b4)}

  transposeY f CornerPointsNothing = CornerPointsNothing


  ---------------- z-axis ----------------------

  transposeZ _ (CornerPointsError err) = CornerPointsError err
  
  transposeZ f (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) =
    CubePoints {f1=(transposeZ f f1),
                f2=(transposeZ f f2),
                f3=(transposeZ f f3),
                f4=(transposeZ f f4),
                b1=(transposeZ f b1),
                b2=(transposeZ f b2),
                b3=(transposeZ f b3),
                b4=(transposeZ f b4)}

  transposeZ f  (BackFace b1 b2 b3 b4 ) = 
    BackFace {  b1=(transposeZ f b1),
                b2=(transposeZ f b2),
                b3=(transposeZ f b3),
                b4=(transposeZ f b4)}

  transposeZ f (BottomFace b1 f1 b4 f4) = 
    BottomFace {f1=(transposeZ f f1),
                f4=(transposeZ f f4 ),
                b1=(transposeZ f b1 ),
                b4=(transposeZ f b4 )}

  transposeZ f (FrontFace f1 f2 f3 f4) = 
    FrontFace  {f1=(transposeZ f f1),
                f2=(transposeZ f f2 ),
                f3=(transposeZ f f3 ),
                f4=(transposeZ f f4 )}

  transposeZ f (LeftFace b1 b2 f1 f2)=
    LeftFace   {b1=(transposeZ f b1),
                b2=(transposeZ f b2),
                f1=(transposeZ f f1),
                f2=(transposeZ f f2)}

  transposeZ f (RightFace b3 b4 f3 f4) =
    RightFace  {b3=(transposeZ f b3),
                b4=(transposeZ f b4),
                f3=(transposeZ f f3),
                f4=(transposeZ f f4)}

  transposeZ f  (TopFace b2 f2 b3 f3 ) = 
    TopFace {   f2=(transposeZ f f2),
                f3=(transposeZ f f3),
                b2=(transposeZ f b2),
                b3=(transposeZ f b3)}


  transposeZ f (BackBottomLine b1 b4) = 
    BackBottomLine {b1=(transposeZ f b1),
                    b4=(transposeZ f b4)}

  transposeZ f (BackTopLine b2 b3) = 
    BackTopLine {b2=(transposeZ f b2),
                 b3=(transposeZ f b3)}

  transposeZ f (BottomFrontLine f1 f4) = 
    BottomFrontLine {f1=(transposeZ f f1),
                     f4=(transposeZ f f4)}

  transposeZ f (BottomLeftLine b1 f1) =
    BottomLeftLine {b1=(transposeZ f b1),
                    f1=(transposeZ f f1)}

  transposeZ f (BackRightLine b3 b4) =
    BackRightLine {b3=(transposeZ f b3),
                   b4=(transposeZ f b4)}

  transposeZ f (BottomRightLine b4 f4) =
    BottomRightLine {b4=(transposeZ f b4),
                     f4=(transposeZ f f4)}

  transposeZ f  (FrontLeftLine f1 f2) = 
    FrontLeftLine {f1=(transposeZ f f1),
                   f2=(transposeZ f f2)}

  transposeZ f  (FrontRightLine f3 f4) = 
    FrontRightLine {f3=(transposeZ f f3),
                    f4=(transposeZ f f4)}

  transposeZ f  (FrontTopLine f2 f3) = 
    FrontTopLine {f2=(transposeZ f f2),
                  f3=(transposeZ f f3)}

  transposeZ f  (TopLeftLine b2 f2) = 
    TopLeftLine {b2=(transposeZ f b2),
                 f2=(transposeZ f f2)}

  transposeZ f  (TopRightLine b3 f3) = 
    TopRightLine {b3=(transposeZ f b3),
                  f3=(transposeZ f f3)}

  transposeZ f (F1 f1 ) =
    F1 {f1=(transposeZ f f1)}

  transposeZ f (F2 f2 ) =
    F2 {f2=(transposeZ f f2)}

  transposeZ f (F3 f3 ) =
    F3 {f3=(transposeZ f f3)}

  transposeZ f (F4 f4 ) =
    F4 {f4=(transposeZ f f4)}

  transposeZ f (B1 b1 ) =
    B1 {b1=(transposeZ f b1)}

  transposeZ f (B2 b2 ) =
    B2 {b2=(transposeZ f b2)}

  transposeZ f (B3 b3 ) =
    B3 {b3=(transposeZ f b3)}

  transposeZ f (B4 b4 ) =
    B4 {b4=(transposeZ f b4)}

  transposeZ f CornerPointsNothing = CornerPointsNothing

transposeZWithList :: [Double] -> [CornerPoints] -> [CornerPoints]
transposeZWithList doubles cPoints =
  [ transposeZ (+ double) cPoint 
   | double <- doubles
   | cPoint <- cPoints
  ]

transposeZWithList' :: [(Double -> Double)] -> [CornerPoints] -> [CornerPoints]
transposeZWithList' doublesFx cPoints =
  [ transposeZ (doubleFx) cPoint 
   | doubleFx <- doublesFx
   | cPoint <- cPoints
  ]
