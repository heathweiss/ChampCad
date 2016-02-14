module CornerPoints.Transpose (
transposeZ,
transposeX,
transposeY
) where
import CornerPoints.CornerPoints
import CornerPoints.Points(Point(..), transposeZ)
import CornerPoints.Transposable(TransposePoint, transposeX, transposeY, transposeZ)
------------------------------------- transposing cubes/points ----------------------------------------------
{-
Used for: changing points by adding values, as opposed to mulipling with the scalePoints
-}
instance TransposePoint CornerPoints where
 
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

  transposeZ f  (TopFace b2 f2 b3 f3 ) = 
    TopFace {   f2=(transposeZ f f2),
                f3=(transposeZ f f3),
                b2=(transposeZ f b2),
                b3=(transposeZ f b3)}

  transposeZ f (BottomFace b1 f1 b4 f4) = 
    BottomFace {f1=(transposeZ f f1),
                f4=(transposeZ f f4 ),
                b1=(transposeZ f b1 ),
                b4=(transposeZ f b4 )}

  transposeZ f (RightFace b3 b4 f3 f4) =
    RightFace  {b3=(transposeZ f b3),
                b4=(transposeZ f b4),
                f3=(transposeZ f f3),
                f4=(transposeZ f f4)}

  transposeZ f (LeftFace b1 b2 f1 f2)=
    LeftFace   {b1=(transposeZ f b1),
                b2=(transposeZ f b2),
                f1=(transposeZ f f1),
                f2=(transposeZ f f2)

               }

  transposeZ f (BottomFrontLine f1 f4) = 
    BottomFrontLine {f1=(transposeZ f f1),
                     f4=(transposeZ f f4)}

  transposeZ f (BackTopLine b2 b3) =
    BackTopLine {b2=(transposeZ f b2),
                 b3=(transposeZ f b3)}

  transposeZ f  (FrontTopLine f2 f3) = 
    FrontTopLine {f2=(transposeZ f f2),
                  f3=(transposeZ f f3)}

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

  transposeX f (TopFace b2 f2 b3 f3)= 
    TopFace {   f2=(transposeX f f2),
                f3=(transposeX f f3),
                b2=(transposeX f b2),
                b3=(transposeX f b3)}

  transposeX f (BottomFace b1 f1 b4 f4)= 
    BottomFace {f1=(transposeX f f1),
                f4=(transposeX f f4),
                b1=(transposeX f b1),
                b4=(transposeX f b4)}

  transposeX f (F1 f1 ) =
    F1 {f1=(transposeX f f1)}

  transposeX f (F4 f4 ) =
    F4 {f4=(transposeX f f4)}

  transposeX f (B1 b1 ) =
    B1 {b1=(transposeX f b1)}

  transposeX f (B4 b4 ) =
    B4 {b4=(transposeX f b4)}

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

  transposeY f (BottomFace b1 f1 b4 f4)= 
    BottomFace {f1=(transposeY f f1),
                f4=(transposeY f f4),
                b1=(transposeY f b1),
                b4=(transposeY f b4)}

  transposeY f (TopFace b2 f2 b3 f3)= 
    TopFace    {f2=(transposeY f f2),
                f3=(transposeY f f3),
                b2=(transposeY f b2),
                b3=(transposeY f b3)}



  transposeY f (B1 b1 ) =
    B1 {b1=(transposeY f b1)}

  transposeY f (B4 b4 ) =
    B4 {b4=(transposeY f b4)}

  transposeY f (F1 f1 ) =
    F1 {f1=(transposeY f f1)}

  transposeY f (F4 f4 ) =
    F4 {f4=(transposeY f f4)}

{-
get rid of all this, replaces with Transposable class.
----------------------------- z-axis ---------------------------------------
transposeCornerPointsZ :: (Double -> Double) -> CornerPoints -> CornerPoints
transposeCornerPointsZ transposeFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposeZ transposeFactor f1),
                f2=(transposeZ transposeFactor f2),
                f3=(transposeZ transposeFactor f3),
                f4=(transposeZ transposeFactor f4),
                b1=(transposeZ transposeFactor b1),
                b2=(transposeZ transposeFactor b2),
                b3=(transposeZ transposeFactor b3),
                b4=(transposeZ transposeFactor b4)}

transposeCornerPointsZ transposeFactor (TopFace b2 f2 b3 f3)= 
    TopFace {f2=(transposeZ transposeFactor f2),
                f3=(transposeZ transposeFactor f3),
                b2=(transposeZ transposeFactor b2),
                b3=(transposeZ transposeFactor b3)}

transposeCornerPointsZ transposeFactor (BottomFace b1 f1 b4 f4) = 
    BottomFace {f1=(transposeZ transposeFactor f1),
                f4=(transposeZ transposeFactor f4 ),
                b1=(transposeZ transposeFactor b1 ),
                b4=(transposeZ transposeFactor b4 )}

transposeCornerPointsZ transposeFactor (BottomFrontLine f1 f4) = 
    BottomFrontLine {f1=(transposeZ transposeFactor f1),
         f4=(transposeZ transposeFactor f4)
               }

--untested
transposeCornerPointsZ transposeFactor (BackTopLine b2 b3) =
    BackTopLine {b2=(transposeZ transposeFactor b2),
         b3=(transposeZ transposeFactor b3)
        }

transposeCornerPointsZ transposeFactor (FrontTopLine f2 f3) = 
    FrontTopLine {f2=(transposeZ transposeFactor f2),
         f3=(transposeZ transposeFactor f3)
               }
-}
{-
Should be able to get rid of this.
Replaces by Transposable class.
transposePointZ :: (Double -> Double)-> Point -> Point
transposePointZ transposeFactor (Point x y z) = Point {x_axis=x, y_axis=y, z_axis=(transposeFactor z)}

transposePointX :: (Double -> Double)-> Point -> Point
transposePointX transposeFactor (Point x y z) = Point {x_axis=(transposeFactor x), y_axis=y, z_axis=z}

transposeCornerPointsX transposeFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposePointX transposeFactor f1),
                f2=(transposePointX transposeFactor f2),
                f3=(transposePointX transposeFactor f3),
                f4=(transposePointX transposeFactor f4),
                b1=(transposePointX transposeFactor b1),
                b2=(transposePointX transposeFactor b2),
                b3=(transposePointX transposeFactor b3),
                b4=(transposePointX transposeFactor b4)}

transposeCornerPointsX transposeFactor (TopFace b2 f2 b3 f3)= 
    TopFace {   f2=(transposePointX transposeFactor f2),
                f3=(transposePointX transposeFactor f3),
                b2=(transposePointX transposeFactor b2),
                b3=(transposePointX transposeFactor b3)}

transposeCornerPointsX transposeFactor (BottomFace b1 f1 b4 f4)= 
    BottomFace {f1=(transposePointX transposeFactor f1),
                f4=(transposePointX transposeFactor f4),
                b1=(transposePointX transposeFactor b1),
                b4=(transposePointX transposeFactor b4)}

------------------------------------ y-axis ---------------------------------
transposePointY :: (Double -> Double)-> Point -> Point
transposePointY transposeFactor (Point x y z) = Point {x_axis=(x), y_axis=(transposeFactor y), z_axis=z}

transposeCornerPointsY transposeFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposePointY transposeFactor f1),
                f2=(transposePointY transposeFactor f2),
                f3=(transposePointY transposeFactor f3),
                f4=(transposePointY transposeFactor f4),
                b1=(transposePointY transposeFactor b1),
                b2=(transposePointY transposeFactor b2),
                b3=(transposePointY transposeFactor b3),
                b4=(transposePointY transposeFactor b4)}

transposeCornerPointsY transposeFactor (TopFace b2 f2 b3 f3)= 
    TopFace {   f2=(transposePointY transposeFactor f2),
                f3=(transposePointY transposeFactor f3),
                b2=(transposePointY transposeFactor b2),
                b3=(transposePointY transposeFactor b3)}

transposeCornerPointsY transposeFactor (BottomFace b1 f1 b4 f4)= 
    BottomFace {f1=(transposePointY transposeFactor f1),
                f4=(transposePointY transposeFactor f4),
                b1=(transposePointY transposeFactor b1),
                b4=(transposePointY transposeFactor b4)}


-}
