
{- |
A cube which runs along the y-axis.
The x,y,z axis are directly input along with the width. The width supplies an offset along the x-axis.
Tyically used for a series of cubes running along the x-axis.

ToDo:
Should not be limited to running along the x-axis. 
-}
module Cubical.Cubical (CubicalInput(..), createXaxisLine, zDownSlope, adjustWidth) where

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose  as T (transposeX, transposeY, transposeZ)

import TypeClasses.Transposable(transposeX, transposeY, transposeZ, TransposePoint)

import Math.Trigonometry (sinDegrees, cosDegrees)



{-
datatype to handle the basic input in cubical format.. Setup to use Lens package.
leftPoint: A point with represents one of  CornerPoint(F1,F2,B1,B2).
width: The X-axis offset from the point. Allows for the creation of a F3,F4,B3,B4
-}
data CubicalInput =
  CubeIn {_cornerPoint::CornerPoints, _width::Double}
  deriving (Show)


instance Eq CubicalInput where
  (CubeIn cornerPoint width) == (CubeIn cornerPoint' width') =
     cornerPoint == cornerPoint' && width == width'
    

{- | Creates a line along the x-axis, given a left point(B1, B2, F1. F2) and a width.
Used to give the cube its width.-}
createXaxisLine :: CubicalInput -> CornerPoints
createXaxisLine (CubeIn (B1(point')) width'') =
  (B1 point')
  +++
  createRightPoint (CubeIn (B1(point')) width'')

createXaxisLine (CubeIn (F1(point')) width'') =
  (F1 point')
  +++
  createRightPoint (CubeIn (F1(point')) width'')

createXaxisLine (CubeIn (B2(point')) width'') =
  (B2 point')
  +++
  createRightPoint (CubeIn (B2(point')) width'')

createXaxisLine (CubeIn (F2(point')) width'') =
  (F2 point')
  +++
  createRightPoint (CubeIn (F2(point')) width'')



{-Using the left point (B1, B2, F1, F2), and a width, create a right point(B3, B4, F3, F4).
Used internally by createXaxisLine give the cube its width along the x-axis. -}
createRightPoint :: CubicalInput -> CornerPoints
createRightPoint (CubeIn (B1(point')) width'') =
        (B4
         (transposeX
          (+ width'')
          (point') 
         )
       )

createRightPoint (CubeIn (F1(point')) width'') =
        (F4
         (transposeX
          (+ width'')
          (point') 
         )
       )

createRightPoint (CubeIn (B2(point')) width'') =
        (B3
         (transposeX
          (+ width'')
          (point') 
         )
       )

createRightPoint (CubeIn (F2(point')) width'') =
        (F3
         (transposeX
          (+ width'')
          (point') 
         )
       )

------------------------------------------- transposePoint -------------------------------------------------
instance TransposePoint CubicalInput where
  transposeX f (CubeIn cpoint width) = CubeIn (T.transposeX f cpoint) width
  transposeY f (CubeIn cornerpoint width) = CubeIn (T.transposeY f cornerpoint) width
  transposeZ f (CubeIn cornerpoint width) = CubeIn (T.transposeZ f cornerpoint) width
{-
instance TransposePoint CubicalInput where
  transposeZ f (CubeIn (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) width) = CubeIn (CubePoints (transposeZ f f1)
                                                                                        (transposeZ f f2)
                                                                                        (transposeZ f f3)
                                                                                        (transposeZ f f4)
                                                                                        (transposeZ f b1)
                                                                                        (transposeZ f b2)
                                                                                        (transposeZ f b3)
                                                                                        (transposeZ f b4)
                                                                                                         ) width
  transposeZ f (CubeIn (BackFace b1 b2 b3 b4) width) = CubeIn (BackFace 
                                                                                        (transposeZ f b1)
                                                                                        (transposeZ f b2)
                                                                                        (transposeZ f b3)
                                                                                        (transposeZ f b4)
                                                                                                         ) width
  transposeZ f (CubeIn (BottomFace b1 f1 b4 f4) width) = CubeIn (BottomFace 
                                                                                        (transposeZ f b1)
                                                                                        (transposeZ f f1)
                                                                                        (transposeZ f b4)
                                                                                        (transposeZ f f4)
                                                                                                         ) width
  transposeZ f (CubeIn (FrontFace f1 f2 f3 f4) width) = CubeIn (FrontFace 
                                                                                        (transposeZ f f1)
                                                                                        (transposeZ f f2)
                                                                                        (transposeZ f f3)
                                                                                        (transposeZ f f4)
                                                                                                         ) width
  transposeZ f (CubeIn (LeftFace b1 b2 f1 f2) width) = CubeIn (LeftFace 
                                                                                        (transposeZ f b1)
                                                                                        (transposeZ f b2)
                                                                                        (transposeZ f f1)
                                                                                        (transposeZ f f2)
                                                                                                         ) width
  transposeZ f (CubeIn (RightFace b3 b4 f3 f4) width) = CubeIn (RightFace 
                                                                                        (transposeZ f b3)
                                                                                        (transposeZ f b4)
                                                                                        (transposeZ f f3)
                                                                                        (transposeZ f f4)
                                                                                                         ) width
  transposeZ f (CubeIn (TopFace b2 f2 b3 f3) width) = CubeIn (TopFace 
                                                                                        (transposeZ f b2)
                                                                                        (transposeZ f f2)
                                                                                        (transposeZ f b3)
                                                                                        (transposeZ f f3)
                                                                                                         ) width
  transposeZ f (CubeIn (BackBottomLine b1 b4) width) = CubeIn (BackBottomLine 
                                                                                        (transposeZ f b1)
                                                                                        (transposeZ f b4)
                                                                                                         ) width
  transposeZ f (CubeIn (BackTopLine b2 b3) width) = CubeIn (BackTopLine 
                                                                                        (transposeZ f b2)
                                                                                        (transposeZ f b3)
                                                                                                         ) width
  transposeZ f (CubeIn (BottomFrontLine f1 f4) width) = CubeIn (BottomFrontLine 
                                                                                        (transposeZ f f1)
                                                                                        (transposeZ f f4)
                                                                                                         ) width
  transposeZ f (CubeIn (BottomLeftLine b1 f1) width) = CubeIn (BottomLeftLine 
                                                                                        (transposeZ f b1)
                                                                                        (transposeZ f f1)
                                                                                                         ) width
  transposeZ f (CubeIn (BackRightLine b3 b4) width) = CubeIn (BackRightLine 
                                                                                        (transposeZ f b3)
                                                                                        (transposeZ f b4)
                                                                                                         ) width
  transposeZ f (CubeIn (BottomRightLine b4 f4) width) = CubeIn (BottomRightLine 
                                                                                        (transposeZ f b4)
                                                                                        (transposeZ f f4)
                                                                                                         ) width
  transposeZ f (CubeIn (FrontLeftLine f1 f2) width) = CubeIn (FrontLeftLine 
                                                                                        (transposeZ f f1)
                                                                                        (transposeZ f f2)
                                                                                                         ) width
  transposeZ f (CubeIn (FrontRightLine f3 f4) width) = CubeIn (FrontRightLine 
                                                                                        (transposeZ f f3)
                                                                                        (transposeZ f f4)
                                                                                                         ) width
  transposeZ f (CubeIn (FrontTopLine f2 f3) width) = CubeIn (FrontTopLine 
                                                                                        (transposeZ f f2)
                                                                                        (transposeZ f f3)
                                                                                                         ) width
  transposeZ f (CubeIn (TopLeftLine b2 f2) width) = CubeIn (TopLeftLine 
                                                                                        (transposeZ f b2)
                                                                                        (transposeZ f f2)
                                                                                                         ) width
  transposeZ f (CubeIn (TopRightLine b3 f3) width) = CubeIn (TopRightLine 
                                                                                        (transposeZ f b3)
                                                                                        (transposeZ f f3)
                                                                                                         ) width
  transposeZ f (CubeIn (B1 b1) width) = CubeIn ( (B1(transposeZ f b1))) width
  transposeZ f (CubeIn (B2 b2) width) = CubeIn ( (B2(transposeZ f b2))) width
  transposeZ f (CubeIn (B3 b3) width) = CubeIn ( (B3(transposeZ f b3))) width
  transposeZ f (CubeIn (B4 b4) width) = CubeIn ( (B4(transposeZ f b4))) width
  transposeZ f (CubeIn (F1 f1) width) = CubeIn ( (F1(transposeZ f f1))) width
  transposeZ f (CubeIn (F2 f2) width) = CubeIn ( (F2(transposeZ f f2))) width
  transposeZ f (CubeIn (F3 f3) width) = CubeIn ( (F3(transposeZ f f3))) width
  transposeZ f (CubeIn (F4 f4) width) = CubeIn ( (F4(transposeZ f f4))) width
  
  transposeY f (CubeIn (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) width) = CubeIn (CubePoints (transposeY f f1)
                                                                                        (transposeY f f2)
                                                                                        (transposeY f f3)
                                                                                        (transposeY f f4)
                                                                                        (transposeY f b1)
                                                                                        (transposeY f b2)
                                                                                        (transposeY f b3)
                                                                                        (transposeY f b4)
                                                                                                         ) width
  transposeY f (CubeIn (BackFace b1 b2 b3 b4) width) = CubeIn (BackFace 
                                                                                        (transposeY f b1)
                                                                                        (transposeY f b2)
                                                                                        (transposeY f b3)
                                                                                        (transposeY f b4)
                                                                                                         ) width
  transposeY f (CubeIn (BottomFace b1 f1 b4 f4) width) = CubeIn (BottomFace 
                                                                                        (transposeY f b1)
                                                                                        (transposeY f f1)
                                                                                        (transposeY f b4)
                                                                                        (transposeY f f4)
                                                                                                         ) width
  transposeY f (CubeIn (FrontFace f1 f2 f3 f4) width) = CubeIn (FrontFace 
                                                                                        (transposeY f f1)
                                                                                        (transposeY f f2)
                                                                                        (transposeY f f3)
                                                                                        (transposeY f f4)
                                                                                                         ) width
  transposeY f (CubeIn (LeftFace b1 b2 f1 f2) width) = CubeIn (LeftFace 
                                                                                        (transposeY f b1)
                                                                                        (transposeY f b2)
                                                                                        (transposeY f f1)
                                                                                        (transposeY f f2)
                                                                                                         ) width
  transposeY f (CubeIn (RightFace b3 b4 f3 f4) width) = CubeIn (RightFace 
                                                                                        (transposeY f b3)
                                                                                        (transposeY f b4)
                                                                                        (transposeY f f3)
                                                                                        (transposeY f f4)
                                                                                                         ) width
  transposeY f (CubeIn (TopFace b2 f2 b3 f3) width) = CubeIn (TopFace 
                                                                                        (transposeY f b2)
                                                                                        (transposeY f f2)
                                                                                        (transposeY f b3)
                                                                                        (transposeY f f3)
                                                                                                         ) width
  transposeY f (CubeIn (BackBottomLine b1 b4) width) = CubeIn (BackBottomLine 
                                                                                        (transposeY f b1)
                                                                                        (transposeY f b4)
                                                                                                         ) width
  transposeY f (CubeIn (BackTopLine b2 b3) width) = CubeIn (BackTopLine 
                                                                                        (transposeY f b2)
                                                                                        (transposeY f b3)
                                                                                                         ) width
  transposeY f (CubeIn (BottomFrontLine f1 f4) width) = CubeIn (BottomFrontLine 
                                                                                        (transposeY f f1)
                                                                                        (transposeY f f4)
                                                                                                         ) width
  transposeY f (CubeIn (BottomLeftLine b1 f1) width) = CubeIn (BottomLeftLine 
                                                                                        (transposeY f b1)
                                                                                        (transposeY f f1)
                                                                                                         ) width
  transposeY f (CubeIn (BackRightLine b3 b4) width) = CubeIn (BackRightLine 
                                                                                        (transposeY f b3)
                                                                                        (transposeY f b4)
                                                                                                         ) width
  transposeY f (CubeIn (BottomRightLine b4 f4) width) = CubeIn (BottomRightLine 
                                                                                        (transposeY f b4)
                                                                                        (transposeY f f4)
                                                                                                         ) width
  transposeY f (CubeIn (FrontLeftLine f1 f2) width) = CubeIn (FrontLeftLine 
                                                                                        (transposeY f f1)
                                                                                        (transposeY f f2)
                                                                                                         ) width
  transposeY f (CubeIn (FrontRightLine f3 f4) width) = CubeIn (FrontRightLine 
                                                                                        (transposeY f f3)
                                                                                        (transposeY f f4)
                                                                                                         ) width
  transposeY f (CubeIn (FrontTopLine f2 f3) width) = CubeIn (FrontTopLine 
                                                                                        (transposeY f f2)
                                                                                        (transposeY f f3)
                                                                                                         ) width
  transposeY f (CubeIn (TopLeftLine b2 f2) width) = CubeIn (TopLeftLine 
                                                                                        (transposeY f b2)
                                                                                        (transposeY f f2)
                                                                                                         ) width
  transposeY f (CubeIn (TopRightLine b3 f3) width) = CubeIn (TopRightLine 
                                                                                        (transposeY f b3)
                                                                                        (transposeY f f3)
                                                                                                         ) width
  transposeY f (CubeIn (B1 b1) width) = CubeIn ( (B1(transposeY f b1))) width
  transposeY f (CubeIn (B2 b2) width) = CubeIn ( (B2(transposeY f b2))) width
  transposeY f (CubeIn (B3 b3) width) = CubeIn ( (B3(transposeY f b3))) width
  transposeY f (CubeIn (B4 b4) width) = CubeIn ( (B4(transposeY f b4))) width
  transposeY f (CubeIn (F1 f1) width) = CubeIn ( (F1(transposeY f f1))) width
  transposeY f (CubeIn (F2 f2) width) = CubeIn ( (F2(transposeY f f2))) width
  transposeY f (CubeIn (F3 f3) width) = CubeIn ( (F3(transposeY f f3))) width
  transposeY f (CubeIn (F4 f4) width) = CubeIn ( (F4(transposeY f f4))) width
  
  transposeX f (CubeIn (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) width) = CubeIn (CubePoints (transposeX f f1)
                                                                                        (transposeX f f2)
                                                                                        (transposeX f f3)
                                                                                        (transposeX f f4)
                                                                                        (transposeX f b1)
                                                                                        (transposeX f b2)
                                                                                        (transposeX f b3)
                                                                                        (transposeX f b4)
                                                                                                         ) width
  transposeX f (CubeIn (BackFace b1 b2 b3 b4) width) = CubeIn (BackFace 
                                                                                        (transposeX f b1)
                                                                                        (transposeX f b2)
                                                                                        (transposeX f b3)
                                                                                        (transposeX f b4)
                                                                                                         ) width
  transposeX f (CubeIn (BottomFace b1 f1 b4 f4) width) = CubeIn (BottomFace 
                                                                                        (transposeX f b1)
                                                                                        (transposeX f f1)
                                                                                        (transposeX f b4)
                                                                                        (transposeX f f4)
                                                                                                         ) width
  transposeX f (CubeIn (FrontFace f1 f2 f3 f4) width) = CubeIn (FrontFace 
                                                                                        (transposeX f f1)
                                                                                        (transposeX f f2)
                                                                                        (transposeX f f3)
                                                                                        (transposeX f f4)
                                                                                                         ) width
  transposeX f (CubeIn (LeftFace b1 b2 f1 f2) width) = CubeIn (LeftFace 
                                                                                        (transposeX f b1)
                                                                                        (transposeX f b2)
                                                                                        (transposeX f f1)
                                                                                        (transposeX f f2)
                                                                                                         ) width
  transposeX f (CubeIn (RightFace b3 b4 f3 f4) width) = CubeIn (RightFace 
                                                                                        (transposeX f b3)
                                                                                        (transposeX f b4)
                                                                                        (transposeX f f3)
                                                                                        (transposeX f f4)
                                                                                                         ) width
  transposeX f (CubeIn (TopFace b2 f2 b3 f3) width) = CubeIn (TopFace 
                                                                                        (transposeX f b2)
                                                                                        (transposeX f f2)
                                                                                        (transposeX f b3)
                                                                                        (transposeX f f3)
                                                                                                         ) width
  transposeX f (CubeIn (BackBottomLine b1 b4) width) = CubeIn (BackBottomLine 
                                                                                        (transposeX f b1)
                                                                                        (transposeX f b4)
                                                                                                         ) width
  transposeX f (CubeIn (BackTopLine b2 b3) width) = CubeIn (BackTopLine 
                                                                                        (transposeX f b2)
                                                                                        (transposeX f b3)
                                                                                                         ) width
  transposeX f (CubeIn (BottomFrontLine f1 f4) width) = CubeIn (BottomFrontLine 
                                                                                        (transposeX f f1)
                                                                                        (transposeX f f4)
                                                                                                         ) width
  transposeX f (CubeIn (BottomLeftLine b1 f1) width) = CubeIn (BottomLeftLine 
                                                                                        (transposeX f b1)
                                                                                        (transposeX f f1)
                                                                                                         ) width
  transposeX f (CubeIn (BackRightLine b3 b4) width) = CubeIn (BackRightLine 
                                                                                        (transposeX f b3)
                                                                                        (transposeX f b4)
                                                                                                         ) width
  transposeX f (CubeIn (BottomRightLine b4 f4) width) = CubeIn (BottomRightLine 
                                                                                        (transposeX f b4)
                                                                                        (transposeX f f4)
                                                                                                         ) width
  transposeX f (CubeIn (FrontLeftLine f1 f2) width) = CubeIn (FrontLeftLine 
                                                                                        (transposeX f f1)
                                                                                        (transposeX f f2)
                                                                                                         ) width
  transposeX f (CubeIn (FrontRightLine f3 f4) width) = CubeIn (FrontRightLine 
                                                                                        (transposeX f f3)
                                                                                        (transposeX f f4)
                                                                                                         ) width
  transposeX f (CubeIn (FrontTopLine f2 f3) width) = CubeIn (FrontTopLine 
                                                                                        (transposeX f f2)
                                                                                        (transposeX f f3)
                                                                                                         ) width
  transposeX f (CubeIn (TopLeftLine b2 f2) width) = CubeIn (TopLeftLine 
                                                                                        (transposeX f b2)
                                                                                        (transposeX f f2)
                                                                                                         ) width
  transposeX f (CubeIn (TopRightLine b3 f3) width) = CubeIn (TopRightLine 
                                                                                        (transposeX f b3)
                                                                                        (transposeX f f3)
                                                                                                         ) width
  transposeX f (CubeIn (B1 b1) width) = CubeIn ( (B1(transposeX f b1))) width
  transposeX f (CubeIn (B2 b2) width) = CubeIn ( (B2(transposeX f b2))) width
  transposeX f (CubeIn (B3 b3) width) = CubeIn ( (B3(transposeX f b3))) width
  transposeX f (CubeIn (B4 b4) width) = CubeIn ( (B4(transposeX f b4))) width
  transposeX f (CubeIn (F1 f1) width) = CubeIn ( (F1(transposeX f f1))) width
  transposeX f (CubeIn (F2 f2) width) = CubeIn ( (F2(transposeX f f2))) width
  transposeX f (CubeIn (F3 f3) width) = CubeIn ( (F3(transposeX f f3))) width
  transposeX f (CubeIn (F4 f4) width) = CubeIn ( (F4(transposeX f f4))) width
  
-}

------------------------------ adjust for slope ---------------------------------------

zSlopeBase :: Point -> Point -> Double -> (Double -> Double -> Double) -> Point
zSlopeBase    point    origin   angle polarity  =
        let z = (z_axis point) `polarity` (sinDegrees angle * (y_axis point))
            y = (y_axis origin) + (cosDegrees angle * (y_axis point))
        in   Point (x_axis point) y z

{- |
Rotate a point down, along the z-axis and y-axis, a given degrees of rotation from an origin.

        z' = z - (sin theta y)
        y' = y-origin + (cos theta y)

Known uses:
Given a series of points along the y-axis, which make up a straight line, all with the same z-axis, slope them down,
while maintaining a straight line. You would map zDownSlope over the series of points. The origin would typically
be the 1st point in the series.
      -}

--ToDo: zDownSlope and zUpSlope could be much more DRY.
zDownSlope :: Double -> Point ->   CubicalInput   -> CubicalInput
zDownSlope    angle     origin    (CubeIn (B2(point')) width')  =
        CubeIn (B2 (zSlopeBase point' origin angle (-) )) width'
zDownSlope    angle origin (CubeIn (F2(point')) width')   =
        CubeIn (F2 (zSlopeBase point' origin angle (-) )) width'

{- |
Same as zDownSlope, but in a upwards direction along the z-axis.
-}
zUpSlope :: Double -> Point ->   CubicalInput   -> CubicalInput
zUpSlope    angle     origin    (CubeIn (B2(point')) width')  =
        CubeIn (B2 (zSlopeBase point' origin angle (-) )) width'
zUpSlope    angle origin (CubeIn (F2(point')) width')   =
        CubeIn (F2 (zSlopeBase point' origin angle (-) )) width'



{- |
Adjust the width of a CubicalInput.

-}
adjustWidth :: (Double -> Double) -> CubicalInput -> CubicalInput
adjustWidth widenX (CubeIn (B1(Point x y z)) width'')  =
        CubeIn (B1 (Point x y z) ) (widenX width'')
adjustWidth widenX  (CubeIn (F1(Point x y z)) width'')  =
        CubeIn (F1 (Point x y z)) (widenX width'')
adjustWidth widenX (CubeIn (B2(Point x y z)) width'')  =
        CubeIn (B2 (Point x y z) ) (widenX width'')
adjustWidth widenX  (CubeIn (F2(Point x y z)) width'')  =
        CubeIn (F2 (Point x y z)) (widenX width'')
