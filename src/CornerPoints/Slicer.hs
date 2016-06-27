module CornerPoints.Slicer(slice) where

import CornerPoints.CornerPoints (CornerPoints(..),(+++))
import CornerPoints.Points (Point(..))


import Test.HUnit

{-
ToDo:
Move much of the testing to external test.

ToDo:
Figure out why the slicing for AFO sandal did not work out.
Seems like x for sure, and possibly y, are out.
-}

{- | Distance along x axis, between 2 points-}
data DeltaX = DeltaX {_deltaX :: Double}
  deriving (Eq, Show)

{- | Distance along y axis, between 2 points-}
data DeltaY = DeltaY {_deltaY :: Double}
  deriving (Eq, Show)

{- | Distance along z axis, between 2 points-}
data DeltaZ = DeltaZ {_deltaZ :: Double}
  deriving (Eq, Show)

{- | Distance along each axis, between 2 points-}
data DeltaXYZ = DeltaXYZ {_deltaX' :: DeltaX,
                          _deltaY' :: DeltaY,
                          _deltaZ' :: DeltaZ
                         }
  deriving (Eq, Show)

-- | Distance along X axis, between 2 points
deltaX :: CornerPoints -> CornerPoints -> DeltaX
deltaX (B2 (Point x _ _)) (B1 (Point x' _ _)) =
  DeltaX $ x - x'
deltaX (B3 (Point x _ _)) (B4 (Point x' _ _)) =
  DeltaX $ x - x'
deltaX (F2 (Point x _ _)) (F1 (Point x' _ _)) =
  DeltaX $ x - x'
deltaX (F3 (Point x _ _)) (F4 (Point x' _ _)) =
  DeltaX $ x - x'


-- | Distance along Y axis, between 2 points
deltaY :: CornerPoints -> CornerPoints -> DeltaY
deltaY (B2 (Point  _ y _)) (B1 (Point  _ y' _)) =
  DeltaY $ y - y'
deltaY (B3 (Point  _ y _)) (B4 (Point  _ y' _)) =
  DeltaY $ y - y'
deltaY (F2 (Point  _ y _)) (F1 (Point  _ y' _)) =
  DeltaY $ y - y'
deltaY (F3 (Point  _ y _)) (F4 (Point  _ y' _)) =
  DeltaY $ y - y'


-- | Distance along Z axis, between 2 points
deltaZ :: CornerPoints -> CornerPoints -> DeltaZ
deltaZ (B2 (Point  _ _ z)) (B1 (Point  _ _ z')) =
  DeltaZ $ z - z'
deltaZ  (B1 (Point  _ _ z')) (B2 (Point  _ _ z)) =
  DeltaZ $ z - z'
deltaZ (B3 (Point  _ _ z)) (B4 (Point  _ _ z')) =
  DeltaZ $ z - z'
deltaZ  (B4 (Point  _ _ z')) (B3 (Point  _ _ z)) =
  DeltaZ $ z - z'
deltaZ (F2 (Point  _ _ z)) (F1 (Point  _ _ z')) =
  DeltaZ $ z - z'
deltaZ  (F1 (Point  _ _ z')) (F2 (Point  _ _ z)) =
  DeltaZ $ z - z'
deltaZ (F3 (Point  _ _ z)) (F4 (Point  _ _ z')) =
  DeltaZ $ z - z'
deltaZ  (F4 (Point  _ _ z')) (F3 (Point  _ _ z)) =
  DeltaZ $ z - z'

-- | Distance along each axis
deltaXYZ :: CornerPoints -> CornerPoints -> DeltaXYZ
deltaXYZ (B2 point) (B1 point') =
  DeltaXYZ 
    (deltaX (B2 point) (B1 point') )
    (deltaY (B2 point) (B1 point') )
    (deltaZ (B2 point) (B1 point') )

{- |
Slice a vertical line horizontally.

If the "top" vertex, line, or face is given first, result will be a top vertex, line, or face.
If the bottom vertex, line, or face is given first, result will be a bottom vertex, line, or face.
ToDo: Make the same for Left and Right

height :: This is the z-value for the slice point.
-}
slice :: Double -> CornerPoints -> CornerPoints -> CornerPoints
slice zValue (B2 topPoint) (B1 btmPoint) =
  let totalXDelta = _deltaX $ deltaX (B2 topPoint) (B1 btmPoint)
      totalYDelta = _deltaY $ deltaY (B2 topPoint) (B1 btmPoint)
      zValueAsTopPoint = B2 (Point 0 0 zValue)
      btmZdelta = _deltaZ $ deltaZ zValueAsTopPoint (B1 btmPoint)
      totalZDelta = _deltaZ $ deltaZ (B2 topPoint) (B1 btmPoint)
      zRatio = btmZdelta / totalZDelta
      xTop = (zRatio * totalXDelta) + (x_axis btmPoint)
      yTop = (zRatio * totalYDelta) + (y_axis btmPoint)
  in
  B2 (Point xTop yTop zValue)

slice zValue  (B1 btmPoint) (B2 topPoint) =
  let b2' = slice zValue (B2 topPoint) (B1 btmPoint)
  in
  B1 (b2 b2')

slice zValue (B3 topPoint) (B4 btmPoint) =
  let totalXDelta = _deltaX $ deltaX (B3 topPoint) (B4 btmPoint)
      totalYDelta = _deltaY $ deltaY (B3 topPoint) (B4 btmPoint)
      zValueAsTopPoint = B3 (Point 0 0 zValue)
      btmZdelta = _deltaZ $ deltaZ zValueAsTopPoint (B4 btmPoint)
      totalZDelta = _deltaZ $ deltaZ (B3 topPoint) (B4 btmPoint)
      zRatio = btmZdelta / totalZDelta
      xTop = (zRatio * totalXDelta) + (x_axis btmPoint)
      yTop = (zRatio * totalYDelta) + (y_axis btmPoint)
  in
  B3 (Point xTop yTop zValue)

slice zValue  (B4 btmPoint) (B3 topPoint) =
  let b3' = slice zValue (B3 topPoint) (B4 btmPoint)
  in
  B4 $ b3 b3'


slice zValue (F2 topPoint) (F1 btmPoint) =
  let totalXDelta = _deltaX $ deltaX (F2 topPoint) (F1 btmPoint)
      totalYDelta = _deltaY $ deltaY (F2 topPoint) (F1 btmPoint)
      zValueAsTopPoint = F2 (Point 0 0 zValue)
      btmZdelta = _deltaZ $ deltaZ zValueAsTopPoint (F1 btmPoint)
      totalZDelta = _deltaZ $ deltaZ (F2 topPoint) (F1 btmPoint)
      zRatio = btmZdelta / totalZDelta
      xTop = (zRatio * totalXDelta) + (x_axis btmPoint)
      yTop = (zRatio * totalYDelta) + (y_axis btmPoint)
  in
  F2 (Point xTop yTop zValue)

--reverse it. Not tested or sure about this.
slice zValue  (F1 btmPoint) (F2 topPoint) =
  let f2' = slice zValue (F2 topPoint) (F1 btmPoint)
  in
  F1 (f2 f2')

slice zValue (F3 topPoint) (F4 btmPoint) =
  let totalXDelta = _deltaX $ deltaX (F3 topPoint) (F4 btmPoint)
      totalYDelta = _deltaY $ deltaY (F3 topPoint) (F4 btmPoint)
      zValueAsTopPoint = F3 (Point 0 0 zValue)
      btmZdelta = _deltaZ $ deltaZ zValueAsTopPoint (F4 btmPoint)
      totalZDelta = _deltaZ $ deltaZ (F3 topPoint) (F4 btmPoint)
      zRatio = btmZdelta / totalZDelta
      xTop = (zRatio * totalXDelta) + (x_axis btmPoint)
      yTop = (zRatio * totalYDelta) + (y_axis btmPoint)
  in
  F3 (Point xTop yTop zValue)

--reverse it. Not tested or sure about this.
slice zValue  (F4 btmPoint) (F3 topPoint) =
  let f3' = slice zValue (F3 topPoint) (F4 btmPoint)
  in
  F4 (f3 f3')
  
slice zValue (BackTopLine (b2point) (b3point)) (BackBottomLine (b1point) (b4point)) =
  let newB2point = b2 $ slice zValue (B2 b2point) (B1 b1point)
      newB3point = b3 $ slice zValue (B3 b3point) (B4 b4point)
  in
  BackTopLine newB2point newB3point

--reverse it
slice zValue  (BackBottomLine (b1point) (b4point)) (BackTopLine (b2point) (b3point)) =
  let newB1point = b1 $ slice zValue (B1 b1point) (B2 b2point)
      newB4point = b4 $ slice zValue (B4 b3point) (B3 b4point)
  in
  BackBottomLine newB1point newB4point

slice zValue (FrontTopLine f2point f3point) (BottomFrontLine f1point f4point) =
  let newF2point = f2 $ slice zValue (F2 f2point) (F1 f1point)
      newF3point = f3 $ slice zValue (F3 f3point) (F4 f4point)
  in
  FrontTopLine newF2point newF3point

--reverse it
slice zValue (BottomFrontLine f1point f4point) (FrontTopLine f2point f3point) =
  let newF1point = f1 $ slice zValue  (F1 f1point) (F2 f2point)
      newF4point = f4 $ slice zValue  (F4 f4point) (F3 f3point)
  in
  BottomFrontLine newF1point newF4point

slice zValue (TopFace b2point f2point b3point f3point) (BottomFace b1point f1point b4point f4point)  =
  let newBackTopLine = slice zValue (BackTopLine b2point b3point) (BackBottomLine b1point b4point)
      newFrontTopLine = slice zValue (FrontTopLine f2point f3point) (BottomFrontLine f1point f4point)
  in
  newFrontTopLine +++ newBackTopLine

--reverse it
slice zValue  (BottomFace b1point f1point b4point f4point) (TopFace b2point f2point b3point f3point)  =
  let newBackBottomLine = slice zValue  (BackBottomLine b1point b4point) (BackTopLine b2point b3point)
      newBottomFrontLine = slice zValue  (BottomFrontLine f1point f4point) (FrontTopLine f2point f3point)
  in
  newBottomFrontLine +++ newBackBottomLine 
  

--next
--1: do top and bottom faces
--2: now add bottoms to tops, by putting the bottom: vertex, line, or face first
--3: Add an catch-all pattern match producing a: CornerPointsError "illegal slice operation"

testLocalFunctions = do

  --have not cx'd to see if numbers are correct.
  let putTopOnCube = TestCase $ assertEqual
        "putTopOnCube"
        (TopFace {b2 = Point {x_axis = 4.0, y_axis = 0.0, z_axis = 5.0},
                  f2 = Point {x_axis = 4.0, y_axis = 10.0, z_axis = 5.0},
                  b3 = Point {x_axis = 8.0, y_axis = 0.0, z_axis = 5.0},
                  f3 = Point {x_axis = 8.0, y_axis = 10.0, z_axis = 5.0}}
        )
        (
          let frontTopLine = FrontTopLine       (Point 4 10 10) (Point 8 10 10)
              bottomFrontLine = BottomFrontLine (Point 4 10 0)  (Point 8 10 0)
              
              backTopLine = BackTopLine         (Point 4 0 10) (Point 8 0 10)
              backBottomLine = BackBottomLine   (Point 4 0 0)  (Point 8 0 0)
                        
          in
          slice 5 (frontTopLine +++ backTopLine) (backBottomLine +++ bottomFrontLine)
         )
  runTestTT putTopOnCube

  let putFlatTopOnFrontLines = TestCase $ assertEqual
        "putFlatTopOnFrontLines"
        (FrontTopLine (Point 1 1 5) (Point 5 1 5))
        (
          let frontTopLine = FrontTopLine    (Point 2 2 10) (Point 6 2 10)
              btmFrontLine = BottomFrontLine (Point 0 0 0)  (Point 4 0 0)
          in
          slice 5 frontTopLine btmFrontLine
        )

  let putFlatBottomOnFrontLines = TestCase $ assertEqual
        "putFlatBottomOnFrontLines"
        (BottomFrontLine (Point 1.4 2.8 7) (Point 11.4 2.8 7))
        (
          let frontTopLine = FrontTopLine    (Point 2 4 10) (Point 12 4 10)
              btmFrontLine = BottomFrontLine (Point 0 0 0)  (Point 10 0 0)
          in
          slice 7 btmFrontLine frontTopLine
        )

  runTestTT putFlatBottomOnFrontLines

  let putFlatTopOnBackLines = TestCase $ assertEqual
        "putFlatTopOnBackLines"
        (BackTopLine (Point 1 1 5) (Point 5 1 5))
        (
          let backTopLine = BackTopLine    (Point 2 2 10) (Point 6 2 10)--b2 b3
              backBtmLine = BackBottomLine (Point 0 0 0)  (Point 4 0 0)--b1 b4
          in
          slice 5 backTopLine backBtmLine
        )

  runTestTT putFlatTopOnBackLines

  let putFlatBottomOnBackLines = TestCase $ assertEqual
        "putFlatBottomOnBackLines"
        (BackBottomLine (Point 1.4 2.8 7) (Point 11.4 2.8 7))
        (
          let backTopLine = BackTopLine    (Point 2 4 10) (Point 12 4 10)
              backBottomLine = BackBottomLine (Point 0 0 0)  (Point 10 0 0)
          in
          slice 7 backBottomLine backTopLine
        )

  runTestTT putFlatBottomOnBackLines

  let putFlatTopOnF2F1 = TestCase $ assertEqual
        "putFlatTopOnF2F1"
        (F2 (Point 1.4 2.8 7))
        (
          slice 7 (F2 (Point 2 4 10)) (F1 (Point 0 0 0))
        )

  runTestTT putFlatTopOnF2F1

  let putFlatBottomOnF1F2 = TestCase $ assertEqual
        "putFlatBottomOnF1F2"
        (F1 (Point 1.4 2.8 7))
        (
          slice 7 (F1 (Point 2 4 10)) (F2 (Point 0 0 0))
        )

  runTestTT putFlatBottomOnF1F2

  let putFlatTopOnF3F4 = TestCase $ assertEqual
        "putFlatTopOnF3F4"
        (F3 (Point 1.4 2.8 7))
        (
          slice 7 (F3 (Point 2 4 10)) (F4 (Point 0 0 0))
        )

  runTestTT putFlatTopOnF3F4

  let putFlatTBottomOnF4F3 = TestCase $ assertEqual
        "putFlatTBottomOnF4F3"
        (F4 (Point 1.4 2.8 7))
        (
          slice 7 (F4 (Point 2 4 10)) (F3 (Point 0 0 0))
        )

  runTestTT putFlatTBottomOnF4F3
  
  let putFlatTopOnB2B1 = TestCase $ assertEqual
        "putFlatTopOnB2B1"
        (B2 (Point 1.4 2.8 7))
        (
          slice 7 (B2 (Point 2 4 10)) (B1 (Point 0 0 0))
        )

  runTestTT putFlatTopOnB2B1

  let putFlatBtmOnB1B2 = TestCase $ assertEqual
        "putFlatBtmOnB1B2"
        (B1 (Point 1.4 2.8 7))
        (
          slice 7 (B1 (Point 2 4 10)) (B2 (Point 0 0 0))
        )

  runTestTT putFlatBtmOnB1B2

  let putFlatTopOnB3B4 = TestCase $ assertEqual
        "putFlatTopOnB3B4"
        (B3 (Point 1.4 2.8 7))
        (
          slice 7 (B3 (Point 2 4 10)) (B4 (Point 0 0 0))
        )

  runTestTT putFlatTopOnB3B4

  let putFlatBtmOnB4B3 = TestCase $ assertEqual
        "putFlatBtmOnB4B3"
        (B4 (Point 1.4 2.8 7))
        (
          slice 7 (B4 (Point 0 0 0)) (B3 (Point 2 4 10))
        )

  runTestTT putFlatBtmOnB4B3
  
  let deltaXB2B1  = TestCase $ assertEqual
        "deltaXB2B1"
        (DeltaX 9)
        (
          deltaX (B2 (Point 10 0 0)) (B1 (Point 1 0 0))
        )

  runTestTT deltaXB2B1

  let deltaYB2B1  = TestCase $ assertEqual
        "deltaYB2B1"
        (DeltaY 9)
        (
          deltaY (B2 (Point 0 10 0)) (B1 (Point 0 1 0))
        )

  runTestTT deltaYB2B1

  let deltaZB2B1  = TestCase $ assertEqual
        "deltaZB2B1"
        (DeltaZ 9)
        (
          deltaZ (B2 (Point 0 0 10)) (B1 (Point 0 0 1))
        )

  runTestTT deltaZB2B1


  let deltaXYZB2B1  = TestCase $ assertEqual
        "deltaXYZB2B1"
        (DeltaXYZ (DeltaX 1) (DeltaY 1) (DeltaZ 1))
        (
          deltaXYZ (B2 (Point 1 2 3)) (B1 (Point 0 1 2))
        )

  runTestTT deltaXYZB2B1
