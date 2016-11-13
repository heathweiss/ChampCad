module  HeelGenerators.SandalsToe(sandalToeDebugToFile, sandalToeStlToFile ) where
import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFacesSimplified,
  radiusAdjustedForZslope,
  xyQuadrantAngle,
  Slope(..),
  Radius(..),
  Angle(..),
  flatXSlope,
  flatYSlope,
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape, stlShapeToText)
import TriCad.CornerPointsFaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace ) 
import TriCad.CornerPointsFaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)
import TriCad.CornerPointsDebug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))
import TriCad.StlFileWriter(writeStlToFile, writeStlDebugToFile)

sandalToeDebugToFile = writeStlDebugToFile riserCubesDebug

sandalToeStlToFile = writeStlToFile sandalToeStlFile

sandalToeStlFile = newStlShape "SandalToe"   $ shoeTopTriangles ++ shoeMiddleTriangles -- riserTriangles -- treadTriangles --

angles = [0,10..360]

{------------------------------------ the layer that meets the shoe ----------------------------------
It is made up of 2 layer, so that the bottom layer does not have a slope. This will allow for
a keyway that is not sloped.

-}

shoeRadius = 
 [
  Radius 56,--0
  Radius 57,--1
  Radius 57.5,--2
  Radius 57.2,--3
  Radius 55,--4
  Radius 51,--5
  Radius 48,--6
  Radius 46,--7
  Radius 45,--8
  Radius 45.2,--9
  Radius 46,--10
  Radius 48,--11
  Radius 51,--12
  Radius 55,--13
  Radius 62,--14
  Radius 66,--15
  Radius 60.5,--16
  Radius 57.8,--17
  Radius 57,--18
  Radius 58,--17
  Radius 61,--16
  Radius 61,--15
  Radius 54,--14
  Radius 48,--13
  Radius 43,--12
  Radius 40.5,--11
  Radius 39,--10
  Radius 38,--9
  Radius 37.5,--8
  Radius 37.5,--7
  Radius 38.5,--6
  Radius 40,--5
  Radius 42,--4
  Radius 45,--3
  Radius 50,--2
  Radius 54,--1
  Radius 56--0
 ]



shoeTopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=70})
shoeMiddleOrigin = (Point{x_axis=0, y_axis=(0), z_axis=50})
shoeBtmOrigin = (Point{x_axis=0, y_axis=(0), z_axis=40})

shoeTopTriangles =
   [FacesBackFrontTop | x <- [1,2..36]] 
  +++^
  shoeTopCubes
  

shoeTopCubesDebug =
   [CubeName "shoeTopCubes" | x <- [1..]]
    +++^?
    shoeTopCubes

shoeTopCubes =
  shoeMiddleCubes
  ++++
  shoeTopFaces
  
  

shoeTopFacesDebug =
    [CubeName "shoeTopFaces" | x <- [1..]]
    +++^?
    shoeTopFaces

--only need top faces as it will get added to the tread top faces
shoeTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces shoeTopOrigin shoeRadius angles flatXSlope (PosYSlope 10))
    ++++
    --back line. Note that treadInnerRadius is used so that the keyway is kept consistent.
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces shoeTopOrigin treadInnerRadius angles flatXSlope (PosYSlope 10))


shoeMiddleTriangles =
  [FacesBackBottomFront | x <- [1,2..36]] 
  +++^
  shoeMiddleCubes

shoeMiddleCubesDebug =
   [CubeName "shoeMiddleCubes" | x <- [1..]]
    +++^?
    shoeMiddleCubes

shoeMiddleCubes =
  shoeMiddleFaces
  ++++
  shoeBtmFaces

shoeMiddleFaces =
   --front line
    map (extractFrontTopLine) (createTopFaces shoeMiddleOrigin shoeRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line. Note that treadInnerRadius is used so that the keyway is kept consistent.
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces shoeMiddleOrigin treadInnerRadius angles flatXSlope (PosYSlope 0))

shoeBtmFacesDebug =
    [CubeName "shoeBtmFaces" | x <- [1..]]
    +++^?
    shoeBtmFaces
shoeBtmFaces =
      --front line
    map (extractBottomFrontLine)
        (createBottomFacesSimplified shoeBtmOrigin shoeRadius (map (Angle) angles) flatXSlope (PosYSlope 0))
    ++++
    --back line. Note that treadInnerRadius is used so that the keyway is kept consistent.
    map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
        (createBottomFacesSimplified shoeBtmOrigin treadInnerRadius (map (Angle) angles) flatXSlope (PosYSlope 0))
{------------------------------------ the riser layer -----------------------------------------------
This riser has the tread radius on the bottom and shoe radius on the top, which means it adapts between the 2, as well as providing extra height.

It has the treadInnerRadius key, as all layers have to keep it consistent.

It must not have a sloped top, so that the keyway is straight.
-}
riserOrigin = (Point{x_axis=0, y_axis=(0), z_axis=40})

riserTriangles =  [FacesBackBottomFrontTop | x <- [1,2..36]] 
  +++^
  riserCubes

riserCubesDebug =
    [CubeName "riserCubes" | x <- [1..]]
    +++^?
    riserCubes

riserCubes =
  treadCubes
  ++++
  riserTopFaces
 

riserTopFacesDebug = 
  [CubeName "riserTopFaces" | x <- [1..]]
    +++^?
    riserTopFaces

riserTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces riserOrigin shoeRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces riserOrigin treadInnerRadius angles flatXSlope (PosYSlope 0))

{------------------------------------ the layer that meets the tread -----------------------------------}

--half in that it is symmetrical, so only need to enter in 1 side, and then add the mirrored side later
--to get the full radius.
treadHalfRadius = 
 [
  Radius 56,--0
  Radius 57,--1
  Radius 57,--2
  Radius 57,--3
  Radius 55,--4
  Radius 53,--5
  Radius 51,--6
  Radius 50,--7
  Radius 50,--8
  Radius 50,--9
  Radius 52,--10
  Radius 55,--11
  Radius 58,--12
  Radius 63,--13
  Radius 70,--14
  Radius 79,--15
  Radius 81,--16
  Radius 77--17
 ]

--the center val is 180 deg
--It is symmetrical, so can be mirrored.
treadRadius = concat [treadHalfRadius, [Radius 76], reverse treadHalfRadius]

treadInnerRadius = map  (\(Radius x) -> (Radius (x * 0.5))) treadRadius


topTreadOrigin = (Point{x_axis=0, y_axis=(0), z_axis=20})
btmTreadOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

treadTriangles =  [FacesBackBottomFrontTop | x <- [1,2..36]] 
  +++^
  treadCubes

treadCubesDebug =
    [CubeName "treadCubes" | x <- [1..]]
    +++^?
    treadCubes

treadCubes = 
  treadTopFaces
  ++++
  treadBtmFaces

treadTopFacesDebug = 
  [CubeName "treadTopFaces" | x <- [1..]]
    +++^?
    treadTopFaces

treadTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces topTreadOrigin treadRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces topTreadOrigin treadInnerRadius angles flatXSlope (PosYSlope 0))

treadBtmFacesDebug =
  [CubeName "treadBtmFaces" | x <- [1..]]  
  +++^?
  treadBtmFaces

treadBtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFacesSimplified btmTreadOrigin treadRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified btmTreadOrigin treadInnerRadius (map (Angle) angles) flatXSlope flatYSlope)
