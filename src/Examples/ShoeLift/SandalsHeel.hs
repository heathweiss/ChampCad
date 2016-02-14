module  HeelGenerators.SandalsHeel(sandalHeelDebugToFile, sandalHeelStlToFile ) where
import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFaces,
  radiusAdjustedForZslope,
  xyQuadrantAngle,
  QuadrantAngle(..),
  createCornerPoint,
  Slope(..),
  Radius(..),
  flatXSlope,
  flatYSlope,
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape, stlShapeToText)
import TriCad.CornerPointsFaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace ) 
import TriCad.CornerPointsFaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine,
                                          frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)
import TriCad.CornerPointsDebug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))
import TriCad.StlFileWriter(writeStlToFile, writeStlDebugToFile)

sandalHeelDebugToFile = writeStlDebugToFile strapTopFaceDebug

sandalHeelStlToFile = writeStlToFile sandalToeStlFile
sandalToeStlFile = newStlShape "SandalToe" strapTriangles --  $  shoeFlatTriangles ++ shoeSlopeTriangles 

angles = [0,10..360]


{--------------------------------------------- strap ---------------------------------------
a flat strap that can be heated and glued to 2 adjoining pieces.
-}
strapWidth = 20
strapLength = 30
strapHeight = 1.5

strapTriangles = concat [
                          [FacesAll]
                        ]  
  +++^
  [strapCube]


strapTopFaceDebug =
   [CubeName "strapCube" | x <- [1..]]
   +++^?
   [strapCube]

strapCube = strapBtmFace +++ strapTopFace

strapTopFace = strapBackTopLn +++ strapFrontTopLn

strapFrontTopLn = strapF2 +++ strapF3
strapF3 = F3 (Point strapWidth strapLength strapHeight)
strapF2 = F2 (Point 0 strapLength strapHeight)
   
strapBackTopLn = strapB2 +++ strapB3
strapB3 = B3 (Point strapWidth 0 strapHeight)
strapB2 = B2 (Point 0 0 strapHeight)

strapBtmFrontLnDebug =
   [CubeName "strapBtmFrontLn" | x <- [1..]]
   +++^?
   [strapBtmFrontLn]
strapBtmFrontLn = strapF1 +++ strapF4
strapF4 = F4 (Point strapWidth strapLength 0)
strapF1 = F1 (Point 0 strapLength 0)

strapBtmFaceDebug =
   [CubeName "strapBtmFace" | x <- [1..]]
   +++^?
   [strapBtmFace]
strapBtmFace = strapBackBtmLn +++ strapBtmFrontLn

strapBackBtmLnDebug =
   [CubeName "strapBackBtmLn" | x <- [1..]]
   +++^?
   [strapBackBtmLn]
strapBackBtmLn = strapB1 +++ strapB4
strapB1 = B1 (Point 0 0 0)
strapB4 = B4 (Point strapWidth 0 0)

{------------------------------------------------ brace -----------------------------------------------
The brace that gives ankle support.

Make it without a bottom, so that it can be glued to the 
-}
braceTopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=30})
braceBtmOrigin = (Point{x_axis=0, y_axis=(0), z_axis=0})

braceTriangles = concat [
                          [FacesBackBottomFrontTop | x <- [1..12]],
                          [FacesAllButRight],
                          [FacesNada | x <- [14..23]],
                          [FacesAllButLeft],
                          [FacesBackBottomFrontTop | x <- [24..36]] 
                        ]  
  +++^
  braceCubes

braceCubesDebug =
    [CubeName "braceCubes" | x <- [1..]]
    +++^?
    braceCubes

braceCubes = 
  braceTopFaces
  ++++
  braceBtmFaces

braceTopFacesDebug =
  [CubeName "braceTopFaces" | x <- [1..]]  
  +++^?
  braceTopFaces

braceTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces braceTopOrigin collarRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces braceTopOrigin braceRadius angles flatXSlope (PosYSlope 0))

braceBtmFacesDebug =
  [CubeName "braceBtmFaces" | x <- [1..]]  
  +++^?
  braceBtmFaces

braceBtmFaces = 
  --front line
  map (extractBottomFrontLine) (createBottomFaces braceBtmOrigin collarRadius angles flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces braceBtmOrigin braceRadius angles flatXSlope flatYSlope)


{----------------------------------------------- shoe layer ---------------------------------------------
Fits onto the heel of the shoe.
Has a sloped top.
Has a non-sloped bottom section so keyway will not be tapered for that section.

Bottom is still the shoe radius.
-}
shoeHalfRadius = 
 [
  Radius 38,--0
  Radius 38,--1
  Radius 37.5,--2
  Radius 37,--3
  Radius 36,--4
  Radius 35,--5
  Radius 33,--6
  Radius 31.5,--7
  Radius 30.5,--8
  Radius 31,--9
  Radius 31,--10
  Radius 32,--11
  Radius 35,--12
  Radius 38,--13
  Radius 32.5,--14
  Radius 29,--15
  Radius 26.5,--16
  Radius 25 --17
 ]
--the center val is 180 deg
--It is symmetrical, so can be mirrored.
shoeRadius = concat [shoeHalfRadius, [Radius 24.5], reverse shoeHalfRadius]
--make it an extra 5mm radius to account for the fact that the brace has to go around the heel of shoe.
braceRadius = map (\(Radius x) -> (Radius (x + 5))) shoeRadius
collarRadius = map (\(Radius x) -> (Radius (x + 8))) shoeRadius

shoeSlopeOrigin = (Point{x_axis=0, y_axis=(0), z_axis=90})
shoeFlatOrigin = (Point{x_axis=0, y_axis=(0), z_axis=70})

shoeSlopeTriangles =  [FacesBackFrontTop | x <- [1,2..36]] 
  +++^
  shoeSlopeCubes

shoeSlopeCubesDebug =
    [CubeName "shoeCubes" | x <- [1..]]
    +++^?
    shoeSlopeCubes

shoeSlopeCubes =
  shoeFlatCubes
  ++++
  shoeSlopeTopFaces

shoeSlopeTopFacesDebug =
    [CubeName "shoeSlopeTopFaces" | x <- [1..]]
    +++^?
    shoeSlopeTopFaces

shoeSlopeTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces shoeSlopeOrigin braceRadius angles flatXSlope (NegYSlope 20))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces shoeSlopeOrigin treadInnerRadius angles flatXSlope (NegYSlope 20))

shoeFlatTriangles =  [FacesBackBottomFront | x <- [1,2..36]] 
  +++^
  shoeFlatCubes

shoeFlatCubesDebug =
    [CubeName "shoeCubes" | x <- [1..]]
    +++^?
    shoeFlatTopFaces

shoeFlatCubes =
  riserCubes
  ++++
  shoeFlatTopFaces

shoeFlatTopFacesDebug =
    [CubeName "shoeFlatTopFaces" | x <- [1..]]
    +++^?
    shoeFlatTopFaces


shoeFlatTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces shoeFlatOrigin shoeRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces shoeFlatOrigin treadInnerRadius angles flatXSlope (PosYSlope 0))
{------------------------------------ riser -----------------------------------------------
Has shoe radius for top and bottom.
-}
riserOrigin = (Point{x_axis=0, y_axis=(0), z_axis=60})

riserTriangles =  [FacesBackBottomFrontTop | x <- [1,2..36]] 
  +++^
  riserCubes

riserCubesDebug =
    [CubeName "riserCubes" | x <- [1..]]
    +++^?
    riserCubes

riserCubes =
  adaptorCubes
  ++++
  riserTopFaces

riserTopFacesDebug =
    [CubeName "riserTopFaces" | x <- [1..]]
    +++^?
    riserTopFaces

riserTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces riserOrigin treadRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces riserOrigin treadInnerRadius angles flatXSlope (PosYSlope 0))


{------------------------------------------------ adaptor:  tread to riser adaptor layer -------------------------------------------
Adapts from the tread radius to the shoe radius.
Uses standare half-tread radius for inner key.
-}
adaptorOrigin = (Point{x_axis=0, y_axis=(0), z_axis=30})

adaptorTriangles =  [FacesBackBottomFrontTop | x <- [1,2..36]] 
  +++^
  adaptorCubes

adaptorCubesDebug =
    [CubeName "adaptorCubes" | x <- [1..]]
    +++^?
    adaptorCubes

adaptorCubes =
  treadCubes
  ++++
  adaptorTopFaces
 

adaptorTopFacesDebug =
    [CubeName "adaptorTopFaces" | x <- [1..]]
    +++^?
    adaptorTopFaces

adaptorTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces adaptorOrigin braceRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorOrigin treadInnerRadius angles flatXSlope (PosYSlope 0))

{----------------------------------------------- tread layer------------------------------------------------
It is symmetrical, so use half radius.

Goes from tread radius to shoe radius
-}
treadHalfRadius = 
 [
  Radius 38,--0
  Radius 38,--1
  Radius 38,--2
  Radius 38.5,--3
  Radius 38,--4
  Radius 37,--5
  Radius 35.5,--6
  Radius 34,--7
  Radius 34,--8
  Radius 33.5,--9
  Radius 34,--10
  Radius 36,--11
  Radius 38.5,--12
  Radius 41,--13
  Radius 35,--14
  Radius 30.5,--15
  Radius 28,--16
  Radius 26--17
 ]

topTreadOrigin = (Point{x_axis=0, y_axis=(0), z_axis=15})
btmTreadOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

--the center val is 180 deg
--It is symmetrical, so can be mirrored.
treadRadius = concat [treadHalfRadius, [Radius 26], reverse treadHalfRadius]

treadInnerRadius = map  (\(Radius x) -> (Radius (x * 0.5))) treadRadius

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
    map (extractFrontTopLine) (createTopFaces topTreadOrigin treadRadius angles flatXSlope (PosYSlope 18))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces topTreadOrigin treadInnerRadius angles flatXSlope (PosYSlope 18))


treadBtmFacesDebug =
  [CubeName "treadBtmFaces" | x <- [1..]]  
  +++^?
  treadBtmFaces


treadBtmFaces = 
  --front line
  map (extractBottomFrontLine) (createBottomFaces btmTreadOrigin treadRadius angles flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces btmTreadOrigin treadInnerRadius angles flatXSlope flatYSlope)
