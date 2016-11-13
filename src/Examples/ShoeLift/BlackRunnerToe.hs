{-
Next:
Slope the top and bottom to match the shape of the runner.
Do this by mapping transposeCornerPointsZ to a list of heights that will slope the toe.
--do I leave the origin where it is, or move it closer to the toe.
Later, this could be better done by adding a height factor to the createCornerPoint.
-}
{-# LANGUAGE ParallelListComp #-}
module HeelGenerators.BlackRunnerToe(blackRunnerToeStlFile, blackRunnerToeDebug, treadRadius) where
import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFacesSimplified,
  createTopFacesWithVariableSlope,
  createBottomFacesWithVariableSlope,
  xyQuadrantAngle,
  Slope(..),
  Radius(..),
  flatXSlope,
  flatYSlope,
  Angle(..),
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape)
import TriCad.CornerPointsFaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace ) 
import TriCad.CornerPointsFaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)
import TriCad.CornerPointsTranspose ( transposeZ, transposeX, transposeY)
import TriCad.CornerPointsDebug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))


blackRunnerToeStlFile = newStlShape "BlackRunnerToe"  $  keyTriangles -- treadTriangles -- adaptorTriangles  --  
                                                        
blackRunnerToeDebug = adaptorCubesDebug -- treadKeywayCubesDebug -- treadKeywayTopFacesDebug -- treadKeywayBtmFacesDebug -- keyDebug



angles = [0,10..380]

shoeSetback = (-15) --move the shoe forward on the tread
--ridgeTopOrigin = (Point{x_axis=0, y_axis=shoeSetback, z_axis=35})
--ridgeBottomOrigin = topOrigin
--height should be 30. use 5 for testing
topOrigin = (Point{x_axis=0, y_axis=(-20), z_axis=30})
bottomOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

--the tread to be attached to the shoe
--it is symmetrical, so us reverse to mirror the values
--180 gets put in separate as it does not get mirrored

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
  Radius 67,--14
  Radius 59,--15
  Radius 54,--16
  Radius 52--17
 ]


--the center val is 180 deg
--It is symmetrical, so can be mirrored.
treadRadius = concat [treadHalfRadius, [Radius 51], reverse treadHalfRadius]

--radius of the actual shoe
shoeRadius = map (\(Radius x) -> Radius (x * 0.9))
    [Radius 57,--0
     Radius 56,--1
     Radius 55,--2
     Radius 52.5,--3
     Radius 48,--4
     Radius 45.5, --5
     Radius 43,--6
     Radius 42,--7
     Radius 41,--8
     Radius 41,--9
     Radius 42,--10
     Radius 43,--11
     Radius 45,--12
     Radius 49, --13
     Radius 53.5,--14
     Radius 51,--15
     Radius 47,--16
     Radius 45,--17
     Radius 44, --18 180 degrees
     Radius 45, --17
     Radius 47, --16
     Radius 51.5,--15
     Radius 59,--14
     Radius 60,--13
     Radius 56,--12
     Radius 53,--11
     Radius 49.5,--10
     Radius 48,--9
     Radius 46,--8
     Radius 46,--7
     Radius 46.5,--6
     Radius 48,--5
     Radius 50,--4
     Radius 51.5,--3
     Radius 53,--2
     Radius 55,--1
     Radius 57--0
    ] 

{-

The original shoe measurements.
They are too far back from the center of the tread.

shoeRadius = map (\(Radius x) -> Radius (x * 0.9))
    [Radius 36,--0
     Radius 36,--1
     Radius 36,--2
     Radius 37,--3
     Radius 37,--4
     Radius 37, --5
     Radius 37,--6
     Radius 37,--7
     Radius 38,--8
     Radius 40,--9
     Radius 42,--10
     Radius 47,--11
     Radius 52,--12
     Radius 57, --13
     Radius 65,--14
     Radius 74,--15
     Radius 69,--16
     Radius 66,--17
     Radius 65, --18 180 degrees
     Radius 67, --19
     Radius 71, --20
     Radius 78,--21
     Radius 70,--22
     Radius 60,--23
     Radius 52,--24
     Radius 46,--25
     Radius 42,--26
     Radius 39,--27
     Radius 37,--28
     Radius 36,--29
     Radius 35,--30
     Radius 34,--31
     Radius 33,--32
     Radius 33,--33
     Radius 34,--34
     Radius 35,--35
     Radius 36--36
    ] 

-}

{------------------------------------------------------ adaptor ----------------------------------------------------------------------
Goes from the shoe to the riser.

Top and bottom use shoe radius

Has a keyway.

Production print 1:
2 perimeters
0 bottom/top
15% infill
abs

-}
adaptorTopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=30})
adaptorBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

adaptorTriangles =
   [FacesBackBottomFrontTop | x <- [1,2..36]]
   +++^
   adaptorCubes

adaptorCubesDebug = 
   [CubeName "adaptorCubes" | x <- [1..]]
   +++^?
   adaptorCubes

adaptorCubes = adaptorTopFaces ++++ adaptorBtmFaces




adaptorTopFacesDebug =
   [CubeName "adaptorTopFaces" | x <- [1..]]
   +++^?
   adaptorTopFaces

adaptorTopFaces =
     --front line
    map (extractFrontTopLine) (createTopFaces adaptorTopOrigin shoeRadius angles flatXSlope flatYSlope)
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorTopOrigin keywayRadius angles flatXSlope flatYSlope)   


adaptorBtmFacesDebug =
   [CubeName "adaptorBtmFaces" | x <- [1..]]
   +++^?
   adaptorBtmFaces
    
 
adaptorBtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFacesSimplified adaptorBtmOrigin shoeRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified adaptorBtmOrigin keywayRadius (map (Angle) angles) flatXSlope flatYSlope)

{-------------------------------------------------------tread ------------------------------------------------------------------------
Bottom is the shape of the tread.
Top is the shape of the shoe.
Has a keyway.

Production print 1:
15% infill
perims 2
bottom 2
top 0
abs
67 layers
81 minutes

The bottom layers stuck to platform and ripped off.
Was still very strong and light without the bottom layers.
-}
topTreadOrigin = (Point{x_axis=0, y_axis=(0), z_axis=20})
btmTreadOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

treadTriangles =  [FacesBackBottomFrontTop | x <- [1,2..36]] 
  +++^
  treadCubes

treadKeywayCubesDebug =
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
    map (extractFrontTopLine) (createTopFaces topTreadOrigin shoeRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces topTreadOrigin keywayRadius angles flatXSlope (PosYSlope 0))

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
      (createBottomFacesSimplified btmTreadOrigin keywayRadius (map (Angle) angles) flatXSlope flatYSlope)


{----------------------------------------------------- key ----------------------------------------------------------------------------
Use the radius of the tread as it is symmetrical.

-}
topKeyOrigin = (Point{x_axis=0, y_axis=(0), z_axis=25})
btmKeyOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

keyRadius = map (\(Radius x) -> (Radius (x * 0.4))) treadRadius 
keywayRadius =  map (\(Radius x) -> (Radius (x * 0.4))) treadRadius

keyDebug = 
  [CubeName "keyCubes" | x <- [1..]]
  +++^?
  keyCubes

keyTriangles = [FacesBottomFrontTop | x <- [1,2..36]]
 +++^
 keyCubes
 

keyCubes =
 createTopFaces topKeyOrigin keyRadius angles flatXSlope flatYSlope
 ++++
 createBottomFacesSimplified btmKeyOrigin keyRadius (map (Angle) angles) flatXSlope flatYSlope




