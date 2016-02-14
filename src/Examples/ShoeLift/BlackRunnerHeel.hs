module HeelGenerators.BlackRunnerHeel(blackRunnerHeelStlFile, blackRunnerHeelDebug,
                                     blackRunnerHeelToFile, blackRunnerHeelDebugToFile) where
import CornerPoints.Create(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFacesSimplified,
  radiusAdjustedForZslope,
  xyQuadrantAngle,
  Slope(..),
  Radius(..),
  Angle(..),
  )
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import CornerPoints.StlCornerPoints((+++^))
import CornerPoints.StlBase (StlShape(..), newStlShape, stlShapeToText)
import CornerPoints.FaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace ) 
import CornerPoints.FaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)
import CornerPoints.Debug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))
import TriCad.StlFileWriter(writeStlToFile, writeStlDebugToFile)

blackRunnerHeelStlFile = newStlShape "BlackRunnerHeel"    braceTriangles-- adaptorTriangles
blackRunnerHeelDebug =  braceV2CubesDebug

{-
Writes the stl file to the Tricad/src directory in which this file exsits,
when run from emacs repl.
-}
blackRunnerHeelToFile = writeStlToFile blackRunnerHeelStlFile
blackRunnerHeelDebugToFile = writeStlDebugToFile  braceV2CubesDebug


flatXSlope = PosXSlope 0
flatYSlope = PosYSlope 0
angles = [0,10..380]




topTreadOrigin = (Point{x_axis=0, y_axis=(0), z_axis=40})
btmTreadOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

--the heel of the tread, cut off from the boot, to be attached to the shoe
treadRadius =  
 [
  Radius 36.5,--18
  Radius 36,--17
  Radius 36,--16
  Radius 35,--15
  Radius 35,--14
  Radius 35,--13
  Radius 35,--12
  Radius 36,--11
  Radius 36,--10
  Radius 37,--9
  Radius 38,--8
  Radius 40,--7
  Radius 42.5,--6
  Radius 47,--5
  Radius 46, --4
  Radius 40,--3
  Radius 37, --2
  Radius 36, --1
  Radius 35, --180 deg
  Radius 36, --1
  Radius 37, --2
  Radius 40, --3
  Radius 46, --4
  Radius 47, --5
  Radius 42.5, --6
  Radius 40, --7
  Radius 38, --8
  Radius 37, --9
  Radius 36, --10
  Radius 36, --11
  Radius 35, --12
  Radius 35, --13
  Radius 35, --14
  Radius 35, --15
  Radius 36, --16
  Radius 36, --17
  Radius 36.5 --18
 ]

 --the heel of the shoe, to which to tread will be attached

shoeRadiusOneSide =
  [
     Radius 33,--18
     Radius 33,--17
     Radius 33,--16
     Radius 34,--15
     Radius 35, --14
     Radius 36,--13
     Radius 36,--12
     Radius 35,--11
     Radius 34,--10
     Radius 33.5,--9
     Radius 34,--8
     Radius 35,--7
     Radius 37, --6
     Radius 41,--5
     Radius 34.3,--4
     Radius 30.25,--3
     Radius 28,--2
     Radius 26.5 --1

  ]

shoeRadius = 
  concat [shoeRadiusOneSide, [Radius 26], reverse shoeRadiusOneSide]

{----------------------------------------------------------------brace--------------------------------------------------------

-}
braceV2TopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=30})

braceV1TopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=20})
braceV1BtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

braceRadius = map (\(Radius x) -> (Radius (x * 1.1))) shoeRadius

braceTriangles = braceV2Triangles ++ braceV1Triangles

braceV2Triangles =
  concat
   [
    [FacesBackFrontTop | x <- [1..5]],
    [FacesAllButRight],
    [FacesNada | x <- [7..30]],
    [FacesAllButLeft],
    [FacesBackBottomFrontTop | x <- [32..36]]
   ] 
  +++^
  braceV2Cubes

braceV2CubesDebug =
    [CubeName "braceV2Cubes" | x <- [1..]]
    +++^?
    braceV2Cubes

braceV2Cubes = 
  braceV1Cubes
  ++++
  braceV2TopFaces 
  

braceV2TopFaces = 
  --front line
  map (extractFrontTopLine) (createTopFaces braceV2TopOrigin braceRadius angles flatXSlope (NegYSlope 25))
  ++++
  --back line
  map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces braceV2TopOrigin shoeRadius angles flatXSlope (NegYSlope 25))

braceV1Triangles = concat
   [
    [FacesBackBottomFront | x <- [1..5]],
    [FacesAllButRight],
    [FacesNada | x <- [7..30]],
    [FacesAllButLeft],
    [FacesBackBottomFrontTop | x <- [32..36]]
   ]
   
   +++^
   braceV1Cubes 
   


braceV1CubesDebug =
    [CubeName "braceV1Cubes" | x <- [1..]]
    +++^?
    braceV1Cubes

braceV1Cubes = 
  braceV1TopFaces ++++ braceV1BtmFaces

braceV1TopFacesDebug =
    [CubeName "braceV1TopFaces" | x <- [1..]]
    +++^?
    braceV1TopFaces

braceV1TopFaces = 
  --front line
  map (extractFrontTopLine) (createTopFaces braceV1TopOrigin braceRadius angles flatXSlope flatYSlope)
  ++++
  --back line
  map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces braceV1TopOrigin shoeRadius angles flatXSlope flatYSlope)

braceV1BtmFacesDebug =
    [CubeName "braceV1BtmFaces" | x <- [1..]]
    +++^?
    braceV1BtmFaces

braceV1BtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFacesSimplified braceV1BtmOrigin braceRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified braceV1BtmOrigin shoeRadius (map (Angle) angles) flatXSlope flatYSlope)


{---------------------------------sloped adaptor-----------------------------------------------------------------
slope: between 30-35 degrees. Will need to do some testing to see what is best.

height:
Just high enough for the slope, with maybe an extra 10 mm.

Will have an open keyway.

Has 2 vertical sections so that bottom keyway will not be tapered.

Production print:
2 perimeters
2 bottom
0 top
12% infill
abs
146 layers
1 hour

-}
adaptorV2TopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=30})
--adaptorV2BtmOrigin = adaptorV1TopOrigin

adaptorV1TopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=17})
adaptorV1BtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

adaptorTriangles = adaptorV1Triangles ++ adaptorV2Triangles

{---------------v2 cubes ----------------------
The top part of adaptor.
Has slope for angle of shoe.
Keyway will be tapered due to slope, but key should not have to go into it.
-}

adaptorV2Triangles =
   [FacesBackFrontTop | x <- [1,2..36]]
   +++^
   adaptorV2Cubes

adaptorV2CubesDebug =
    [CubeName "adaptorV2Cubes" | x <- [1..]]
    +++^?
    adaptorV2Cubes


adaptorV2Cubes = 
  adaptorV1Cubes
  ++++
  adaptorV2TopFaces

adaptorV2TopFacesDebug = 
   [CubeName "adaptorV2TopFaces" | x <- [1..]]
   +++^?
   adaptorV2TopFaces

adaptorV2TopFaces =
     --front line
    map (extractFrontTopLine) (createTopFaces adaptorV2TopOrigin shoeRadius angles flatXSlope (NegYSlope 25))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorV2TopOrigin keywayRadius angles flatXSlope (NegYSlope 25))   

{-------------- v1 cubes ----------------------
The bottom part of the adaptor.
Has no sloped top so that the keyway will not be tapered.
-} 
adaptorV1Triangles =
   [FacesBackBottomFront | x <- [1,2..36]]
   +++^
   adaptorV1Cubes

adaptorV1CubesDebug = 
   [CubeName "adaptorV1Cubes" | x <- [1..]]
   +++^?
   adaptorV1Cubes

   

adaptorV1Cubes = adaptorV1TopFaces ++++ adaptorV1BtmFaces

adaptorV1TopFacesDebug =
   [CubeName "adaptorTopFaces" | x <- [1..]]
   +++^?
   adaptorV1TopFaces

adaptorV1TopFaces =
     --front line
    map (extractFrontTopLine) (createTopFaces adaptorV1TopOrigin shoeRadius angles flatXSlope flatYSlope)
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorV1TopOrigin keywayRadius angles flatXSlope flatYSlope)   


adaptorV1BtmFacesDebug =
   [CubeName "adaptorV1BtmFaces" | x <- [1..]]
   +++^?
   adaptorV1BtmFaces
    
 
adaptorV1BtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFacesSimplified adaptorV1BtmOrigin shoeRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified adaptorV1BtmOrigin keywayRadius (map (Angle) angles) flatXSlope flatYSlope)

{------------------------------ riser -----------------------------------------
riser between tread and adpator.

Will have a keyway all the way through.

The final height will depend on the fit of everything else.

-}
riserTopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=80})
riserBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

riserTriangles =  [FacesBackBottomFrontTop | x <- [1,2..36]] -- final version [FacesBottomTop | x <- [1,2..36]]
  +++^
  riserCubes

riserCubesDebug = 
  [CubeName "riserCubes" | x <- [1..]]
   +++^?
   riserCubes

riserCubes = riserTopFaces ++++ riserBtmFaces

riserTopFacesDebug =
   [CubeName "riserTopFaces" | x <- [1..]]
   +++^?
   riserTopFaces

riserTopFaces =
     --front line
    map (extractFrontTopLine) (createTopFaces riserTopOrigin shoeRadius angles flatXSlope flatYSlope)
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces riserTopOrigin keywayRadius angles flatXSlope flatYSlope)   


riserBtmFacesDebug =
   [CubeName "riserBtmFaces" | x <- [1..]]
   +++^?
   riserBtmFaces
    

riserBtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFacesSimplified riserBtmOrigin shoeRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified riserBtmOrigin keywayRadius (map (Angle) angles) flatXSlope flatYSlope)

{-----------------------------the tread with keyway.----------------------- 
Has the shape of the tread on bottom and shape of shoe on top.
Has an empty keyway.
Should make it about 4 cm tall for a nice transition from tread to shoe, but still leave lots of room for a riser.

Printing at 40 mm high.
2 perimeters
2 top/bottom layers
12% infill
keywayRadius =  map (\(Radius x) -> (Radius (x * 0.53))) shoeRadius
--may need to adjust the key to be looser
-}


treadKeywayTriangles =  [FacesBackBottomFrontTop | x <- [1,2..36]] -- final version [FacesBottomTop | x <- [1,2..36]]
  +++^
  treadKeywayCubes

treadKeywayCubesDebug =
    [CubeName "treadKeywayCubes" | x <- [1..]]
    +++^?
    treadKeywayCubes

treadKeywayCubes = 
  treadKeywayTopFaces
  ++++
  treadKeywayBtmFaces

treadKeywayTopFacesDebug = 
  [CubeName "treadKeywayTopFaces" | x <- [1..]]
    +++^?
    treadKeywayTopFaces

treadKeywayTopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces topTreadOrigin shoeRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces topTreadOrigin keywayRadius angles flatXSlope (PosYSlope 0))

treadKeywayBtmFacesDebug =
  [CubeName "treadKeywayBtmFaces" | x <- [1..]]  
  +++^?
  treadKeywayBtmFaces

treadKeywayBtmFaces = 
  --front line
  map (extractBottomFrontLine) (createBottomFacesSimplified btmTreadOrigin treadRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  --map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces btmTreadOrigin keywayRadius angles flatXSlope flatYSlope)
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified btmTreadOrigin keywayRadius (map (Angle) angles) flatXSlope flatYSlope)


-------------------------------------------------- create the inner key ----------------------------------
{-------------------------------- key sizing -------------------------------------
test print 1:
keyRadius = map (\(Radius x) -> (Radius (x * 0.5))) shoeRadius 
keywayRadius =  map (\(Radius x) -> (Radius (x * 0.53))) shoeRadius
Had about 0.5 mm clearance which was too much.

test print 2:
keyRadius = map (\(Radius x) -> (Radius (x * 0.52))) shoeRadius 
keywayRadius =  map (\(Radius x) -> (Radius (x * 0.53))) shoeRadius
A little bit tight, but will touch it up in the shop 1st before adjusting the radius again.
Maybe it is better that way, to get a tighter fit than would be possible without grinding/sanding.

Production print 1 nfg
height = 80 mm
keyRadius = map (\(Radius x) -> (Radius (x * 0.52))) shoeRadius 
keywayRadius =  map (\(Radius x) -> (Radius (x * 0.53))) shoeRadius
2 perim.
12% infill
2 bottom layers
0 top layers
295 layers, 53 minutes print time
abs

The key is too loose. 
The bottom of the key is not bad, but the top is smaller and quite loose. Why?

The key has a sloped top. It is about 2 mm smaller on the y axis, but consistent on the x axis.
Is the difference in size due to the slope?

The keyway size is consistent.

Problem solved:
The sloped top was designed to keep a consistent horizontal surface area,
so that the piece would still fit on the shoe. So the problem is the sloped top.

Production print 2
60 mm high
flatYSlope to avoid tapering
200 layers
37 minutes
All else same as production print 1.

Keyway fit into the tread section was a perfect fit. Could not ask for any better.

-}
topKeyOrigin = (Point{x_axis=0, y_axis=(0), z_axis=60})
btmKeyOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

keyRadius = map (\(Radius x) -> (Radius (x * 0.52))) shoeRadius 
keywayRadius =  map (\(Radius x) -> (Radius (x * 0.53))) shoeRadius

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
