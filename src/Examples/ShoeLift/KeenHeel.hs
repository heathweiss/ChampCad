module Examples.ShoeLift.KeenHeel where

import CornerPoints.Create(slopeAdjustedForVerticalAngle, Slope(..), Angle(..), flatXSlope, flatYSlope )
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFacesWithVariableSlope, createTopFaces,)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|))
import Stl.StlCornerPoints((|+++^|), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape, stlShapeToText)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.FaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace ) 
import CornerPoints.FaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace,
                                          bottomFrontLineFromBackBottomLine, lowerFaceFromUpperFace)
import CornerPoints.Transpose ( transposeZ, transposeX, transposeY)
import CornerPoints.Debug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))
import CornerPoints.Radius(Radius(..))

-------------------------------------------------------------------------- tread attachment and adaptor ----------------------------------------
{-
Attaches to the tread.
Keep the shape of the tread for 1 horizontal cm then convert to the shape of the sole attachment.
-}

writeTreadToStlFile :: IO()
writeTreadToStlFile  =  writeFile "src/Data/temp.stl" $ stlShapeToText treadStlFile

treadStlFile = newStlShape "KeenHeelTread"  $  treadTriangles

treadTriangles =
   (
   [FacesBackFrontTop | x <- [1,2..36]]
   |+++^|
   treadTopCubes
  )
  ++
  (
   [FacesBackBottomFront | x <- [1,2..36]]
   |+++^|
   treadBtmCubes
  )

treadTopCubes =
  --front line
    (
      map (extractFrontTopLine) (createTopFaces treadTopOrigin soleRadius angles flatXSlope flatYSlope)
      |+++|
      --back line
      map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces treadTopOrigin keyWayRadius angles flatXSlope flatYSlope))
      |+++|
     --the top faces of the btm cubes
    (
      map (lowerFaceFromUpperFace .  extractTopFace) treadBtmCubes
    )
  

treadBtmCubes  =
  map ((transposeZ (+ 10)) . upperFaceFromLowerFace  ) treadBtmFaces
  |+++|
  treadBtmFaces


treadBtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFaces treadBtmOrigin treadRadius angles flatXSlope flatYSlope)
  |+++|
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFaces treadBtmOrigin keyWayRadius angles flatXSlope flatYSlope)

treadDebug = 
   [CubeName "treadCubes" | x <- [1..]]
   +++^?
   treadTopCubes

treadTopOrigin = (Point{x_axis=0, y_axis=0, z_axis=30})
treadBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})
-------------------------------------------------------------------------  sole attachment -------------------------------------------------------
{-

This attaches directly to the sole of the keen shoe.

Will go up around the sides of the shoe about 2 cm.
Don't use any slope. Will use an adjuster later, to add the slope.

Top and bottom will both be same shape. Will adapt down to the tread size later.
-}

writeAdaptorToStlFile :: IO()
writeAdaptorToStlFile  =  writeFile "src/Data/temp.stl" $ stlShapeToText adaptorStlFile

adaptorStlFile = newStlShape "KeenHeelAdaptor"  $  adaptorTriangles

adaptorTriangles =
   [FacesBackBottomFrontTop | x <- [1,2..36]]
   |+++^|
   adaptorCubes

adaptorDebug = 
   [CubeName "adaptorCubes" | x <- [1..]]
   +++^?
   adaptorCubes

adaptorCubes = adaptorTopFaces |+++| adaptorBtmFaces

----------- top faces
adaptorTopFaces =
  --front line
    map (extractFrontTopLine) (createTopFaces adaptorTopOrigin soleRadius angles flatXSlope adaptorSlope)
    |+++|
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorTopOrigin keyWayRadius angles flatXSlope adaptorSlope)


adaptorBtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFaces adaptorBtmOrigin soleRadius angles flatXSlope flatYSlope)
  |+++|
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFaces adaptorBtmOrigin keyWayRadius angles flatXSlope flatYSlope)

adaptorTopOrigin = (Point{x_axis=0, y_axis=0, z_axis=30})
adaptorBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

--25 degrees is the original value on oversized print
adaptorSlope = NegYSlope (25)
--------------------------------------------------------------------------- create the keyway ---------------------------------------------------------
{-
Only extend it 1 cm into each piece, to minimize weight.

The top needs to be sloped the same as the sole, as this causes the key to slope the same as the keyway in the sole. Use adaptorSlope.
I have not done the slope for the 1st print


Will need to use the same height for the sloped section, as the sole had, so the slope is the same. Will stop the print when I have the
desired 1 cm of top section.
-}
writeKeyToStlFile :: IO()
writeKeyToStlFile  =  writeFile "src/Data/temp.stl" $ stlShapeToText keyStlFile

keyStlFile = newStlShape "KeenToeKey"  $  keyTriangles

keyTriangles = [FacesBottomFrontTop | x <- [1,2..36]]
 |+++^|
 keyCubes

keyDebug = 
  [CubeName "keyCubes" | x <- [1..]]
  +++^?
  keyCubes

keyCubes =
 createTopFaces topKeyOrigin keyRadius angles flatXSlope flatYSlope
 |+++|
 createBottomFaces btmKeyOrigin keyRadius angles flatXSlope flatYSlope

topKeyOrigin = (Point{x_axis=0, y_axis=(0), z_axis=20})
btmKeyOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

------------------------------------------------------------------------- radius ---------------------------------------------------------------------
angles = map (Angle) [0,10..380]

--radius of the keyway, base on the shape of the keen sole.
keyWayRadius = map (\(Radius x) -> (Radius (x * 0.5))) soleRadius
--radius of the key, slightly undersized from the keyWayRadius
keyRadius = map (\(Radius x) -> (Radius (x * 0.49))) soleRadius 

--radius of the tread of the original keen shoe.
--Increase it by 4 so that it will go up around the sides of the shoe for better attachment.
  
soleRadius =  -- map (\(Radius x) -> Radius (x + 4))
    [Radius 38,--0
     Radius 38,--1
     Radius 38,--2
     Radius 37,--3
     Radius 36,--4
     Radius 36,--5
     Radius 36,--6
     Radius 37,--7
     Radius 37,--8
     Radius 37,--9
     Radius 39,--10
     Radius 40,--11
     Radius 42,--12
     Radius 46,--13
     Radius 53,--14
     Radius 47,--15
     Radius 44,--16
     Radius 42,--17
     Radius 41, --18 180 degrees
     Radius 42, --17
     Radius 44, --16
     Radius 47,--15
     Radius 52,--14
     Radius 46,--13
     Radius 43,--12
     Radius 40,--11
     Radius 39,--10
     Radius 39,--9
     Radius 38,--8
     Radius 38,--7
     Radius 38,--6
     Radius 39,--5
     Radius 39,--4
     Radius 38,--3
     Radius 38,--2
     Radius 38,--1
     Radius 38--0
    ] 

--radius of the heel tread as measured
--oversize it by 4 mm to it goes around the outside of the heel.
treadRadius =   map (\(Radius x) -> Radius (x + 4))
    [Radius 38,--0
     Radius 37,--1
     Radius 37,--2
     Radius 36,--3
     Radius 34,--4
     Radius 33,--5
     Radius 32,--6
     Radius 30,--7
     Radius 30,--8
     Radius 29,--9
     Radius 30,--10
     Radius 32,--11
     Radius 35,--12
     Radius 41,--13
     Radius 48,--14
     Radius 63,--15
     Radius 67,--16
     Radius 64,--17
     Radius 66, --18 180 degrees
     Radius 62, --17
     Radius 64, --16
     Radius 63,--15
     Radius 47,--14
     Radius 42,--13
     Radius 37,--12
     Radius 34,--11
     Radius 33,--10
     Radius 32,--9
     Radius 32,--8
     Radius 33,--7
     Radius 34,--6
     Radius 35,--5
     Radius 36,--4
     Radius 37,--3
     Radius 37,--2
     Radius 38,--1
     Radius 38  --0
    ] 
