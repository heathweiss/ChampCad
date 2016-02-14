module Examples.ShoeLift.KeenToe where

import CornerPoints.Create(slopeAdjustedForVerticalAngle, createCornerPoint, Slope(..),
  Angle(..), flatXSlope, flatYSlope )
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

-------------------------------------------------------------------------- create the sole riser ------------------------------------------------
{-
Will attach directly to the sole of the keen shoe. It will be ~4mm bigger, so that it can go up around the shoe, with the shoe going in
by grinding into the honeycomb fill. Give it 4 perimeter layers
-}

writeSolePlateToStlFile :: IO()
writeSolePlateToStlFile  =  writeFile "src/Data/temp.stl" $ stlShapeToText solePlateStlFile

solePlateStlFile = newStlShape "KeenSolePlateAdaptor"  $  solePlateTriangles

solePlateTriangles =
   [FacesBackBottomFrontTop | x <- [1,2..36]]
   |+++^|
   solePlateCubes

solePlateDebug = [CubeName "solePlateCube" | x <- [1..]]
   +++^?
   solePlateCubes

solePlateCubes = solePlateTopFaces |+++| solePlateBtmFaces

solePlateTopFaces = map ((transposeZ (+30)) .  upperFaceFromLowerFace) solePlateBtmFaces

solePlateBtmFaces = map (lowerFaceFromUpperFace . extractTopFace) adaptorCubes
---------------------------------------------------------------------------- create the shoe sole to tread adaptor ----------------------------------
{-
This will attach directly to the tread, then adapt up to the shape of the sole.
-}
adaptorTopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=35})
adaptorBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

writeAdaptorToStlFile :: IO()
writeAdaptorToStlFile  =  writeFile "src/Data/temp.stl" $ stlShapeToText adaptorStlFile

adaptorStlFile = newStlShape "KeenToeAdaptor"  $  adaptorTriangles

adaptorTriangles =
   [FacesBackBottomFrontTop | x <- [1,2..36]]
   |+++^|
   adaptorCubes

adaptorCubesDebug = 
   [CubeName "adaptorCubes" | x <- [1..]]
   +++^?
   adaptorCubes

adaptorCubes = adaptorTopFaces |+++| adaptorBtmFaces

----------- top faces
adaptorTopFacesDebug =
   [CubeName "adaptorTopFaces" | x <- [1..]]
   +++^?
   adaptorTopFaces

adaptorTopFaces =
     --front line
    map (extractFrontTopLine) (createTopFaces adaptorTopOrigin soleRadius angles flatXSlope flatYSlope)
    |+++|
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorTopOrigin keyRadius angles flatXSlope flatYSlope)   


--------- btm faces
adaptorBtmFacesDebug =
   [CubeName "adaptorBtmFaces" | x <- [1..]]
   +++^?
   adaptorBtmFaces
    

adaptorBtmFaces = 
  --front line
  (map (extractBottomFrontLine)
      (createBottomFaces adaptorBtmOrigin treadRadius angles flatXSlope flatYSlope))
  |+++|
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFaces adaptorBtmOrigin keyRadius angles flatXSlope flatYSlope)

--------------------------------------------------------------------------- create the keyway ---------------------------------------------------------
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

topKeyOrigin = (Point{x_axis=0, y_axis=(0), z_axis=25})
btmKeyOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})




--------------------------------------------------------------------------------- radii and other base info --------------------------------------------------------------------
angles = map (Angle) [0,10..380]

--radius of the keyway, base on the shape of the keen sole.
keyRadius = map (\(Radius x) -> (Radius (x * 0.5))) soleRadius 

--radius of the tread of the original keen shoe
soleRadius =   map (\(Radius x) -> Radius (x + 4))
    [Radius 46,--0
     Radius 47,--1
     Radius 47,--2
     Radius 47,--3
     Radius 47,--4
     Radius 46,--5
     Radius 45,--6
     Radius 44,--7
     Radius 44,--8
     Radius 44,--9
     Radius 47,--10
     Radius 49,--11
     Radius 52,--12
     Radius 41,--13
     Radius 35,--14
     Radius 32,--15
     Radius 30,--16
     Radius 30,--17
     Radius 30, --18 180 degrees
     Radius 31, --17
     Radius 33, --16
     Radius 37,--15
     Radius 43,--14
     Radius 56,--13
     Radius 52,--12
     Radius 47,--11
     Radius 44,--10
     Radius 42,--9
     Radius 41,--8
     Radius 40,--7
     Radius 40,--6
     Radius 40,--5
     Radius 41,--4
     Radius 42,--3
     Radius 43,--2
     Radius 45,--1
     Radius 46--0
    ] 

{-
This was measured with cardboard cutouts, to fit inside the tread.
-}
treadRadius = 
    [
     Radius 45,--0
     Radius 46,--1
     Radius 47,--2
     Radius 45,--3
     Radius 43,--4
     Radius 41, --5
     Radius 40,--6
     Radius 39,--7
     Radius 39,--8
     Radius 39,--9
     Radius 40,--10
     Radius 43,--11
     Radius 47,--12
     Radius 52, --13
     Radius 62,--14
     Radius 76,--15
     Radius 79,--16
     Radius 77,--17
     Radius 73, --18 180 degrees
     Radius 78, --17
     Radius 84, --16
     Radius 64,--15
     Radius 54,--14
     Radius 46,--13
     Radius 42,--12
     Radius 38,--11
     Radius 36,--10
     Radius 34,--9
     Radius 34,--8
     Radius 34,--7
     Radius 35,--6
     Radius 37,--5
     Radius 39,--4
     Radius 42,--3
     Radius 45,--2
     Radius 45,--1
     Radius 45--0
    ]
--the center val is the 180 deg val, which does not get mirrored.
--Create the full 360 deg radius set, by mirroring the first half, and adding in the 180 degree va..
--treadRadius = concat [treadHalfRadius, [Radius (76 * 1 )], reverse treadHalfRadius]


--the final radii, after adustments
--treadHalfRadius = zipWith (adjustRadius)  treadAdjFactors treadRadiusAsMeasured

--adjustRadius ::  (Double -> Double) -> Radius -> Radius
--adjustRadius f (Radius rad)  = Radius (f rad)  

--the amount that most of the radii will be adjusted by.
--treadRadiusAdjustmentFactor = 0.85

--as measured on the bottom of the tread, to the very outside of the grips.
--It is symmetrical, so only measure half. 180 degrees is put in separate so it does not get mirrored.
{-
treadRadiusAsMeasured = --map (\(Radius x) -> Radius (x * treadRadiusAdjustmentFactor))
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
  Radius 70,--15
  Radius 81,--16
  Radius 77--17
 ]
-}
--create an array of adjustment factors, to the radii can be fine tuned.
{-
treadAdjFactors =
  [
  (*1.05),--0
  (*1.05),--1
  (*1.05),--2
  (*1),--3
  (*0.95),--4
  (*0.9),--5
  (*treadRadiusAdjustmentFactor),--6
  (*treadRadiusAdjustmentFactor),--7
  (*treadRadiusAdjustmentFactor),--8
  (*treadRadiusAdjustmentFactor),--9
  (*treadRadiusAdjustmentFactor),--10
  (*treadRadiusAdjustmentFactor),--11
  (*0.9),--12
  (*0.95),--13
  (*1),--14
  (*1.15),--15
  (*1),--16
  (*1)--17
  ]
-}

