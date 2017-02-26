{-# LANGUAGE ParallelListComp #-}
{- |
Snowboard boot.

A single piece lift, done by measuring the radii off of graph paper.
-}
module Examples.ShoeLift.SnowBoardBoot (writeRearSlopedTread, writeRearBoot, writeForwardBoot, writeRearRoundedToeTread) where

import Scan.ParseJuicy( getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                       calculateRadiusFrom, calculatePixelsPerMillmeter)
import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Test.HUnit

import Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe)
import Helpers.Symmetrical.List(mirrorPlusMidPoint)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (&+++#@), CornerPointsBuilder(..))
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.VerticalFaces(createHorizontallyAlignedCubesNoSlope)
import CornerPoints.Radius(resetMultiDegreeRadiiIfNullWithPreviousValue, MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..), )
import CornerPoints.Points(Point(..))
import CornerPoints.FaceExtraction(extractBottomFace, extractTopFace  )
import CornerPoints.FaceConversions(lowerFaceFromUpperFace, upperFaceFromLowerFace)
import CornerPoints.Transpose(transposeZ, transposeY)

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

import Builder.Sequence(newCornerPointsWith5DegreesBuilder, newCornerPointsWithDegreesBuilder, (||@~+++^||), (@~+++@|>) )

import Primitives.Cylindrical.Solid(slopedBottomCylinder, slopedTopCylinder, squaredOffYLengthenedCylinder)

{-
Will be a 1 piece riser that depends on the toe being rounded, instead of flex in the boot as in a 2 piece lift.
-}

--make signatures more readable
type Height = Double
type Power = Double
type LengthenFactor = Double

origin = Point 0 0 0
angles = map Angle [x | x <-[0,5..]]

-- ======================================================== triangles for printing rear/forward sections ============================
{-
To big to print as a single print. Create triangles that can print the front 270-90 degrees section and the rear 90-270 section.
Also works out well, because this will server as a natural flex point, by printing in 2 sections..
-}
forwardFaces =
  [[FacesWithRange FacesBottomFrontRightTop (DegreeRange 90 95)] ++
   [FacesWithRange FacesBottomFrontTop (DegreeRange 95 265)] ++
   [FacesWithRange FacesBottomFrontLeftTop (DegreeRange 265 270)]
  ]

rearFaces =
  [[FacesWithRange FacesBottomFrontTop (DegreeRange 0 85)] ++
   [FacesWithRange FacesBottomFrontLeftTop (DegreeRange 85 90)] ++
   [FacesWithRange FacesBottomFrontRightTop (DegreeRange 270 275)] ++
   [FacesWithRange FacesBottomFrontTop (DegreeRange 275 360)]
  ]
-- ============================================================== boot to tread adaptor  ========================================
{-
Convert from the shape of the boot, to the shape of the tread.

To big to print as a single piece, so print back half and front half separate.
-}
--riserRadius = map Radius [35 | x <- [0,5..360]]

bootToTreadAdaptorCubes =
   newCornerPointsWith5DegreesBuilder
    (
     (map ((transposeY(+15)) . (transposeZ(+50)) . upperFaceFromLowerFace . extractBottomFace)  bootCubes)
     |+++|
     (map (  lowerFaceFromUpperFace  . extractTopFace) treadRoundedToeCubes)
    )
   
   
   

bootToTreadAdaptorRearTriangles = bootToTreadAdaptorCubes
                              ||@~+++^||
                              rearFaces

writeBootToTreadAdaptorRear = writeStlToFile $ newStlShape "adaptor" bootToTreadAdaptorRearTriangles

bootToTreadAdaptorFrontTriangles = bootToTreadAdaptorCubes
                              ||@~+++^||
                              forwardFaces

writeBootToTreadAdaptorForward = writeStlToFile $ newStlShape "adaptor" bootToTreadAdaptorFrontTriangles                              
-- ============================================================ tread ================================================
{-
Round the toe up to make it easier to walk.
-}
treadRadius = map Radius
  [ 188.9,--0
    188.0,--5
    179.3,--10
    152.8,--15
    121.9,--20
    100.7,--25
    87.2,--30
    79.1,--35
    72.5,--40
    67.0,--45
    61.9,--50
    59.2,--55
    56.0,--60
    53.5,--65
    51.8,--70
    50.2,--75
    48.5,--80
    47.8,--85
    46.7,--90
    46.0,--95
    45.8,--100
    45.8,--105
    45.4,--110
    45.9,--115
    46.7,--120
    47.7,--125
    48.7,--130
    50.1,--135
    51.8,--140
    53.5,--145
    54.6,--150
    56.5,--155
    58.5,--160
    59.8,--165
    61.4,--170
    62.9,--175
    64.4,--180
    65.3,--185
    66.7,--190
    67.3,--195
    68.4,--200
    68.3,--205
    67.8,--210
    67.1,--215
    65.8,--220
    63.4,--225
    60.8,--230
    57.7,--235
    54.9,--240
    52.1,--245
    50.0,--250
    48.3,--255
    47.2,--260
    46.0,--265
    45.7,--270
    45.3,--275
    45.7,--280
    46.2,--285
    47.2,--290
    48.4,--295
    50.0,--300
    51.9,--305
    53.7,--310
    56.0,--315
    58.6,--320
    62.8,--325
    67.4,--330
    74.0,--335
    83.2,--340
    101.8,--345
    160.0,--350
    182.3,--355
    188.9 --360
  ]

--create the variable slope to make the toe rounded upwards.
treadYSlopes = mirrorPlusMidPoint
 (
  [ flatYSlope | x <- [0,5..90]]
  ++
  [ PosYSlope 0 | x <- [95,100..270]]
  ++
  [ flatYSlope | x <- [275,280..360]]
 )
 (PosYSlope 0)

{-
Create separately from the builder so that can be easily re-used in the adaptor.

This has the 90-270 degree section with a bottom upwards slope to have a rounded toe.
-}
treadRoundedToeCubes = slopedBottomCylinder treadRadius origin angles [flatXSlope | x <- [1..]] treadYSlopes (20::Height)

treadRearTriangles = (newCornerPointsWith5DegreesBuilder treadRoundedToeCubes)
                 ||@~+++^||
                 rearFaces

writeRearRoundedToeTread = writeStlToFile $ newStlShape "tread rear" treadRearTriangles

treadForwardTriangles = (newCornerPointsWith5DegreesBuilder treadRoundedToeCubes)
                 ||@~+++^||
                 forwardFaces

writeForwardRoundedToeTread = writeStlToFile $ newStlShape "tread forward" treadForwardTriangles

-- ==== print the back again, to fit better into the tread. Needs a sloped top

treadRearSlopedCubes = slopedTopCylinder treadRadius origin angles [flatXSlope | x <- [1..]] [NegYSlope 2 | x <- [1..]] (5::Height)

treadRearSlopedTriangles = (newCornerPointsWith5DegreesBuilder treadRearSlopedCubes)
                 ||@~+++^||
                 rearFaces

writeRearSlopedTread  = writeStlToFile $ newStlShape "tread rear sloped " treadRearSlopedTriangles                 
-- ================================================================ riser meets boot ======================================================

-- radius is centered near toe, where slope changes
bootRadius = map Radius
  [ 207.0,--0
    205.7,--5
    195.7,--10
    158.7,--15
    119.5,--20
    101.4,--25
    91.0,--30
    82.8,--35
    77.1,--40
    71.4,--45
    67.4,--50
    63.9,--55
    60.0,--60
    57.2,--65
    54.6,--70
    52.9,--75
    51.7,--80
    50.7,--85
    49.7,--90
    49.5,--95
    47.9,--100
    47.8,--105
    47.8,--110
    47.9,--115
    47.9,--120
    48.5,--125
    48.5,--130
    49.3,--135
    49.2,--140
    49.8,--145
    49.8,--150
    49.8,--155
    50.2,--160
    51.2,--165
    51.5,--170
    52.2,--175
    52.6,--180
    53.5,--185
    54.2,--190
    55.3,--195
    56.9,--200
    58.4,--205
    59.2,--210
    59.8,--215
    60.7,--220
    61.6,--225
    62.5,--230
    61.0,--235
    60.4,--240
    59.5,--245
    58.5,--250
    57.9,--255
    57.3,--260
    56.3,--265
    55.7,--270
    56.0,--275
    56.9,--280
    57.3,--285
    58.4,--290
    59.5,--295
    61.7,--300
    64.7,--305
    68.2,--310
    71.4,--315
    74.9,--320
    79.5,--325
    85.2,--330
    93.2,--335
    105.2,--340
    156.9,--345
    198.7,--350
    206.9,--355
    207.0 --360
  ]

--create the variable slope that allows for upwards curve of toe.
bootYSlopes = mirrorPlusMidPoint
 (
  [ NegYSlope 20 | x <- [0,5..90]]
  ++
  [ flatYSlope | x <- [95,100..270]]
  ++
  [ NegYSlope 20 | x <- [275,280..360]]
 )
 (NegYSlope 20)
 
--Where the lift meets the boot.
--Make cubes separate from Builder, so that can be re-used in adaptor.
bootCubes =  slopedTopCylinder bootRadius origin angles [flatXSlope | x <- [1..]] bootYSlopes (20::Height)

--print 0-90, 270-360 to split the print as it is too big for my printer.
bootRearTriangles = newCornerPointsWithDegreesBuilder 5 bootCubes
                ||@~+++^||
                rearFaces

writeRearBoot = writeStlToFile $ newStlShape "the rear boot" bootRearTriangles
                
bootForwardTriangles =  newCornerPointsWith5DegreesBuilder bootCubes
                ||@~+++^||
                forwardFaces            

writeForwardBoot = writeStlToFile $ newStlShape "the forward boot" bootForwardTriangles


-- ===================================================== tests ================================================
snowboardBootTestDo = do
  let seeSlopes = TestCase $ assertEqual
        "seeSlopes"
        []
        (bootYSlopes)
  runTestTT seeSlopes
