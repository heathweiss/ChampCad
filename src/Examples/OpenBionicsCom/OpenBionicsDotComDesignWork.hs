module Examples.OpenBionicsCom.OpenBionicsDotComDesignWork
       (fullLengthSocketWithSmallShaftStlGenerator,  topOfSocketStlGenerator, shortSocketToLargeShaftStlGenerator,
        wristToSmallShaftStlGenerator, joinerShaftStlGenerator,
        wristToLargeShaftStlGenerator) where


import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR, transposeMDRList,
                          extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Create(Origin(..), AngleRadius(..), extractAngles, extractRadii)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine,
                                    extractBackBottomLine, extractBackTopLine, extractBottomFrontLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    backBottomLineFromBottomFrontLine, frontTopLineFromBackTopLine, backTopLineFromFrontTopLine)
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.HorizontalFaces(createTopFaces, createBottomFaces)

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)

import TypeClasses.Transposable(transpose)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)

import Helpers.List((++:))

import Primitives.Cylindrical.Walled(cylinder)

import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Scan.ParseJuicy(getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                      calculateRadiusFrom)
import  Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)
import Helpers.List((++:),(++::))

import Builder.Sequence((@~+++@|>))
import Builder.List (newCornerPointsWith10DegreesBuilder, (||@~+++^||))

import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))

import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Flow as Flw
import Control.Lens


--for the Builder system, but does not work with Radial Degrees system.
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader
import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList)

{- |
Design a socket that the OpenBionics.com hand can be attached to.

Design an adpator to join the hand to the socket.
-Hand end must be same shape as comes with the hand.
-Socket end could be round, so it can be rotated on the socket for best positioning.
-}


-- ================================================== half length socket with adaptor===============================================================
{-
Same as fullLengthSocketWithSmallShaft but kick off with the riser from much lower on the socket.
This is because his hand could not fit into the upper section of the socket.

[rawMDR] -----transpose(+3) . reduceRows 35 ----------> [innerMDR]

[innerMDR] ------transpose(+3)--------------------------[outerMDR]

[innerMDR----------- createVerticalWalls---------->    [socket with adaptor::CornerPoints]
 outerMDR
 transposeFactors
 origin
]
-}
shortSocketToLargeShaft :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor ->  PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
shortSocketToLargeShaft    innerSleeveSDR         outerSleeveSDR         rowReductionFactor      pixelsPerMM = do
  let angles = (map (Angle) [0,10..360])
      transposeFactors = [0,((1/ pixelsPerMM) * (fromIntegral rowReductionFactor))..]
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
  
  socketCubes  <- buildCubePointsListAdd "mainCubes"
                  (concat $ (drop 10) (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                      [CornerPointsId | x <-[1..]]

  let shaftOrigin = (Point (-3) (-5) (z_axis . (transposeZ (+10)) . f2 .  extractFrontTopLine .  head $ socketCubes))

  shaftTopFrontLines <- buildCubePointsListAdd "shaftTopFrontLines"
                        (map extractFrontTopLine
                        (createTopFaces
                          shaftOrigin
                          largeShaftOuterRadii 
                          outerWristAngles
                          
                        )
                       )
                       [CornerPointsId | x <-[1..]]

  
  shaftBackTopLines <- buildCubePointsListAdd "shaftBackTopLines"
                       (map (backTopLineFromFrontTopLine . extractFrontTopLine)
                        (createTopFaces
                          shaftOrigin
                          largeShaftInnerRadii 
                          outerWristAngles
                          
                        )
                       )
                       [CornerPointsId | x <-[1..]]
                   

  
  shaftTopFaces          <- buildCubePointsListAdd "topFaces"
                       shaftTopFrontLines
                       shaftBackTopLines
  
  socketToShaftCubes          <- buildCubePointsListAdd "socketToShaftCubes"
                       shaftTopFaces
                       (map (lowerFaceFromUpperFace .extractTopFace)
                         socketCubes
                       )

  shaftCubes <- buildCubePointsListAdd "shaftCubes"
                       (map (transposeZ (+ largeShaftHeight))
                          shaftTopFaces  
                       )
                       ( map lowerFaceFromUpperFace
                         shaftTopFaces
                        )
                       
  return shaftCubes


--load the json file and call generate stl
shortSocketToLargeShaftStlGenerator :: IO ()
shortSocketToLargeShaftStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 40::RowReductionFactor
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) $ (MultiDegreeRadii name' degrees')
            outerSleeveMDR = (transpose (+3)) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (shortSocketToLargeShaft (degrees innerSleeveMDR) (degrees outerSleeveMDR) rowReductionFactor pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket no reduction"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

-- ===============================================half inner/outer hand base============================
{-


connect together the inner/outer walls of the base of the hand(wrist). Wrist inner/outer walls will be different because:
The inner wall has 12 radii, as it is a 12 sided shape.
This has to be matched up to a 36 sided shape, which is what the socket scan has, and which was used
to take the shape of the wrist of the hand.

Name of the wrist section corresponding to the shape of the base of the hand which consists of:
-outer shape: wrist
-inner shape: openBionicsShaft

test print 1:
abs
1 permineter: should have f2 perimeters
5% infill: should use 10%

Sloped section would/should print better with more infill and perimeters.

Shape of outer wrist could use some further adjustment.


-}



wristToLargeShaft :: ExceptT BuilderError (State CpointsStack ) CpointsList
wristToLargeShaft = do
      
  
  wristBackBtmLines <- buildCubePointsListAdd "wristBackBtmLines"
               ( map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
                 (createBottomFaces (Point 0 0 0) (extractRadii angleRadii) (extractAngles angleRadii) )
               )
               [CornerPointsId | x <-[1..]]
  
  wristBtmFrontLines <- buildCubePointsListAdd "wristBtmFrontLines"
               ( map extractBottomFrontLine
                 (createBottomFaces (Point 0 0 0) outerWristRadii outerWristAngles )
               )
               [CornerPointsId | x <-[1..]]

  wristBtmFaces <- buildCubePointsListAdd "wristBtmFaces"
              wristBackBtmLines
              wristBtmFrontLines

  wristCubes <- buildCubePointsListAdd "wristCubes"
              (map (upperFaceFromLowerFace . (transposeZ (+wristHeight))) wristBtmFaces)
              wristBtmFaces
              
  --now build the 36 sided shaft top faces and add them to btm faces of the wrist cubes
  --to convert from the wrist to the shaft
  let shaftOrigin = (Point 0 0 (-10)) -- -10 mm below the btm of the wristCubes
                     
  shaftBtmFrontLines <- buildCubePointsListAdd "shaftBtmFrontLines"
              (map extractBottomFrontLine
                        (createBottomFaces
                          shaftOrigin
                          largeShaftOuterRadii 
                          outerWristAngles
                          
                        )
              )
              [CornerPointsId | x <-[1..]]
              
  shaftBackBtmLines <- buildCubePointsListAdd "shaftBackBtmLines"
              (map (backBottomLineFromBottomFrontLine.  extractBottomFrontLine)
                        (createBottomFaces
                          shaftOrigin
                          largeShaftInnerRadii 
                          outerWristAngles
                          
                        )
                       )
              [CornerPointsId | x <-[1..]]
   
  shaftBackBtmFaces <- buildCubePointsListAdd "shaftBackBtmLines"
              shaftBackBtmLines
              shaftBtmFrontLines

  shaftToWristCubes <- buildCubePointsListAdd "shaftToWristCubes"
              (map upperFaceFromLowerFace wristBtmFaces)
              shaftBackBtmFaces
              
  --build another set of shaft btm faces to give the shaft some depth below the wrist-to-shaft cubes
  shaftBottomFaces <- buildCubePointsListAdd "shaftBottomFaces"
              
              (shaftToWristCubes)
              (map (transposeZ (+ (negate largeShaftHeight))) shaftBackBtmFaces)
  
  return shaftBottomFaces


wristToLargeShaftStlGenerator :: IO ()
wristToLargeShaftStlGenerator = do
  let cpoints = ((execState $ runExceptT (wristToLargeShaft)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


-- ====================================================== top of socket =================================================
{-
Take the top layer of the socket just to have a look at it.
Could be handy for adding a squared off top, where the squared off section is rotated,
instead of rotating the whole socket as I did earlier. This would make more sense to do.

Can be deleted.
-}
topOfSocket :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor ->  PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
topOfSocket    innerSleeveSDR         outerSleeveSDR         rowReductionFactor      pixelsPerMM = do
  let angles = (map (Angle) [0,10..360])
      transposeFactors = [0,((1/ pixelsPerMM) * (fromIntegral rowReductionFactor))..]
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
  
  mainCubes  <- buildCubePointsListAdd "mainCubes"
                (head $ tail (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]
  
  return mainCubes

--load the json file and call generate stl
topOfSocketStlGenerator :: IO ()
topOfSocketStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 40::RowReductionFactor
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) $ (MultiDegreeRadii name' degrees')
            outerSleeveMDR = (transpose (+3)) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (topOfSocket (degrees innerSleeveMDR) (degrees outerSleeveMDR) rowReductionFactor pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket no reduction"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

-- ================================================== full length socket with small shaft===============================================================
{-
Create a socket with adaptor starting at top of socket.

Failure:
His hand could not fit in the socket as the riser is to skinny.
New version with larger adaptor and shorter socke is in:  shortSocketToLargeShaft

[rawMDR] -----transpose(+3) . reduceRows 35 ----------> [innerMDR]

[innerMDR] ------transpose(+3)--------------------------[outerMDR]

[innerMDR----------- createVerticalWalls---------->    [CornerPoints: socket w/o adaptor]
 outerMDR
 transposeFactors
 origin
]
-}
fullLengthSocketWithSmallShaft :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor ->  PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
fullLengthSocketWithSmallShaft    innerSleeveSDR         outerSleeveSDR         rowReductionFactor      pixelsPerMM = do
  let angles = (map (Angle) [0,10..360])
      transposeFactors = [0,((1/ pixelsPerMM) * (fromIntegral rowReductionFactor))..]
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
  
  topOfSocketCubes  <- buildCubePointsListAdd "mainCubes"
                      (concat $ tail (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                      [CornerPointsId | x <-[1..]]

  let shaftOrigin = (Point (-3) 5 (z_axis . (transposeZ (+10)) . f2 .  extractFrontTopLine .  head $ topOfSocketCubes))

  shaftTopFrontLines <- buildCubePointsListAdd "shaftTopFrontLines"
                        (map extractFrontTopLine
                        (createTopFaces
                          shaftOrigin
                          smallShaftOuterRadii 
                          outerWristAngles
                          
                        )
                       )
                       [CornerPointsId | x <-[1..]]

  
  shaftBackTopLines <- buildCubePointsListAdd "wristTopBackLine"
                       (map (backTopLineFromFrontTopLine . extractFrontTopLine)
                        (createTopFaces
                          shaftOrigin
                          smallShaftInnerRadii 
                          outerWristAngles
                          
                        )
                       )
                       [CornerPointsId | x <-[1..]]
                   

  
  shaftTopFaces          <- buildCubePointsListAdd "shaftTopFaces"
                       shaftTopFrontLines
                       shaftBackTopLines
  
  socketToShaftCubes          <- buildCubePointsListAdd "socketToShaftCubes"
                       shaftTopFaces
                       (map (lowerFaceFromUpperFace .extractTopFace)
                         topOfSocketCubes
                       )

  shaftCubes <- buildCubePointsListAdd "shaftCubes"
                       (map (transposeZ (+ smallShaftHeight))
                          shaftTopFaces  
                       )
                       ( map lowerFaceFromUpperFace
                         shaftTopFaces
                        )
                       
  return socketToShaftCubes


--load the json file and call generate stl
fullLengthSocketWithSmallShaftStlGenerator :: IO ()
fullLengthSocketWithSmallShaftStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 40::RowReductionFactor
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) $ (MultiDegreeRadii name' degrees')
            outerSleeveMDR = (transpose (+3)) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (fullLengthSocketWithSmallShaft (degrees innerSleeveMDR) (degrees outerSleeveMDR) rowReductionFactor pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket no reduction"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"
-- ===============================================inner/outer hand base============================
{-
connect together the inner/outer walls of the base of the hand(wrist). Wrist inner/outer walls will be different because:
The inner wall has 12 radii, as it is a 12 sided shape.
This has to be matched up to a 36 sided shape, which is what the socket scan has, and which was used
to take the shape of the wrist of the hand.

Name of the wrist section corresponding to the shape of the base of the hand which consists of:
-outer shape: wrist
-inner shape: dodecagon (12 sided)

test print 1:
abs
1 permineter: should have f2 perimeters
5% infill: should use 10%

Sloped section would/should print better with more infill and perimeters.

Shape of outer wrist could use some further adjustment.
-}



wristToSmallShaft :: ExceptT BuilderError (State CpointsStack ) CpointsList
wristToSmallShaft = do
      
  
  wristAsBackBtmLines <- buildCubePointsListAdd "wristAsBackBtmLines"
               ( map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
                 (createBottomFaces (Point 0 0 0) (extractRadii angleRadii) (extractAngles angleRadii) )
               )
               [CornerPointsId | x <-[1..]]
  
  wristBtmFrontLines <- buildCubePointsListAdd "wristBtmFrontLines"
               ( map extractBottomFrontLine
                 (createBottomFaces (Point 0 0 0) outerWristRadii outerWristAngles )
               )
               [CornerPointsId | x <-[1..]]

  wristAsBtmFaces <- buildCubePointsListAdd "wristAsBtmFaces"
              wristAsBackBtmLines
              wristBtmFrontLines       

  wristCubes <- buildCubePointsListAdd "wristCubes"
              (map (upperFaceFromLowerFace . (transposeZ (+wristHeight))) wristAsBtmFaces)
              wristAsBtmFaces
  --now build the 36 sided(triacontakaihexagon) adapator top faces and add it to btm off wrist cubes
  --to convert from the wrist to the triacontakaihexagon
  let shaftOrigin = (Point 0 0 (-10)) -- -10 mm below the btm of the wristCubes
                     
  shaftBtmFrontLines <- buildCubePointsListAdd "shaftBtmFrontLines"
              (map extractBottomFrontLine
                        (createBottomFaces
                          shaftOrigin
                          smallShaftOuterRadii 
                          outerWristAngles
                          
                        )
              )
              [CornerPointsId | x <-[1..]]
              
  shaftBackBtmLines <- buildCubePointsListAdd "shaftBackBtmLines"
              (map (backBottomLineFromBottomFrontLine.  extractBottomFrontLine)
                        (createBottomFaces
                          shaftOrigin
                          smallShaftInnerRadii 
                          outerWristAngles
                          
                        )
                       )
              [CornerPointsId | x <-[1..]]
              
  shaftBackBtmFaces <- buildCubePointsListAdd "shaftBackBtmFaces"
              shaftBackBtmLines
              shaftBtmFrontLines

  shaftToWristCubes <- buildCubePointsListAdd "shaftToWristCubes"
              (map upperFaceFromLowerFace wristAsBtmFaces)
              shaftBackBtmFaces
              
  shaftCubes <- buildCubePointsListAdd "shaftCubes"
              (shaftToWristCubes)
              (map (transposeZ (+ (negate smallShaftHeight))) shaftBackBtmFaces)
  
  return shaftCubes

wristToSmallShaftStlGenerator :: IO ()
wristToSmallShaftStlGenerator = do
  let cpoints = ((execState $ runExceptT (wristToSmallShaft)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

-- =====================================shaft joiner===========================
{-
The 36 sided shaft, which the socket and hand adaptors will slide over, to join them together

The hand and socket each have a female shaft and so the shaft
should be shaftHeight * 2, or slightly under for some tolerance.

The inside inside of the female pieces is smallShaftInnerRadii, which will be the
outer radii of the shaft, - some tolerance.

|-------|
|       |
|       |
|-------|
-}

joinerShaft :: ExceptT BuilderError (State CpointsStack ) CpointsList
joinerShaft = do
  cylinder <- buildCubePointsListAdd "cylinder"
              (cylinder (map (transpose (\length -> length - 3))largeShaftInnerRadii)
                         (map (transpose (\length -> length - 0.05))largeShaftInnerRadii)  -- -.2 is a bit sloppy. Try 0.05
                         (map (Angle) [0,10..360])
                         (Point 0 0 0)
                         ((largeShaftHeight * 2) - 2)
              )
              [CornerPointsId | x <-[1..]]

  
  return cylinder

joinerShaftStlGenerator :: IO ()
joinerShaftStlGenerator = do
  let cpoints = ((execState $ runExceptT (joinerShaft)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
-- ======================= outer wrist info ===================================
      --make them every 10 degees to match up with socket.
outerWristAngles =  [Angle a | a <- [0,10..360]]

--outer shape of the wrist
outerWristRadii = map Radius
        [29.64, --0
         28.74, --10
         26.77, --20
         25.25, --30
         23.98, --40
         23.01, --50
         22.25, --60
         21.95, --70
         22.19, --80
         23.1, --90
         24.32, --100
         26.23, --110
         28.62, --120
         30.55, --130
         32.42, --140
         32.53, --150
         31.75, --160
         31.76, --170
         32.5, --180
         32.25, --190
         31.11, --200
         29.47, --210
         26.38, --220
         24.49, --230
         23.19, --240
         22.83, --250
         23.25, --260
         24.93, --270
         28.53, --280
         33.02, --290
         34.9, --300
         34.2, --310
         33.62, --320
         33.4, --330
         32.15, --340
         30.96, --350
         29.64 --360
        ]

-- ============================================== common data======================================
--triacontakaihexagon is the 36 sided riser that is the common shape of the wrist that fits into the hand and the socket.
--This 1st set is for the full length socket, which is hand did not fit in
smallShaftOuterRadii = [Radius 20 | x <- [1..]]
smallShaftInnerRadii = [Radius 17 | x <- [1..]]
smallShaftHeight = 20
wristHeight = 15
--this 2nd set is enlarged so it can take off from lower on the socket
largeShaftOuterRadii = [Radius 26 | x <- [1..]]
largeShaftInnerRadii = [Radius 23 | x <- [1..]]
largeShaftHeight = 20

{-The AngleRadius pairs representing the 12 points of the wrist in the base of the hand.
Note that this is less than the 37 angles used in the socket scan, and so will have to be built up into
a list of 37-}
angleRadius0 = AngleRadius (Angle 0) (Radius 27.36)
angleRadius8 = AngleRadius (Angle 8) (Radius 26.22)
angleRadius44 = AngleRadius (Angle 44) (Radius 20.01)
angleRadius88 = AngleRadius (Angle 88) (Radius 20.01)
angleRadius124 = AngleRadius (Angle 124) (Radius 26.22)
angleRadius136 = AngleRadius (Angle 136) (Radius 27.36)
angleRadius180 = AngleRadius (Angle 180) (Radius 27.36)
angleRadius188 = AngleRadius (Angle 188) (Radius 26.22)
angleRadius224 = AngleRadius (Angle 224) (Radius 20.01)
angleRadius268 = AngleRadius (Angle 268) (Radius 20.01)
angleRadius304 = AngleRadius (Angle 304) (Radius 26.22)
angleRadius316 = AngleRadius (Angle 316) (Radius 27.36)
angleRadius360 = AngleRadius (Angle 360) (Radius 27.36)

-- ==================================================== hand data============================================
{-Now build up a list of AngleRadius, to match the 37 angles or the wrist outer radii,
which in turn has to match the 37 angles used for the socket scan.-}
angleRadii =
  [
   angleRadius0, --0
   angleRadius8, --10
   angleRadius8, --20
   angleRadius44, --30
   angleRadius44, --40
   angleRadius44, --50
   angleRadius44, --60
   angleRadius88, --70
   angleRadius88, --80
   angleRadius88, --90
   angleRadius88, --100
   angleRadius88, --110
   angleRadius124, --120
   angleRadius124, --130
   angleRadius136, --140
   angleRadius136, --150
   angleRadius136, --160
   angleRadius180, --170
   angleRadius180, --180
   angleRadius188, --190
   angleRadius188, --200
   angleRadius188, --210
   angleRadius224, --220
   angleRadius224, --230
   angleRadius224, --240
   angleRadius224, --250
   angleRadius268, --260
   angleRadius268, --270
   angleRadius268, --280
   angleRadius268, --290
   angleRadius304, --300
   angleRadius304, --310
   angleRadius316, --320
   angleRadius316, --330
   angleRadius316, --340
   angleRadius360, --350
   angleRadius360  --360
   
  ]

-- ============================================== helpers ==========================================
--curry in the stack pushing function
buildCubePointsListAdd = buildCubePointsList (++)
pixelsPerMM = 696/38
type PixelsPerMillimeter = Double
type RowReductionFactor = Int

--rotate the measured radii.
--will need to be put in some permanent module
rotate :: [a] -> [a]
rotate list = (last list) : (init list)

rotateBack :: [a] -> [a]
rotateBack (x:xs) = xs ++ [x]

removeDefectiveTopRow :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']
