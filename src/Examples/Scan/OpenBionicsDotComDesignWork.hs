module Examples.Scan.OpenBionicsDotComDesignWork (socketWithAdaptorStlGenerator,  topOfSocketStlGenerator, 
                                                  innerOuterHandBaseStlGenerator) where

{- |
Design a socket that the OpenBionics.com hand can be attached to.

Design an adpator to join the hand to the socket.
-Hand end must be same shape as comes with the hand.
-Socket end could be round, so it can be rotated on the socket for best positioning.
-}

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..), Origin(..), createCornerPoint, AngleRadius(..), extractAngles, extractRadii)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine,
                                    extractBackBottomLine, extractBackTopLine, extractBottomFrontLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    backBottomLineFromBottomFrontLine, frontTopLineFromBackTopLine, backTopLineFromFrontTopLine)
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.HorizontalFaces(createTopFaces, createBottomFaces)

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

-- ====================================================== top of socket =================================================
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

-- ================================================== full length socket w/o connector===============================================================
{-
Create a socket without using any reduction, to see what it is like

[rawMDR] -----transpose(+3) . reduceRows 35 ----------> [innerMDR]

[innerMDR] ------transpose(+3)--------------------------[outerMDR]

[innerMDR----------- createVerticalWalls---------->    [CornerPoints: socket w/o adaptor]
 outerMDR
 transposeFactors
 origin
]
-}
socketWithAdaptor :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor ->  PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
socketWithAdaptor    innerSleeveSDR         outerSleeveSDR         rowReductionFactor      pixelsPerMM = do
  let angles = (map (Angle) [0,10..360])
      transposeFactors = [0,((1/ pixelsPerMM) * (fromIntegral rowReductionFactor))..]
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
  
  topOfSocketCubes  <- buildCubePointsListAdd "mainCubes"
                      (concat $ tail (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                      [CornerPointsId | x <-[1..]]

  let triacontakaihexagonOrigin = (Point (-3) 5 (z_axis . (transposeZ (+10)) . f2 .  extractFrontTopLine .  head $ topOfSocketCubes))

  btmOfTriacontakaihexagonAsTopFrontLines <- buildCubePointsListAdd "btmOfTriacontakaihexagonAsTopFrontLines"
                        (map extractFrontTopLine
                        (createTopFaces
                          triacontakaihexagonOrigin
                          [Radius 20 | x <- [1..]]
                          outerWristAngles
                          flatXSlope
                          flatYSlope
                        )
                       )
                       [CornerPointsId | x <-[1..]]

  
  btmOfTriacontakaihexagonAsBackTopLines <- buildCubePointsListAdd "wristTopBackLine"
                       (map (backTopLineFromFrontTopLine . extractFrontTopLine)
                        (createTopFaces
                          triacontakaihexagonOrigin
                          [Radius 17 | x <- [1..]]
                          outerWristAngles
                          flatXSlope
                          flatYSlope
                        )
                       )
                       [CornerPointsId | x <-[1..]]
                   

  
  btmOfTriacontakaihexagonAsTopFaces          <- buildCubePointsListAdd "topFaces"
                       btmOfTriacontakaihexagonAsTopFrontLines
                       btmOfTriacontakaihexagonAsBackTopLines
  
  socketToTriacontakaihexagonAdaptor          <- buildCubePointsListAdd "topFaces"
                       btmOfTriacontakaihexagonAsTopFaces
                       (map (lowerFaceFromUpperFace .extractTopFace)
                         topOfSocketCubes
                       )

  triacontakaihexagonAdaptor <- buildCubePointsListAdd "triacontakaihexagonAdaptor"
                       (map (transposeZ (+20))
                          btmOfTriacontakaihexagonAsTopFaces  
                       )
                       ( map lowerFaceFromUpperFace
                         btmOfTriacontakaihexagonAsTopFaces
                        )
                       
  return socketToTriacontakaihexagonAdaptor


--load the json file and call generate stl
socketWithAdaptorStlGenerator :: IO ()
socketWithAdaptorStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 40::RowReductionFactor
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) $ (MultiDegreeRadii name' degrees')
            outerSleeveMDR = (transpose (+3)) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (socketWithAdaptor (degrees innerSleeveMDR) (degrees outerSleeveMDR) rowReductionFactor pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket no reduction"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"
-- ===============================================inner/outer hand base============================
{-
connect together the inner/outer hand bases so inner/outer walls will be different.
This would be the proper shape that would then transfomr to circle and socket.
-}

{-The AngleRadius pairs representing the 12 points of the dodecagon in the base of the hand.
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


innerOuterHandBase :: ExceptT BuilderError (State CpointsStack ) CpointsList
innerOuterHandBase = do
  let
      
      
      {-
      Everty 10 degrees to match up with the socket.
      -}
      -- ============================= inners radii info========================
      

  innerWall <- buildCubePointsListAdd "innerWall"
               ( map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
                 (createBottomFaces (Point 0 0 0) (extractRadii angleRadii) (extractAngles angleRadii) flatXSlope flatYSlope)
               )
               [CornerPointsId | x <-[1..]]

  outerWall <- buildCubePointsListAdd "outerWall"
               ( map extractBottomFrontLine
                 (createBottomFaces (Point 0 0 0) outerWristRadii outerWristAngles flatXSlope flatYSlope)
               )
               [CornerPointsId | x <-[1..]]

  btmFaces <- buildCubePointsListAdd "btmFaces"
              innerWall
              outerWall

  walls    <- buildCubePointsListAdd "walls"
              (map (upperFaceFromLowerFace . (transposeZ (+3))) btmFaces)
              btmFaces

  return walls

innerOuterHandBaseStlGenerator :: IO ()
innerOuterHandBaseStlGenerator = do
  let cpoints = ((execState $ runExceptT (innerOuterHandBase)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


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
