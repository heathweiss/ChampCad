module Examples.Scan.OpenBionicsDotComDesignWork (socketNoConnectorStlGenerator, handBaseStlGenerator) where

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
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..), Origin(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)

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

{-
Create a socket without using any reduction, to see what it is like
-}
socketNoConnector :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor ->  PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
socketNoConnector    innerSleeveSDR         outerSleeveSDR         rowReductionFactor      pixelsPerMM = do
  let angles = (map (Angle) [0,10..360])
      transposeFactors = [0,((1/ pixelsPerMM) * (fromIntegral rowReductionFactor))..]
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
  
  mainCubes  <- buildCubePointsListAdd "mainCubes"
                (concat (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]
  
  return mainCubes


--load the json file and call generate stl
socketNoConnectorStlGenerator :: IO ()
socketNoConnectorStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 40::RowReductionFactor
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) $ (MultiDegreeRadii name' degrees')
            outerSleeveMDR = (transpose (+3)) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (socketNoConnector (degrees innerSleeveMDR) (degrees outerSleeveMDR) rowReductionFactor pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket no reduction"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

-- =============================================== hand base ======================================
{-
Have a look at the base of the hand on its own, to print and fit
-}

handBase :: ExceptT BuilderError (State CpointsStack ) CpointsList
handBase = do
  let innerRadii' =
        map Radius
        [27.42, --0
         26.5, --8.5
         26.5, --8.6
         19.4, --43.9
         19.4, --44
         19.4, --44.1
         19.4, --44.2
         19.4, --87.8
         19.4, --87.9
         19.4, --88
         19.4, --88.1
         26.5, --123.4
         26.5, --123.5
         27.42, --132
         27.42, --132.1
         27.42, --132.2
         27.42, --179.8
         27.42, --179.9
         27.42, --180
         26.5, --188.5
         26.5, --188.6
         26.5, --188.7
         19.4, --224
         19.4, --224.1
         19.4, --224.2
         19.4, --267.8
         19.4, --267.9
         19.4, --268
         19.4, --268.1
         26.5, --303.4
         26.5, --303.5
         27.42, --312
         27.42, --312.1
         27.42, --312.2
         27.42, --359.8
         27.42, --359.9
         27.42  --360
        ]

      innerRadiiBuilder a b c =
        map Radius
        (concat
        [
        [a], --0
        [b,b], --8.5, 8.6
        [c | c' <- [1,2..8]], -- 43.9-44.2 87.8-88.1
        [b,b], --123.4, 123.5
        [a | a' <- [1,2..6]], -- 132, 132.1, 132.2 179.8-180
        [b,b,b], -- 188.5-7
        [c | c' <- [1,2..7]], --224-224.2 267.8-268.1
        [b,b], -- 303.4-5
        [a | a' <- [1,2..6]] --312.0-2  359.8-360
        ]
        )
        
      innerRadii = innerRadiiBuilder 27.42  26.5 19.4
        
      angles = map Angle
        [0,8.5,8.6,43.9,44,44.1,44.2,87.8,87.9,88,88.1,123.4,123.5,132,132.1,132.2,179.8,179.9,180,
         188.5,188.6,188.7,224,224.1,224.2,267.8,267.9,268,268.1,303.4,303.5,312,312.1,312.2,359.8,359.9,360
        ]
  base <- buildCubePointsListAdd "base"
          (cylinder  innerRadii (map (transpose (+3)) innerRadii ) angles (Point 0 0 0) 5)
          [CornerPointsId | x <-[1..]]

  return base

handBaseStlGenerator :: IO ()
handBaseStlGenerator = do
  let cpoints = ((execState $ runExceptT (handBase)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
-- ============================================== helpers ==========================================
--curry in the stack pushing function
buildCubePointsListAdd = buildCubePointsList (++)
pixelsPerMM = 696/38
type PixelsPerMillimeter = Double
type RowReductionFactor = Int

