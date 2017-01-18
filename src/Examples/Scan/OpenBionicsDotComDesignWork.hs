module Examples.Scan.OpenBionicsDotComDesignWork (socketNoConnectorStlGenerator, innerHandBaseStlGenerator, outerHandBaseStlGenerator,
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
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..), Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine,
                                    extractBackBottomLine, extractBackTopLine, extractBottomFrontLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace, backBottomLineFromBottomFrontLine )
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
-- ===============================================inner/outer hand base============================
{-
connect together the inner/outer hand bases so inner/outer walls will be different.
This would be the proper shape that would then transfomr to circle and socket.
-}

innerOuterHandBase :: ExceptT BuilderError (State CpointsStack ) CpointsList
innerOuterHandBase = do
  let
      -- ======================= outer radii info ===================================
      outerAngles =  [Angle a | a <- [0,10..360]]

      --outerRadii = rotateBack outerRadii' 
      --outerRadii will have to be rotated forward to match up with inner angles as they are measured
      --with a different initial 0 degrees.
      outerRadii = map Radius
        [33.86, --0
         32.08, --10
         30.91, --20
         30.03, --30
         27.63, --40
         25.5,  --50
         23.94, --60
         23.15, --70
         21.75, --80
         21.8,  --90
         21.71, --100
         22.25, --110
         23.54, --120
         25.41, --130
         27.05, --140
         29.7,  --150
         31.42, --160
         32.08, --170
         31.09, --180
         31.35, --190
         31.82, --200
         31.58, --210
         31.04, --220
         29.62, --230
         27.43, --240
         25.27, --250
         24.15, --260
         23.83, --270
         24.38, --280
         25.55, --290
         27.98, --300
         32.18, --310
         35.47, --320
         36.03, --330
         35.16, --340
         34.46, --350
         33.86  --360
        ]
      -- ============================= inners radii info========================
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

      --slightly too small at 27.42  26.5 19.4 
      innerRadii = innerRadiiBuilder 28.0  27.0 20.0
      innerAngles = map (\(Angle x) -> Angle (x + 20)) innerAngles'         
      innerAngles' = map Angle
        [0,8.5,8.6,43.9,44,44.1,44.2,87.8,87.9,88,88.1,123.4,123.5,132,132.1,132.2,179.8,179.9,180,
         188.5,188.6,188.7,224,224.1,224.2,267.8,267.9,268,268.1,303.4,303.5,312,312.1,312.2,359.8,359.9,360
        ]
      

  innerWall <- buildCubePointsListAdd "innerWall"
               ( map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
                 (createBottomFaces (Point 0 0 0) innerRadii innerAngles flatXSlope flatYSlope)
               )
               [CornerPointsId | x <-[1..]]
  outerWall <- buildCubePointsListAdd "outerWall"
               ( map extractBottomFrontLine
                 (createBottomFaces (Point 0 0 0) outerRadii outerAngles flatXSlope flatYSlope)
               )
               [CornerPointsId | x <-[1..]]

  btmFaces <- buildCubePointsListAdd "btmFaces"
              innerWall
              outerWall

  walls    <- buildCubePointsListAdd "walls"
              (map (upperFaceFromLowerFace . (transposeZ (+10))) btmFaces)
              btmFaces

  return walls

innerOuterHandBaseStlGenerator :: IO ()
innerOuterHandBaseStlGenerator = do
  let cpoints = ((execState $ runExceptT (innerOuterHandBase)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

-- =============================================== outer hand base ==================================
{-
The shape of the base of the hand, on the outside of hand. So the shape of the wrist.

Just look at the wrist on its own for now.

-}
outerHandBase :: ExceptT BuilderError (State CpointsStack ) CpointsList
outerHandBase = do
  let
      -- ======================= outer radii info ===================================
      outerAngles =  [Angle a | a <- [0,10..360]]

      --outerRadii = rotateBack outerRadii' 
      --outerRadii will have to be rotated forward to match up with inner angles as they are measured
      --with a different initial 0 degrees.
      outerRadii = map Radius
        [33.86, --0
         32.08, --10
         30.91, --20
         30.03, --30
         27.63, --40
         25.5,  --50
         23.94, --60
         23.15, --70
         21.75, --80
         21.8,  --90
         21.71, --100
         22.25, --110
         23.54, --120
         25.41, --130
         27.05, --140
         29.7,  --150
         31.42, --160
         32.08, --170
         31.09, --180
         31.35, --190
         31.82, --200
         31.58, --210
         31.04, --220
         29.62, --230
         27.43, --240
         25.27, --250
         24.15, --260
         23.83, --270
         24.38, --280
         25.55, --290
         27.98, --300
         32.18, --310
         35.47, --320
         36.03, --330
         35.16, --340
         34.46, --350
         33.86  --360
        ]

  base <- buildCubePointsListAdd "base"
          (cylinder  outerRadii (map (transpose (+3)) outerRadii ) outerAngles (Point 0 0 0) 5)
          [CornerPointsId | x <-[1..]]

  return base

outerHandBaseStlGenerator :: IO ()
outerHandBaseStlGenerator = do
  let cpoints = ((execState $ runExceptT (outerHandBase)) [])
  writeStlToFile $ newStlShape "cpoinst"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

-- ===============================================inner hand base ======================================
{-
Have a look at the base of the hand on its own, to print and fit.
This is the inner section, the semi-octagon shape
-}

innerHandBase :: ExceptT BuilderError (State CpointsStack ) CpointsList
innerHandBase = do
  let
      -- ============================= inners radii info========================
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
      innerAngles = map (\(Angle x) -> Angle (x + 20)) innerAngles'         
      innerAngles' = map Angle
        [0,8.5,8.6,43.9,44,44.1,44.2,87.8,87.9,88,88.1,123.4,123.5,132,132.1,132.2,179.8,179.9,180,
         188.5,188.6,188.7,224,224.1,224.2,267.8,267.9,268,268.1,303.4,303.5,312,312.1,312.2,359.8,359.9,360
        ]

      
  base <- buildCubePointsListAdd "base"
          (cylinder  innerRadii (map (transpose (+3)) innerRadii ) innerAngles (Point 0 0 0) 5)
          [CornerPointsId | x <-[1..]]

  return base

innerHandBaseStlGenerator :: IO ()
innerHandBaseStlGenerator = do
  let cpoints = ((execState $ runExceptT (innerHandBase)) [])
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

{-

outerRadii = map Radius
        [33.86, --0
         32.08, --10
         30.91, --20
         30.03, --30
         27.63, --40
         25.5,  --50
         23.94, --60
         23.15, --70
         21.75, --80
         21.8,  --90
         21.71, --100
         22.25, --110
         23.54, --120
         25.41, --130
         27.05, --140
         29.7,  --150
         31.42, --160
         32.08, --170
         31.09, --180
         31.35, --190
         31.82, --200
         31.58, --210
         31.04, --220
         29.62, --230
         27.43, --240
         25.27, --250
         24.15, --260
         23.83, --270
         24.38, --280
         25.55, --290
         27.98, --300
         32.18, --310
         35.47, --320
         36.03, --330
         35.16, --340
         34.46, --350
         33.86  --360
        ]
-}
