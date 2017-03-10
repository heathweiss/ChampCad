{-# LANGUAGE ParallelListComp #-}
{- |

The section of the socket which will have platform to attach the motor mount to.

It will need to be oversized, to fit over the wrist section.

It will have a full 360 wrist section, but the rest of the lenght will only be the back part of his elbow, as the main socket will be a ninja-flex with holes.
The ninja-flex will fit inside of it, and be attached to give it support and allow attachment to the wrist.

-}

module Examples.OpenHand.SocketMount(socketMountStlGenerator, socketMountShowCubes, socketMountTestsDo) where
  
import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine,
                                    extractBackTopLine, extractRightFace, extractFrontRightLine, extractFrontLeftLine, extractBottomFrontLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    f12LineFromF34Line, f34LineFromF12Line )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.HorizontalFaces(createTopFaces,  createBottomFaces, createTopFacesSquaredOff)

import TypeClasses.Transposable(transpose)


import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), )
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)
import Geometry.Radius(doubleCylinderZip, squaredOff)
import Geometry.Rotation(rotateCornerPointAroundZAxis)
import Geometry.AngleRadius(AngleRadius(..), extractAngles, extractRadii,)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)

import Helpers.List((++:))

import Primitives.Cylindrical.Walled(squaredYLengthenedCylinder, squaredCylinder, cylinder)

import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Scan.ParseJuicy(getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                      calculateRadiusFrom)
import  Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)
import Helpers.List((++:),(++::))

import Builder.Sequence((@~+++@|>))
import Builder.List (newCornerPointsWith10DegreesBuilder, (||@~+++^||))
import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList, buildCubePointsListWithAdd, buildCubePointsListSingle)

import Stl.StlCornerPointsWithDegrees(FacesWithRange(..))


import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Flow as Flw
import Control.Lens

--import qualified Test.HUnit as T
import Test.HUnit hiding (State)

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

pixelsPerMM = 696/38
type RowReductionFactor = Int
type ExtensionHeight = Double
type PlateRadius = Double
type PixelsPerMillimeter = Double
type TransposeFactor = Double
type Thickness = Double
type Height = Double
type Power = Double
type LengthenYFactor = Double

buildFrontFace :: Double -> Double -> Double -> Double -> Double -> Double -> CornerPoints
buildFrontFace    ztop       zbtm     lx        ly        rx        ry =
                  (
                    (F2
                      (Point lx ly ztop)
                    )
                    +++
                    (F3
                      (Point rx ry ztop)
                    )
                  ) 
                  +++
                  ( 
                    (F1
                      (Point lx ly zbtm)
                    )
                    +++
                    (F4
                      (Point rx ry zbtm)
                    )
                  )

  


{-
Generate a list of mount faces, each being transposed downwards

-}
generateMounts :: Height -> CornerPoints -> [[CornerPoints]]
generateMounts height frontFace =
  let 
     mountList = buildMountList frontFace
  in
     mountList : generateMounts' height  mountList []

--recursive call for generateMounts
generateMounts' :: Height ->  [CornerPoints] -> [[CornerPoints]] -> [[CornerPoints]]
generateMounts'    height      currCpts          xs  =
  let
    mountNew =
     (head currCpts)
     +++
     ((transposeZ (+(height))) . extractBottomFrontLine  $ head currCpts)

    mountList = buildMountList mountNew
  in
    mountList : generateMounts' height mountList (xs) 


buildMountList :: CornerPoints -> [CornerPoints]
buildMountList frontFace =
  let 
      a = extractFrontRightLine frontFace
      b = f12LineFromF34Line a
      c = b +++ a
      rightMount1 = b +++ a
      d = extractFrontLeftLine frontFace
      e = f34LineFromF12Line d
      leftMount1 = d +++ e

  in
      [frontFace, leftMount1 ]
      ++
      [CornerPointsNothing | x <-[1,2..29]]
      ++
      [rightMount1, rightMount1, rightMount1, rightMount1, rightMount1]

-- | The wrist and back strip of the socket, with a platform to attach the motor/board box.
socketMount :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
socketMount    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM = do
  let extensionHeight = 30
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])
      radii = repeat $ Radius 15 -- [Radius x | x <- [15,15..]] --
      radiiSquared = --take 2 
                     [squaredOff 3 radius' angle'
                       | radius' <- radii
                       | angle'  <- angles
                     ]
      backStripCutterCubes =
        [CornerPointsId, CornerPointsId]
        ++
        [CornerPointsNothing | x <-[1,2..29]]
        ++
        --(repeat CornerPointsId)
        [CornerPointsId | x <-[1,2..5]]

      backStripCutterCubesList = concat [backStripCutterCubes | x <-  [1..]]

  {-
  wristCubes  <- buildCubePointsListWithAdd "wristCubes"
                (concat $ take 3 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]
  -}
  
  wristCubes1  <- buildCubePointsListWithAdd "wristCubes"
                (head $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]
  
  wristCubes2  <- buildCubePointsListWithAdd "wristCubes"
                --(head $ drop 2  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                (concat $ take 2 $ drop 2  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]
  
 
                 
      
      
  {-
  platformWrist2
             <- buildCubePointsListWithAdd "platformWrist2"
                (wristCubes2)
                ([mount1, leftMount1 ])
  -}
  
  let clist = --(take 2 $ drop 4  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
              (drop 4  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
      bGen  = --generateMounts (-10) mount1
              generateMounts (-5.2) $ buildFrontFace 30   20   15 (-55) (-40) (-45)
              --                                     ztop zbtm lx ly    rx    ry 
  handleCubes1
              <- buildCubePointsListWithAdd "handleCubes1"
                --(concat $ take 2 $ drop 4  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                (concat $ drop 4  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                backStripCutterCubesList
  
  platformHandle1
             <- buildCubePointsListSingle "platformWrist2"
                (concat $ 
                   [gen1 |+++| gen2
                    | gen1 <- clist
                    | gen2 <- bGen
                   ]
                )
  
  {-
  platformHandle1
             <- buildCubePointsListWithAdd "platformWrist2"
                (handleCubes1)
                (concat $ generateMounts mount1)

  
  platformHandle1
             <- buildCubePointsListWithAdd "platformWrist2"
                (handleCubes1)
                (concat $ generateMounts mount1)
  -}
  {-
  platformHandle1
             <- buildCubePointsListWithAdd "platformWrist2"
                (handleCubes1)
                ([mount1, leftMount1 ]
                 ++
                 [CornerPointsNothing | x <-[1,2..29]]
                 ++
                 [rightMount1, rightMount1, rightMount1, rightMount1, rightMount1]
                )
  -}
  {-
  handleCubes2
              <- buildCubePointsListWithAdd "handleCubes2"
                --take 3 is normal, but use more to see what is going on
                (concat $ take 1 $ drop 5  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                backStripCutterCubes

  platformFrontFaces
             <- buildCubePointsListWithAdd "platformFrontFaces"
                (handleCubes2)
                (mount1)
  {- generate all the handle layers in 1 pass.
     But will I be able to match up the platform this way.
     Leave this here till know if can remove.
  allHandleCubes1
              <- buildCubePointsListWithAdd "wristCubes"
                --take 3 is normal, but use more to see what is going on
                (concat $ drop 4  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                (concat $ repeat backStripCutterCubes)             
  -}

  {-
  platformFrontFaces
             <- buildCubePointsListWithAdd "platformFrontFaces"
                (handleCubes2)
                (mount1)
  -}              
  -}
  return platformHandle1

--load the json file and call generate stl
socketMountStlGenerator :: IO ()
socketMountStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let rowReductionFactor = 100::RowReductionFactor
            innerSleeveTransposeFactor = (+6) --3 is that starndard value. Make it 6 as this has to fit over the wrist and ninjaflex
            innerSleeveMDR = (transpose innerSleeveTransposeFactor) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                --(rotateSDR . rotateSDR . rotateSDR $ degrees')
                                degrees'
                              )
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (socketMount (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"


socketMountShowCubes :: IO ()
socketMountShowCubes = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 100::RowReductionFactor 
            --innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) . (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $ (MultiDegreeRadii name' degrees')
            innerSleeveMDR = (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                --(rotateSDR . rotateSDR . rotateSDR $ degrees')
                                degrees'
                              )
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            --cpoints =  ((execState $ runExceptT (socketMount (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
            cpoints =  ((evalState $ runExceptT (socketMount (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  --writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
            print $ show cpoints
      Nothing                                ->
        putStrLn "File not decoded"


removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']


-- ========================================================== local testing ==================================================
mount1 = buildFrontFace   30         20       5         (-60)     (-50)     (-60)

socketMountTestsDo = do
  runTestTT mount1BuilderTest
  runTestTT mount2BuilderTest

mount1BuilderTest =  TestCase $ assertEqual
   "mount1BuilderTest"
    [FrontFace {
       f1 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 20.0},
       f2 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 30.0},
       f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
       f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 20.0}},
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,
    FrontFace {
               f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}}]

   (last $ take 1 $ generateMounts (-10) mount1)


mount2BuilderTest =  TestCase $ assertEqual
   "mount2BuilderTest"
    [FrontFace {
       f1 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 20.0},
       f2 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 30.0},
       f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
       f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = 5.0, y_axis = -60.0, z_axis = 20.0}},
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,CornerPointsNothing,
    CornerPointsNothing,
    FrontFace {
               f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}},
    FrontFace {f1 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0},
               f2 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f3 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 30.0},
               f4 = Point {x_axis = -50.0, y_axis = -60.0, z_axis = 20.0}}]

  (last $ reverse $ take 2 $ generateMounts (-10) mount1)


-- ======================================================= test 2 with head =============================================================
