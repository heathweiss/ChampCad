{-# LANGUAGE ParallelListComp #-}
{- |
The wrist section attach to which will be attached:
 The fingers.
 The motor and board sections.
 The main socket made out of semi-flex.
-}
module Examples.OpenHand.Wrist(wristAndDoubleCylinderStlGenerator, wristAndDoubleCylinderShowCubes, wristSquaredOffStlGenerator, wristSquaredOffShowCubes) where

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.HorizontalFaces(createTopFaces,  createBottomFaces, createTopFacesSquaredOff)

import TypeClasses.Transposable(transpose)


import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), )
import Geometry.Slope(Slope(..), flatXSlope, flatYSlope, slopeAdjustedForVerticalAngle)
import Geometry.Radius(doubleCylinderZip, squaredOff)
import Geometry.Rotation(rotateCornerPointAroundZAxis)

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

--rotate the measured radii.
--will need to be put in some permanent module
rotate :: [a] -> [a]
rotate list = (last list) : (init list)

rotateBack :: [a] -> [a]
rotateBack (x:xs) = xs ++ [x]

{- |
Wrist section with a squared off partial riser, to which the fingers can be attached.
-}
wristSquaredOff :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
wristSquaredOff    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM = do
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

   
  wristCubes  <- buildCubePointsListWithAdd "wristCubes"
                --take 3 is normal, but use more to see what is going on
                (concat $ take 3 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]

  topOfWristAsLowerFaces
             <- buildCubePointsListSingle "extrudeTop"
                (map (lowerFaceFromUpperFace . extractTopFace) (head (drop 1 $ createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors))
                )

  let rotate' = 40
  -- just see what happens, to know if topOfWristAsLowerFaces is good, and it is.
  --then added the rotation which, like squared off, is NFG
  {-
  topOfWristExtended
             <- buildCubePointsListWithAdd "topOfWristExtended"
                ( {-rotate . rotate . rotate . rotate . rotate . rotate. rotate . rotate . rotate  $-}
                  (map  ( (rotateCornerPointAroundZAxis 10 (Point 0 0 75)) . (transposeZ (+50)) . upperFaceFromLowerFace)
                        topOfWristAsLowerFaces
                  )
                )
                topOfWristAsLowerFaces
  
  -}
  --built up from basics, and rotated at a single point level. It works but so does the squaredCylinder method now.
  --Should be able to get rid of this
  {-
  topOfWristExtendedFromCorners
           <- buildCubePointsListSingle "topOfWristExtendedFromCorners"
              
              ( (
                 (
                  ( rotateCornerPointAroundZAxis rotate' (Point 0 0 75))
                  (createCornerPoint (F3) (Point 0 0 75) (transpose (+5) $ head radiiSquared) (head angles))
                 )
                 +++
                 (
                  ( rotateCornerPointAroundZAxis rotate' (Point 0 0 75))
                  (createCornerPoint (B3) (Point 0 0 75) (head radiiSquared) (head angles))
                 )
                )
                +++> 
                [  ((rotateCornerPointAroundZAxis rotate' (Point 0 0 75))
                   (createCornerPoint (F2) (Point 0 0 75) (radiusF) (angle')))
                  
                  +++
                   ( rotateCornerPointAroundZAxis rotate' (Point 0 0 75))
                    (createCornerPoint (B2) (Point 0 0 75) (radiusB) (angle'))
                   
                  | radiusF <- map (transpose (+5)) (tail radiiSquared)
                  | radiusB <- (tail radiiSquared)
                  | angle'  <- tail angles
                ]

                
               )
                
              
              

  -}
  
  --need to do some tests to see what is going wrong with squaredTopCubes.
        
  squaredTopFaces
             <- buildCubePointsListWithAdd "squaredTopCubes"
                ( rotate . rotate . rotate . rotate . rotate . rotate . rotate $
                  (map
                   ((rotateCornerPointAroundZAxis 80 (Point 0 0 75)) . extractTopFace)
                    (squaredCylinder radii 4 (Point 0 0 65) angles 0 5)
                  )
                ) 
                
                ([CornerPointsId |  x <- [1..4]] ++
                 [CornerPointsNothing | x <- [1..24]] ++
                 [CornerPointsId | x <- [1..]]
                )
            
  squaredTopCubes
             <-  buildCubePointsListWithAdd "newTop"
                 squaredTopFaces
                 topOfWristAsLowerFaces

{- backup: 
 squaredTopCubes
             <- buildCubePointsListWithAdd "squaredTopCubes"
                ( rotate . rotate . rotate . rotate . rotate $
                  (map
                   ((rotateCornerPointAroundZAxis 60 (Point 0 0 75)) . extractTopFace)
                    (squaredCylinder radii 4 (Point 0 0 75) angles 0 5)
                  )
                ) 
                
                topOfWristAsLowerFaces

-}
  {-
  newTop   <-  buildCubePointsListWithAdd "newTop"
               squaredTopCubes
               topOfWristAsLowerFaces
  -}             
  return squaredTopCubes

--load the json file and call generate stl
wristSquaredOffStlGenerator :: IO ()
wristSquaredOffStlGenerator = do
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
            cpoints =  ((execState $ runExceptT (wristSquaredOff (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"


wristSquaredOffShowCubes :: IO ()
wristSquaredOffShowCubes = do
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
            --cpoints =  ((execState $ runExceptT (wristSquaredOff (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
            cpoints =  ((evalState $ runExceptT (wristSquaredOff (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  --writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
            print $ show cpoints
      Nothing                                ->
        putStrLn "File not decoded"


  
{- |
Wrist section that is topped off with a partial double cylinder.
May work but the curvature could be a problem.
-}
wristAndDoubleCylinder :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
wristAndDoubleCylinder    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM = do
  let extensionHeight = 30
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])
      radii = [Radius x | x <- [15,15..]]

   
  mainCubes  <- buildCubePointsListWithAdd "mainCubes"
                --take 3 is normal, but use more to see what is going on
                (concat $ take 3 $ drop 1  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]
  
  
  getTop     <- buildCubePointsListSingle "extrudeTop"
                (map (lowerFaceFromUpperFace . extractTopFace) (head (drop 1 $ createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors))
                )
                
        
  newTop     <- buildCubePointsListWithAdd "newTop"
              --(createTopFaces (Point 0 0 100) (doubleCylinderZip radii angles) angles  )
                ( map
                  (extractTopFace)
                  ( let rotatedAngles = map (rotateAngle 110) angles
                    in
                     cylinder
                      (doubleCylinderZip radii rotatedAngles)
                      (doubleCylinderZip (map (transpose (+2) )   radii) rotatedAngles)
                      angles
                      (Point (-4) (-9) 0)
                      60
                  )
                )
              ([CornerPointsId | x <-[1,2..8]] ++ [CornerPointsNothing | x <-[1,2..16]] ++ [CornerPointsId | x <-[1,2..]])

  

  doubleTop  <- buildCubePointsListWithAdd "newTop"
                   (getTop)
                   (newTop)
  
  return doubleTop

--load the json file and call generate stl
wristAndDoubleCylinderStlGenerator :: IO ()
wristAndDoubleCylinderStlGenerator = do
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
            cpoints =  ((execState $ runExceptT (wristAndDoubleCylinder (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"


wristAndDoubleCylinderShowCubes :: IO ()
wristAndDoubleCylinderShowCubes = do
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
            --cpoints =  ((execState $ runExceptT (wristAndDoubleCylinder (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
            cpoints =  ((evalState $ runExceptT (wristAndDoubleCylinder (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  --writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
            print $ show cpoints
      Nothing                                ->
        putStrLn "File not decoded"



removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']

