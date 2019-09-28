{-# LANGUAGE ParallelListComp #-}
--for persist
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{- |

The section of the socket which will have platform to attach the motor mount to.

It will need to be oversized, to fit over the wrist section.

It will have a full 360 wrist section, but the rest of the lenght will only be the back part of his elbow, as the main socket will be a ninja-flex with holes.
The ninja-flex will fit inside of it, and be attached to give it support and allow attachment to the wrist.

-}

module Examples.OpenHand.SocketMount(removeDefectiveTopRow', socketMountTestsDo,
                                     initializeDatabase, insertMount, {-viewMountByName,-} setCurrentMount,
                                     {-generateSocketMountStlUsingDbValues,-} showFaceDimensions, {-showSocketMountCubesUsingDbValues,-}
                                     generateSocketMountWithDegreesStlUsingDbValues) where

import Examples.OpenHand.Common(Dimensions(..), commontDBName, uniqueDimensionName, CommonFactors(..), setMountCommonFactors)

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (+++>))
import CornerPoints.Create(Origin(..), createCornerPoint)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractF2,
                                    extractBackTopLine, extractRightFace, extractFrontRightLine, extractFrontLeftLine, extractBottomFrontLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace,
                                    {-f12LineFromF34Line,-}toFrontLeftLine, f34LineFromF12Line )
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

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Persistable.Base as PstB

import Control.Lens

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
type Xslope = Double
type Yslope = Double
type RightLineToFrontFaceCount = Int

-- =================================== database =====================================================
socketMountDatabaseConnStr = "src/Examples/OpenHand/SocketMount.sql"

{-
Added in zHeight manually to the db as got error trying to add it to exsiting db.
Did not want to delete the db as I did not want to lose my data.
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Mount
   name String
   UniqueName name
   desc String
  deriving Show

FaceSlope
  xslope Double
  yslope Double
  --How many FrontRightLine to FrontFace to do when building the mount FrontFace's
  rightHandCount Int
  mountId MountId
  MountIdForFaceSlope mountId
 deriving Show

FaceDimensions
  zHeight Double default=5.0
  leftx Double
  lefty Double
  rightx Double
  righty Double
  mountId MountId
  MountIdForFaceDimensions mountId
 deriving Show

CurrentMount
  mountId MountId
 deriving Show
|]

-- | Initialize a new database with all tables. Will alter tables of existing db.
initializeDatabase :: IO ()
initializeDatabase = runSqlite socketMountDatabaseConnStr . PstB.asSqlBackendReader $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

-- | Insert a new Mount, FaceSlope, and FaceDimensions into the database. Sqlite browser will not do this.
insertMount :: IO ()
insertMount  = runSqlite socketMountDatabaseConnStr . PstB.asSqlBackendReader $ do
  mountId <- insert $ Mount "mount 1" "fits upright motors with board over top"
  insert $ FaceSlope 7  1 (-2) mountId
  insert $ FaceDimensions 5 15 (-35) (-40) (-25) mountId
  liftIO $ putStrLn "mounts inserted"


-- | Set the current Mount in the database. Can be done directlly in sqlite browser.
--ToDo: get rid of this and currentMount as I get it using a name String
setCurrentMount :: IO ()
setCurrentMount = runSqlite socketMountDatabaseConnStr . PstB.asSqlBackendReader $ do
  maybeMount <- getBy $ UniqueName "mount 1"
  case maybeMount of
    Nothing -> liftIO $ putStrLn "mount not found"
    Just (Entity mountId mount) -> do
      insert $ CurrentMount $  mountId
      liftIO $ putStrLn "current mount set"

-- Have a look at the FaceDimensions from the db
showFaceDimensions :: IO ()
showFaceDimensions = runSqlite socketMountDatabaseConnStr . PstB.asSqlBackendReader $ do
  maybeMount <- getBy $ UniqueName "mount 1"
  case maybeMount of
    Nothing -> liftIO $ putStrLn "mount not found"
    Just (Entity mountId mount) -> do
      maybeDimension <- (getBy $ MountIdForFaceDimensions mountId  )
      case maybeDimension of
        Nothing -> liftIO $ putStrLn "dimensions not found"
        Just (Entity dimensionId dimension) -> do
          liftIO $ print dimension
          

 -- =============================================== face/mount builder =====================================
{- Results are passed into generateMounts to build the 1st FrontFace of the mount.
   Other functions will turn this into a 2D grid of FrontFaces's to match all the target cubes of the socket.

Given:
  Location of the points, height, and slope of the FrontFace.

Task:
  Build the FrontFace from the input values.

Return:
  FrontFace: This is a wide Face that is the full width of the mount, and
  the height is ~= height of single layer of socket, as given by $ topZaxis - btmZaxis
-}
buildFrontFace :: Double -> Double -> Double -> Double -> Double -> Double -> Xslope -> Yslope -> CornerPoints
buildFrontFace    topZaxis       btmZaxis     leftXaxis        leftYaxis        rightXaxis        rightYaxis        xSlope    ySlope       =
                  (
                    (F2
                      (Point leftXaxis leftYaxis topZaxis)
                    )
                    +++
                    (F3
                      (Point rightXaxis rightYaxis topZaxis)
                    )
                  ) 
                  +++
                  ( 
                    (F1
                      (Point (leftXaxis + xSlope) (leftYaxis + ySlope) btmZaxis)
                    )
                    +++
                    (F4
                      (Point (rightXaxis + xSlope) (rightYaxis + ySlope) btmZaxis)
                    )
                  )



-- Base function for generating the Stl or showing the cubes from state of the builder mtl.
--Loads all the db values, and passed them into the stl/show function.
generateSocketMountStlUsingDbValuesBase :: String -> (FaceDimensions -> FaceSlope -> CommonFactors -> IO ()) -> IO ()
generateSocketMountStlUsingDbValuesBase commonDimensionsToUse processor  = runSqlite socketMountDatabaseConnStr . PstB.asSqlBackendReader $ do
  maybeMount <- getBy $ UniqueName "mount 1"
  maybeCommonDimensions <- runSqlite commontDBName . PstB.asSqlBackendReader $ do
     getBy $ uniqueDimensionName commonDimensionsToUse
  case maybeMount of
    Nothing -> liftIO $ putStrLn "mount not found"
    Just (Entity mountId mount) -> do
      maybeMountDimension <- (getBy $ MountIdForFaceDimensions mountId  )
      case maybeMountDimension of
        Nothing -> liftIO $ putStrLn "dimensions not found"
        Just (Entity dimensionId mountDimension) -> do
          maybeFaceSlope <- getBy $ MountIdForFaceSlope mountId
          case maybeFaceSlope of
            Nothing -> liftIO $ putStrLn "face slope not found"
            Just (Entity faceSlopeId faceSlope) -> do
              case maybeCommonDimensions of
                 Nothing -> liftIO $ putStrLn "common dimensions not found"
                 Just (Entity commonDimensionsId commonDimensions) -> do
                   liftIO $ processor mountDimension faceSlope $ setMountCommonFactors commonDimensions
                   liftIO $ putStrLn "socket mount has been processed"
                   

-- | Generate the socket stl
generateSocketMountWithDegreesStlUsingDbValues :: String -> IO ()
generateSocketMountWithDegreesStlUsingDbValues commonDimensionsToUse =
  generateSocketMountStlUsingDbValuesBase commonDimensionsToUse (socketMountWithDegreesStlGenerator)

-- | Output the state of the the socket builder.
showSocketMountWithDegreesStlUsingDbValues :: String -> IO ()
showSocketMountWithDegreesStlUsingDbValues commonDimensionsToUse =
  generateSocketMountStlUsingDbValuesBase commonDimensionsToUse (socketMountWithDegreesShowCubes)

-- Use by generateSocketMountStlUsingDbValuesBase to output the socket stl
socketMountWithDegreesStlGenerator :: FaceDimensions -> FaceSlope -> CommonFactors -> IO ()
socketMountWithDegreesStlGenerator faceDimensions faceSlope (CommonFactors outerFlexSocketTranspose outerMountSocketTranspose dropFactor takeFactor)  = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let rowReductionFactor = 100::RowReductionFactor
            innerSleeveMDR = (transpose (+ outerFlexSocketTranspose)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                degrees'
                              )

            outerSleeveMDR = transpose (+ (outerMountSocketTranspose - outerFlexSocketTranspose)) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (socketMountWithDegrees (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM faceDimensions faceSlope dropFactor takeFactor) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

-- Use by generateSocketMountStlUsingDbValuesBase to output the state of the builder mtl.
socketMountWithDegreesShowCubes :: FaceDimensions -> FaceSlope -> CommonFactors -> IO ()
socketMountWithDegreesShowCubes faceDimensions faceSlope (CommonFactors outerFlexSocketTranspose outerMountSocketTranspose dropFactor takeFactor)  = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let rowReductionFactor = 100::RowReductionFactor
            innerSleeveMDR = (transpose (+ outerFlexSocketTranspose)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                degrees'
                              )

            outerSleeveMDR = transpose (+ (outerMountSocketTranspose - outerFlexSocketTranspose)) innerSleeveMDR
            cpoints =  ((evalState $ runExceptT (socketMountWithDegrees
                                                 (degrees innerSleeveMDR)
                                                 (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM faceDimensions faceSlope dropFactor takeFactor
                                                )
                        ) []
                       )
        in  print $ show cpoints
      Nothing                                ->
        putStrLn "File not decoded"

  
--Build the mount [FrontFace] that gets attached to the socket.
--As there is a section of socket to attach to, it must convert from a single mount FrontFace to
--a  [FrontFace] that matches the number of horizontal cubes of the socket section being attached to.
buildMountList :: RightLineToFrontFaceCount -> CornerPoints -> [CornerPoints]
buildMountList rightLineToFrontFaceCount frontFace =
  let 
      frontRightLine  = extractFrontRightLine frontFace
      frontLeftLine = toFrontLeftLine frontRightLine 
      rightLineAsFace = frontLeftLine +++ frontRightLine 
      frontLeftLine' = extractFrontLeftLine frontFace
      frontRightLine' = f34LineFromF12Line frontLeftLine' 
      leftLineAsFace = frontLeftLine' +++ frontRightLine'

      totalFaceCount = 17
      frontFaceCount = 1
      leftHandCount = totalFaceCount - (rightLineToFrontFaceCount + frontFaceCount)

  in  [rightLineAsFace | face <- [1..rightLineToFrontFaceCount]]
      ++
      frontFace : [leftLineAsFace | face <- [1..leftHandCount]]
     

{-
Take the horizontal [FrontFace] from buildMountList  and extend them downwards for each layer of the socket
being attached to.

Given:
height: height of the socket layer
xSlope    ySlope: add a slope
rightLineToFrontFaceCount: calculate the number of frontFaceFromLeft/RightLine as used by buildMountList
frontFace: The previous FrontFace to extend downwards from.
-}      
generateMountsWithDegrees :: Height -> Xslope -> Yslope -> RightLineToFrontFaceCount -> CornerPoints -> [[CornerPoints]]
generateMountsWithDegrees    height    xSlope    ySlope    rightLineToFrontFaceCount     frontFace =
  let 
     mountList = buildMountList rightLineToFrontFaceCount frontFace
  in
     mountList : generateMountsWithDegrees' height xSlope ySlope rightLineToFrontFaceCount  frontFace []

generateMountsWithDegrees' :: Height -> Xslope -> Yslope -> RightLineToFrontFaceCount ->  CornerPoints -> [[CornerPoints]] -> [[CornerPoints]]
generateMountsWithDegrees'    height    xSlope    ySlope    righHandCount       prevFrontFace          xs  =
  let
    currFrontFace =
     (prevFrontFace)
     +++
     ((transposeZ (+(height))) . (transposeY(+(ySlope))) . (transposeX(+(xSlope))) . extractBottomFrontLine  $ prevFrontFace)

    mountList = buildMountList righHandCount currFrontFace
  in
    mountList : generateMountsWithDegrees' height xSlope ySlope righHandCount currFrontFace (xs) 


--Build the socket mount.
socketMountWithDegrees :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter ->
               FaceDimensions -> FaceSlope -> Int -> Int -> 
               ExceptT BuilderError (State CpointsStack ) CpointsList
socketMountWithDegrees    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM
               (FaceDimensions  zHeight_ leftx_ lefty_ rightx_ righty_ _) (FaceSlope xSlope ySlope rightLineToFrontFaceCount _) drop' take'  = do
  let 
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      targetDegrees = extractSDRWithinRange [40,50..210] --[40,50..210]
      
  {-
  The is the section of the socket which the mount attaches to.
  It will sit against the socket, so it's inside transpose factors are the same as the outer flex socket transpose.
  -}
  socketCubesToWhichTheMountAttaches
             <- buildCubePointsListSingle "wristCubes"
                (concat $ take 100 $ drop (drop' +2)  (createVerticalWalls
                                                       (targetDegrees innerSleeveSDR)
                                                       (targetDegrees outerSleeveSDR)
                                                       origin transposeFactors)
                ) 
               
  
  
  let
    socketCubesForInnerMountWalls  = (take 100 $ drop (drop' + 2)
                                      (createVerticalWalls
                                       (targetDegrees innerSleeveSDR)
                                       (targetDegrees outerSleeveSDR)
                                       origin transposeFactors)
                                     )
    topOfFirstMountFrontFace = z_axis $ f2 $ extractF2 $ head $ head socketCubesForInnerMountWalls
    bottomOfFirstMountFrontFace = topOfFirstMountFrontFace - zHeight_
    allMountFrontFaces = generateMountsWithDegrees
                           (- zHeight_)  xSlope (ySlope) rightLineToFrontFaceCount  $
                           buildFrontFace topOfFirstMountFrontFace  bottomOfFirstMountFrontFace
                           leftx_ lefty_ rightx_ righty_ xSlope (ySlope)
                                                            
  
  
  motorMountThatExtendsOutFromTheSocket
             <- buildCubePointsListSingle "wristMount"
                (concat $ 
                   [gen1 |+++| gen2
                    | gen1 <-  socketCubesForInnerMountWalls 
                    | gen2 <- allMountFrontFaces 
                   ]
                )
  
  return socketCubesToWhichTheMountAttaches


removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']


-- ========================================================== local testing ==================================================
mount1 = buildFrontFace   30         20       5         (-60)     (-50)     (-60) 0 0

socketMountTestsDo = do
  
  
  runTestTT buildFrontFaceNoSlope
  runTestTT buildFrontFaceWithSlope



buildFrontFaceNoSlope = TestCase $ assertEqual 
  "buildFrontFaceNoSlope"
  (
   FrontFace {
      f1 = Point {x_axis = 15.0, y_axis = -55.0, z_axis = 20.0},
      f2 = Point {x_axis = 15.0, y_axis = -55.0, z_axis = 30.0},
      f3 = Point {x_axis = -40.0, y_axis = -45.0, z_axis = 30.0},
      f4 = Point {x_axis = -40.0, y_axis = -45.0, z_axis = 20.0}}

  )
  (buildFrontFace 30   20   15 (-55) (-40) (-45) 0 (0))

buildFrontFaceWithSlope = TestCase $ assertEqual 
  "buildFrontFaceWithSlope"
  (
   FrontFace {
      f1 = Point {x_axis = 15.0, y_axis = -57.0, z_axis = 20.0},
      f2 = Point {x_axis = 15.0, y_axis = -55.0, z_axis = 30.0},
      f3 = Point {x_axis = -40.0, y_axis = -45.0, z_axis = 30.0},
      f4 = Point {x_axis = -40.0, y_axis = -47.0, z_axis = 20.0}}

  )
  (buildFrontFace 30   20   15 (-55) (-40) (-45) 0 (-2))
-- ======================================================= test 2 with head =============================================================
