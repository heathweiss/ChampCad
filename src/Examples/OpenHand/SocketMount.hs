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

module Examples.OpenHand.SocketMount(socketMountStlGenerator, socketMountShowCubes, socketMountTestsDo,
                                     initializeDatabase, insertMount, viewMountByName, setCurrentMount,
                                     generateSocketMountStlUsingDbValues, showFaceDimensions, showSocketMountCubesUsingDbValues) where

import Examples.OpenHand.Common(Dimensions(..), commontDBName, uniqueDimensionName, CommonFactors(..), setMountCommonFactors)

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
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
  mountId MountId
  MountIdForFaceSlope mountId
 deriving Show

FaceDimensions
  --zTop Double
  --zBtm Double
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
initializeDatabase = runSqlite socketMountDatabaseConnStr $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

-- | Insert a new Mount, FaceSlope, and FaceDimensions into the database. Sqlite browser will not do this.
insertMount :: IO ()
insertMount  = runSqlite socketMountDatabaseConnStr $ do
  mountId <- insert $ Mount "mount 1" "fits upright motors with board over top"
  insert $ FaceSlope 1 (-2) mountId
  insert $ FaceDimensions 5 15 (-35) (-40) (-25) mountId
  liftIO $ putStrLn "mounts inserted"
{-before adding xslope field 
insertMount :: IO ()
insertMount  = runSqlite socketMountDatabaseConnStr $ do
  mountId <- insert $ Mount "mount 1" "fits upright motors with board over top"
  insert $ FaceSlope (-2) mountId
  insert $ FaceDimensions 5 15 (-35) (-40) (-25) mountId
  liftIO $ putStrLn "mounts inserted"
-}
--View a Mount in the Db. Probably useless not that there is a CurrentMount.
viewMountByName :: IO ()
viewMountByName = runSqlite socketMountDatabaseConnStr $ do
  maybeMount <- getBy $ UniqueName "mount 1"
  case maybeMount of
    Nothing -> liftIO $ putStrLn "Just kidding, not really there"
    Just (Entity mountId mount) -> liftIO $ print mount



-- | Set the current Mount in the database. Can be done directlly in sqlite browser. 
setCurrentMount :: IO ()
setCurrentMount = runSqlite socketMountDatabaseConnStr $ do
  maybeMount <- getBy $ UniqueName "mount 1"
  case maybeMount of
    Nothing -> liftIO $ putStrLn "mount not found"
    Just (Entity mountId mount) -> do
      insert $ CurrentMount $  mountId
      liftIO $ putStrLn "current mount set"
-- Have a look at the FaceDimensions from the db
showFaceDimensions :: IO ()
showFaceDimensions = runSqlite socketMountDatabaseConnStr $ do
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

{-
Generate a list of mount faces, each being transposed downwards

-}
generateMounts :: Height -> Xslope -> Yslope -> CornerPoints -> [[CornerPoints]]
generateMounts    height    xSlope    ySlope    frontFace =
  let 
     mountList = buildMountList frontFace
  in
     mountList : generateMounts' height xSlope ySlope  frontFace []

--recursive call for generateMounts
generateMounts' :: Height -> Xslope -> Yslope ->  CornerPoints -> [[CornerPoints]] -> [[CornerPoints]]
generateMounts'    height    xSlope    ySlope     prevFrontFace          xs  =
  let
    currFrontFace =
     (prevFrontFace)
     +++
     ((transposeZ (+(height))) . (transposeY(+(ySlope))) . (transposeX(+(xSlope))) . extractBottomFrontLine  $ prevFrontFace)

    mountList = buildMountList currFrontFace
  in
    mountList : generateMounts' height xSlope ySlope currFrontFace (xs) 

{-
Used by generateMounts which supplies initial FrontFace of the socket mount.
From this intial FrontFace, all the subseqent faces to be added to the currernt layer of the socket
are generated. This must account for all the cubes of the socket layer, which is all 36.

It is by maniplating this list, that the socket cubes are chosen, to be attached to.
Works in conjuction with the db values for the acutual position if the initial FrontFace.
That is done before it is passed in here.

-}
buildMountList :: CornerPoints -> [CornerPoints]
buildMountList frontFace =
  let 
      frontRightLine  = extractFrontRightLine frontFace
      frontLeftLine = toFrontLeftLine frontRightLine 
      rightLineAsFace = frontLeftLine +++ frontRightLine 
      frontLeftLine' = extractFrontLeftLine frontFace
      frontRightLine' = f34LineFromF12Line frontLeftLine' 
      leftLineAsFace = frontLeftLine' +++ frontRightLine'

  in  [CornerPointsNothing | x <-[1,2..4]]
      ++
      [rightLineAsFace, rightLineAsFace, rightLineAsFace, rightLineAsFace, rightLineAsFace, rightLineAsFace,  rightLineAsFace , rightLineAsFace,
       frontFace,
       leftLineAsFace, leftLineAsFace, leftLineAsFace, leftLineAsFace, leftLineAsFace, leftLineAsFace, leftLineAsFace, leftLineAsFace ]
      ++
      [CornerPointsNothing | x <-[1..]]

-- ========================================= Builder ====================================================

-- | The wrist and back strip of the socket, with a platform to attach the motor/board box.
socketMount :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter ->
               FaceDimensions -> FaceSlope -> Int -> Int ->
               ExceptT BuilderError (State CpointsStack ) CpointsList
socketMount    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM
               (FaceDimensions  zHeight_ leftx_ lefty_ rightx_ righty_ _) (FaceSlope xSlope ySlope _) drop' take' = do
  let extensionHeight = 30
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])
      radii = repeat $ Radius 15 -- [Radius x | x <- [15,15..]] --
      radiiSquared =  
                     [squaredOff 3 radius' angle'
                       | radius' <- radii
                       | angle'  <- angles
                     ]
      attachmentArmCutterCubes =
        [CornerPointsNothing | x <-[1,2..4]]
        ++
        [CornerPointsId | x <-[5..21]] 
        ++
        [CornerPointsNothing | x <-[22..36]]
      

      attachmentArmCutterCubesList = concat [attachmentArmCutterCubes | x <-  [1..]]
  {-
  The section of the mount that attaches to the socket.
  Use [CornerPointsId | x <-[1..108]] ++ to see the full circumference top section socket for alignment.
  Remove it for printing as full circumference is not used.
  -}
  socketCubeForWristAndBottomAttachmentArm
             <- buildCubePointsListWithAdd "wristCubes"
                (concat $ take 100 $ drop (drop' +2)  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                ({-[CornerPointsId | x <-[1..108]] ++-}  attachmentArmCutterCubesList)
                                     
  
  
  let
    socketCubesForInnerMountWalls  = (take 100{-(take' - 3)-} $ drop (drop' + 2)  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
    topOfFirstMountFrontFace = z_axis $ f2 $ extractF2 $ head $ head socketCubesForInnerMountWalls
    --topOfFirstMountFrontFace = z_axis $ f2 $ extractF2 $ head socketCubesForInnerMountWalls
    bottomOfFirstMountFrontFace = topOfFirstMountFrontFace - zHeight_
    allMountFrontFaces = generateMounts (- zHeight_)  xSlope (ySlope) $ buildFrontFace topOfFirstMountFrontFace  bottomOfFirstMountFrontFace leftx_ lefty_ rightx_ righty_ xSlope (ySlope)
                                                            --ztop  zbtm  lx     ly     rx      ry      ySlope
  
  
  motorMountThatExtendsOutFromTheSocket
             <- buildCubePointsListSingle "wristMount"
                (concat $ 
                   [gen1 |+++| gen2
                    -- | gen1 <- socketCubesForInnerMountWalls
                    | gen1 <-  socketCubesForInnerMountWalls 
                    | gen2 <- allMountFrontFaces 
                   ]
                )
  
  return motorMountThatExtendsOutFromTheSocket
  

--load the scan file and generate stl
socketMountStlGenerator :: FaceDimensions -> FaceSlope -> CommonFactors -> IO ()
socketMountStlGenerator faceDimensions faceSlope (CommonFactors outerFlexSocketTranspose outerMountSocketTranspose dropFactor takeFactor)  = do
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
            cpoints =  ((execState $ runExceptT (socketMount (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM faceDimensions faceSlope dropFactor takeFactor) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

-- | Generate the socket stl, using database values. 
generateSocketMountStlUsingDbValues :: String -> IO ()
generateSocketMountStlUsingDbValues commonDimensionsToUse =
  generateSocketMountStlUsingDbValuesBase commonDimensionsToUse (socketMountStlGenerator)

-- | Generate the socket cubes, using database values. 
showSocketMountCubesUsingDbValues :: String -> IO ()
showSocketMountCubesUsingDbValues commonDimensionsToUse =
  generateSocketMountStlUsingDbValuesBase commonDimensionsToUse socketMountShowCubes

-- Base function for  generateSocketMountStlUsingDbValues/showSocketMountCubesUsingDbValues      
generateSocketMountStlUsingDbValuesBase :: String -> (FaceDimensions -> FaceSlope -> CommonFactors -> IO ()) -> IO ()
generateSocketMountStlUsingDbValuesBase commonDimensionsToUse processor  = runSqlite socketMountDatabaseConnStr $ do
  maybeMount <- getBy $ UniqueName "mount 1"
  maybeCommonDimensions <- runSqlite commontDBName $ do
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
                   


-- | load the scan file, generate socket with mount, and show current cubes from state.
--   This has not yet been implemented with the db. See socketMountStlGenerator on how to do this.
--   It already has the database objects added.
socketMountShowCubes :: FaceDimensions -> FaceSlope -> CommonFactors -> IO ()
socketMountShowCubes faceDimensions faceSlope (CommonFactors outerFlexSocketTranspose outerMountSocketTranspose dropFactor takeFactor)  = do
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
            cpoints =  ((evalState $ runExceptT (socketMount (degrees innerSleeveMDR) (degrees outerSleeveMDR)
                                                 rowReductionFactor    pixelsPerMM faceDimensions faceSlope dropFactor takeFactor) ) [])
        in  print $ show cpoints
      Nothing                                ->
        putStrLn "File not decoded"


removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']


-- ========================================================== local testing ==================================================
mount1 = buildFrontFace   30         20       5         (-60)     (-50)     (-60) 0 0

socketMountTestsDo = do
  runTestTT mount1BuilderTest
  runTestTT mount2BuilderTest
  runTestTT buildFrontFaceNoSlope
  runTestTT buildFrontFaceWithSlope

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

   --(last $ take 1 $ generateMounts (-10) mount1)
   (last $ take 1 $ generateMounts (-10) 0 (0) mount1)


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

  --(last $ reverse $ take 2 $ generateMounts (-10) mount1)
  (last $ reverse $ take 2 $ generateMounts (-10) 0 0 mount1)

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
