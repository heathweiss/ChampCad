module Examples.Scan.SideMountSocket() where
{- |
Build a socket from the original scan.
Add a piece off the side into which the quick release inserts.
Get rid of all the old SDR manipulation techniques which are quite confusing.
Instead us a simple Gaurd, in conjuction with Take/Drop, to manipulate the SDR.

Have a system of adjusting the scan for growth.
-}

import Builder.Monad(BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                     CpointsStack, CpointsList, buildCubePointsListWithAdd, buildCubePointsListSingle)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateSDR, transposeMDRList,
                          extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.VerticalFaces(createVerticalWalls)
import CornerPoints.MeshGeneration(autoGenerateEachCube)
import CornerPoints.Degree (Degree(..))

import Examples.Scan.SocketBase(RowReductionFactor(..), PixelsPerMillimeter(..), AgeCompensatorFactor(..), WallThickness(..),
                                standardRowReduction, pixelsPerMM,  age9LengthCompensator, adjustPixelsPerMillimeterForGrowth)
import Examples.Scan.WalkerSocketDesignWork(removeDefectiveTopRow')

import TypeClasses.Transposable(transpose)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Scan.ParseJuicy(getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                      calculateRadiusFrom)
import  Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)
import Helpers.List((++:),(++::))

--type RowReductionFactor = Int --Reduce the # of layers from each pixel layer of the scan.

{-
Given:
innerSocketTranspose: Increase socket circumference to compensate for growth since time scan was taken.
wallThickness: Thickness of socket wall.
-}
socketMountWithDegreesStlGenerator :: AgeCompensatorFactor -> WallThickness ->  IO ()
socketMountWithDegreesStlGenerator innerSocketTranspose wallThickness   = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --prepare the innerSleeveMDR by: reducing rows, remove defective top row, and increasing circumference for growth.
            innerSleeveMDR = (transpose (+ innerSocketTranspose)) . (reduceScan standardRowReduction) . removeDefectiveTopRow' $
                              (MultiDegreeRadii
                                name'
                                degrees'
                              )

            --adjust the height of the scan to adjust for growth by adjusting pixelsPerMM
            heightPerPixel = adjustPixelsPerMillimeterForGrowth pixelsPerMM (2::AgeCompensatorFactor) standardRowReduction
            
            cpoints =  ((execState $ runExceptT (sideMountQuickReleaseSocket (degrees innerSleeveMDR) 
                                                 heightPerPixel  (3::WallThickness) (13::WallThickness)
                                                 (50::Degree) (100::Degree)
                                                )
                        ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"

leftOff --add the mountStartDegree mountEndDegree and top layer cutoff, to runSocketMountWithDegreesStlGenerator so all adjustable params are in 1 spot.
runSocketMountWithDegreesStlGenerator =
  socketMountWithDegreesStlGenerator 3 3


sideMountQuickReleaseSocket :: [SingleDegreeRadii] ->  PixelsPerMillimeter ->
                               WallThickness -> WallThickness -> Degree -> Degree -> ExceptT BuilderError (State CpointsStack ) CpointsList
sideMountQuickReleaseSocket    mainSocketInnerSDR  pixelsPerMillimeter wallThickness mountThickness mountStartDegree mountEndDegree  = do
  let
    origin = (Point{x_axis=0, y_axis=0, z_axis=50})
    
    transposeFactors = [0,pixelsPerMillimeter.. ]

    --remove top layers to shorten
    removeTopLayersCount = 4
    
    
        
    {-Transpose mainSocketInnerMDR to get thickness of walls, as well as protusion for quick-release attachment.-}
    mainSocketOuterSDR = 
          createOuterSdrWithSideMount wallThickness mountThickness mountStartDegree mountEndDegree  mainSocketInnerSDR
      
  buildCubePointsListSingle "create socket walls" 
      (concat $ drop removeTopLayersCount  (createVerticalWalls  mainSocketInnerSDR mainSocketOuterSDR origin transposeFactors))
      


{-
Task:
Create the outer wall [SingleDegreeRadii] with the attachment point added to the side of socket.

Limitations:
The mount can't span <0/360> degrees.
-}

createOuterSdrWithSideMount :: WallThickness -> WallThickness -> Degree -> Degree -> [SingleDegreeRadii] -> [SingleDegreeRadii]
createOuterSdrWithSideMount socketWallThickness mountWallThickness mountStartDegree mountEndDegree innerSDR =
  
  map (createOuterSdrWithSideMount' socketWallThickness mountWallThickness mountStartDegree mountEndDegree) innerSDR
  --map (transpose (+3)) innerSDR

createOuterSdrWithSideMount' :: WallThickness -> WallThickness -> Degree -> Degree -> SingleDegreeRadii -> SingleDegreeRadii
createOuterSdrWithSideMount' wallThickness mountThickness mountStartDegree mountEndDegree innerSDR
  | d < mountStartDegree = (transpose (+ wallThickness)) innerSDR
  | d <= mountEndDegree =  (transpose (+ (wallThickness + mountThickness))) innerSDR
  | otherwise = (transpose (+ wallThickness)) innerSDR
  where d = (degree innerSDR)


