{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocketProcessScan() where
import Scan.ParseJuicy(getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                       calculateRadiusFrom)
import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, resetMultiDegreeRadiiIfNull)
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, 
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope)
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)
import Helpers.List((++:))

import TypeClasses.Transposable(transpose)
import Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..))


{----------------------------------------------------- overview------------------------------------------------------------
This module does the scanning work for the WalkerSocket. It processes the raw data into json files.

The 1st json file processed the scanner images into pixel values.
--Stored in: Home/Dropbox/3D/MDRfiles/walkerSocketRaw.json

The 2nd json file is the result of converting pixels into millimeters.
-Stored in: Home/Dropbox/3D/MDRfiles/walkerSocketProcessed.json
          : src/Data/scanFullData.json which is also pushed to github.

The design work is done in WalkerSocketSquared. It used the processed json version.

-}

writeStlFileFromRawScanWrapper :: IO ()
writeStlFileFromRawScanWrapper = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        writeStlFileFromRawScan (MultiDegreeRadii name' degrees')
      Nothing                                ->
        putStrLn "File not decoded"

{-
Read in the processed json file and
replace all the invalid RadiusNaN with a default value.
First I need to read the file, so that the new RadiusNaN is used.
-}
resetJsonFileMDRNaN :: IO ()
resetJsonFileMDRNaN = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let multiDegreeRadii = resetMultiDegreeRadiiIfNull 2000 $ MultiDegreeRadii name' degrees'
        in  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
        
      Nothing                                ->
        putStrLn "File not decoded"

{-
This was orignally is set up to have rows reduced by 10, and from a smaller jpeg image so
will need to change the number of of center front triangles

This is a support function, not to be called directly.
-}
writeStlFileFromRawScan :: MultiDegreeRadii -> IO ()
writeStlFileFromRawScan (MultiDegreeRadii name' degrees') = 
  let smoothedScan = MultiDegreeRadii name' (map (runningAvgSingleDegreeRadii 10) degrees')
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      heightPerPixel = 1/pixelsPerMM
      leftFaces = createLeftFacesMultiColumns origin (tail $ degrees smoothedScan) flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFaces origin (head $ degrees smoothedScan) flatXSlope flatYSlope [0,heightPerPixel..]
      frontTriangles = [FaceFront | x <- [1..]]
      triangles =
       zipWith  (|+++^|)
        ([FacesFrontTop | x <- [1..]] :   [frontTriangles | x <- [1..1940]] ++:  [FacesBottomFront | x <- [1..]] 

        ) 
        --  |+++^|
        (createHorizontallyAlignedCubes rightFaces leftFaces)
      stlFile = newStlShape "backscratcher" $ concat triangles
  in
      writeStlToFile stlFile
      --print origin
{-
process the raw MultiDegreeRadii into radius.
-}
processMultiDegreeRadii :: IO ()
processMultiDegreeRadii = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let multiDegreeRadii =
              MultiDegreeRadii
                name'
                [ processSingleDegreeRadii currSingleDegreeRadii
                  | currSingleDegreeRadii <- degrees'
                ]
               
        in
           BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
           
      Nothing                              -> putStrLn "Nothing"



{-process a SingleDegreeRadii, to go from raw data to a Radius of the scan.-}
processSingleDegreeRadii :: SingleDegreeRadii -> SingleDegreeRadii
processSingleDegreeRadii    (SingleDegreeRadii degree' radii')    =
  let adjustForCenter = getThePixelsRightOfCenter $ andThen  (removeLeftOfCenterPixels btmCenterIndex topCenterIndex imageHeight)
  in
      SingleDegreeRadii degree'
         [
          let currLaserLineIndex = round $ radius currDegree
              rightOfCenterPixelDistanceOfLaser = adjustForCenter  (forThe currLaserLineIndex ) (ofThe currRow)
              radius'' = calculateRadiusFrom  rightOfCenterPixelDistanceOfLaser (adjustedFor pixelsPerMM) $ andThe cameraAngle
              
          in radius''
         | currDegree <- radii'
         | currRow <- [0..]
         ]

--look at the results of processSingleDegreeRadii
--can be deleted later.
showSingleDegreeRadii = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of
      Just (MultiDegreeRadii name degrees) -> print $ show $ processSingleDegreeRadii $ head degrees
      Nothing                              -> putStrLn "Nothing"


{- |Process all 36 images, reducing down to MultiDegreeRadii of the red laser line.
This is the red laser line in pixels Right of Left hand edge.
It is written out to json as a MultiDegreeRadii, even though the data is
in the form of pixel indices RightOfLHS, in other words, not processed at all beyond finding the laser line.
-}
--has now been replaced with processImagesIntoFull360DegreeMultiDegreeRadiiAt10DegreeIntervals
--processImagesToRedLaserLineAsPixelsRightOfLHS = process10DegreeImagesToMultiDegreeRadii (getRedLaserLineSingleImage redLaserLine)



--as calculated by the calibration picture center.JPG
pixelsPerMM = 696/38 -- 620 pixels / 38 mm
--pixelsPerMMVertical = 704.2/38   -- 520 pixels / 38 mm

--ToDo: Find these 2 values from a centering image using
topCenterIndex = 1591
btmCenterIndex = 1563.5
imageWidth = 2592
imageHeight = 1944
--pixelsPerMillimeter = 38/623 -- 623/38
cameraAngle = 30


--180 fails at degree 340
type TargetValue = Word8
redLaserLine :: TargetValue
redLaserLine = 175

type RowReductionFactor = Int
type Origin = Point
type ExtensionHeight = Double
type PlateRadius = Double
type PixelsPerMillimeter = Double
type TransposeFactor = Double
type Thickness = Double
type Height = Double
