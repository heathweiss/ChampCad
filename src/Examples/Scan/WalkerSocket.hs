{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocket() where
import Scan.ParseJuicy(getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                       calculateRadiusFrom)
import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, resetMultiDegreeRadiiIfNull)
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import Builder.Builder(CornerPointsBuilder(..),(&+++#@))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..))
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)
import Helpers.List((++:))
import Primitives.Cylindrical(cylinderSolidNoSlope)
import Primitives.Cylindrical(cylinderWallsNoSlope)
import CornerPoints.Transposable(transpose)
import Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)

{----------------------------------------------------- overview------------------------------------------------------------
This is the original walker socket module. It includes the scanning work, which is the only thing that is important here.

The rest of the work, for designing the socket is outdated. Refer to WalkerSocketSquared for more up to date design.
-}

{-Global values such as commom measurements.-}
plateRadius = 30

{-
read in the Multidegree json file, which has valid Radii,
and process it into stl using whatever function required for the current shape. -}
loadMDRAndPassToProcessor :: IO ()
loadMDRAndPassToProcessor = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  let removeDefectiveTopRow :: MultiDegreeRadii -> MultiDegreeRadii
      removeDefectiveTopRow (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']
      rowReductionFactor = 100
      extensionHeight = 30
      
            
      extensionFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      extensionFaceBuilder leftFace emptyFaces rightFace fillerFaces =
        [leftFace] ++ [emptyFaces | x <- [2..30]] ++ [rightFace]   ++  [fillerFaces | x <- [31..]]
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let innerSleeveMDR = reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
            outerSleeveMDR = transpose (+2) innerSleeveMDR
        in  mainSocketStl innerSleeveMDR outerSleeveMDR extensionFaceBuilder extensionHeight rowReductionFactor pixelsPerMM
            --pushPlateStl outerSleeveMDR extensionFaceBuilder extensionHeight plateRadius
            
      Nothing                                ->
        putStrLn "File not decoded"


{-
                     ||  ||   hose
                     ||  ||   
                    |||  |||  riser
              |||||||||||||||||||  innerHose riserBase outsideScrews
This will be re-written in hoseAttachmentWithDegrees
-}
hoseAttachment :: IO ()
hoseAttachment     =
  let wallThickness = 3
      hoseInnerRadius = Radius 7.5
      hoseThickness = wallThickness
      hoseOuterRadius = transpose (+ hoseThickness) hoseInnerRadius
      riserInnerRadius = hoseOuterRadius
      riserThickness = 10
      riserOuterRadius = riserThickness + (radius riserInnerRadius)
      screwInsideRadius = Radius riserOuterRadius
      screwsThickness = plateRadius - (radius screwInsideRadius)

      baseHeight = 3
      riserHeight = 17
      hoseHeight = 20
      
      baseOrigin = Point 0 0 0
      riserOrigin = transposeZ ( + baseHeight) baseOrigin
      
      angles = (map (Angle) [0,10..360])
      
      
      
      cubes =  getCornerPoints $
        (CornerPointsBuilder
         [
           cylinderWallsNoSlope riserInnerRadius riserThickness  baseOrigin angles baseHeight,     -- riserBase
           cylinderWallsNoSlope screwInsideRadius screwsThickness   baseOrigin angles baseHeight, --outsideScrews
           cylinderSolidNoSlope hoseOuterRadius  baseOrigin angles baseHeight                  --innerHose
           
         ]
          &+++#@ (|+++|  [extractTopFace x | x <- (cylinderWallsNoSlope hoseInnerRadius hoseThickness   riserOrigin angles riserHeight)]) -- riser
          &+++#@ (|@+++#@| ((transposeZ (+20)) . extractTopFace) ) --hose
         
        )
      
      triangles = [
                    [FacesBackFrontTop | x <- [1..]],    --hose
                    [FacesNada | x <- [1..]],       --riser
                    [FacesNada | x <- [1..]],       --riserBase
                    [FacesNada | x <- [1..]], --ousideScrews
                    [FacesNada | x <- [1..]]      --innerHose
                    
                  ]
                  ||+++^|| 
                  cubes
      
      cubesStl = newStlShape "hose attachment" triangles
  in  
      --putStrLn "temp"
      writeStlToFile cubesStl
        
{- =============================================== hose attachement with degrees ======================================
Re-write hose attachment for:
-Use the new CornerPointsWithDegrees system, including the Builder that goes with it.
-Make 4 support support spokes around the riser, instead of a single flared support, as it should print better.
-}
hoseAttachmentWithDegrees :: IO ()
hoseAttachmentWithDegrees = putStrLn "on hold till a CornerPoints.Data.Sequence is built."
  
     

{-



                                                ||  
                                                ||   joiner :joins the socket to this shape. Shaped like top row of socket, but with bigger radius.
                                                ||
                                                 ||  riserToJoiner : converts from the round outer(plate) to the shape of the joiner
                                                  || riser : round like outer(plate). 
                                                  || 
                                                  |||||||||||| || inner and outer
-}



pushPlateStl :: MultiDegreeRadii ->  ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) -> ExtensionHeight -> PlateRadius -> IO ()
pushPlateStl    outerSleeveMDR       extensionFaceBuilder                                     extensionHeight    plateRadius  =
  let --plateRadius = 30
      conOuterMDR = transpose (+2) outerSleeveMDR
      conTopOrigin = (Point (-10) (-15) 20)
      plateHeight = 3
      plateOrigin = (Point 0 0 0)

      --gives the shape of the top row of the socket, which is the shape of the joiner 
      topRowOfSocketCubes = head $ createVerticalWalls (extractList (take 2) outerSleeveMDR ) (extractList (take 2)  conOuterMDR) conTopOrigin [0,1..]

      cubes =
        getCornerPoints $
        (CornerPointsBuilder
         [cylinderWallsNoSlope (Radius plateRadius) (2 :: Thickness) plateOrigin (map (Angle) [0,10..360]) plateHeight, --outer cubes
          cylinderSolidNoSlope (Radius plateRadius) plateOrigin (map (Angle) [0,10..360]) plateHeight ] --inner cubes
        )
        &+++#@ (|@+++#@| ((transposeZ (+5)) . extractTopFace)) --riser cubes
        &+++#@  ( |+++| [extractTopFace currCube | currCube <- topRowOfSocketCubes]) -- riser to joiner cubes
        &+++#@ (|@+++#@| ((transposeZ (+extensionHeight)) . extractTopFace )) --joiner cubes
      
      cubesTriangles = 
                        [ (extensionFaceBuilder (FacesBackFrontLeftTop) (FacesNada) (FacesBackFrontRightTop) (FacesBackFrontTop)), --joiner
                          (extensionFaceBuilder (FacesBackFrontLeft) (FacesNada) (FacesBackFrontRight) (FacesBackFront)), --riser to joiner
                          (extensionFaceBuilder (FacesBackFrontLeft) (FacesNada) (FacesBackFrontRight) (FacesBackFront)), -- riser
                          (extensionFaceBuilder (FacesBottomFront) (FacesBottomFrontTop) (FacesBottomFront) (FacesBottomFront)), --outer
                          (extensionFaceBuilder (FacesBottomTop) (FacesBottomTop) (FacesBottomTop) (FacesBottomTop)) --  innerCubes
                        ]
                        ||+++^||
                        cubes
     
      
      cubesStl = newStlShape "with new &+++#@" cubesTriangles
      
  in  --putStrLn "temp"
      writeStlToFile cubesStl
      
{-
reduce the rows of the MulitiDegreeRadii by a factor of 100
chop of the top layer as it has an error,
output to stl

--do not call directly.
--called via reducedRowsMultiDegreeScanWrapper as it reads the json file-}
mainSocketStl :: MultiDegreeRadii -> MultiDegreeRadii -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) ->
                 ExtensionHeight -> RowReductionFactor -> PixelsPerMillimeter ->  IO ()
mainSocketStl    innerSleeveMDR      outerSleeveMDR      extensionFaceBuilder extensionHeight    rowReductionFactor pixelsPerMM  =
  let transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = 1/pixelsPerMM * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})

      topOfExtension = extensionFaceBuilder (FacesBackFrontLeftTop) (FacesNada) (FacesBackFrontRightTop) (FacesBackFrontTop)
      bodyOfExtension = [
                        extensionFaceBuilder (FacesBackFrontLeft)(FacesNada)(FacesBackFrontRight)(FacesBackFront)
                        | x <- [1..6]
                      ]
      topOfMainBody = extensionFaceBuilder (FacesBackFront) (FacesBackFrontTop) (FacesBackFront) (FacesBackFront)
      mainBody = [
                           [FacesBackFront | x <- [1..]]
                           | x <- [1..9]
                 ] 
      btmOfMainBody = [FacesBackBottomFront | x <- [1..3]]   ++
                      [FacesBackBottom | x <- [1]] ++
                      [FacesBackBottomFront | x <- [1..19]] ++
                      [FacesBackBottom | x <- [1..2 ]] ++
                      [FacesBackBottomFront | x <- [1.. ]]
      
      tieDownCubes = tail $ createVerticalWalls  outerSleeveMDR (transpose (+20) outerSleeveMDR) origin transposeFactors
      noShowTieDownFaces = [
                           [FacesNada | x <- [1..]]
                           | x <- [1..16]
                 ]
      showTieDownFaces = [ [FacesNada | x <- [1..3]] ++
                           [FacesBottomFrontLeftRightTop] ++
                           [FacesNada | x <- [1..19]] ++
                           [FacesBottomFrontRightTop] ++
                           [FacesBottomFrontLeftTop] ++
                           [FacesNada | x <- [1..]]
                           | x <- [1..]
                 ]
      tieDownTriangles = (noShowTieDownFaces ++ showTieDownFaces) ||+++^|| tieDownCubes
                  
      mainBodyCubes = createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors
      extensionCubes = (map (transposeZ (+extensionHeight). extractTopFace)  (head  mainBodyCubes)) |+++| (map ( extractBottomFace)  (head  mainBodyCubes))
      

      triangles =
        (
          ((topOfExtension : bodyOfExtension) ++  (topOfMainBody : mainBody ))   ++:  btmOfMainBody
        )
        ||+++^||
        ( extensionCubes : tail mainBodyCubes)

      sleeveStlFile = newStlShape "walker sleeve" $ triangles ++ tieDownTriangles
  in
      writeStlToFile sleeveStlFile


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
