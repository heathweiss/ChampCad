{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocketSquared(sideMountQuickReleaseSocket) where

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR, transposeMDRList,
                          transposeSDRList, extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..), Origin(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import TypeClasses.Transposable(transpose)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))


import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)

import Helpers.List((++:))

import Primitives.Cylindrical(cylinderWallsNoSlopeSquaredOff, cylinderSolidNoSlopeSquaredOff,
                                   cylinderWallsNoSlopeSquaredOffLengthenY, cylinderSolidNoSlopeSquaredOffLengthenY)
import Primitives.Cylindrical(cylinderWallsNoSlope)

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

import Test.HUnit

import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Flow as Flw
import Control.Lens

--import Control.Arrow hiding ((+++))
--import Control.Category hiding ((.))
{------------------------------------------------------------- overview ---------------------------------------------------
The original scan work is in WalkerSocket module.
++++++
***********************************    important *******************************************************
********************************************************************************************************
Remember to use WalkerSocket.processMultiDegreeRadii if I have to get the json file from storage(Dropbox/3D/MDRFiles/walkerSocket.json
The one in storage is in pixels, rather than millimeters.
-}

{-
read in the Multidegree json file, which has valid Radii,
and process it into stl using whatever function required for the current shape.
I have this master function, as the individual shapes all have common requirements that can be passed in. Keep it DRY.
For example: The top row is bad, and has to be removed. Use a rowReductionFactor of 100 for all shapes.-}
loadMDRAndPassToProcessor :: IO ()
loadMDRAndPassToProcessor = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  let removeDefectiveTopRow :: MultiDegreeRadii -> MultiDegreeRadii
      removeDefectiveTopRow (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']
      rowReductionFactor = 100
      extensionHeight = 30
      plateRadius = 30
      power = 2.5
      lengthenYFactor = 20
      extensionFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      extensionFaceBuilder leftFace emptyFaces rightFace fillerFaces =
        [leftFace] ++ [emptyFaces | x <- [2..30]] ++ [rightFace]   ++  [fillerFaces | x <- [31..]]
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let ----------------------------for the socket that has the bottom extension to attach to the walker
            --enlarge it to fit over the socket already printed with WalkerSocket. 1st attempt at +2 was not quite big enough, trying 3.
            --rotate it to line up better with the riser
            innerSleeveMDR = rotateMDR $ rotateMDR $ rotateMDR $ transpose (+3) $ reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
            --give it a thickness of 3 mm
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            plateRadius = 24

            ------------------------------------- for the socket with the quick-release mounted on the side
            --need to adjust transposefactor, as +3 was a loose fit for his walker. Try +2
            innerSleeveMDRForSideMount = transpose (+2) $ reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
        in  ------------------------------------choose the shape to process---------------------------------------------.
            ---------socket attached to walker       
            socketWithRiser innerSleeveMDR outerSleeveMDR extensionFaceBuilder extensionHeight rowReductionFactor pixelsPerMM
            --pushPlate plateRadius power lengthenYFactor
            --hosePlate plateRadius power lengthenYFactor

            ---------socket with sidemount quick releas
            --sideMountQuickReleaseSocket innerSleeveMDRForSideMount rowReductionFactor pixelsPerMM
            
      Nothing                                ->
        putStrLn "File not decoded"


{-=========================================================== side mounted quick-release socket ============================================
Generate the socket with a thicker section of wall, into which a hole can be drilled for a quick coupler.
-}




      
sideMountQuickReleaseSocket :: MultiDegreeRadii ->  RowReductionFactor -> PixelsPerMillimeter ->  IO ()
sideMountQuickReleaseSocket      mainSocketInnerMDR             rowReductionFactor    pixelsPerMillimeter  =
  let
    mainWallThickness = 3
    quickReleaseWallThickness = 10
    
    {-Transpose mainSocketInnerMDR to get thickness of walls, as well as protusion for quick-release attachment.-}
    outerMDR =
      let origSDR = degrees mainSocketInnerMDR 
          sdrMap = singleDegreeRadiiListToMap origSDR
          
            
          transposedSDR =
            --0-220 degrees
            transformRangeOfSDR [(+3) | y <- [1..]] [0,10..220] origSDR
            --230 degrees
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDR ([(+7) | y <- [1..12]] ++ [(+3) | y <- [1..7]]) (sdrMap^.at 230.0)))
            --240-270 degrees
            Flw.|> (\sdrSeq -> sdrSeq S.>< (transformRangeOfSDR ([(+15) | y <- [1..12]] ++ [(+3) | y <- [1..7]]) [240,250..270] origSDR))
            --280 degrees
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDR ([(+9) | y <- [1..12]] ++ [(+3) | y <- [1..7]]) (sdrMap^.at 280.0)))
            --290-360
            Flw.|> (\sdrSeq -> sdrSeq S.>< (transformRangeOfSDR [(+3) | y <- [1..]] [290,300..360] origSDR))
            --convert sdrSeq back into a list
            Flw.|> (\sdrSeq -> F.toList sdrSeq)
      in
          mainSocketInnerMDR {degrees = transposedSDR}
                 
    origin = (Point{x_axis=0, y_axis=0, z_axis=50})
    transposeFactors = [0,heightPerPixel.. ]
    heightPerPixel = 1/pixelsPerMM * (fromIntegral rowReductionFactor)
           
    mainSocketWalls =
      [ newCornerPointsWith10DegreesBuilder currWalls | currWalls <-
           drop 6  (createVerticalWalls  mainSocketInnerMDR outerMDR origin transposeFactors)]

    --ToDo: Reduce the need for so much nesting of lists.
    mainSocketFaceTriangles = 
       let processSingleRow currRow faceConstructor =
              currRow ||@~+++^|| [[FacesWithRange faceConstructor (DegreeRange 0 360)]]
       in  --top row 1
           S.fromList [processSingleRow (head mainSocketWalls) FacesBackFrontTop ]
           --rows 2-11
           Flw.|> (\facesSeq ->
                    (
                     facesSeq S.>< ( S.fromList [ processSingleRow currRow FacesBackFront | currRow <- take 10 (tail mainSocketWalls)])
                    ,last  mainSocketWalls
                     )
                  )
           --row 12
           Flw.|> (\(facesSeq, cubes) ->
                     facesSeq S.|> ( processSingleRow cubes FacesBackBottomFront )
                  )
           --turn it back to a list
           Flw.|> (\faces -> concat $ F.toList faces)
     
    
  in
     writeStlToFile $ newStlShape "socket with quick release"  mainSocketFaceTriangles

{-
test =   [[3 | y <- [1,2]] | x <- [1,2]]
         ++ [[4,4]]
-}
{-========================================================== socket attached to walker=====================================================
===========================================================================================================================================-}
{-
The attaches to the pushPlate and the hose
                     ||  ||   hose
                     ||  ||   
                    |||  |||  riser
              |||||||||||||||||||  innerHose riserBase outsideScrews
-}
hosePlate ::  PlateRadius -> Power -> LengthenYFactor -> IO ()
hosePlate plateRadius power lengthenYFactor =
  let
    
    wallThickness = 3
    hoseInnerRadius = 7.5
    hoseThickness = wallThickness
    hoseOuterRadius = hoseThickness + hoseInnerRadius
    riserInnerRadius = hoseOuterRadius
    riserThickness = 5 --10
    riserOuterRadius = riserThickness + riserInnerRadius
    screwInsideRadius = riserOuterRadius
    screwsThickness = plateRadius - screwInsideRadius
    
    

    baseHeight = 3
    riserHeight = 17
    hoseHeight = 20
      
    baseOrigin = Point 0 0 0
    riserOrigin = transposeZ ( + baseHeight) baseOrigin

    --Build up the cubes, and output the stl triangles
    buildCubesOutputTriangles =
      let riserBaseCubes = cylinderWallsNoSlopeSquaredOffLengthenY
             (Radius riserInnerRadius) baseOrigin angles baseHeight riserThickness power lengthenYFactor
          
          addFacesTo faceConstructor =  ([faceConstructor | x <- [1..]] |+++^|)
          addTrianglesToSeq sequence faceConstructor cubes = sequence S.><
                                                (S.fromList
                                                  ([faceConstructor | x <- [1..]] |+++^| cubes)
                                                )
      in
          --riser base cube triangles
          (S.fromList ( addFacesTo FacesBottomTop riserBaseCubes))
          --outer screw ring
          Flw.|> (\trianglesSeq ->
                    addTrianglesToSeq
                     trianglesSeq
                     FacesBottomFrontTop
                      (cylinderWallsNoSlopeSquaredOffLengthenY
                        (Radius screwInsideRadius) baseOrigin angles baseHeight screwsThickness power lengthenYFactor)
                 )
          --innerHose
          Flw.|> (\triangleSeq ->
                   let innerHoseCubes = (cylinderSolidNoSlopeSquaredOffLengthenY
                                 (Radius hoseOuterRadius)   baseOrigin    angles     baseHeight  power    lengthenYFactor)
                       triangleSeq' = addTrianglesToSeq
                                       triangleSeq
                                       FacesBottomTop
                                       innerHoseCubes
                   in (triangleSeq', innerHoseCubes)
                 )
          
          --riser
          Flw.|>(\(triangleSeq, innerHoseCubes) ->
                  let riserTopFaces =
                        [extractTopFace x |
                          x <- (cylinderWallsNoSlope (Radius hoseInnerRadius) hoseThickness   riserOrigin angles riserHeight)
                        ]
                      riserCubes = riserTopFaces |+++| innerHoseCubes
                      triangleSeq' = addTrianglesToSeq triangleSeq FacesBackFront riserCubes
                      
                  in  (triangleSeq', riserCubes)
                )
          --hose
          Flw.|>(\(triangleSeq, riserCubes) ->
                  let hoseTopFaces = map ((transposeZ (+20)) . extractTopFace) riserCubes
                      hoseCubes    = hoseTopFaces |+++| riserCubes
                      
                  in  addTrianglesToSeq triangleSeq FacesBackFrontTop hoseCubes
                )
          --triangle sequence to a list
          Flw.|>(\triangleSeq -> F.toList triangleSeq)



    angles = (map (Angle) [0,10..360])
    
  
  in
    writeStlToFile $ newStlShape "walker socket hose plate" buildCubesOutputTriangles      
  
     

{-
Attaches directly to the socket

Printing notes:
I printed with no infill, and 2 top layers. This resulted in the top 2 layers of the plate,
printing over a gap of about 1 mm. This was not a problem. Should have had solid infill or more bottom layers.

Used abs glue directly on upper glass plate with a primer wash just before printing. Worked great.
-}
pushPlate :: PlateRadius -> Power -> LengthenYFactor -> IO ()
pushPlate    plateRadius    power    lengthenYFactor  = 
  let
    origin = Point 0 0 0 :: Origin
    angles = (map (Angle) [0,10..360])
    plateHeight = 3
    --power = 2.5
    riserThickness = 3
    --lengthenFactor = 20

    --create the cubes and the stl together without the use of builder
    createCubesStlCompositionally = 
      let centerPlate = (cylinderSolidNoSlopeSquaredOffLengthenY  (Radius 21)   origin    angles     plateHeight  power    lengthenYFactor)
          addFacesTo faceConstructor =  ([faceConstructor | x <- [1..]] |+++^|)
          addTrianglesToSeq sequence faceConstructor cubes = sequence S.><
                                                (S.fromList
                                                  ([faceConstructor | x <- [1..]] |+++^| cubes)
                                                )
      in  --centerPlate
          S.fromList(addFacesTo FacesBackBottomTop centerPlate)
          --the outer ring, which has an inner radius = outer radius of riser on socket
          Flw.|>(\triangleSeq ->
                  let cubes = (cylinderWallsNoSlopeSquaredOffLengthenY
                                (Radius (plateRadius - riserThickness))  origin angles plateHeight riserThickness power lengthenYFactor)
                      faces = (riserFaceBuilder FacesBottomFront  FacesBottomFront FacesBottomFrontTop FacesBottomFront FacesBottomFront)
                      triangles = S.fromList (faces |+++^| cubes)
                  in triangleSeq S.>< triangles
                )
          --the riser that takes off from the outer ring
          Flw.|>(\triangleSeq ->
                  let cubes = (cylinderWallsNoSlopeSquaredOffLengthenY
                                (Radius (plateRadius - riserThickness))
                                (transposeZ (+plateHeight)origin)  angles (30 :: Height) riserThickness  power  lengthenYFactor)
                      faces = (riserFaceBuilder FacesBackFrontTop FacesBackFrontLeftTop FacesNada FacesBackFrontRightTop FacesBackFrontTop)
                      triangleSeq' = S.fromList (faces |+++^| cubes)
                  in  triangleSeq S.>< triangleSeq'
                )
          --change back to [Triangles]
          Flw.|>(\triangleSeq -> F.toList triangleSeq)
    
  in
    writeStlToFile $  newStlShape "walker socket push plate" createCubesStlCompositionally


{-
reduce the rows of the MulitiDegreeRadii by a factor of 100
chop of the top layer as it has an error,
output to stl

--do not call directly.
--called via reducedRowsMultiDegreeScanWrapper as it reads the json file-}
socketWithRiser :: MultiDegreeRadii -> MultiDegreeRadii -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) ->
                 ExtensionHeight -> RowReductionFactor -> PixelsPerMillimeter ->  IO ()
socketWithRiser    innerSleeveMDR      outerSleeveMDR      extensionFaceBuilder extensionHeight    rowReductionFactor pixelsPerMM  =
  let transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = 1/pixelsPerMM * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])

      riserCubes = cylinderWallsNoSlopeSquaredOff  (Radius 18)  (transposeX (+0)(transposeY (+(-15))(transposeZ (+(-15))origin)))    angles (20::Height) (3::Thickness) (2.5::Power)
      {-
      riserTriangles =
        (riserFaceBuilder FacesBackFrontTop FacesBackFrontLeftTop FacesNada FacesBackFrontRightTop FacesBackFrontTop)
        |+++^|
        riser
      -}
      --rewrite using Flw.|> to have everything built in 1 pass
      cubesAndTrianglesWithFunctionComposition =
        let mainCubes = --main body of the socket, with 1st 6 rows removed removed.
                drop 6  (createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors) 
        in
           --top row of scan
           S.fromList((riserFaceBuilder FacesBackFront FacesBackFront (FacesBackFrontTop) FacesBackFront FacesBackFront)  |+++^| (head mainCubes))
           --
           --create faces for row 2-10, which are all the same
           
           Flw.|>(\triangleSeq ->
                   let rows2To10 = take 10 $ tail mainCubes
                       faceConstructors = [[FacesBackFront | x <- [1..]] | x <- [1..10]]
                       faces = S.fromList (faceConstructors ||+++^|| rows2To10 )
                   in  triangleSeq S.>< faces
                 )
           --the bottom row
           Flw.|>(\triangleSeq ->
                   let bottomCubes = last mainCubes
                       faceConstructors = [FacesBackBottomFront | x <- [1..]]
                       faces = S.fromList (faceConstructors |+++^| bottomCubes)
                   in  triangleSeq S.>< faces
                 )
           --add the socket to riser adaptor
           Flw.|>(\triangleSeq ->
                   let --mainCubesTopFaces = head mainCubes
                       riserBtmFacesAsTopFaces = [upperFaceFromLowerFace $ extractBottomFace x | x <- riserCubes ]
                       faceConstructors = riserFaceBuilder FacesBackFront FacesBackFrontLeft FacesNada FacesBackFrontRight FacesBackFront
                       adaptorCubes = riserBtmFacesAsTopFaces |+++| (head mainCubes)
                       adaptorTriangles = S.fromList (faceConstructors |+++^| adaptorCubes)
                   in triangleSeq S.>< adaptorTriangles
                       
                 )
           --add the riser
           Flw.|>(\triangleSeq ->
                   let riserTriangles = (riserFaceBuilder FacesBackFrontTop FacesBackFrontLeftTop FacesNada FacesBackFrontRightTop FacesBackFrontTop) |+++^| riserCubes
                   in  triangleSeq S.>< (S.fromList riserTriangles)
                 )
           --convert triangles seq back to list
           Flw.|>(\triangleSeq -> F.toList triangleSeq)
            
      {-     
      mainBodyCubes =  getCornerPoints $
        (CornerPointsBuilder $
          drop 6  (createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors) --main body of the socket, with head removed. 
          
        )
        &+++#@ (|+++| [upperFaceFromLowerFace $ extractBottomFace x | x <- riser ] ) --socketToRiser
      
      socketTriangles =
        (
          [riserFaceBuilder FacesBackFront FacesBackFrontLeft FacesNada FacesBackFrontRight FacesBackFront] ++  --the socket to riser transition
          [riserFaceBuilder FacesBackFront FacesBackFront (FacesBackFrontTop) FacesBackFront FacesBackFront]  ++ --top layer of the socket
          [[FacesBackFront | x <- [1..]] | x <- [1..9]] ++  --get rid of a most layers, so that just the top is printed out, for fixing 1st print of socket.
                                                           --leave empty to get rid of all mid-layers. Use 1..9 for all layers printed
          [ [FacesBackBottomFront | x <- [1..]]   ]         --bottom of the socket      
        )
        ||+++^|| 
        ( getCornerPoints $
          (CornerPointsBuilder $
            drop 6  (createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors) --main body of the socket, with head removed. 
          )
          &+++#@ (|+++| [upperFaceFromLowerFace $ extractBottomFace x | x <- riser ] ) --socketToRiser
        )

                  
      
      sleeveStlFile = newStlShape "walker sleeve" $ socketTriangles ++ riserTriangles
      -}
  in
      --writeStlToFile sleeveStlFile
      writeStlToFile $ newStlShape "walker sleeve" cubesAndTrianglesWithFunctionComposition


--build the faces for the squared riser and assoc'd layers
riserFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
riserFaceBuilder faces1 faces2 faces3 faces4 faces5 =
 [faces1| x <- [1..8]] ++ [faces2] ++ [faces3| x <- [10..28]] ++ [faces4]   ++  [faces5 | x <- [11..]]

pixelsPerMM = 696/38


type RowReductionFactor = Int
--type Origin = Point
type ExtensionHeight = Double
type PlateRadius = Double
type PixelsPerMillimeter = Double
type TransposeFactor = Double
type Thickness = Double
type Height = Double
type Power = Double
type LengthenYFactor = Double

