{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor) where
{------------------------------ ToDo------------------------------------
Use the autogenerate stl in all functions.
-}

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR, transposeMDRList,
                          transposeSDRList, extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..), Origin(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import TypeClasses.Transposable(transpose)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)

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


--import Control.Arrow hiding ((+++))
--import Control.Category hiding ((.))
{------------------------------------------------------------- overview ---------------------------------------------------
The original scan work is in Examples/Scan/WalkerSocketProcessScan module, the result of which is a json file as required by this module.
There is a json processed version stored in: Dropbox/3D/MDRFiles/walkerSocketProcessed.json which is in millimeters. This is the version required for this module.
This is the same file as src/Data/scanFullData.json" which is pushed to github.

The entry point into this module is loadMDRAndPassToProcessor. It is a wrapper around whichever stl producing funtion is to be run.

The scan was done every 10 degrees starting at 0: [0,10..360]
-}

{-
Read in the Multidegree json file, which has valid Radii in millimeters,
and process it into stl using whatever function required for the current shape.
I have this master function, as the individual shapes all have common requirements that can be passed in. Keep it DRY.
For example: The top row is bad, and has to be removed. Use a rowReductionFactor of 100 for all shapes.

I just comment out the stl producing functions that are not required. Should clean that up by wrapping loadMDRAndPassToProcessor in a function which
would take the required function (with parameters), as a parameter. Or just a function for each required stl shape.
-}
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
            --socketWithRiser (degrees innerSleeveMDR) (degrees outerSleeveMDR) extensionFaceBuilder extensionHeight rowReductionFactor pixelsPerMM
            --pushPlate plateRadius power lengthenYFactor
            --hosePlate plateRadius power lengthenYFactor

            ---------socket with sidemount quick release------------
            generateSideMountQuickReleaseSocketStl (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []

            ----------------- swim fin---------------------
            --swimFinSocketOnlyInsideFin (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM
            --generateSwimFinStl (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []
            --showSwimFinCumulativeCornerPoints (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []
            
            
      Nothing                                ->
        putStrLn "File not decoded"

{-----------------------------------------------------------swim fin--------------------------------------------------------------
A socket with fins on both sides to act as attachment points for poly-carbonate fins.
Uses the mtl builder system.
-}
--curry in the stack pushing function
buildCubePointsList' = buildCubePointsList (++)

swimSocketWithFinBothSides :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
swimSocketWithFinBothSides originalSDR           rowReductionFactor    pixelsPerMillimeter  = do
  let
    mainWallThickness = 3
    --start/end degrees of the base of fin1/2
    --1st print at 60, totally out of line with his arm action. Should -50
    fin1BaseStartDegree = 10
    fin1BaseThicknessInDegrees   = 4
    --1st print at 250, totally out of line with his arm action. Should -50
    fin2BaseStartDegree = 200
    fin2BaseThicknessInDegrees   = 5
    --start/end degrees of the tip of fin1/2
    fin1TipStartDegree = 60
    fin1TipThicknessInDegrees   = 2
    fin2TipStartDegree = 200
    fin2TipThicknessInDegrees   = 3
    
    fin1ExtrusionTranposeValues = [(\r -> 65) | y <- [1..]]
    fin2ExtrusionTranposeValues = [(\r -> 45) | y <- [1..]]
    
    origSDR = originalSDR -- degrees mainSocketInnerMDR 
    sdrMap = singleDegreeRadiiListToMap origSDR

    --how many of the 10mm layers to drop of top off scan.
    dropTopScanLayers = drop 4
    origin = (Point{x_axis=0, y_axis=0, z_axis=50})
    transposeFactors = [0,heightPerPixel.. ]
    heightPerPixel = 1/pixelsPerMM * (fromIntegral rowReductionFactor)

    innerSDRWithExtraFinDegrees =
            transformRangeOfSDR [(+0) | y <- [1..]] [0,10] origSDR
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin1BaseThicknessInDegrees) (sdrMap^.at fin1BaseStartDegree)) )
            Flw.|> (\sdrSeq -> sdrSeq S.>< transformRangeOfSDR [(+0) | y <- [1..]] [20,30..200] origSDR)
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin2BaseThicknessInDegrees) (sdrMap^.at fin2BaseStartDegree)) )
            Flw.|> (\sdrSeq -> sdrSeq S.>< transformRangeOfSDR [(+0) | y <- [1..]] [210,220..360] origSDR)
            Flw.|> (\sdrSeq -> F.toList sdrSeq)
    
    innerSDRWithExtraFinDegreesMap = singleDegreeRadiiListToMap innerSDRWithExtraFinDegrees
    
    {-Transpose mainSocketInnerMDR to get thickness of walls, as well as protusion for quick-release attachment.-}
    outerMDR = MultiDegreeRadii "a useless name" (map (transformSDRWithList [(+3) | x <- [1..]]) innerSDRWithExtraFinDegrees)
    outerSDR = degrees outerMDR
    outerMDRMap =  singleDegreeRadiiListToMap $ degrees outerMDR           
    

    {-
    Join the inner/outer MDR's together, then drop the 1st 6 rows to make the socket shorter.
    Concat them so they will work with the stl autogenerate function.
    -}
    mainSocketWalls = concat $ 
      dropTopScanLayers  (createVerticalWalls  innerSDRWithExtraFinDegrees outerSDR origin transposeFactors)
  
  mainSocketCubes <- buildCubePointsList' "create socket cubes" mainSocketWalls [CornerPointsId | x <-[1..]]


  let --fin1
      backOfFin1Sdr  =
        transformRangeOfSDR [(+0) | x <- [1..]] [fin1BaseStartDegree] (degrees outerMDR)
        Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin1BaseThicknessInDegrees) (outerMDRMap^.at fin1BaseStartDegree)) )
        Flw.|> (\sdrSeq -> F.toList sdrSeq)
      
      frontOfFin1Sdr =
        transformRangeOfSDR fin1ExtrusionTranposeValues [fin1BaseStartDegree] (degrees outerMDR)
        Flw.|> (\sdrSeq -> sdrSeq S.><
                           (transformRangeOfSDR fin1ExtrusionTranposeValues [(fin1BaseStartDegree + fin1TipThicknessInDegrees)] 
                             [(transformMaybeSDRDegree (+fin1TipThicknessInDegrees) (outerMDRMap^.at fin1BaseStartDegree))]
                           )
               )
        Flw.|> (\sdrSeq -> F.toList sdrSeq)
        
  fin1Cubes     <- buildCubePointsList' "create fin 1 cubes"
                     (concat $ dropTopScanLayers $ createVerticalWalls  backOfFin1Sdr frontOfFin1Sdr origin transposeFactors)
                     [CornerPointsId | x <-[1..]]
  
  
  let --fin2
      backOfFin2Sdr  =
        transformRangeOfSDR [(+0) | x <- [1..]] [fin2BaseStartDegree] (degrees outerMDR)
        Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin2BaseThicknessInDegrees) (outerMDRMap^.at fin2BaseStartDegree)) )
        Flw.|> (\sdrSeq -> F.toList sdrSeq)

      frontOfFin2Sdr =
        transformRangeOfSDR fin2ExtrusionTranposeValues [fin2BaseStartDegree] (degrees outerMDR)
        Flw.|> (\sdrSeq -> sdrSeq S.><
                           (transformRangeOfSDR fin2ExtrusionTranposeValues [(fin2BaseStartDegree + fin2TipThicknessInDegrees)] 
                             [(transformMaybeSDRDegree (+fin2TipThicknessInDegrees) (outerMDRMap^.at fin2BaseStartDegree))]
                           )
               )
        Flw.|> (\sdrSeq -> F.toList sdrSeq)

  
  fin2Cubes     <- buildCubePointsList' "create fin 2 cubes"
                     (concat $ dropTopScanLayers $ createVerticalWalls  backOfFin2Sdr frontOfFin2Sdr origin transposeFactors)
                     [CornerPointsId | x <-[1..]]
  
  return fin2Cubes

--output the stl
generateSwimFinStl :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO () --[CornerPoints]
generateSwimFinStl     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  let cpoints =  ((execState $ runExceptT (swimSocketWithFinBothSides originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)
  in  writeStlToFile $ newStlShape "socket with quick release"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


--print out the CornerPoints built up in the state monad.
showSwimFinCumulativeCornerPoints :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO ()
showSwimFinCumulativeCornerPoints     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  print $ show  ((evalState $ runExceptT (swimSocketWithFinBothSides originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)







{-=========================================================== side mounted quick-release socket ============================================
Generate the socket with a thicker section of wall, into which a hole can be drilled for a quick coupler.
-}

sideMountQuickReleaseSocket :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
sideMountQuickReleaseSocket    mainSocketInnerSDR           rowReductionFactor    pixelsPerMillimeter  = do
  let
    mainWallThickness = 3
    quickReleaseWallThickness = 10
    origin = (Point{x_axis=0, y_axis=0, z_axis=50})
    transposeFactors = [0,heightPerPixel.. ]
    heightPerPixel = 1/pixelsPerMM * (fromIntegral rowReductionFactor)
    sdrMap = singleDegreeRadiiListToMap mainSocketInnerSDR
    
    {-Transpose mainSocketInnerMDR to get thickness of walls, as well as protusion for quick-release attachment.-}
    mainSocketOuterSDR = 
          --0-220 degrees
          transformRangeOfSDR [(+3) | y <- [1..]] [0,10..220] mainSocketInnerSDR 
          --230 degrees
          Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDR ([(+7) | y <- [1..12]] ++ [(+3) | y <- [1..7]]) (sdrMap^.at 230.0)))
          --240-270 degrees
          Flw.|> (\sdrSeq -> sdrSeq S.>< (transformRangeOfSDR ([(+15) | y <- [1..12]] ++ [(+3) | y <- [1..7]]) [240,250..270] mainSocketInnerSDR))
          --280 degrees
          Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDR ([(+9) | y <- [1..12]] ++ [(+3) | y <- [1..7]]) (sdrMap^.at 280.0)))
          --290-360
          Flw.|> (\sdrSeq -> sdrSeq S.>< (transformRangeOfSDR [(+3) | y <- [1..]] [290,300..360] mainSocketInnerSDR))
          --convert sdrSeq back into a list
          Flw.|> (\sdrSeq -> F.toList sdrSeq)
      
  buildCubePointsList' "create socket walls" 
      (concat $ drop 6  (createVerticalWalls  mainSocketInnerSDR mainSocketOuterSDR origin transposeFactors))
      [CornerPointsId | x <-[1..]]
   
  


--output the stl
generateSideMountQuickReleaseSocketStl :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO () --[CornerPoints]
generateSideMountQuickReleaseSocketStl     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  let cpoints =  ((execState $ runExceptT (sideMountQuickReleaseSocket originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)
  in  writeStlToFile $ newStlShape "socket with quick release"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
  {-========================================================== socket attached to walker=====================================================
A series of functions, to produce a socket with riser so push plate can be attached to it, a push plate for his hand to push against, and the hose plate that allows the hose to be attached.
-}
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
socketWithRiser :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) ->
                 ExtensionHeight -> RowReductionFactor -> PixelsPerMillimeter ->  IO ()
socketWithRiser    innerSleeveSDR      outerSleeveSDR      extensionFaceBuilder extensionHeight    rowReductionFactor pixelsPerMM  =
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
                drop 6  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) 
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

