{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocketDesignWork(loadMDRAndPassToProcessor, socketWithRiserStlGenerator) where
{------------------------------ ToDo------------------------------------
Use the autogenerate stl in all functions.
-}

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR, transposeMDRList,
                          {-transposeSDRList,-} extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns,  createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..), Origin(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace, extractFrontTopLine, extractBackTopLine)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace, frontFaceFromBackFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import TypeClasses.Transposable(transpose)
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|), DegreeRange(..))
import CornerPoints.MeshGeneration(autoGenerateEachCube)

import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)

import Helpers.List((++:))

import Primitives.Cylindrical.Walled(squaredYLengthenedCylinder, squaredCylinder)
import Primitives.Cylindrical.Solid(cylinder, squaredOffYLengthenedCylinder)

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
loadMDRAndPassToProcessor ::  IO ()
loadMDRAndPassToProcessor  = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  let removeDefectiveTopRow :: MultiDegreeRadii -> MultiDegreeRadii
      removeDefectiveTopRow (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']
      rowReductionFactor = 100
      extensionHeight = 30
      plateRadius = 30
      power = 2.5
      lengthenYFactor = 20
      
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let ----------------------------for the socket that has the bottom extension to attach to the walker
            --enlarge it to fit over the socket already printed with WalkerSocket. 1st attempt at +2 was not quite big enough, trying 3.
            --rotate it to line up better with the riser, when using squared off function.
            --innerSleeveMDR = rotateMDR $ rotateMDR $ rotateMDR $ transpose (+3) $ reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
            innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) .  (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow   $ (MultiDegreeRadii name' degrees')
            --give it a thickness of 3 mm
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            plateRadius = 24

            ------------------------------------- for the socket with the quick-release mounted on the side
            --need to adjust transposefactor, as +3 was a loose fit for his walker. Try +2
            innerSleeveMDRForSideMount = transpose (+2) $ reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
        in  ------------------------------------choose the shape to process---------------------------------------------.
            ---------socket attached to walker
            --Moved to it's own processor, loadMDRAndCallSocketWithRiserStlGenerator, and probably will no longer compile this way.
            
            
            --pushPlate plateRadius power lengthenYFactor
            generatePushPlateStl plateRadius power lengthenYFactor []

            --generatehosePlateStl plateRadius power lengthenYFactor []
            --showHosePlateError plateRadius power lengthenYFactor []

            ---------socket with sidemount quick release------------
            --generateSideMountQuickReleaseSocketStl (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []

            ----------------- swim fin---------------------
            --swimFinSocketOnlyInsideFin (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM
            --generateSwimFinStl (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []
            --showSwimFinCumulativeCornerPoints (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []

            ------------------ hammerhead shark swim fin -----------------------------------
            --generateHammerHeadSharkHeadSectionStl (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []
            --generateHammerHeadSharkBodySectionStl (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []
            
      Nothing                                ->
        putStrLn "File not decoded"

removeDefectiveTopRow' :: MultiDegreeRadii -> MultiDegreeRadii
removeDefectiveTopRow' (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']


{---------------------------------------------------------hammer head shark swim fin---------------------------------------------
Prints in 2 sections:
1: Upper socket with the head. This will allow back of head to print flat on the print bed.

2: Lower sockt with the body. The body should flare out like a set of curved fins to catch water underneath.
-}

{-
ToDo:
fin2 tip degrees need to be reduced to bring the tip around
-}
hammerHeadSharkHeadSection :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
hammerHeadSharkHeadSection originalSDR           rowReductionFactor    pixelsPerMillimeter  = do
  let
    mainWallThickness = 3
    --start/end degrees of the base of fin1/2
    --1st print at 60, totally out of line with his arm action. Should -50
    fin1BaseStartDegree = 10
    fin1BaseThicknessInDegrees   = 8 --orig 4
    --1st print at 250, totally out of line with his arm action. Should -50
    fin2BaseStartDegree = 200
    fin2BaseThicknessInDegrees   = 8 --orig 5
    --start/end degrees of the tip of fin1/2
    fin1TipStartDegree = 60
    fin1TipThicknessInDegrees   = 5 --orig 2
    fin2TipStartDegree = 200 
    fin2TipThicknessInDegrees   = 7 --orig 3
    
    fin1ExtrusionTranposeValues = [(\r -> 43) | y <- [1..]] --orig 65, too short 25, 40;  bit long 45
    fin2ExtrusionTranposeValues = [(\r -> 25) | y <- [1..]] --orig 45, no good 15
    
    origSDR = originalSDR -- degrees mainSocketInnerMDR 
    sdrMap = singleDegreeRadiiListToMap origSDR

    --how many of the 10mm layers to drop of top off scan.
    dropTopScanLayers = drop 1
    takeTopRemainingScanLayers = take 7
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
      takeTopRemainingScanLayers $ dropTopScanLayers  (createVerticalWalls  innerSDRWithExtraFinDegrees outerSDR origin transposeFactors)
  
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
                     (concat $ takeTopRemainingScanLayers $ dropTopScanLayers $ createVerticalWalls  backOfFin1Sdr frontOfFin1Sdr origin transposeFactors)
                     [CornerPointsId | x <-[1..]]
  
  
  let --fin2
      backOfFin2Sdr  =
        transformRangeOfSDR [(+0) | x <- [1..]] [fin2BaseStartDegree] (degrees outerMDR)
        Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin2BaseThicknessInDegrees) (outerMDRMap^.at fin2BaseStartDegree)) )
        Flw.|> (\sdrSeq -> F.toList sdrSeq)

      frontOfFin2Sdr =
        transformRangeOfSDR fin2ExtrusionTranposeValues [fin2TipStartDegree] (degrees outerMDR)
        Flw.|> (\sdrSeq -> sdrSeq S.><
                           (transformRangeOfSDR fin2ExtrusionTranposeValues [(fin2TipStartDegree + fin2TipThicknessInDegrees)] 
                             [(transformMaybeSDRDegree (+fin2TipThicknessInDegrees) (outerMDRMap^.at fin2TipStartDegree))]
                           )
               )
        Flw.|> (\sdrSeq -> F.toList sdrSeq)

  
  fin2Cubes     <- buildCubePointsList' "create fin 2 cubes"
                     (concat $ takeTopRemainingScanLayers $ dropTopScanLayers $ createVerticalWalls  backOfFin2Sdr frontOfFin2Sdr origin transposeFactors)
                     [CornerPointsId | x <-[1..]]
  
  return fin2Cubes

--output the stl
generateHammerHeadSharkHeadSectionStl :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO () --[CornerPoints]
generateHammerHeadSharkHeadSectionStl     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  let cpoints =  ((execState $ runExceptT (hammerHeadSharkHeadSection originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)
  in  writeStlToFile $ newStlShape "socket with quick release"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


--print out the CornerPoints built up in the state monad.
showHammerHeadSharkHeadSectionCumulativeCornerPoints :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO ()
showHammerHeadSharkHeadSectionCumulativeCornerPoints     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  print $ show  ((evalState $ runExceptT (hammerHeadSharkHeadSection originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)

{-
ToDo:
Shorten the fins slightly.
-}
hammerHeadSharkBodySection :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
hammerHeadSharkBodySection originalSDR           rowReductionFactor    pixelsPerMillimeter  = do
  let
    mainWallThickness = 3
    --start/end degrees of the base of fin1/2
    --1st print at 60, totally out of line with his arm action. Should -50
    fin1BaseStartDegree = 10
    fin1BaseThicknessInDegrees   = 8 --orig 4
    --1st print at 250, totally out of line with his arm action. Should -50
    fin2BaseStartDegree = 200
    fin2BaseThicknessInDegrees   = 8 --orig 5
    --start/end degrees of the tip of fin1/2
    fin1TipStartDegree = 60
    fin1TipThicknessInDegrees   = 5 --orig 2
    fin2TipStartDegree = 200
    fin2TipThicknessInDegrees   = 7 --orig 3
    
    fin1ExtrusionTranposeValues = [(\r -> 50) | y <- [1..]] --orig 65, too short 43, 
    fin2ExtrusionTranposeValues = [(\r -> 25) | y <- [1..]] --orig 45, no good 15
    
    origSDR = originalSDR -- degrees mainSocketInnerMDR 
    sdrMap = singleDegreeRadiiListToMap origSDR

    --how many of the 10mm layers to drop of top off scan.
    dropTopScanLayers = drop 8
    takeTopRemainingScanLayers = take 100
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
      takeTopRemainingScanLayers $ dropTopScanLayers  (createVerticalWalls  innerSDRWithExtraFinDegrees outerSDR origin transposeFactors)
  
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
                     (concat $ takeTopRemainingScanLayers $ dropTopScanLayers $ createVerticalWalls  backOfFin1Sdr frontOfFin1Sdr origin transposeFactors)
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
                     (concat $ takeTopRemainingScanLayers $ dropTopScanLayers $ createVerticalWalls  backOfFin2Sdr frontOfFin2Sdr origin transposeFactors)
                     [CornerPointsId | x <-[1..]]
  
  return fin2Cubes

--output the stl
generateHammerHeadSharkBodySectionStl :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO () --[CornerPoints]
generateHammerHeadSharkBodySectionStl     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  let cpoints =  ((execState $ runExceptT (hammerHeadSharkBodySection originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)
  in  writeStlToFile $ newStlShape "socket with quick release"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)


--print out the CornerPoints built up in the state monad.
showHammerHeadSharkBodySectionCumulativeCornerPoints :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO ()
showHammerHeadSharkBodySectionCumulativeCornerPoints     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  print $ show  ((evalState $ runExceptT (hammerHeadSharkBodySection originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)
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
              |||||||||||||||||||  innerHose riserBase screwRing
-}
hosePlate :: PlateRadius -> Power -> LengthenYFactor -> ExceptT BuilderError (State CpointsStack ) CpointsList
hosePlate plateRadius power lengthenYFactor = do
  let
    
    
    hoseInnerRadius = 7.5
    hoseOuterRadius = hoseInnerRadius + 3
    riserInnerRadius = hoseOuterRadius
    riserHeight = 5 
    riserOuterRadius = riserHeight + riserInnerRadius
    screwInsideRadius = riserOuterRadius
    screwsThickness = plateRadius - screwInsideRadius
    baseHeight = 3
    hoseHeight = 20
    angles = (map (Angle) [0,10..360])  
    baseOrigin = Point 0 0 0
    riserOrigin = transposeZ ( + baseHeight) baseOrigin

  screwRingFrontFaces <- buildCubePointsList' "baseScrewRingFrontFaces"
    (map extractFrontFace
    (squaredYLengthenedCylinder
             (Radius plateRadius) baseOrigin angles baseHeight riserHeight power lengthenYFactor))
    [CornerPointsId | x <-[1..]]

  screwRingCubes <- buildCubePointsList' "screwRingCubes"
    (map extractBackFace
    (squaredYLengthenedCylinder
             (Radius screwInsideRadius) baseOrigin angles baseHeight riserHeight power lengthenYFactor))
    screwRingFrontFaces

  riserBaseBackFaces <- buildCubePointsList' "riserBaseInsideFaces"
    (map (backFaceFromFrontFace . extractFrontFace)
    (cylinder (Radius hoseOuterRadius)  baseOrigin angles baseHeight))
    [CornerPointsId | x <-[1..]]
  
  riserBaseCubes <- buildCubePointsList' "riserBaseCubes"
    riserBaseBackFaces
    (map (frontFaceFromBackFace . extractBackFace) screwRingCubes )

  --base upon which the hose rests
  hoseBaseCubes <- buildCubePointsList' "hoseBaseCubes"
    (cylinder (Radius hoseInnerRadius) baseOrigin angles baseHeight)
    [CornerPointsId | x <-[1..]]

  --walls that surround the hose.
  hoseWallBaseCubes <- buildCubePointsList' "hoseWalls"
    (map (frontFaceFromBackFace . extractBackFace) riserBaseCubes)
    (map (backFaceFromFrontFace . extractFrontFace) hoseBaseCubes)
  
  --top face is a line
  riserCubes <-
    buildCubePointsList' "riserCubes"
    (map ( lowerFaceFromUpperFace . extractTopFace) riserBaseCubes)
    (
      (map ((transposeZ (+riserHeight)). extractFrontTopLine . extractFrontFace) hoseWallBaseCubes)
      |+++|
      (map ( (transposeZ (+riserHeight)) . extractBackTopLine . backFaceFromFrontFace . extractFrontFace) hoseWallBaseCubes)
    )
  
  hoseWallRiserCubes <- buildCubePointsList' "hoseWallRiserCubes"
    (map (lowerFaceFromUpperFace . extractTopFace) hoseWallBaseCubes)
    (map ((transposeZ (+riserHeight)) . extractTopFace) hoseWallBaseCubes)    
  
  --hose wall that rise about the riser.
  upperHoseWallCubes <- buildCubePointsList' "upperHoseWalls"
    (map (lowerFaceFromUpperFace . extractTopFace) hoseWallRiserCubes)
    (map ((transposeZ (+10)) . extractTopFace) hoseWallRiserCubes)
  
  return screwRingCubes

--output the stl
generatehosePlateStl :: PlateRadius -> Power ->           LengthenYFactor ->   CpointsStack -> IO () --[CornerPoints]
generatehosePlateStl     originalSDR   rowReductionFactor pixelsPerMillimeter  inState =
  let cpoints =  ((execState $ runExceptT (hosePlate originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)
  in  writeStlToFile $ newStlShape "hose plate"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)

showHosePlateError :: PlateRadius -> Power ->           LengthenYFactor ->   CpointsStack -> IO ()
showHosePlateError originalSDR   rowReductionFactor pixelsPerMillimeter  inState =
  print $ show ((evalState $ runExceptT (hosePlate originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)

     

{-
Attaches directly to the socketWithRiser

Printing notes:
I printed with no infill, and 2 top layers. This resulted in the top 2 layers of the plate,
printing over a gap of about 1 mm. This was not a problem. Should have had solid infill or more bottom layers.
-}

pushPlate :: PlateRadius -> Power -> LengthenYFactor ->  ExceptT BuilderError (State CpointsStack ) CpointsList
pushPlate plateRadius    power    lengthenYFactor  = do
  let
    origin = Point 0 0 0 :: Origin
    angles = (map (Angle) [0,10..360])
    plateHeight = 3
    riserHeight = 3

  centerPlate <- buildCubePointsList' "centerPlate"
                  (squaredOffYLengthenedCylinder  (Radius 21)   origin    angles     plateHeight  power    lengthenYFactor)
                  [CornerPointsId | x <-[1..]]

  outerRing <- buildCubePointsList' "outerRing"
                 (squaredYLengthenedCylinder
                                (Radius (plateRadius - riserHeight))  origin angles plateHeight riserHeight power lengthenYFactor)
                 [CornerPointsId | x <-[1..]]

  --riser has to be created in 2 parts, as it spans the 0/360 degree segment, but with section from ~80 degrees -> ~270 excluded.
  startRiser     <- buildCubePointsList' "outerRingStartRiser"
                      (take 8
                        (squaredYLengthenedCylinder (Radius (plateRadius - riserHeight))
                                (transposeZ (+plateHeight)origin)  angles (30 :: Height) riserHeight  power  lengthenYFactor)
                      )
                      [CornerPointsId | x <-[1..]]

  endRiser       <- buildCubePointsList' "outerRingEndRiser"
                      (drop  28
                        (squaredYLengthenedCylinder (Radius (plateRadius - riserHeight))
                                (transposeZ (+plateHeight)origin)  angles (30 :: Height) riserHeight  power  lengthenYFactor)
                      )
                      [CornerPointsId | x <-[1..]]

  return endRiser

generatePushPlateStl :: PlateRadius -> Power -> LengthenYFactor ->  CpointsStack  -> IO ()
generatePushPlateStl plateRadius    power    lengthenYFactor initialState  =
  let cpoints = execState ( runExceptT $ pushPlate plateRadius    power    lengthenYFactor ) initialState 
  in  writeStlToFile $ newStlShape "push plate"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
    
-- ============================================================================================================================================================
{-
Socket which has a riser attached to the top. This allows the hose attachment/push plate to be attached to it.

It differs from the original socketWithRiser, in that the riser is closed in, instead of half open.
If it needs to be make open sided, see pushPlate on how to do it.
It has not been printed yet, to test this new closed in system.
-}
socketWithRiser :: [SingleDegreeRadii] -> [SingleDegreeRadii] -> RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
socketWithRiser    innerSleeveSDR         outerSleeveSDR         rowReductionFactor    pixelsPerMM = do
  let extensionHeight = 30
      transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = (1/ pixelsPerMM) * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])

  riserCubes <- buildCubePointsList' "riserCubes"
                (squaredCylinder  [Radius 18 | x <- [1..]] (3::Thickness) (transposeX (+0)(transposeY (+(-15))(transposeZ (+(-15))origin)))    angles (20::Height)  (2.5::Power))
                [CornerPointsId | x <-[1..]]

  mainCubes  <- buildCubePointsList' "mainCubes"
                (concat $ drop 6  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors) )
                [CornerPointsId | x <-[1..]]

  joinRiserToMainCubes <- buildCubePointsList' "joinRiserToMainCubes"
                          (
                            map (upperFaceFromLowerFace . extractBottomFace) riserCubes
                          )
                          (
                            map (lowerFaceFromUpperFace . extractTopFace) (head $ drop 6  (createVerticalWalls  innerSleeveSDR outerSleeveSDR origin transposeFactors))
                          )
                
  return riserCubes

--load the json file and call generate stl
socketWithRiserStlGenerator :: IO ()
socketWithRiserStlGenerator = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --Changing this to 50 does not work. It should be made so this can be changed.
            --Is it some combination with PixelsPerMillimeter that messes it up.
            --ToDo: Make a diff. version of reduceScan that perhaps uses mm instead of mod of some number.
            rowReductionFactor = 100::RowReductionFactor 
            innerSleeveMDR = (rotateMDR) . (rotateMDR) . (rotateMDR) . (transpose (+3)) . (reduceScan rowReductionFactor) . removeDefectiveTopRow' $ (MultiDegreeRadii name' degrees')
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            cpoints =  ((execState $ runExceptT (socketWithRiser (degrees innerSleeveMDR) (degrees outerSleeveMDR)         rowReductionFactor    pixelsPerMM ) ) [])
        in  writeStlToFile $ newStlShape "socket with riser"  $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
      Nothing                                ->
        putStrLn "File not decoded"




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

