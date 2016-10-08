{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocketDesignWork(sideMountQuickReleaseSocket) where

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR, transposeMDRList,
                          transposeSDRList, extractSDRWithinRange, singleDegreeRadiiListToMap, transformSDRWithList, extractMaybeSDR,
                          transformRangeOfSDR, transformMaybeSDR, transformMaybeSDRDegree, transformSDRDegree)
  
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

--import Test.HUnit

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
            --socketWithRiser innerSleeveMDR outerSleeveMDR extensionFaceBuilder extensionHeight rowReductionFactor pixelsPerMM
            --pushPlate plateRadius power lengthenYFactor
            --hosePlate plateRadius power lengthenYFactor

            ---------socket with sidemount quick release------------
            --sideMountQuickReleaseSocket innerSleeveMDRForSideMount rowReductionFactor pixelsPerMM

            ----------------- swim fin---------------------
            --swimFinSocketOnlyInsideFin (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM
            generateSwimFinStl (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []
            --showSwimFinCumulativeCornerPoints (degrees innerSleeveMDRForSideMount) rowReductionFactor pixelsPerMM []
            
            
      Nothing                                ->
        putStrLn "File not decoded"

{-----------------------------------------------------------swim fin use builder--------------------------------------------------------------
Reproduce the swimFinSocketOnlyInsideFin, using the cornerpoints builder system
-}
--curry in the stack pushing function
buildCubePointsList' = buildCubePointsList (++)

swimSocketWithFinBothSides :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> ExceptT BuilderError (State CpointsStack ) CpointsList
swimSocketWithFinBothSides originalSDR           rowReductionFactor    pixelsPerMillimeter  = do
  let
    mainWallThickness = 3
    --start/end degrees of the base of fin1/2
    fin1BaseStartDegree = 60
    fin1BaseThicknessInDegrees   = 4
    fin2BaseStartDegree = 250
    fin2BaseThicknessInDegrees   = 5
    --start/end degrees of the tip of fin1/2
    fin1TipStartDegree = 60
    fin1TipThicknessInDegrees   = 2
    fin2TipStartDegree = 250
    fin2TipThicknessInDegrees   = 3
    
    --finExtrusionTranposeValues = [(\r -> 65),(\r -> 75),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80),(\r -> 80)]
    fin1ExtrusionTranposeValues = [(\r -> 65) | y <- [1..]]
    fin2ExtrusionTranposeValues = [(\r -> 45) | y <- [1..]]
    
    origSDR = originalSDR -- degrees mainSocketInnerMDR 
    sdrMap = singleDegreeRadiiListToMap origSDR

    --how many of the 10mm layers to drop of top off scan
    dropTopScanLayers = drop 3

    {-------------------------------------------------- left off----------------------------------------
    For the new fin at 60 degrees:
    Need to have an extra degree so that tip can be made same thichkness as base of fin.
    Add and use: fin1TipThicknessInDegrees
    -will have a thinner thickness than base, as the tip thickens as it is transposed
    --So this degree will have to be added before the base thickness, as it must be thinner.
    -}
    
    innerSDRWithExtraFinDegrees =
            --0-60 degrees. Leading up to the outer fin(1st fin)
            transformRangeOfSDR [(+0) | y <- [1..]] [0,10..60] origSDR
            --add in the fin tip ending degree
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin1TipThicknessInDegrees) (sdrMap^.at fin1BaseStartDegree)) )
            --add in the fin base ending degree
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin1BaseThicknessInDegrees) (sdrMap^.at fin1BaseStartDegree)) )
            {-Leave them out as will try to add a transposed front face(and thus an outer layer of cubes) instead of directly transposing the main socket layer.
            This means I only need a single skinny set of cubes.
            --add in the 132 sdr
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+2) (sdrMap^.at 60.0)) )
            --add in the 133 sdr. 
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+3) (sdrMap^.at 80.0)) )
            -}
            --140-220 degrees
            Flw.|> (\sdrSeq -> sdrSeq S.>< transformRangeOfSDR [(+0) | y <- [1..]] [70,80..250] origSDR)
            {- This is old fin2 system. Delete when new system made
            --add in the 251 sdr
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+1) (sdrMap^.at 250.0)) )
            --add in the 252 sdr
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+2) (sdrMap^.at 250.0)) )
            --add in the 253 sdr
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+3) (sdrMap^.at 250.0)) )
            -}
            --add in the fin tip ending degree
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin2TipThicknessInDegrees) (sdrMap^.at fin2BaseStartDegree)) )
            --add in the fin base ending degree
            Flw.|> (\sdrSeq -> sdrSeq S.|> (transformMaybeSDRDegree (+fin2BaseThicknessInDegrees) (sdrMap^.at fin2BaseStartDegree)) )


            --add on the rest of the origSDR
            Flw.|> (\sdrSeq -> sdrSeq S.>< transformRangeOfSDR [(+0) | y <- [1..]] [260,270..360] origSDR)
            --convert back into a list. Should be a [SDR] such as makes up the degrees of an MDR.
            Flw.|> (\sdrSeq -> F.toList sdrSeq)
    
    innerSDRWithExtraFinDegreesMap = singleDegreeRadiiListToMap innerSDRWithExtraFinDegrees
    
    {-Transpose mainSocketInnerMDR to get thickness of walls, as well as protusion for quick-release attachment.-}
    outerMDR =
      let 
          --starts at the top of socket
          
          
          transposedSDR =
            --Should be able to map over the innerSDRWithExtraFinDegrees, instead of this way.
            
            transformRangeOfSDR [(+3) | y <- [1..]] ([0,10..60] ++
                                                     [(fin1BaseStartDegree + fin1TipThicknessInDegrees),(fin1BaseStartDegree + fin1BaseThicknessInDegrees)] ++
                                                     [70,80..250] ++
                                                     [(fin2BaseStartDegree + fin2TipThicknessInDegrees),(fin2BaseStartDegree + fin2BaseThicknessInDegrees)] ++
                                                     [260,270..360]
                                                    ) innerSDRWithExtraFinDegrees
            
            {-Perform the mapping to replace above transformation.
              innerSDRWithExtraFinDegrees is(should be) a [SDR]. Need to map over it with something that in turn transforms each SDR.
              Problem is, transformSDRWithList needs it's args flipped, so it can be used to map.
            -}
            --S.fromList (map (transformSDRWithList [(+3) | x <- [1..]]) innerSDRWithExtraFinDegrees)
            
            --convert sdrSeq back into a list
            Flw.|> (\sdrSeq -> F.toList sdrSeq)
      in
        --mainSocketInnerMDR {degrees = transposedSDR}
        MultiDegreeRadii "a useless name" transposedSDR
                 
    origin = (Point{x_axis=0, y_axis=0, z_axis=50})
    transposeFactors = [0,heightPerPixel.. ]
    heightPerPixel = 1/pixelsPerMM * (fromIntegral rowReductionFactor)

    {-
    Join the inner/outer MDR's together, then drop the 1st 6 rows to make the socket shorter.
    Concat them so they will work with the stl autogenerate function.
    -}
    mainSocketWalls = concat $ 
      --drop 6  (createVerticalWalls  (mainSocketInnerMDR {degrees = innerSDRWithExtraFinDegrees} ) outerMDR origin transposeFactors)
      dropTopScanLayers  (createVerticalWalls  (MultiDegreeRadii "a useless name" innerSDRWithExtraFinDegrees) outerMDR origin transposeFactors)

  {-The main socket, with extra degrees inserted for the attachement points of fin1/fin2.
    Fin1: Transposed the same thickness as the socket walls. Will use diff system for projecting out the front face.
    Fin2: Transposed out to fin2ExtrusionTranposeValues. Should change it later to be same system as fin1, if it works out good.
  -}
  mainSocketCubes <- buildCubePointsList' "create socket cubes" mainSocketWalls [CornerPointsId | x <-[1..]]

  let
      {-Get the sdr for the fin. Create [CornerPoints] from them.-}
      fin1BackOfSocketSdr =  F.toList $ transformRangeOfSDR [(+0) | y <- [1..]] [fin1BaseStartDegree, fin1BaseStartDegree + fin1BaseThicknessInDegrees] innerSDRWithExtraFinDegrees
      fin1FrontOfSocketSdr =  F.toList $ transformRangeOfSDR [(+3) | y <- [1..]] [fin1BaseStartDegree, fin1BaseStartDegree + fin1BaseThicknessInDegrees] innerSDRWithExtraFinDegrees
      fin1FrontOfFinSdr         =  F.toList $ transformRangeOfSDR fin1ExtrusionTranposeValues [fin1BaseStartDegree, fin1BaseStartDegree + fin1TipThicknessInDegrees] innerSDRWithExtraFinDegrees
      --Faces from front of socket, coverted to the back faces of the fin.
      fin1BackFaces' = map (backFaceFromFrontFace . extractFrontFace) $
                          concat $
                          dropTopScanLayers
                          (createVerticalWalls  (MultiDegreeRadii "a useless name" fin1BackOfSocketSdr) (MultiDegreeRadii "a useless name" fin1FrontOfSocketSdr) origin transposeFactors)
      --
      fin1FrontFaces = map (extractFrontFace) $
                           concat $
                           dropTopScanLayers
                           (createVerticalWalls  (MultiDegreeRadii "a useless name" fin1FrontOfSocketSdr) (MultiDegreeRadii "a useless name" fin1FrontOfFinSdr) origin transposeFactors)
      
  fin1BackFaces <- buildCubePointsList' "create back faces of fin 1" fin1BackFaces' [CornerPointsId | x <-[1..]]
  fin1Cubes     <- buildCubePointsList' "create fin 1 cubes" fin1BackFaces fin1FrontFaces
  
  {-Now copy over fin1-}
  let
      {-Get the sdr for the fin. Create [CornerPoints] from them.-}
      fin2BackOfSocketSdr =  F.toList $ transformRangeOfSDR [(+0) | y <- [1..]] [fin2BaseStartDegree, fin2BaseStartDegree + fin2BaseThicknessInDegrees] innerSDRWithExtraFinDegrees
      fin2FrontOfSocketSdr =  F.toList $ transformRangeOfSDR [(+3) | y <- [1..]] [fin2BaseStartDegree, fin2BaseStartDegree + fin2BaseThicknessInDegrees] innerSDRWithExtraFinDegrees
      fin2FrontOfFinSdr         =  F.toList $ transformRangeOfSDR fin2ExtrusionTranposeValues [fin2BaseStartDegree, fin2BaseStartDegree + fin2TipThicknessInDegrees] innerSDRWithExtraFinDegrees
      --Faces from front of socket, coverted to the back faces of the fin.
      fin2BackFaces' = map (backFaceFromFrontFace . extractFrontFace) $
                          concat $
                          dropTopScanLayers
                          (createVerticalWalls  (MultiDegreeRadii "a useless name" fin2BackOfSocketSdr) (MultiDegreeRadii "a useless name" fin2FrontOfSocketSdr) origin transposeFactors)
      --
      fin2FrontFaces = map (extractFrontFace) $
                           concat $
                           dropTopScanLayers
                           (createVerticalWalls  (MultiDegreeRadii "a useless name" fin2FrontOfSocketSdr) (MultiDegreeRadii "a useless name" fin2FrontOfFinSdr) origin transposeFactors)
  
  fin2BackFaces <- buildCubePointsList' "create back faces of fin 2" fin2BackFaces' [CornerPointsId | x <-[1..]]
  fin2Cubes     <- buildCubePointsList' "create fin 2 cubes" fin2BackFaces fin2FrontFaces

  return fin2Cubes

--output the stl
generateSwimFinStl :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO () --[CornerPoints]
generateSwimFinStl     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  let cpoints = autoGenerateEachCube [] ((execState $ runExceptT (swimSocketWithFinBothSides originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)
  in  writeStlToFile $ newStlShape "socket with quick release"  $ [FacesAll | x <- [1..]] |+++^| (cpoints)


--print out the CornerPoints built up in the state monad.
showSwimFinCumulativeCornerPoints :: [SingleDegreeRadii] ->  RowReductionFactor -> PixelsPerMillimeter -> CpointsStack -> IO ()
showSwimFinCumulativeCornerPoints     originalSDR            rowReductionFactor    pixelsPerMillimeter    inState =
  print $ show  ((evalState $ runExceptT (swimSocketWithFinBothSides originalSDR rowReductionFactor pixelsPerMillimeter) ) inState)







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

