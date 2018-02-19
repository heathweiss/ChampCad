{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{- |
Build shoelift for German hiking boots

Will have a central pillar with flexible carbon fiber toe and heel.
-}

module Examples.ShoeLift.GermanHikers.GermanHikerBase() where

import Primitives.Cylindrical.Walled(cylinder)
import qualified Primitives.Cylindrical.Solid as CS (cylinder, yLengthenedCylinder)

import Geometry.Angle(rotateAngle)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)

import Persistable.Radial (Layer(..), AngleHeightRadius(..), AnglesHeightsRadii(..), nameUnique', angleHeightRadius', layerId',
                           angleHeightRadiusLayerId', extractAnglesHeightsRadiiFromEntity, ExtractedAngleHeightRadius(..),
                           extractRadii, extractAngles, extractHeights, extractLayerId, extractOrigin, loadAndExtractedAngleHeightRadiusFromDB)

import Builder.Monad(BuilderError(..),
                     cornerPointsErrorHandler, buildCubePointsList, buildCubePointsListSingle,
                     buildCubePointsListWithIOCpointsListBase, buildCubePointsListWithAdd,
                     CpointsStack, CpointsList, ExceptStackCornerPointsBuilder)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy hiding (get)
import qualified Control.Monad.State.Lazy as ST (get)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

import CornerPoints.Points(Point(..))
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, createTopFacesVariableHeight, createBottomFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.FaceConversions(toTopFace, toBackFace, toFrontFace, toFrontLeftLine, toFrontRightLine, toBottomFace, toTopRightLine,
                                    toF2, toB2, toB3, toBackTopLine, toBackBottomLine, toBottomFrontLine)
import CornerPoints.FaceExtraction(extractBackFace, extractBackTopLine, extractFrontTopLine, extractFrontFace, extractLeftFace, 
                                   extractFrontLeftLine, extractFrontRightLine, extractBackRightLine, extractBackLeftLine, extractBottomFace, extractBottomFrontLine,
                                   extractTopFace, extractB4, extractB3, extractB2, extractB1, extractF4, extractF3, extractF2, extractF1)

import Joiners.AdvanceComposable(Advancer(..), OuterAdvancerOutput(..), InnerAdvancerOutput(..),naiveAdvCpointFromInnerPerims, naiveAdvCPointFromOuterPerims, advancerRecur,
                                 advCPointFromClosestInnerOuterAdvCPoint, extractAdvCPointsFromAdvancer, advCPointFromClosestInnerOuterUsedCPointBase,
                                 createAdvCPointFromInnerPerimsCheckLegalIntersection, outerAdvancerOutPutHasLegalIntersections, checkInnerAdvCPtForLegality)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import TypeClasses.Transposable(transpose)

import Test.HUnit

import Control.Lens

makeLenses ''Advancer
makeLenses ''AngleHeightRadius

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
------------------------------------------------------------------ Pillars-----------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
Make a center riser that joins the german hikers tread to the pillars golf tread I already printed.

NFG:
Need to have my joiner work on a <bottom/top>Face vertical direction because the angles do not align between the 2 scans.
-}
pillarTreadScanLayer = "bottom"
pillarDatabaseName = "src/Examples/ShoeLift/Pillars/geoxPillarsWithAnkleBrace.db"
currentPillarBuilder = pillarsToGermanCenterRiserBuilder 
                       --pillarsToGermanCenterRiserBuilder

--make a riser to convert from pillars to german centers
runPillarsToGermanCenterRiserBuilder :: IO () 
runPillarsToGermanCenterRiserBuilder = runSqlite pillarDatabaseName $ do
  pillarLayerId <- getBy $ nameUnique' pillarTreadScanLayer
  pillarAngleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId pillarLayerId)] []

  
  case pillarLayerId of
    Nothing -> liftIO $ putStrLn "pillar scan layer was not found"
    
    (Just (Entity key pillarLayerVal)) -> do
      liftIO $ putStrLn "pillar tread scan layer was found"
      
      let
       runGermanDatabase = runSqlite germanDatabaseName $ do
        germanLayerId <- getBy $ nameUnique' germanTreadScanLayer
        germanAngleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId germanLayerId)] []
        
        case germanLayerId of
          Nothing -> liftIO $ putStrLn "german tread scan layer was not found"
          (Just (Entity key germanLayerVal)) -> do
            liftIO $ putStrLn "german tread scan layer was found"
            let builder = currentPillarBuilder
                            (extractAnglesHeightsRadiiFromEntity pillarAngleHeightRadiusEntity)
                            (extractOrigin pillarLayerVal)
                            (extractAnglesHeightsRadiiFromEntity germanAngleHeightRadiusEntity)
                            (extractOrigin germanLayerVal)
                valCpoints = ((evalState $ runExceptT builder) [])
                cpoints = ((execState $ runExceptT builder) [])
            case valCpoints of
              (Left e) -> liftIO $ print $ e
              (Right a) -> do
                liftIO $ putStrLn "output from Builder was good"
                liftIO $ writeStlToFile $ newStlShape "boot tread meets full lenght contoured riser" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
                liftIO $ putStrLn "stl should have been output"
      runGermanDatabase

showPillarsToGermanCenterRiserBuilder :: IO () 
showPillarsToGermanCenterRiserBuilder = runSqlite pillarDatabaseName $ do
  pillarLayerId <- getBy $ nameUnique' pillarTreadScanLayer
  pillarAngleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId pillarLayerId)] []

  
  case pillarLayerId of
    Nothing -> liftIO $ putStrLn "pillar scan layer was not found"
    
    (Just (Entity key pillarLayerVal)) -> do
      liftIO $ putStrLn "pillar tread scan layer was found"
      
      let
       runGermanDatabase = runSqlite germanDatabaseName $ do
        germanLayerId <- getBy $ nameUnique' germanTreadScanLayer
        germanAngleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId germanLayerId)] []
        
        case germanLayerId of
          Nothing -> liftIO $ putStrLn "german tread scan layer was not found"
          (Just (Entity key germanLayerVal)) -> do
            liftIO $ putStrLn "german tread scan layer was found"
            let builder = currentPillarBuilder
                            (extractAnglesHeightsRadiiFromEntity pillarAngleHeightRadiusEntity)
                            (extractOrigin pillarLayerVal)
                            (extractAnglesHeightsRadiiFromEntity germanAngleHeightRadiusEntity)
                            (extractOrigin germanLayerVal)
                valCpoints = ((evalState $ runExceptT builder) [])
                cpoints = ((execState $ runExceptT builder) [])
            case valCpoints of
              (Left e) -> liftIO $ print $ e
              (Right a) -> do
                liftIO $ print $ show a
      runGermanDatabase


--make a riser to convert from pillars to german centers
pillarsToGermanCenterRiserBuilder :: AnglesHeightsRadii -> Point -> 
                                  AnglesHeightsRadii -> Point ->
                                  ExceptStackCornerPointsBuilder
pillarsToGermanCenterRiserBuilder pillarAHR origin germanAHR _ = do
  let
    
    germanHeight = 60
    pillarHeight = 50
    germanAngles ang = ((ang >= 32.94) && (ang <= 138.25)) --28 angles
                       ||
                       ((ang >= 212.47) && (ang <= 332.16)) --38 angles
    pillarAngles ang = ((ang >= 35.0) && (ang <= 171.0)) 
                       ||
                       ((ang >= 205.0) && (ang <= 330.0)) 
    pillarCentralAhr = filter (\(AngleHeightRadius ang _ _ _) -> pillarAngles ang) pillarAHR
    germanCentralAhr = filter (\(AngleHeightRadius ang _ _ _) -> germanAngles ang) germanAHR

  let topFronts :: Double -> AnglesHeightsRadii -> [CornerPoints]
      topFronts height ahr =
              let
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii ahr)
                    (extractAngles ahr)
                    [height | val <- [1..]]
              in
              (extractF3 $ head topFaces) : (((map (extractF2) topFaces)) ++ [(toF2 . extractF3 $ head topFaces)]   )
              --need to close the backend by putting the front point onto the end of list
    
  pillarTopFronts <- buildCubePointsListSingle "topHealLeadFronts"
             (topFronts pillarHeight pillarCentralAhr)



  let
    centerAlignment height =
         let
           primaryRad = 21
             --19.9 printed as 38.5 mm. Need 41 so increase by 1.1
           topFaces =
                    createTopFacesVariableHeight
                    (Point 10 0 0) --origin
                    [Radius primaryRad | r <- [1..]] 
                    [Angle a | a <- [0,5..360]]
                    [height | _ <- [1..]]
         in
         map (toBackTopLine . extractFrontTopLine ) topFaces
         
  pillarCenterAlignment <- buildCubePointsListSingle "pillarCenterAlignment"
       (centerAlignment pillarHeight)

  let topLeftRightLines innerPerims outerPerims =
        let
          recurProcessor :: Advancer -> Either String Advancer
          recurProcessor advancer = do
            advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
            advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
            legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter (advancer^.innerPerimetersBeforeExtraction)
            newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner legalizedAdvCPointFromOuter  advancer
            advancerRecur recurProcessor newAdvancer
         in
         extractAdvCPointsFromAdvancer $ 
          recurProcessor $
               Advancer
                 (Just $ [(extractB3 $ head pillarCenterAlignment) : (map (extractB2) innerPerims)])
                 (Just [innerPerims]) --innerPerims before extraction
                 (Just $ outerPerims) Nothing []  --outer perims
  
  pillarTopLeftRightLines <- buildCubePointsListSingle "pillarTopLeftRightLines"
   (topLeftRightLines pillarCenterAlignment pillarTopFronts)

  
  --only used to be converted into btm faces, as the composable joiner missing pattern matches for btm faces
  pillarTopFaces <- buildCubePointsListSingle "pillarTopFaces"
              ((head pillarTopLeftRightLines) +++> (tail pillarTopLeftRightLines))

  pillarBtmFaces <- buildCubePointsListSingle "topFaces"
              (map ((transposeZ (\z -> z - pillarHeight) ) . toBottomFace) pillarTopFaces)

  germanTopFronts <- buildCubePointsListSingle "germanTopFronts"
             (topFronts (germanHeight ) germanCentralAhr)
  
  germanTopLeftRightLines <- buildCubePointsListSingle "germanTopLeftRightLines"
   (topLeftRightLines pillarCenterAlignment germanTopFronts)

  
  germanTopFaces <- buildCubePointsListSingle "germanTopFaces"
              ((head germanTopLeftRightLines) +++> (tail germanTopLeftRightLines))

  


  cubes <- buildCubePointsListWithAdd "cubes"
           (pillarBtmFaces)
           (germanTopFaces) --need to be replaced with german top faces
  
  return pillarTopLeftRightLines


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
------------------------------------------------------------------ German -----------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------      

germanTreadScanLayer = "bottom"
germanDatabaseName = "src/Examples/ShoeLift/GermanHikers/lineScanner.db"
germanAlignmentOrigin = Point 15 0 0

germanCenterAngles ang = ((ang >= 32.94) && (ang <= 138.25)) 
                         ||
                         ((ang >= 212.47) && (ang <= 332.16))

--choose which builder to run to keep run/show builder dry
currentGermanBuilder = topGermanTreadBuilder
                 --germanCenterRiserWithBottomOffsetBuilder
                 --centerRiserBuilder
                 --flatRectangularTestCubeForCarbonFlexTestBuilder
                 --btmGermanTreadBuilder
                 --buildGermanTestPerimeterForFullScan
                 --buildTestPillarToTryHose
                 --topGermanTreadBuilder
                 --secondAdvCpt

advCPointFromClosestInnerOuterUsedCPoint = advCPointFromClosestInnerOuterUsedCPointBase

runGermanTreadScanBuilder :: IO () 
runGermanTreadScanBuilder = runSqlite germanDatabaseName $ do
  layerId <- getBy $ nameUnique' germanTreadScanLayer
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  --angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId), (angle angleHeightRadius') <=. 50] []
    -- !work. Need to fix persistable to properly use make lenses.
  
  case layerId of
    Nothing -> liftIO $ putStrLn "tread scan layer was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = currentGermanBuilder ( extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ putStrLn "output from Builder was good"
          liftIO $ writeStlToFile $ newStlShape "boot tread meets full lenght contoured riser" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] cpoints)
          liftIO $ putStrLn "stl should have been output"


showGermanTreadScanBuilderValue :: IO () 
showGermanTreadScanBuilderValue = runSqlite germanDatabaseName $ do
  layerId <- getBy $ nameUnique' germanTreadScanLayer
  angleHeightRadiusEntity <- selectList [ angleHeightRadiusLayerId' ==. (extractLayerId layerId)] []
  
  case layerId of
    Nothing -> liftIO $ putStrLn "tread scan layer was not found"
    
    (Just (Entity key layerVal)) -> do
      liftIO $ putStrLn "tread scan layer was found"
      let builder = currentGermanBuilder ( extractAnglesHeightsRadiiFromEntity angleHeightRadiusEntity) (extractOrigin layerVal)
          valCpoints = ((evalState $ runExceptT builder) [])
          cpoints = ((execState $ runExceptT builder) [])
      case valCpoints of
        (Left e) -> liftIO $ print $ e
        (Right a) -> do
          liftIO $ print $ show a


--used in conjuction with the offset version.
--
centerRiserBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
centerRiserBuilder ahr origin = do
  let
    centralAhr = filter (\(AngleHeightRadius ang _ _ _) -> germanCenterAngles ang) ahr
    layerHeight = 50 --100
    
    
  topFronts <- buildCubePointsListSingle "topHealLeadFronts"
             (let
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii centralAhr)
                    --(zipWith (\(Radius r) adjuster -> Radius $ r + adjuster ) (extractRadii ahr) [1 | val <- [1..]])
                      --this would allow change Radii outside the db.
                      --Could use if changes would not apply to all builders using the db.
                    (extractAngles centralAhr)
                    --(extractHeights ahr)
                    [layerHeight | val <- [1..]]
              in
              --(extractF3 $ head topFaces) : (map (extractF2) topFaces)
              (extractF3 $ head topFaces) : (((map (extractF2) topFaces)) ++ [(toF2 . extractF3 $ head topFaces)]   )
              --need to close the backend by putting the front point onto the end of list
             )
  
  centerAlignmentPillar <- buildCubePointsListSingle "centerAlignmentPillar"
       (
         let
           backExtLength = 40
           primaryRad = 21
             --19.9 printed as 38.5 mm. Need 41 so increase by 1.1
           topFaces =
                    createTopFacesVariableHeight
                    germanAlignmentOrigin --(Point 10 0 0) --origin
                    [Radius primaryRad | r <- [1..]] 
                    [Angle a | a <- [0,5..360]]
                    [layerHeight | height <- [1..]]
         in
         map (toBackTopLine . extractFrontTopLine ) topFaces
         
       )

  
  topLeftRightLines <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String Advancer
     recurProcessor advancer = do
       advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter (advancer^.innerPerimetersBeforeExtraction)
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner legalizedAdvCPointFromOuter  advancer
       advancerRecur recurProcessor newAdvancer
    in
    extractAdvCPointsFromAdvancer $ 
     recurProcessor $
          Advancer
            (Just $ [(extractB3 $ head centerAlignmentPillar) : (map (extractB2) centerAlignmentPillar)])
            (Just [centerAlignmentPillar]) --innerPerims before extraction
            (Just $ topFronts) Nothing []  --outer perims
   )

  topFaces <- buildCubePointsListSingle "topFaces"
              ((head topLeftRightLines) +++> (tail topLeftRightLines))

  btmFaces <- buildCubePointsListSingle "topFaces"
              (map ((transposeZ (\z -> z - layerHeight) ) . toBottomFace) topFaces)

  cubes <- buildCubePointsListWithAdd "cubes"
           (btmFaces)
           (topFaces) 
  
  return centerAlignmentPillar

--make a riser to convert from pillars to german centers
germanCenterRiserWithBottomOffsetBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
germanCenterRiserWithBottomOffsetBuilder ahr origin = do
  let
    
    germanHeight = 10
    germanOrigin = Point 10 0 0
    centralAhr = filter (\(AngleHeightRadius ang _ _ _) -> germanCenterAngles ang) ahr

  let topFronts :: Double -> AnglesHeightsRadii -> Point -> [CornerPoints]
      topFronts height ahr origin =
              let
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii ahr)
                    (extractAngles ahr)
                    [height | val <- [1..]]
              in
              (extractF3 $ head topFaces) : (((map (extractF2) topFaces)) ++ [(toF2 . extractF3 $ head topFaces)]   )
              --need to close the backend by putting the front point onto the end of list

     
  
  

  let
    centerAlignment height origin =
         let
           primaryRad = 21
             --19.9 printed as 38.5 mm. Need 41 so increase by 1.1
           topFaces =
                    createTopFacesVariableHeight
                    origin --(Point 10 0 0) -
                    [Radius primaryRad | r <- [1..]] 
                    [Angle a | a <- [0,5..360]]
                    [height | _ <- [1..]]
         in
         map (toBackTopLine . extractFrontTopLine ) topFaces
        
  pillarCenterAlignment <- buildCubePointsListSingle "pillarCenterAlignment"
       (centerAlignment 0 $ transposeX (+ (-9.5)) germanAlignmentOrigin)

  germanCenterAlignment <- buildCubePointsListSingle "pillarCenterAlignment"
       (centerAlignment 0 germanAlignmentOrigin)

  pillarTopFronts <- buildCubePointsListSingle "topHealLeadFronts"
             (topFronts 0 centralAhr origin)
  
  germanTopFronts <- buildCubePointsListSingle "germanTopFronts"
             (topFronts 0 centralAhr origin)

  let topLeftRightLines innerPerims outerPerims =
        let
          recurProcessor :: Advancer -> Either String Advancer
          recurProcessor advancer = do
            advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
            advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
            legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter (advancer^.innerPerimetersBeforeExtraction)
            newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner legalizedAdvCPointFromOuter  advancer
            advancerRecur recurProcessor newAdvancer
         in
         extractAdvCPointsFromAdvancer $ 
          recurProcessor $
               Advancer
                 (Just $ [(extractB3 $ head innerPerims) : (map (extractB2) innerPerims)])
                 (Just [innerPerims]) --innerPerims before extraction
                 (Just $ outerPerims) Nothing []  --outer perims
  
  pillarTopLeftRightLines <- buildCubePointsListSingle "pillarTopLeftRightLines"
   (topLeftRightLines pillarCenterAlignment pillarTopFronts)

  germanTopLeftRightLines <- buildCubePointsListSingle "germanTopLeftRightLines"
   (topLeftRightLines germanCenterAlignment germanTopFronts)

  germanTopFaces <- buildCubePointsListSingle "germanTopFaces"
              ((head germanTopLeftRightLines) +++> (tail germanTopLeftRightLines))
  
  --only used to be converted into btm faces, as the composable joiner missing pattern matches for btm faces
  pillarTopFaces <- buildCubePointsListSingle "pillarTopFaces"
              ((head pillarTopLeftRightLines) +++> (tail pillarTopLeftRightLines))

  pillarBtmFaces <- buildCubePointsListSingle "topFaces"
              (map ((transposeZ (\z -> z - 50) ) . toBottomFace) pillarTopFaces)

  
  

  
  
  


  cubes <- buildCubePointsListWithAdd "cubes"
           (pillarBtmFaces)
           --(map ((transposeZ (+ (-10))) . toBottomFace) germanTopFaces)
           (germanTopFaces) --need to be replaced with german top faces
  
  return germanTopFaces


{-
Try a flat carbon fiber, to see if it flexes.
For now just use a rectangular piece.
-}
flatRectangularTestCubeForCarbonFlexTestBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
flatRectangularTestCubeForCarbonFlexTestBuilder _ _ = do
  backBtmLine <- buildCubePointsListSingle "cube"
          ([BackBottomLine {b1=Point 0 0 0, b4=Point 50 0 0}])
  
  btmFace <- buildCubePointsListWithAdd "btmFace"
             (backBtmLine)
             (map (transposeY (+ 200) . toBottomFrontLine ) backBtmLine)
  
  cube    <- buildCubePointsListWithAdd "cube"
             (btmFace)
             (map ( (transposeZ (+10))  .  toTopFace             )  btmFace)
  return cube

{-
Reuse topGermanTreadBuilder by building topFaces, then convert to bottomFaces and transpose up.

Would have to print with semi-flex and perhaps a spring.

Wait till I test out a carbon flat bottom.
-}
btmGermanTreadBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
btmGermanTreadBuilder ahr origin = do
  let
    layerHeight = 10
    
  topFronts <- buildCubePointsListSingle "topHealLeadFronts"
             (let
                  topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii ahr)
                    --(zipWith (\(Radius r) adjuster -> Radius $ r + adjuster ) (extractRadii ahr) [1 | val <- [1..]])
                      --this would allow change Radii outside the db.
                      --Could use if changes would not apply to all builders using the db.
                    (extractAngles ahr)
                    --(extractHeights ahr)
                    [layerHeight | val <- [1..]]
              in
              (extractF3 $ head topFaces) : (map (extractF2) topFaces)
             )
  
  centerAlignmentPillar <- buildCubePointsListSingle "centerAlignmentPillar"
       (
         let
           backExtLength = 40
           primaryRad = 21
             --19.9 printed as 38.5 mm. Need 41 so increase by 1.1
           topFaces =
                    createTopFacesVariableHeight
                    (Point 10 0 0) --origin
                    [Radius primaryRad | r <- [1..]] 
                    [Angle a | a <- [0,5..360]]
                    [layerHeight | height <- [1..]]
         in
         map (toBackTopLine . extractFrontTopLine ) topFaces
         
       )

  
  topLeftRightLines <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String Advancer
     recurProcessor advancer = do
       advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter (advancer^.innerPerimetersBeforeExtraction)
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner legalizedAdvCPointFromOuter  advancer
       advancerRecur recurProcessor newAdvancer
    in
    extractAdvCPointsFromAdvancer $ 
     recurProcessor $
          Advancer
            (Just $ [(extractB3 $ head centerAlignmentPillar) : (map (extractB2) centerAlignmentPillar)])
            (Just [centerAlignmentPillar]) --innerPerims before extraction
            (Just $ topFronts) Nothing []  --outer perims
   )

  topFaces <- buildCubePointsListSingle "topFaces"
              ((head topLeftRightLines) +++> (tail topLeftRightLines))

  btmFaces <- buildCubePointsListSingle "topFaces"
              (map ((transposeZ (\z -> z - layerHeight) ) . toBottomFace) topFaces)

  cubes <- buildCubePointsListWithAdd "cubes"
           (btmFaces)
           (topFaces) 
  
  return centerAlignmentPillar


--germanAlignmentOrigin
topGermanTreadBuilder :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
topGermanTreadBuilder ahr origin = do
  topFronts <- buildCubePointsListSingle "topFronts"
             (let topFaces =
                    createTopFacesVariableHeight
                    origin
                    (extractRadii ahr)
                    --(zipWith (\(Radius r) adjuster -> Radius $ r + adjuster ) (extractRadii ahr) [1 | val <- [1..]])
                      --this would allow change Radii outside the db.
                      --Could use if changes would not apply to all builders using the db.
                    (extractAngles ahr)
                    (extractHeights ahr)
                    
              in
              (extractF3 $ head topFaces) : (map (extractF2) topFaces)
             )
  
  alignmentPillar <- buildCubePointsListSingle "alignmentPillar"
       (
         let
           backExtLength = 40
           primaryRad = 21
             --19.9 printed as 38.5 mm. Need 41 so increase by 1.1
           backHeight = 15
           topFaces =
                    createTopFacesVariableHeight
                    germanAlignmentOrigin --(Point 10 0 0) --origin
                    --Variable lengths. Block out till find out why values < 20 !work
                    ([Radius backExtLength | r <- [1..2]]
                     ++
                     [Radius primaryRad | r <- [1..33]]
                     ++
                     [Radius 80 | r <- [1..3]]
                     ++
                     [Radius primaryRad | r <- [1..32]]
                     ++
                     [Radius backExtLength | r <- [1..]]
                    )
                    --[Radius 21.35 | r <- [1..]] --fails soon as drops below 19.94
                                             --25 works, but can see that it is using illegal outer perims.
                                               --Looks like it is a height issue as it happens around center.
                                               --Need to make/use equalsXY instead of == which uses xyz
                    [Angle a | a <- [0,5..360]]
                    --[20 | height <- [1..]]
                    {- !work: Maybe the variable height causes illegal intersection as it is using xyz instead of only xy.-}
                    ([backHeight | height <- [1..2]] ++ --heel
                     [25 | height <- [1..26]] ++ --beside alignment pillar
                     [20 | height <- [1..7]] ++ --forward of alignment pillar
                     [22 | height <- [1,2,3]] ++ --toe
                     [20 | height <- [1..6]] ++ --forward of alignment pillar
                     [25 | height <- [1..23]] ++ --beside alignment pillar
                     [backHeight | height <- [1..]] --heel
                    )
         in
         map (toBackTopLine . extractFrontTopLine ) topFaces
         
       )

  topLeftRightLines <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String Advancer
     recurProcessor advancer = do
       advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       legalizedAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections advCPointFromOuter (advancer^.innerPerimetersBeforeExtraction)
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner legalizedAdvCPointFromOuter  advancer
       advancerRecur recurProcessor newAdvancer
    in
    extractAdvCPointsFromAdvancer $ 
     recurProcessor $
          Advancer
            (Just $ [(extractB3 $ head alignmentPillar) : (map (extractB2) alignmentPillar)])
            (Just [alignmentPillar])
            (Just topFronts) Nothing []
   )

  topFaces <- buildCubePointsListSingle "topFaces"
              ((head topLeftRightLines) +++> (tail topLeftRightLines))

  cubes <- buildCubePointsListWithAdd "cubes"
           (topFaces)
           (map (toBottomFace . (transposeZ (\z -> 3) )) topFaces) 
  
  return alignmentPillar


buildTestPillarToTryHose :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
buildTestPillarToTryHose _ _ = do
  cylinder <- buildCubePointsListSingle "cylinder"
              (cylinder [Radius 21 | r <- [1..]] [Radius 23 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 10 0 10) 10)

  return cylinder

--remeber to set infill to 100%
buildGermanTestPerimeterForFullScan :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
buildGermanTestPerimeterForFullScan ahr origin = do
  let origin' = Point 0 0 14
      angles = map (rotateAngle 45) (extractAngles ahr)
      radii  = (extractRadii ahr)
      heights = extractHeights ahr
  outerFaces <- buildCubePointsListSingle "outerFaces"
             (let topFaces =
                    createTopFacesVariableHeight
                    origin'
                    radii
                    --(extractAngles ahr)
                    angles
                    --(extractHeights ahr)
                    heights
                  --btmFaces =
                    --map ((transposeZ (\_ -> (12))) . toBottomFace) topFaces
              in
              --topFaces |+++| btmFaces
              (extractF3 $ head topFaces) +++> (map (extractF2) topFaces)
             )

  innerFaces <- buildCubePointsListSingle "innerFaces"
             (let topFaces =
                    createTopFacesVariableHeight
                    origin'
                    --(extractRadii ahr)
                    (map (transpose (+(-1))) radii )
                      --printed using -3, was wider than needed for testing.
                    --(extractAngles ahr)
                    angles
                    --(extractHeights ahr)
                    heights
                  
              in
              map (toBackTopLine) ((extractF3 $ head topFaces) +++> (map (extractF2) topFaces))
             )

  cubes <- buildCubePointsListSingle "cubes"
           (let
               topPerimeter = outerFaces |+++| innerFaces
               btmPerimeter = map ((transposeZ (\_ -> (12))) . toBottomFace) topPerimeter
               --give it a flat bottom for easy printing
            in
              topPerimeter |+++| btmPerimeter
           )
            
  return cubes
  
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--For initial advCPoint, naiveAdvCpointFromInnerPerims should not produce an advCPt, so return a [CornerPointsNothing]
innerPerimemetersOutputOnInitAdvCPt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
innerPerimemetersOutputOnInitAdvCPt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  headOfInnerOuterPerimsFromWhichInitAdvCPointWillBeBuilt <- buildCubePointsListSingle "topLeftRightLines"
   (let
     recurProcessor :: Advancer -> Either String InnerAdvancerOutput
     recurProcessor advancer = do
       advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       return advCPointFromInner
    in
     case  
      recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing [] of
       Right (InnerAdvancerOutput (Just (i:innerP)) Nothing usedCPt advCPts) -> [CornerPointsNothing] --[head alignmentPillar, (head topFronts)]
       Left e -> [CornerPointsError e]
            
   )
  return headOfInnerOuterPerimsFromWhichInitAdvCPointWillBeBuilt


--For initial advCPoint, using naive builder for inner/outer output
outerAdvancerOutputOnInitAdvCPt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
outerAdvancerOutputOnInitAdvCPt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  outerPerimeterOutput <- buildCubePointsListSingle "outerPerimeterOutput"
   (let
     recurProcessor :: Advancer -> Either String OuterAdvancerOutput
     recurProcessor advancer = do
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       return advCPointFromOuter
    in
     case  
      recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing [] of
       Right (OuterAdvancerOutput _ Nothing _ _) -> [CornerPointsNothing] 
       Left e -> [CornerPointsError e]
            
   )
  return outerPerimeterOutput

--For initial advCPoint
initAdvCPt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
initAdvCPt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  initAdvCPt <- buildCubePointsListSingle "outerPerimeterOutput"
    (let
      recurProcessor :: Advancer -> Either String Advancer
      recurProcessor advancer = do
       advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       --advCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection advancer
         --no need for this yet, as it will not create an adv cpoint.
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       newAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint advCPointFromInner advCPointFromOuter  advancer
       return newAdvancer
     in
      case  
       recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [map extractBackTopLine cylinder'])
            (Just topFronts) Nothing [] of
        Right (Advancer _ _ _ (Just advCPt) _) -> [advCPt] 
        Left e -> [CornerPointsError e]
            
   )
  return initAdvCPt


--The next advCPoint after the initial advCPoint which is built using innerPerims:
{-
output with inner cx legal. Did not build from inner as it should have. NFGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
 -Need to test why it was illegal, because it should not have been. NFGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
[TopLeftLine {b2 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 10.0}, f2 = Point {x_axis = 4.607827919499482, y_axis = -113.73482300016958, z_axis = 18.75}}]
output with naive inner. This is the output that legal should have had.
[TopLeftLine {b2 = Point {x_axis = 1.7431148549531632, y_axis = -19.92389396183491, z_axis = 10.0}, f2 = Point {x_axis = 0.0, y_axis = -113.828125, z_axis = 18.75}}]


-}
secondAdvCpt :: AnglesHeightsRadii -> Point -> ExceptStackCornerPointsBuilder
secondAdvCpt ahr origin = do
  
  let
    topFronts = topFronts' ahr origin
  secondAdvCPt <- buildCubePointsListSingle "outerPerimeterOutput"
    (let
      recurProcessor :: Advancer -> Either String Advancer
      recurProcessor advancer = do
       noAdvCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       noAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       initAdvCPtAdvancer       <- advCPointFromClosestInnerOuterUsedCPoint noAdvCPointFromInner noAdvCPointFromOuter  advancer
       --secondAdvCPointFromInner <- createAdvCPointFromInnerPerimsCheckLegalIntersection initAdvCPtAdvancer
         --yeilds nothing, so prob's with intersection
       secondAdvCPointFromInner <- naiveAdvCpointFromInnerPerims initAdvCPtAdvancer 
       legalizedSecondAdvCPointFromInner <- checkInnerAdvCPtForLegality secondAdvCPointFromInner (advancer^.innerPerimetersBeforeExtraction)
       secondAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims initAdvCPtAdvancer
       legalizedSecondAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections secondAdvCPointFromOuter (advancer^.innerPerimetersBeforeExtraction)
       --
       --advancerWithSecondAdvCPt <- advCPointFromClosestInnerOuterUsedCPoint secondAdvCPointFromInner secondAdvCPointFromOuter initAdvCPtAdvancer
       advancerWithSecondAdvCPt <- advCPointFromClosestInnerOuterUsedCPoint legalizedSecondAdvCPointFromInner legalizedSecondAdvCPointFromOuter initAdvCPtAdvancer
       return advancerWithSecondAdvCPt
     in
      case  
       recurProcessor $
          Advancer
            (Just [alignmentPillar])
            (Just [take 34 $ map extractBackTopLine cylinder']) --2nd advCPt from innerP's fails past 33
            (Just topFronts) Nothing [] of
        Right (Advancer _ _ _ (Just advCPt) _) -> [advCPt] 
        Left e -> [CornerPointsError e]
            
   )
  return secondAdvCPt


----------------------------------------------------- support test values-----------------------------------------------------------

cylinder' = cylinder [Radius 20 | r <- [1..]] [Radius 20 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 10 0 10) 10
alignmentPillar =
  (extractB3 $ head cylinder') : (map (extractB2) cylinder')


--these would normally be built in a Builder.
topFronts' :: AnglesHeightsRadii -> Point -> [CornerPoints]
topFronts' ahr origin =
   let topFaces =
           createTopFacesVariableHeight
           origin
           (extractRadii ahr)
           (extractAngles ahr)
           (extractHeights ahr)
   in
   (extractF3 $ head topFaces) : (map (extractF2) topFaces)
              
  
             
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------testing -------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
