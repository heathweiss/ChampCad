{- |
For a HorizontalFaces:

Extract the [<Bottom/Top>Face]
Divide it symmetrically along the y-axis by filtering x_axis for < or > central value
-now have 2 possibly even length [<Bottom/Top>Face]

Create list of y_axis values with desired spacing, does not need to be even.
Should extend off both ends of the shape.




-}

module Joiners.RadialLines(getMinY, getMaxY, extractYaxis, createListOfYaxisValuesToMakeCubesOnFromRadialShapeAsTopFrontPoints) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), cpointType, (+++), (+++>), (#+++#))
import CornerPoints.HorizontalFaces(createTopFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceExtraction(extractFrontTopLine,  extractBottomFrontLine, extractF3, extractF2,)
import CornerPoints.FaceConversions(toF3)

import Data.List(find)

import Helpers.Applicative(extractE, removeMaybe, appendE)

radialLinesTestDo = do
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "RadialLinesTest"
  --runTestTT seeRadialShapeAsTopFrontPointsTest
  runTestTT seeLeadingRadialShapeAsTopFrontPointsTest
  runTestTT seeTrailingRadialShapeAsTopFrontPointsTest
  runTestTT seeMinYCpointOfLeadingRadialShapeAsTopFrontPointsTest
  runTestTT seeMinYCpointOfTrailingRadialShapeAsTopFrontPointsTest
  runTestTT seeMaxYCpointOfLeadingRadialShapeAsTopFrontPointsTest
  runTestTT seeMaxYCpointOfTrailingRadialShapeAsTopFrontPointsTest
  runTestTT generateYaxisListOfCubesToCreateTest
  --getLeadingCPoint
  runTestTT getLeadingCPointExistsTest
  runTestTT getLeadingCPointExistsTest2
  runTestTT getLeadingCPointExistsTest3
  runTestTT getLeadingCPointExistsTest4
  runTestTT getLeadingCPointExistsTest5
  runTestTT getLeadingCPointExistsTest6
  runTestTT getLeadingCPointExistsTest7
  --getTrailingCPoint
  runTestTT getTrailingCPointExistsTest
  runTestTT getTrailingCPointExistsTest2
  runTestTT getTrailingCPointExistsTest3
  runTestTT getTrailingCPointExistsTest4
  runTestTT getTrailingCPointExistsTest5
  runTestTT getTrailingCPointExistsTest6
  runTestTT getTrailingCPointExistsTest7

  --find the center point
  
  runTestTT leadingRatioFromDoubleTest
  runTestTT leadingRatioFromDoubleTest2
  runTestTT leadingRatioFromDoubleTest3
  runTestTT leadingRatioFromCPointsTest
  runTestTT leadingRatioFromCPointsTest2
  runTestTT leadingRatioFromCPointsTest3
  ---------------------------
  runTestTT adjustAxisTest
  runTestTT adjustAxisTest2
  runTestTT adjustAxisTest3
  runTestTT adjustAxisTest4
  runTestTT adjustCornerPointsTest
  runTestTT adjustCornerPointsTest2

  -------------- create the final topFaces grid ------------------------------------
  runTestTT buildGridTopFacesTest
  ----------------------- removed items I should not need--------------------
  --runTestTT containsYaxisValueNotTest
  --runTestTT containsYaxisValueTrueTest
  --runTestTT containsCpointWithYaxisValueLTTargetValFalseTest
  --runTestTT containsCpointWithYaxisValueLTTargetValTrueTest
  --runTestTT getLargestCpointWithLTEYvalExists
  --runTestTT getLargestCpointWithLTEYvalNotExists
  --runTestTT getLargestCpointWithLTEYvalExactlyExists
  --runTestTT getMatchingCPointExistTest
  --runTestTT getMatchingCPointNoExistTest
  --runTestTT getMatchingCPointUnhandledTest
  
radialShapeAsTopFrontPoints =
  let
    circle = createTopFacesVariableHeight
                    (Point 0 0 0)
                    [Radius r | r <- [10,10..]]
                    ([Angle a | a <- [10,20..350]]  ++ [Angle 355])
                    [10,10..]
    frontTopLines = map (extractFrontTopLine) circle
  in (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines

--can go into RadialLines
splitOnXaxis :: (Double -> Double -> Bool) -> Double -> CornerPoints -> Bool
splitOnXaxis tester splitterVal (F2 (Point x _ _))  = 
  tester x splitterVal 
splitOnXaxis tester splitterVal (F3 (Point x _ _))  = 
  tester x splitterVal 

  
leadingRadialShapeAsTopFrontPoints = filter (splitOnXaxis (>) 0) radialShapeAsTopFrontPoints

trailingRadialShapeAsTopFrontPoints =
  let list = filter (splitOnXaxis (<) 0) radialShapeAsTopFrontPoints
  in
    (toF3 $ head list ) : (tail list)
--can be moved to RadialLines, or somewhere
getMinY :: [CornerPoints] -> CornerPoints
getMinY ((F2 (Point x y z)):cpoints) =
  getMinMaxY'Base (<=) y (F2 (Point x y z)) cpoints
  
getMinY ((F3 (Point x y z)):cpoints) =
  getMinMaxY'Base (<=) y (F3 (Point x y z)) cpoints

getMinY (c:cpoints) =
  CornerPointsError "Illegal or unhandled CornerPoints constructor passed into getMinY"

getMinY [] = CornerPointsError "Empty [CornerPoints] passed into getMinY"

getMaxY :: [CornerPoints] -> CornerPoints
getMaxY ((F2 (Point x y z)):cpoints) =
  getMinMaxY'Base (>=) y (F2 (Point x y z)) cpoints

getMaxY ((F3 (Point x y z)):cpoints) =
  getMinMaxY'Base (>=) y (F3 (Point x y z)) cpoints

getMaxY (c:cpoints) =
  CornerPointsError "Illegal or unhandled CornerPoints constructor passed into getMaxY"

getMaxY [] = CornerPointsError "Empty [CornerPoints] passed into getMaxY"

  
getMinMaxY'Base :: (Double -> Double -> Bool) -> Double -> CornerPoints -> [CornerPoints] -> CornerPoints
getMinMaxY'Base comparator currVal currCpoint ((F2 (Point x y z)):cpoints) =
  case comparator currVal y of
    True -> getMinMaxY'Base comparator  currVal currCpoint cpoints
    False -> getMinMaxY'Base comparator  y (F2 (Point x y z)) cpoints
getMinMaxY'Base comparator currVal currCpoint ((F3 (Point x y z)):cpoints) =
  case comparator currVal y of
    True -> getMinMaxY'Base comparator  currVal currCpoint cpoints
    False -> getMinMaxY'Base comparator  y (F3 (Point x y z)) cpoints
getMinMaxY'Base _ _ _ (c:cpoints) = CornerPointsError $ "Illegal or unhandled CornerPoints constructor passed into getMinMaxY'Base for: " ++ (show c)
getMinMaxY'Base _ _ currCpoint [] = currCpoint

extractYaxis :: CornerPoints -> Double
extractYaxis (F2 (Point _ y _)) = y
extractYaxis (F3 (Point _ y _)) = y

{- |
Create a [Double] extending from minY to maxY, at increments of 1.

get the min/max values to create the range of values.
Start the [double] at minY
-increment up to next event double and run list to max y
-
-}
createListOfYaxisValuesToMakeCubesOnFromRadialShapeAsTopFrontPoints :: [CornerPoints] -> [Double]
createListOfYaxisValuesToMakeCubesOnFromRadialShapeAsTopFrontPoints radialShapeAsTopFrontPoints =
  let
    leadingFrontTopPoints  = filter (splitOnXaxis (>) 0) radialShapeAsTopFrontPoints
    trailingFrontTopPoints = filter (splitOnXaxis (<) 0) radialShapeAsTopFrontPoints
    
    leadingMinY  = extractYaxis $ getMinY leadingFrontTopPoints
    leadingMaxY = extractYaxis $ getMaxY leadingFrontTopPoints
    trailingMinY = extractYaxis $ getMinY trailingFrontTopPoints
    trailingMaxY = extractYaxis $ getMaxY trailingFrontTopPoints
    minY =
      case leadingMinY < trailingMinY of
        True -> leadingMinY
        False -> trailingMinY
    maxY =
      case leadingMaxY > trailingMaxY of
        True -> leadingMaxY
        False -> trailingMaxY

    
  in
    case minY == (fromIntegral $ floor minY) of
      True ->
        case maxY == (fromIntegral $ floor maxY) of
          True -> [minY, (minY + 1)..maxY]
          False -> [minY, (minY + 1)..(fromIntegral $ floor maxY)] ++ [maxY]
      False ->
        case maxY == (fromIntegral $ floor maxY) of
          True -> minY : [(fromIntegral $ ceiling minY), ((fromIntegral $ ceiling minY) + 1)..maxY]
          False -> minY : [(fromIntegral $ ceiling minY), ((fromIntegral $ ceiling minY) + 1)..(fromIntegral $ floor maxY)] ++ [maxY]
  





-----------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------get<Leading/Trailing>CPoint-----------------------------------------------------------------

--get the Maybe cpoint with greatest y-axis val that is <= target val
--Left if unhandled cpoint constructor
--Nothing if no CPoint y_axis is <= target value. Should not happen if [targetVal] is properly made, but will it be?
getLeadingCPoint :: Double -> [CornerPoints] -> Either String (Maybe CornerPoints)
getLeadingCPoint _  [] = Left "getLeadingCPoint: empty [CornerPoints] passed in"
getLeadingCPoint targetVal  ((F2 (Point x y z)) : cpoints) =
  case  targetVal <= y of
    True -> getLeadingCPoint' targetVal (Just $ F2 $ Point x y z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
getLeadingCPoint targetVal  ((F3 (Point x y z)) : cpoints) =
  case y <= targetVal of
    True -> getLeadingCPoint' targetVal (Just $ F3 $ Point x y z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
getLeadingCPoint targetVal  (unhandledCPoint : cpoints) =  Left $ "getLeadingCPoint: unhandled or illegal CornerPoints: " ++ (show unhandledCPoint)
--recur for getLeadingCPoint
getLeadingCPoint' :: Double -> (Maybe CornerPoints) -> [CornerPoints] -> Either String (Maybe CornerPoints)
getLeadingCPoint' _ prevCPoint []  = Right prevCPoint
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y <= targetVal of
    True ->
      Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right Nothing
----------------------------------------------------------------------------------------------------------------------------------------
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : cpoints) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : cpoints) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z)) : cpoints) =
  case current_y <= targetVal of
    True ->
      getLeadingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
-----------------------------------------------------------------------------------------------------------------------------------------
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : cpoints) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : cpoints) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z)) : cpoints) =
  case current_y <= targetVal of
    True ->
      getLeadingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
----------------------------------------------------------------------------------------------------------------------------------------------
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y <= targetVal of
    True ->
      case current_y <= previous_y of
        True -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y <= targetVal of
    True ->
      Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right Nothing

--------------------------------------------------------------------------------------------------------------------------------------------------------
getLeadingCPoint' _ _  (unhandled : []) =
  Left $ "getLeadingCPoint': unhandled or illegal CornerPoints: " ++ (show unhandled)

getLeadingCPoint' _ _  (unhandled : cpoints) =
  Left $ "getLeadingCPoint': unhandled or illegal CornerPoints: " ++ (show unhandled)


-- | Get the CornerPoints that has the next largest y_axis value.   
getTrailingCPoint :: Double -> [CornerPoints] -> Either String (Maybe CornerPoints)
getTrailingCPoint _  [] = Left "getTrailingCPoint: empty [CornerPoints] passed in"
getTrailingCPoint targetVal  ((F2 (Point x y z)) : cpoints) =
  case  y >= targetVal of
    True -> getTrailingCPoint' targetVal (Just $ F2 (Point x y z)) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints
getTrailingCPoint targetVal  ((F3 (Point x y z)) : cpoints) =
  case y >= targetVal of
    True -> getTrailingCPoint' targetVal (Just $ F3 (Point x y z)) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints
getTrailingCPoint targetVal  (unhandledCPoint : cpoints) =  Left $ "getTrailingCPoint: unhandled or illegal CornerPoints: " ++ (show unhandledCPoint)
--recur for getTrailingCPoint
getTrailingCPoint' :: Double -> (Maybe CornerPoints) -> [CornerPoints] -> Either String (Maybe CornerPoints)
getTrailingCPoint' _ prevValidCPoint []  = Right prevValidCPoint
getTrailingCPoint' targetVal (Just ( F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> Right $ (Just $ F2 (Point previous_x previous_y previous_z))  
        False -> Right $ Just (F2 (Point current_x current_y current_z))
    False -> Right $ Just (F2 (Point previous_x previous_y previous_z))
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> Right $ (Just $ F3 (Point previous_x previous_y previous_z))  
        False -> Right $ Just (F2 (Point current_x current_y current_z))
    False -> Right $ Just (F3 (Point previous_x previous_y previous_z))
getTrailingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y >= targetVal of
    True ->
      Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right Nothing
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point current_x current_y current_z
getTrailingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point current_x current_y current_z
getTrailingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y >= targetVal of
    True ->
      Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right Nothing
getTrailingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z))  : cpoints) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z))  : cpoints) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z))  : cpoints) =
  case current_y >= targetVal of
    True ->
      getTrailingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints
getTrailingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z))  : cpoints) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z))  : cpoints) =
  case current_y >= targetVal of
    True ->
      case current_y >= previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z))  : cpoints) =
  case current_y >= targetVal of
    True ->
      getTrailingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints

{- |
Given:
<leading/trailing>Constructor :: (Point -> CornerPoints)
-Constructors used to build the B3 and B2 CornerPoints that will make up the initial BackTopLine
-So far only B3 and B2 are used to build a BackTopLine as can only create a [TopFace] so far.

grid :: [Double]
-The y_axis values that make up the grid lines to be produced.

((F3 (Point leadingX leadingY leadingZ) : leadingCPoints)) :: [CornerPoints]
-Is the leading side of the scan. Should be < 180 degrees.
-Must be a F3 : [F2] as only [TopFace] can be built so far

((F3 (Point trailingX trailingY leadingZ) : trailingCPoints)) :: [CornerPoints]
-Is the trailing side of the scan. Should be > 180 degrees.
-Must be a F3 : [F2] as only [TopFace] can be built so far.

Return:
Either String [CornerPoints]
The [CornerPoints] as CubePoints which have been build along the grid lines.
-}
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase :: 
                                                       [Double] ->
                                                       [CornerPoints] -> --leading CPoints
                                                       [CornerPoints] -> --trailing CPoints
                                                       Either String [CornerPoints]    --final CPoints
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase 
                                                        (g:grid)
                                                        ((F3 (Point leadingX leadingY leadingZ) : leadingCPoints))
                                                        ((F3 (Point trailingX trailingY trailingZ) : trailingCPoints))
                                                     =
    extractE $
    buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
      (F3) (F2) grid
      ((F3 (Point leadingX leadingY leadingZ) : leadingCPoints))
      ((F3 (Point trailingX trailingY trailingZ) : trailingCPoints)) <$>
      (Right $ B3 $ Point leadingX leadingY leadingZ) #+++# (Right $ B2 $ Point trailingX trailingY trailingZ) `appendE` []
      
      {-
      ({-Just-} (leadingConstructor $ Point leadingX leadingY leadingZ))
      ({-Just-} (trailingConstructor $ Point trailingX trailingY trailingZ))-}
    {-
    buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
      (F3) (F2) grid
      ((F3 (Point leadingX leadingY leadingZ) : leadingCPoints))
      ((F3 (Point trailingX trailingY trailingZ) : trailingCPoints))
      []
      ({-Just-} (leadingConstructor $ Point leadingX leadingY leadingZ))
      ({-Just-} (trailingConstructor $ Point trailingX trailingY trailingZ))
-}
      
{-
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase (leadingConstructor)
                                                        (trailingConstructor)
                                                        (g:grid)
                                                        ((F3 (Point leadingX leadingY leadingZ) : leadingCPoints))
                                                        ((F3 (Point trailingX trailingY trailingZ) : trailingCPoints))
                                                     =
  
    buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
      (F3) (F2) grid
      ((F3 (Point leadingX leadingY leadingZ) : leadingCPoints))
      ((F3 (Point trailingX trailingY trailingZ) : trailingCPoints))
      (Right (Just (leadingConstructor $ Point leadingX leadingY leadingZ)))
      (Right (Just (trailingConstructor $ Point trailingX trailingY trailingZ)))
      []
-}
{-
Child recur function:
Given:
Same as parent +
workingCPoints :: [CornerPoints]
-The list of BackTopLine : [FrontTopLine]
-For efficiency, build it up backwards using : then reverse it when done.
-Use head +++> tail to create the [TopFace] from the reversed line.

<leading/trailing>AdjustedCPoint :: Maybe CornerPoints
-This is the lead/trailing/ left/right/ CPoints from which a grid line is built.
-They were created previosly, but will not be combined until here, so that pattern matching
 can be used here. Could/should? have done that internally. That would reduce params and the amount of pattern matches.
 In that case, should create a separate fx that does this, as pattern matching is required for combining them.
-}
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' :: (Point -> CornerPoints) ->
                                                        (Point -> CornerPoints) ->
                                                        [Double] ->
                                                        [CornerPoints] -> --leading CPoints
                                                        [CornerPoints] -> --trailing CPoints
                                                        [CornerPoints] -> --working list of final CPoints
                                                        Either String [CornerPoints]    --final CPoints
{-
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' :: (Point -> CornerPoints) ->
                                                        (Point -> CornerPoints) ->
                                                        [Double] ->
                                                        [CornerPoints] -> --leading CPoints
                                                        [CornerPoints] -> --trailing CPoints
                                                        [CornerPoints] -> --working list of final CPoints
                                                        {-Maybe-} CornerPoints -> --leading adjusted CPoint
                                                        {-Maybe-} CornerPoints -> --trailing adjusted CPoint
                                                        Either String [CornerPoints]    --final CPoints



buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' :: (Point -> CornerPoints) ->
                                                        (Point -> CornerPoints) ->
                                                        [Double] ->
                                                        [CornerPoints] -> --leading CPoints
                                                        [CornerPoints] -> --trailing CPoints
                                                        (Either String (Maybe CornerPoints)) -> --leading adjusted CPoint
                                                        (Either String (Maybe CornerPoints)) -> --trailing adjusted CPoint
                                                        [CornerPoints] -> --working list of final CPoints
                                                        Either String [CornerPoints]    --final CPoints
-}
--
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' (leadingConstructor)
                                                     (trailingConstructor)
                                                     (g:grid)
                                                     leadingCPoints
                                                     trailingCPoints
                                                     workingCPoints
                                                     =
  --leftOff -- Change <leading/trailing>CPoint to Maybe (remove Either) then use Applicative to if fx ends instead of pattern matching to see if fx ends.
          --Will also need to change the order of the params so workingCPoints is ahead, to work better with Applicative.
  let
    --For all the following:
    --Can be Either because unhandled/illegal CornerPoint in leading/trailing [CornerPoints]
    --Can be Maybe because of improper grid target values construction. There should be not grid value outside the range
    --of the <leading/trailing>CPoints y_axis. But should gaurd against it.
    leadingLeadingCPoint :: Either String (Maybe CornerPoints)
    leadingLeadingCPoint = getLeadingCPoint g leadingCPoints
    trailingLeadingCPoint = getTrailingCPoint g leadingCPoints
    leadingTrailingCPoint = getLeadingCPoint g trailingCPoints
    trailingTrailingCPoint = getTrailingCPoint g trailingCPoints

    --at this point, cx leadingLeadingCPoint(and the other 3) for Nothing.
    --If Nothing, make it a Left, otherwise remove the Maybe aspect as a Nothing makes no sense. It should be Left if this happens
    --The alternative is to got back and cx adjustCornerPoint to be a Either String CornerPoints , and remove the Maybe.
    --Which is easier. Could do this here 1st, change the pattern matches, then go back and cx adjustCornerPoint, as the
    --problem is going to be coordinating all the cascading changes if adjustCornerPoint if done first.


    --leadingAdjustedCPoint = extractE $ adjustCornerPoint (leadingConstructor) g <$> leadingLeadingCPoint <*> trailingLeadingCPoint
    leadingAdjustedCPoint = removeMaybe "leadingAdjustedCPoint was Nothing" $  extractE $ adjustCornerPoint (leadingConstructor) g <$> leadingLeadingCPoint <*> trailingLeadingCPoint
    --trailingAdjustedCPoint = extractE $ adjustCornerPoint (trailingConstructor) g <$> leadingTrailingCPoint <*>  trailingTrailingCPoint
    trailingAdjustedCPoint = removeMaybe "trailingAdjustedCPoint was Nothing" $ extractE $ adjustCornerPoint (trailingConstructor) g <$> leadingTrailingCPoint <*>  trailingTrailingCPoint
    --determine what type of line will be build.
    --The initial call of this fx will typically build a Back<Top/Bottom>Line.
    --After that Front<Top/Bottom>Line will be build as is standard when using +++>
    --To simplify (not use Either) for missing pattern matches, default to making [TopFace]
    --
    leadingConstructor' =
      case leadingAdjustedCPoint of
        (Right (  (B3 (Point _ _ _)))) -> F3
        (Right (  (F3 (Point _ _ _)))) -> F3
        otherwise -> F3
    trailingConstructor' =
      case trailingAdjustedCPoint of
        (Right (  (B2 (Point _ _ _)))) -> F2
        (Right (  (F2 (Point _ _ _)))) -> F2
        otherwise -> F2

  {-
  leadingConstructor' =
      case leadingAdjustedCPoint of
        (Right ( Just (B3 (Point _ _ _)))) -> F3
        (Right ( Just (F3 (Point _ _ _)))) -> F3
        otherwise -> F3
    trailingConstructor' =
      case trailingAdjustedCPoint of
        (Right ( Just (B2 (Point _ _ _)))) -> F2
        (Right ( Just (F2 (Point _ _ _)))) -> F2
        otherwise -> F2
  -}
  in
   extractE $
     buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
       leadingConstructor' trailingConstructor'
       grid
       leadingCPoints trailingCPoints <$>
       ((leadingAdjustedCPoint #+++# trailingAdjustedCPoint) `appendE` workingCPoints) 
       
     

buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' _
                                                     _
                                                     []
                                                     _
                                                     _
                                                     workingCPoints
                                                     
                                                      =
   let
     lines =  reverse workingCPoints
   in
     Right $ (head lines) +++> (tail lines)
{-
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' (leadingConstructor)
                                                     (trailingConstructor)
                                                     (g:grid)
                                                     leadingCPoints
                                                     trailingCPoints
                                                     workingCPoints
                                                     ({-Just-} leadingCPoint)
                                                     ({-Just-} trailingCPoint)
                                                      =
  --leftOff -- Change <leading/trailing>CPoint to Maybe (remove Either) then use Applicative to if fx ends instead of pattern matching to see if fx ends.
          --Will also need to change the order of the params so workingCPoints is ahead, to work better with Applicative.
  let
    --For all the following:
    --Can be Either because unhandled/illegal CornerPoint in leading/trailing [CornerPoints]
    --Can be Maybe because of improper grid target values construction. There should be not grid value outside the range
    --of the <leading/trailing>CPoints y_axis. But should gaurd against it.
    leadingLeadingCPoint :: Either String (Maybe CornerPoints)
    leadingLeadingCPoint = getLeadingCPoint g leadingCPoints
    trailingLeadingCPoint = getTrailingCPoint g leadingCPoints
    leadingTrailingCPoint = getLeadingCPoint g trailingCPoints
    trailingTrailingCPoint = getTrailingCPoint g trailingCPoints

    --at this point, cx leadingLeadingCPoint(and the other 3) for Nothing.
    --If Nothing, make it a Left, otherwise remove the Maybe aspect as a Nothing makes no sense. It should be Left if this happens
    --The alternative is to got back and cx adjustCornerPoint to be a Either String CornerPoints , and remove the Maybe.
    --Which is easier. Could do this here 1st, change the pattern matches, then go back and cx adjustCornerPoint, as the
    --problem is going to be coordinating all the cascading changes if adjustCornerPoint if done first.


    --leadingAdjustedCPoint = extractE $ adjustCornerPoint (leadingConstructor) g <$> leadingLeadingCPoint <*> trailingLeadingCPoint
    leadingAdjustedCPoint = removeMaybe "leadingAdjustedCPoint was Nothing" $  extractE $ adjustCornerPoint (leadingConstructor) g <$> leadingLeadingCPoint <*> trailingLeadingCPoint
    --trailingAdjustedCPoint = extractE $ adjustCornerPoint (trailingConstructor) g <$> leadingTrailingCPoint <*>  trailingTrailingCPoint
    trailingAdjustedCPoint = removeMaybe "trailingAdjustedCPoint was Nothing" $ extractE $ adjustCornerPoint (trailingConstructor) g <$> leadingTrailingCPoint <*>  trailingTrailingCPoint
    --determine what type of line will be build.
    --The initial call of this fx will typically build a Back<Top/Bottom>Line.
    --After that Front<Top/Bottom>Line will be build as is standard when using +++>
    --To simplify (not use Either) for missing pattern matches, default to making [TopFace]
    --
    leadingConstructor' =
      case leadingAdjustedCPoint of
        (Right (  (B3 (Point _ _ _)))) -> F3
        (Right (  (F3 (Point _ _ _)))) -> F3
        otherwise -> F3
    trailingConstructor' =
      case trailingAdjustedCPoint of
        (Right (  (B2 (Point _ _ _)))) -> F2
        (Right (  (F2 (Point _ _ _)))) -> F2
        otherwise -> F2

  {-
  leadingConstructor' =
      case leadingAdjustedCPoint of
        (Right ( Just (B3 (Point _ _ _)))) -> F3
        (Right ( Just (F3 (Point _ _ _)))) -> F3
        otherwise -> F3
    trailingConstructor' =
      case trailingAdjustedCPoint of
        (Right ( Just (B2 (Point _ _ _)))) -> F2
        (Right ( Just (F2 (Point _ _ _)))) -> F2
        otherwise -> F2
  -}
  in
   extractE $
     buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
       leadingConstructor' trailingConstructor'
       grid
       leadingCPoints trailingCPoints
       ((leadingCPoint +++ trailingCPoint) : workingCPoints) <$>
         leadingAdjustedCPoint <*> trailingAdjustedCPoint
     

buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' _
                                                     _
                                                     []
                                                     _
                                                     _
                                                     workingCPoints
                                                     ({-Just-} leadingCPoint)
                                                     ({-Just-} trailingCPoint)
                                                      =
   let
     lines =  reverse $ (leadingCPoint +++ trailingCPoint) : workingCPoints
   in
     Right $ (head lines) +++> (tail lines)



buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' (leadingConstructor)
                                                     (trailingConstructor)
                                                     (g:grid)
                                                     leadingCPoints
                                                     trailingCPoints
                                                     (Right (Just leadingCPoint))
                                                     (Right (Just trailingCPoint))
                                                     workingCPoints =
  leftOff -- Change <leading/trailing>CPoint to Maybe (remove Either) then use Applicative to if fx ends instead of pattern matching to see if fx ends.
          --Will also need to change the order of the params so workingCPoints is ahead, to work better with Applicative.
  let
    leadingLeadingCPoint = getLeadingCPoint g leadingCPoints
    trailingLeadingCPoint = getTrailingCPoint g leadingCPoints
    leadingTrailingCPoint = getLeadingCPoint g trailingCPoints
    trailingTrailingCPoint = getTrailingCPoint g trailingCPoints
    leadingAdjustedCPoint = extractE $ adjustCornerPoint (leadingConstructor) g <$> leadingLeadingCPoint <*> trailingLeadingCPoint
    trailingAdjustedCPoint = extractE $ adjustCornerPoint (trailingConstructor) g <$> leadingTrailingCPoint <*>  trailingTrailingCPoint
    --determine what type of line will be build.
    --The initial call of this fx will typically build a Back<Top/Bottom>Line.
    --After that Front<Top/Bottom>Line will be build as is standard when using +++>
    --To simplify (not use Either) for missing pattern matches, default to making [TopFace]
    --
    leadingConstructor' =
      case leadingAdjustedCPoint of
        (Right ( Just (B3 (Point _ _ _)))) -> F3
        (Right ( Just (F3 (Point _ _ _)))) -> F3
        otherwise -> F3
    trailingConstructor' =
      case trailingAdjustedCPoint of
        (Right ( Just (B2 (Point _ _ _)))) -> F2
        (Right ( Just (F2 (Point _ _ _)))) -> F2
        otherwise -> F2
  in
   buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
     leadingConstructor' trailingConstructor'
     grid
     leadingCPoints trailingCPoints
     leadingAdjustedCPoint trailingAdjustedCPoint
     ((leadingCPoint +++ trailingCPoint) : workingCPoints)

buildLeftRightLineFromGridAndLeadingTrailingCPointsBase' _
                                                     _
                                                     []
                                                     _
                                                     _
                                                     (Right (Just leadingCPoint))
                                                     (Right (Just trailingCPoint))
                                                     workingCPoints =
   let
     lines =  reverse $ (leadingCPoint +++ trailingCPoint) : workingCPoints
   in
     Right $ (head lines) +++> (tail lines)

-}
--curry in the initial constructors to build [TopFace], which is the only 1 handled so far.
--Only thing needed to do bottom faces, would be to add CornerPoints constructors B4, B1 (BackBottomLine) to case statement in recur fx
--to go to F1, F4 (FrontBottomLine).
buildGridTopFaces = buildLeftRightLineFromGridAndLeadingTrailingCPointsBase -- (B3) (B2)


{-or do I even need this
Used in: buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
The leading/trailing CornerPoints that each grid line are build from, are Maybe CornerPoints.
Combine them here to use pattern matching rules.

combineLeftRightTrailingLeadingCPoints :: (Maybe CornerPoints) -> (Maybe CornerPoints) -> (Maybe CornerPoints)
combineLeftRightTrailingLeadingCPoints (Just leadingCPoints) (Just trailingCPoints) =
  Just $ leadingCPoints +++ trailingCPoints
combineLeftRightTrailingLeadingCPoints (Just leadingCPoints) Nothing =
  Just $ leadingCPoints +++ trailingCPoints
-}
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------Tests----------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------- look at the [CornerPoints]----------------------------------------------------------------
seeRadialShapeAsTopFrontPointsTest = TestCase $ assertEqual
  "look at complete radial shape as front points"
  ([])
  (radialShapeAsTopFrontPoints)


seeLeadingRadialShapeAsTopFrontPointsTest = TestCase $ assertEqual
  "see the leading section of radial shape as front points"
  ([])
  (leadingRadialShapeAsTopFrontPoints)

seeTrailingRadialShapeAsTopFrontPointsTest = TestCase $ assertEqual
  "see the trailing section of radial shape as front points"
  ([])
  (trailingRadialShapeAsTopFrontPoints)

-----------------------------------------------------see min/may y vals and generate grid [double]--------------------------------------
seeMinYCpointOfLeadingRadialShapeAsTopFrontPointsTest = TestCase $ assertEqual
  "get the cpoint with min y val of the leading radial shape"
  (F3 {f3 = Point {x_axis = 1.7364817766693033, y_axis = -9.84807753012208, z_axis = 10.0}})
  --(CornerPointsError "filler")
  (getMinY leadingRadialShapeAsTopFrontPoints)

seeMinYCpointOfTrailingRadialShapeAsTopFrontPointsTest = TestCase $ assertEqual
  "get the cpoint with min y val of the trailiing radial shape"
  (F2 {f2 = Point {x_axis = -0.8715574274765816, y_axis = -9.961946980917455, z_axis = 10.0}})
  --(CornerPointsError "filler")
  (getMinY trailingRadialShapeAsTopFrontPoints)


seeMaxYCpointOfLeadingRadialShapeAsTopFrontPointsTest = TestCase $ assertEqual
  "get the cpoint with max y val of the leading radial shape"
  (F2 {f2 = Point {x_axis = 1.7364817766693033, y_axis = 9.84807753012208, z_axis = 10.0}})
  --(CornerPointsError "filler")
  (getMaxY leadingRadialShapeAsTopFrontPoints)


seeMaxYCpointOfTrailingRadialShapeAsTopFrontPointsTest = TestCase $ assertEqual
  "get the cpoint with max y val of the trailiing radial shape"
  (F2 {f2 = Point {x_axis = -1.7364817766693033, y_axis = 9.84807753012208, z_axis = 10.0}})
  --(CornerPointsError "filler")
  (getMaxY trailingRadialShapeAsTopFrontPoints)

generateYaxisListOfCubesToCreateTest = TestCase $ assertEqual
  "generateYaxisListOfCubesToCreate"
  ([-9.961946980917455,-9.0,-8.0,-7.0,-6.0,-5.0,-4.0,-3.0,-2.0,-1.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,9.84807753012208])
  (createListOfYaxisValuesToMakeCubesOnFromRadialShapeAsTopFrontPoints radialShapeAsTopFrontPoints)


-------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------get<Leading/Trailing>CPoint -----------------------------------------------------


------------------------------------------------get leadingCPoint--------------------------------------------------------------
getLeadingCPointExistsTest = TestCase $ assertEqual
  "exists: getLeadingCPoint"
  (Right $ Just $ F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 1.7364817766693041, z_axis = 10.0}})
  (getLeadingCPoint 2 leadingRadialShapeAsTopFrontPoints)

getLeadingCPointExistsTest2 = TestCase $ assertEqual
  "not exists due to too small of target value: getLeadingCPoint"
  (Right Nothing)
  (getLeadingCPoint (-100) leadingRadialShapeAsTopFrontPoints)

getLeadingCPointExistsTest3 = TestCase $ assertEqual
  "error due to empty list: getLeadingCPoint"
  (Left "getLeadingCPoint: empty [CornerPoints] passed in")
  (getLeadingCPoint 5 [])

getLeadingCPointExistsTest4 = TestCase $ assertEqual
  "gets 1 and only valid cpoint: getLeadingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 9.396926207859085, z_axis = 10.0}})))
  (getLeadingCPoint 5 [F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 9.396926207859085, z_axis = 10.0}}])

getLeadingCPointExistsTest5 = TestCase $ assertEqual
  "gets 2nd of 2 cpoints: getLeadingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 4.736481776669304, z_axis = 10.0}})))
  (getLeadingCPoint 5 [F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 1.396926207859085, z_axis = 10.0}},
                       F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 4.7364817766693041, z_axis = 10.0}}
                      ])

getLeadingCPointExistsTest6 = TestCase $ assertEqual
  "gets the last of many cpoints: getLeadingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 1.7364817766693033, y_axis = 9.84807753012208, z_axis = 10.0}})))
  (getLeadingCPoint 50 leadingRadialShapeAsTopFrontPoints)

getLeadingCPointExistsTest7 = TestCase $ assertEqual
  "exact match will result in leading == trailing: get<Leading/Trailing>CPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 7.66044443118978, y_axis = -6.427876096865393, z_axis = 10.0}})),
   Right (Just (F2 {f2 = Point {x_axis = 7.66044443118978, y_axis = -6.427876096865393, z_axis = 10.0}}))
  )
  (let
      testList =
         [F2 {f2 = Point {x_axis = 6.4278760968653925, y_axis = -7.66044443118978, z_axis = 10.0}},
          F2 {f2 = Point {x_axis = 7.66044443118978, y_axis = -6.427876096865393, z_axis = 10.0}},
          F2 {f2 = Point {x_axis = 8.660254037844386, y_axis = -5.000000000000001, z_axis = 10.0}}
         ]
      leadingPoint = 
        getLeadingCPoint
          (-6.427876096865393)
          testList
      trailingPoint = 
        getTrailingCPoint
          (-6.427876096865393)
          testList
   in
     (leadingPoint,trailingPoint)
  )

------------------------------------------------get trailingCPoint--------------------------------------------------------------
getTrailingCPointExistsTest = TestCase $ assertEqual
  "exists: getTrailingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 9.396926207859083, y_axis = 3.4202014332566884, z_axis = 10.0}})))
  (getTrailingCPoint 2 leadingRadialShapeAsTopFrontPoints)

getTrailingCPointExistsTest2 = TestCase $ assertEqual
  "not exists due to too big of target value: getTrailingCPoint"
  (Right Nothing)
  (getTrailingCPoint (100) leadingRadialShapeAsTopFrontPoints)

getTrailingCPointExistsTest3 = TestCase $ assertEqual
  "error due to empty list: getTrailingCPoint"
  (Left "getTrailingCPoint: empty [CornerPoints] passed in")
  (getTrailingCPoint 5 [])


getTrailingCPointExistsTest4 = TestCase $ assertEqual
  "gets 1 and only valid cpoint: getTrailingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 9.396926207859085, z_axis = 10.0}})))
  (getTrailingCPoint 5 [F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 9.396926207859085, z_axis = 10.0}}])

getTrailingCPointExistsTest5 = TestCase $ assertEqual
  "gets 2nd of 2 cpoints: getTrailingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 4.736481776669304, z_axis = 10.0}})))
  (getTrailingCPoint 4 [F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 1.396926207859085, z_axis = 10.0}},
                       F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 4.7364817766693041, z_axis = 10.0}}
                      ])

getTrailingCPointExistsTest6 = TestCase $ assertEqual
  "gets 1st of 2 cpoints: getTrailingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 1.396926207859085, z_axis = 10.0}})))
  (getTrailingCPoint 1 [F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 1.396926207859085, z_axis = 10.0}},
                       F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 4.7364817766693041, z_axis = 10.0}}
                      ])

getTrailingCPointExistsTest7 = TestCase $ assertEqual
  "gets the last of many cpoints: getLeadingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 1.7364817766693033, y_axis = 9.84807753012208, z_axis = 10.0}})))
  (getTrailingCPoint 9.4 leadingRadialShapeAsTopFrontPoints)


-------------------------------------------------------- buildGridTopFaces tests ------------------------------------------
buildGridTopFacesTest = TestCase $ assertEqual
  "buildGridTopFaces: build a good grid"
  (Left "filler")
  (let
      grid = createListOfYaxisValuesToMakeCubesOnFromRadialShapeAsTopFrontPoints radialShapeAsTopFrontPoints
   in
    buildLeftRightLineFromGridAndLeadingTrailingCPointsBase grid leadingRadialShapeAsTopFrontPoints trailingRadialShapeAsTopFrontPoints
  )

{-leadingRadialShapeAsTopFrontPoints
[
F3 {f3 = Point {x_axis = 1.7364817766693033, y_axis = -9.84807753012208, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = -9.396926207859085, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 4.999999999999999, y_axis = -8.660254037844387, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 6.4278760968653925, y_axis = -7.66044443118978, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 7.66044443118978, y_axis = -6.427876096865393, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 8.660254037844386, y_axis = -5.000000000000001, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 9.396926207859083, y_axis = -3.4202014332566884, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = -1.7364817766693041, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 10.0, y_axis = -6.123233995736766e-16, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 1.7364817766693041, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 9.396926207859083, y_axis = 3.4202014332566884, z_axis = 10.0}}
,F2 {f2 = Point {x_axis = 8.660254037844386, y_axis = 5.000000000000001, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 7.66044443118978, y_axis = 6.427876096865393, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 6.4278760968653925, y_axis = 7.66044443118978, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 4.999999999999999, y_axis = 8.660254037844387, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 3.420201433256687, y_axis = 9.396926207859085, z_axis = 10.0}},
F2 {f2 = Point {x_axis = 1.7364817766693033, y_axis = 9.84807753012208, z_axis = 10.0}}]
-}


--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
---------------------calculate the point from <leading/trailing>CPoint and the target y_axis value------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
leadingRatio :: Double -> Double -> Double -> Double
leadingRatio    targetVal yLeading yTrailing  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    
    leadingRatio = (totalDiff - trailingDiff)/totalDiff
  in
    leadingRatio

leadingRatioFromCPoint :: Double -> CornerPoints -> CornerPoints -> Either String Double
leadingRatioFromCPoint    targetVal (F2 (Point _ yLeading _)) (F2 (Point _ yTrailing _))  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    
    leadingRatio = (totalDiff - trailingDiff)/totalDiff
  in
    Right leadingRatio
leadingRatioFromCPoint    targetVal (F2 (Point _ yLeading _)) (F3 (Point _ yTrailing _))  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    
    leadingRatio = (totalDiff - trailingDiff)/totalDiff
  in
    Right leadingRatio
leadingRatioFromCPoint    targetVal (F3 (Point _ yLeading _)) (F2 (Point _ yTrailing _))  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    
    leadingRatio = (totalDiff - trailingDiff)/totalDiff
  in
    Right leadingRatio
leadingRatioFromCPoint    targetVal (F3 (Point _ yLeading _)) (F3 (Point _ yTrailing _))  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    
    leadingRatio = (totalDiff - trailingDiff)/totalDiff
  in
    Right leadingRatio

leadingRatioFromCPoint    targetVal leadingCPoint trailingCPoint  =
  Left $ "leadingRatioFromCPoint: illegal or unhandled CornerPoints for leadingCPoint: " ++ (show leadingCPoint) ++ " and trailingCPoint: " ++ (show trailingCPoint)
  
adjustAxis :: Double       -> Double   -> Double    -> Double
adjustAxis    leadingRatio    leadingAxis    trailingAxis    =
  ((trailingAxis - leadingAxis) * leadingRatio) + leadingAxis

{-
Given:
cPointsConstructor:
  The CornerPoints constructor used to create the resulting adjusted CornerPoints.
targetValue:
  A y_axis value within the range of leadingY and trailingY.
leadingCPoint:
  The CPoint which will have the lowest y_axis value.
trailingCPoint:
  The CPoint which will have the greatest y_axis value.

Task:
Create a CornerPoints in which all the y_axis has been set to the target value.
Adjust the <x/z>_axis so that their value is changed at the same ratio of change as the y_axis change.
EG: If the y_axis is set exactly 1/2 way between the <leading/trailing>Y, then set the <x/z>_axis exactly half way between leading/trailing.

Return:
The Right adjusted CornerPoints.
Left if an error occurrs such as undhandled or illegal CornerPoints or CornerPoints Constructor.

Should not be Maybe. The only way of getting Nothing, is if both <leading/trailing>CPoints passed in are Nothing. This shud be Left.
But maybe that decision should be left to what uses it. Is there any situation where Maybe would be used?
-}
--ToDo: cPointsConstructor should be matched. What happens if a TopFace is passed in. NFG.
adjustCornerPoint :: (Point -> CornerPoints) -> Double -> Maybe CornerPoints -> Maybe CornerPoints -> Either String (Maybe CornerPoints)
adjustCornerPoint cPointsConstructor targetValue (Just (F2 (Point  leadingX leadingY leadingZ))) (Just (F2 (Point trailingX trailingY trailingZ))) =
  let
    leadingRatio = leadingRatioFromCPoint targetValue (F2 (Point  leadingX leadingY leadingZ)) (F2 (Point trailingX trailingY trailingZ))
  in
    case leadingRatio of
      Right leadingRatio'' -> 
        Right $ Just $ cPointsConstructor $ Point
                                             (adjustAxis leadingRatio'' leadingX trailingX)
                                             (adjustAxis leadingRatio'' leadingY trailingY)
                                             (adjustAxis leadingRatio'' leadingZ trailingZ)
      Left e -> Left $ "adjustCornerPoint: " ++ e
  --needs testing 
adjustCornerPoint cPointsConstructor targetValue (Just (F2 (Point  leadingX leadingY leadingZ))) Nothing =
  Right $ Just $ cPointsConstructor $ Point  leadingX leadingY leadingZ


adjustCornerPoint cPointsConstructor targetValue (Just (F2 (Point  leadingX leadingY leadingZ))) (Just(F3 (Point trailingX trailingY trailingZ))) =
  let
    leadingRatio = leadingRatioFromCPoint targetValue (F2 (Point  leadingX leadingY leadingZ)) (F3 (Point trailingX trailingY trailingZ))
  in
    case leadingRatio of
      Right leadingRatio' ->
        Right $ Just $ cPointsConstructor $ Point
                                              (adjustAxis leadingRatio' leadingX trailingX)
                                              (adjustAxis leadingRatio' leadingY trailingY)
                                              (adjustAxis leadingRatio' leadingZ trailingZ)
      Left e -> Left $ "adjustCornerPoint: " ++ e

adjustCornerPoint cPointsConstructor targetValue (Just(F3 (Point  leadingX leadingY leadingZ))) (Just(F2 (Point trailingX trailingY trailingZ))) =
  let
    leadingRatio = leadingRatioFromCPoint targetValue (F3 (Point  leadingX leadingY leadingZ)) (F2 (Point trailingX trailingY trailingZ))
  in
    case leadingRatio of 
      Right leadingRatio' -> Right $ Just $ cPointsConstructor $ Point
                                                                   (adjustAxis leadingRatio' leadingX trailingX)
                                                                   (adjustAxis leadingRatio' leadingY trailingY)
                                                                   (adjustAxis leadingRatio' leadingZ trailingZ)
      Left e -> Left $ "adjustCornerPoint: " ++ e

adjustCornerPoint cPointsConstructor targetValue (Just (F3 (Point  leadingX leadingY leadingZ))) (Just (F3 (Point trailingX trailingY trailingZ))) =
  let
    leadingRatio = leadingRatioFromCPoint targetValue (F3 (Point  leadingX leadingY leadingZ)) (F3 (Point trailingX trailingY trailingZ))
  in
    case leadingRatio of 
      Right leadingRatio' -> 
        Right $ Just $ cPointsConstructor $ Point
                                              (adjustAxis leadingRatio' leadingX trailingX)
                                              (adjustAxis leadingRatio' leadingY trailingY)
                                              (adjustAxis leadingRatio' leadingZ trailingZ)
      Left e -> Left $ "adjustCornerPoint: " ++ e

adjustCornerPoint cPointsConstructor targetValue Nothing (Just (F2 (Point  trailingX trailingY trailingZ)))  =
  Right $ Just $ cPointsConstructor $ Point  trailingX trailingY trailingZ

adjustCornerPoint cPointsConstructor targetValue Nothing Nothing  =
  Right $ Nothing

adjustCornerPoint _ _ (Just leadingCPoint) (Just trailingCPoint) =
  Left $ "adjustCornerPoint: illegal or unhandled leadingCPoint: " ++ (cpointType leadingCPoint) ++ " and trailingCPoint: " ++ (cpointType trailingCPoint)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------calculate the point from <leading/trailing>CPoint and the target y_axis value---------------------------------------------------------------
------------------------------------------------------------tests------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------


leadingRatioFromDoubleTest = TestCase $ assertEqual
  "get the ratio: leadingRatioFromDoubleTest"
  (0.5)
  (leadingRatio 5 0 10)

leadingRatioFromCPointsTest = TestCase $ assertEqual
  "get the ratio: leadingRatioFromCPointsTest"
  (Right 0.5)
  (leadingRatioFromCPoint
     5
     (F2 $ Point 10 0 100)
     (F2 $ Point 0 10 100)
  )

leadingRatioFromDoubleTest2 = TestCase $ assertEqual
  "get the ratio: leadingRatioFromDoubleTest"
  (0.2)
  (leadingRatio 2 0 10)

leadingRatioFromCPointsTest2 = TestCase $ assertEqual
  "get the ratio: leadingRatioFromCPointsTest"
  (Right 0.2)
  (leadingRatioFromCPoint
     2
     (F2 $ Point 10 0 100)
     (F3 $ Point 0 10 100)
  )

leadingRatioFromDoubleTest3 = TestCase $ assertEqual
  "get the ratio: leadingRatioFromDoubleTest"
  (0.2)
  (leadingRatio (-8) (-10) 0)

leadingRatioFromCPointsTest3 = TestCase $ assertEqual
  "get the ratio: leadingRatioFromCPointsTest"
  (Right 0.2)
  (leadingRatioFromCPoint
     (-8)
     (F2 $ Point 10 (-10) 100)
     (F2 $ Point 0 0 100)
  )

------------------------------------------------------------------------------------------
adjustAxisTest = TestCase $ assertEqual
  "positive z change: adjustAxis"
  (2)
  (let
      ratio = (leadingRatio 2 0 10)
   in
     adjustAxis ratio 0 10
  )

adjustCornerPointsTest = TestCase $ assertEqual
  "positive ratio and axis change: adjustCornerPoint"
  (Right $ Just $ F2 $ Point 2 2 2)
  (let
      ratio = (leadingRatio 2 0 10)
   in
     --now uses target value instead of already calc'd ratio
     adjustCornerPoint (F2) 2 (Just $ F2 $ Point 0 0 0) (Just $ F2 $ Point 10 10 10)
  )
{- 
adjustCornerPointsTest = TestCase $ assertEqual
  "positive ratio and axis change: adjustCornerPoint"
  (Right $ F2 $ Point 2 2 2)
  (let
      ratio = (leadingRatio 2 0 10)
   in
     --now uses target value instead of already calc'd ratio
     adjustCornerPoint (F2) 2 (F2 $ Point 0 0 0) (F2 $ Point 10 10 10)
  )
-}
adjustAxisTest2 = TestCase $ assertEqual
  "neg z change: adjustAxis"
  (-2)
  (let
      ratio = (leadingRatio 2 0 10)
   in
     adjustAxis ratio 0 (-10)
  )

adjustCornerPointsTest2 = TestCase $ assertEqual
  "positive ratio and axis change: adjustCornerPoint2"
  (Right $ Just $ F3 $ Point (-2) (-2) (-2))
  (let
      ratio = leadingRatio (-2) 0 (-10)
   in
     --now uses target value instead of already calc'd ratio
     adjustCornerPoint (F3) (-2) (Just $ F2 $ Point 0 0 0) (Just $ F2 $ Point (-10) (-10) (-10))
  )
{-
adjustCornerPointsTest2 = TestCase $ assertEqual
  "positive ratio and axis change: adjustCornerPoint2"
  (Right $ F3 $ Point (-2) (-2) (-2))
  (let
      ratio = (leadingRatio 2 0 10)
   in
     adjustCornerPoint (F3) ratio (F2 $ Point 0 0 0) (F2 $ Point (-10) (-10) (-10))
  )
-}

adjustAxisTest3 = TestCase $ assertEqual
  "another neg z change: adjustAxis"
  (-12)
  (let
      ratio = (leadingRatio 2 0 10)
   in
     adjustAxis ratio (-10) (-20)
  )

adjustAxisTest4 = TestCase $ assertEqual
  "neg ratio pos z change: adjustAxis"
  (2)
  (let
      ratio = leadingRatio  (-18) (-20) (-10)
   in
     adjustAxis ratio 0 10
  )











-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------junk I problably can get rid of ---------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------


--boolean to see if a cornerpoint contains a y value.
--Used to filter a list, looking for y val.
--Was used for Data.List(any), which will not be used any more.
containsYaxisValue :: Double -> CornerPoints -> Bool
containsYaxisValue targetYval (F2 (Point _ y _)) =
  targetYval == y
containsYaxisValue targetYval (F3 (Point _ y _)) =
  targetYval == y



--get the cpoint with greatest y-axis val that is <= target val
--I want to get only the LT, EQ, or GT values. Not <= or >=
getLargestCpointWithLTEYval :: Double -> [CornerPoints] -> Maybe CornerPoints
getLargestCpointWithLTEYval targetVal cpoints =
  let foundCpointWithTargetYval = find (containsYaxisValue targetVal) cpoints
  in
  case foundCpointWithTargetYval of
    Just cpoint -> Just cpoint
    Nothing ->
      case (any (cPointHasYaxisLTTargetVal targetVal) cpoints) of
        False -> Nothing
        True ->
          Just $ last $ takeWhile (cPointHasYaxisLTTargetVal targetVal) cpoints

containsYaxisValueNotTest = TestCase $ assertEqual
  "containsYaxisValueTest"
  False
  (
   any (containsYaxisValue 100) leadingRadialShapeAsTopFrontPoints
  )

containsYaxisValueTrueTest = TestCase $ assertEqual
  "containsYaxisValueTrueTest"
  True
  (
   any (containsYaxisValue  9.84807753012208) leadingRadialShapeAsTopFrontPoints
  )


--does it contain any values with y_axis LT target val
cPointHasYaxisLTTargetVal :: Double -> CornerPoints -> Bool
cPointHasYaxisLTTargetVal targetYval (F2 (Point _ y _)) =
  y < targetYval
cPointHasYaxisLTTargetVal targetYval (F3 (Point _ y _)) =
  y < targetYval


containsCpointWithYaxisValueLTTargetValFalseTest = TestCase $ assertEqual
  "False: contains a cpoint with a y-axis val < target val"
  False
  (
   any (cPointHasYaxisLTTargetVal  (-100)) leadingRadialShapeAsTopFrontPoints
  )

containsCpointWithYaxisValueLTTargetValTrueTest = TestCase $ assertEqual
  "True: contains a cpoint with a y-axis val < target val"
  True
  (
   any (cPointHasYaxisLTTargetVal  (100)) leadingRadialShapeAsTopFrontPoints
  )

getLargestCpointWithLTEYvalExists = TestCase $ assertEqual
  "exists as smaller y val: getLargestCpointWithLTEYval"
  (Just (F2 {f2 = Point {x_axis = 9.84807753012208, y_axis = 1.7364817766693041, z_axis = 10.0}}))
  (getLargestCpointWithLTEYval 2 leadingRadialShapeAsTopFrontPoints)

getLargestCpointWithLTEYvalNotExists = TestCase $ assertEqual
  "not exists: getLargestCpointWithLTEYval"
  (Nothing)
  (getLargestCpointWithLTEYval (-20) leadingRadialShapeAsTopFrontPoints)


getLargestCpointWithLTEYvalExactlyExists = TestCase $ assertEqual
  "exists as exact value: getLargestCpointWithLTEYval"
  (Just (F2 {f2 = Point {x_axis = 4.999999999999999, y_axis = -8.660254037844387, z_axis = 10.0}}))
  (getLargestCpointWithLTEYval (-8.660254037844387) leadingRadialShapeAsTopFrontPoints)

{-can i get rid of this
cPointHasYaxisLTTargetValE :: Double -> CornerPoints -> Either String Bool
cPointHasYaxisLTTargetValE targetYval (F2 (Point _ y _)) =
  case y < targetYval of
    True -> Right True
    False -> Right False
cPointHasYaxisLTTargetVal targetYval (F3 (Point _ y _)) =
  case y < targetYval of
    True -> Right True
    False -> Right False
cPointHasYaxisLTTargetVal _ unhandledCPoint = Left $ "cPointHasYaxisLTTargetVal: illegal or unhandled CornerPoints for: " ++ (show unhandledCPoint)
-}


--get the first CPoint that has the targetValue yaxis. Nothing if not found.
--Left if CornerPoints not handled or illegal
-- <x/y/z>_axis should have a ADT, so that the can be a part of eq, the way Points are. That would allow comparing to 2 decimal places.
getMatchingCPoint :: Double -> [CornerPoints] -> Either String (Maybe CornerPoints)
getMatchingCPoint targetVal ((F2 (Point x y z)) : cpoints) =
  case y == targetVal of
    True -> Right $ Just $ F2 (Point x y z)
    False -> getMatchingCPoint targetVal cpoints
getMatchingCPoint targetVal ((F3 (Point x y z)) : cpoints) =
  case y == targetVal of
    True -> Right $ Just $ F2 (Point x y z)
    False -> getMatchingCPoint targetVal cpoints
getMatchingCPoint _ [] = Right Nothing
getMatchingCPoint _ (unhandledCPoint : cpoints) =
  Left $ "getMatchingCPoint: illegal or unhandled CornerPoint: " ++ (show unhandledCPoint)

getMatchingCPointExistTest = TestCase $ assertEqual
  "exists: getMatchingCPoint"
  (Right $ Just $ F2 {f2 = Point {x_axis = 7.66044443118978, y_axis = -6.427876096865393, z_axis = 10.0}})
  (getMatchingCPoint (-6.427876096865393) leadingRadialShapeAsTopFrontPoints)

getMatchingCPointNoExistTest = TestCase $ assertEqual
  "no exists: getMatchingCPoint"
  (Right Nothing)
  (getMatchingCPoint 200 leadingRadialShapeAsTopFrontPoints)

getMatchingCPointUnhandledTest = TestCase $ assertEqual
  "unhandled CPoint constructor: getMatchingCPoint"
  (Left "getMatchingCPoint: illegal or unhandled CornerPoint: B2 {b2 = Point {x_axis = 7.66044443118978, y_axis = -6.427876096865393, z_axis = 10.0}}")
  (getMatchingCPoint 200 [B2 {b2 = Point {x_axis = 7.66044443118978, y_axis = -6.427876096865393, z_axis = 10.0}}])

