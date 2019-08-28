{- |
For a HorizontalFaces:

Extract the [<Bottom/Top>Face]
Divide it symmetrically along the y-axis by filtering x_axis for < or > central value
-now have 2 possibly even length [<Bottom/Top>Face]

Create list of y_axis values with desired spacing, does not need to be even.
Should extend off both ends of the shape.




-}

module Joiners.RadialLines(getMinY, getMaxY, extractYaxis,
                           createYaxisGridFromTopFrontPoints, createYaxisGridFromMinMaxY,
                           splitOnXaxis, buildLeftRightLineFromGridAndLeadingTrailingCPointsBase) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (##+++#), (+++#))
import CornerPoints.HorizontalFaces(createTopFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceExtraction(extractFrontTopLine,  extractBottomFrontLine, extractF3, extractF2,)
import CornerPoints.FaceConversions(toF3)

import Data.List(find)
import qualified TypeClasses.Showable as TS
import Helpers.Applicative(extractE, removeMaybe, appendE)

radialShapeAsTopFrontPoints =
  let
    circle = createTopFacesVariableHeight
                    (Point 0 0 0)
                    [Radius r | r <- [10,10..]]
                    ([Angle a | a <- [10,20..350]]  ++ [Angle 355])
                    [10,10..]
    frontTopLines = map (extractFrontTopLine) circle
  in (extractF3 $ head frontTopLines) :  map (extractF2) frontTopLines


splitOnXaxis :: (Double -> Double -> Bool) -> Double -> CornerPoints -> Bool
splitOnXaxis tester splitterVal (F2 (Point x _ _))  = 
  tester x splitterVal 
splitOnXaxis tester splitterVal (F3 (Point x _ _))  = 
  tester x splitterVal 


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
was: createYaxisGridFromCompleteTopRadialShapeSplitOnZeroOfXaxis
-}
createYaxisGridFromTopFrontPoints :: [CornerPoints] -> [Double]
createYaxisGridFromTopFrontPoints radialShapeAsTopFrontPoints =
    createYaxisGridFromMinMaxY
      (extractYaxis $ getMinY radialShapeAsTopFrontPoints)
      (extractYaxis $ getMaxY radialShapeAsTopFrontPoints)

{-
before getting rid of all the leading\trailing stuff
createYaxisGridFromCompleteTopRadialShapeSplitOnZeroOfXaxis :: [CornerPoints] -> [Double]
createYaxisGridFromCompleteTopRadialShapeSplitOnZeroOfXaxis radialShapeAsTopFrontPoints =
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
  createYaxisGridFromMinMaxY minY maxY

-}
--Create a [Double] from min Yaxis to max Yaxis, spaced at 1mm except for the initial min/max positions, which may not start at even numbers.
--If a minY value is not an even #, then start the grid at minY, then move up to the next even # before proceeding every 1 mm.
--If a maxY value is not an even #, then end the grid 1mm values at the preceding even #, followed by the maxY 
--Know uses:
----createYaxisGridFromTopFrontPoints creates grid for a horizontal radial shape of F3:[F2]
createYaxisGridFromMinMaxY :: Double -> Double -> [Double]
createYaxisGridFromMinMaxY minY maxY =
  let
    --is <min/max>Y and even number?
    isAnEvenNumber :: Double -> Bool
    isAnEvenNumber yval = yval == (fromIntegral $ floor minY)
    --build the grid based on combination of <min/max>Y being even/uneven. 
    gridFromEvenMinMaxY = minY : [(fromIntegral $ ceiling minY), ((fromIntegral $ ceiling minY) + 1)..maxY]
    gridFromEvenMinYUnevenMaxY = [minY, (minY + 1)..(fromIntegral $ floor maxY)] ++ [maxY]
    gridFromUnevenMinYUnevenMaxY = minY : [(fromIntegral $ ceiling minY), ((fromIntegral $ ceiling minY) + 1)..(fromIntegral $ floor maxY)] ++ [maxY] 
    gridFromUnevenMinYEvenMaxY = minY : [(fromIntegral $ ceiling minY), ((fromIntegral $ ceiling minY) + 1)..maxY]
  
  in
  case (isAnEvenNumber minY)  of
      True ->
        case (isAnEvenNumber maxY)  of
          True -> gridFromEvenMinMaxY
          False -> gridFromEvenMinYUnevenMaxY
      False ->
        case (isAnEvenNumber maxY)  of
          True -> gridFromUnevenMinYEvenMaxY
          False -> gridFromUnevenMinYUnevenMaxY
          

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
--rename to: buildOuterToInnerWallWithGrid
--and make a new: buildInnerToOuterWallWithGrid
--both will be F3:[F2]. The diff will be how they make up the wall.
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase :: 
                                                       [Double] ->
                                                       [CornerPoints] -> --leading CPoints
                                                       [CornerPoints] -> --trailing CPoints
                                                       Either String [CornerPoints]    --final CPoints
buildLeftRightLineFromGridAndLeadingTrailingCPointsBase 
                                                        (g:grid)
                                                        ((F3 (Point leadingX leadingY leadingZ) : leadingCPoints))
                                                        ((F2 (Point trailingX trailingY trailingZ) : trailingCPoints))
                                                     =
    extractE $
    buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
      (F3) (F2) grid
      ((F3 (Point leadingX leadingY leadingZ) : leadingCPoints))
      ((F3 (Point trailingX trailingY trailingZ) : trailingCPoints)) <$>
      --(Right $ B3 $ Point leadingX leadingY leadingZ) ##+++# (Right $ B2 $ Point trailingX trailingY trailingZ) `appendE` []
      (B3 $ Point leadingX leadingY leadingZ) +++# (B2 $ Point trailingX trailingY trailingZ) `appendE` []
      
      
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
      --(Right $ B3 $ Point leadingX leadingY leadingZ) ##+++# (Right $ B2 $ Point trailingX trailingY trailingZ) `appendE` []
      (B3 $ Point leadingX leadingY leadingZ) +++# (B2 $ Point trailingX trailingY trailingZ) `appendE` []

buildLeftRightLineFromGridAndLeadingTrailingCPointsBase _ (leadingCPoint:leadingCPoints) (trailingCPoint:trailingCPoints) =
  Left $ "buildLeftRightLineFromGridAndLeadingTrailingCPointsBase: " ++ "missing or illegal pattern match for leading: " ++ (TS.showConstructor leadingCPoint)
                                                                     ++ " and trailing : " ++ (TS.showConstructor trailingCPoint)
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

    

    leadingAdjustedCPoint = removeMaybe "leadingAdjustedCPoint was Nothing" $  extractE $ adjustCornerPoint (leadingConstructor) g <$> leadingLeadingCPoint <*> trailingLeadingCPoint
    trailingAdjustedCPoint = removeMaybe "trailingAdjustedCPoint was Nothing" $ extractE $ adjustCornerPoint (trailingConstructor) g <$> leadingTrailingCPoint <*>  trailingTrailingCPoint
    --determine what type of line will be build.
    --The initial call of this fx will typically build a Back<Top/Bottom>Line.
    --After that Front<Top/Bottom>Line will be build as is standard when using +++>
    
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


  in
   extractE $
     buildLeftRightLineFromGridAndLeadingTrailingCPointsBase'
       leadingConstructor' trailingConstructor'
       grid
       leadingCPoints trailingCPoints <$>
       ((leadingAdjustedCPoint ##+++# trailingAdjustedCPoint) `appendE` workingCPoints) 
       
     

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
    leadingRatio =
      case totalDiff == 0 of
        True -> 1
        False -> (totalDiff - trailingDiff)/totalDiff
  in
    Right leadingRatio
leadingRatioFromCPoint    targetVal (F2 (Point _ yLeading _)) (F3 (Point _ yTrailing _))  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    leadingRatio =
      case totalDiff == 0 of
        True -> 1
        False -> (totalDiff - trailingDiff)/totalDiff
    --leadingRatio = (totalDiff - trailingDiff)/totalDiff
  in
    Right leadingRatio
leadingRatioFromCPoint    targetVal (F3 (Point _ yLeading _)) (F2 (Point _ yTrailing _))  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    leadingRatio =
      case totalDiff == 0 of
        True -> 1
        False -> (totalDiff - trailingDiff)/totalDiff
    --leadingRatio = (totalDiff - trailingDiff)/totalDiff
  in
    Right leadingRatio
leadingRatioFromCPoint    targetVal (F3 (Point _ yLeading _)) (F3 (Point _ yTrailing _))  =
  let
    leadingDiff  = targetVal - yLeading
    trailingDiff = yTrailing - targetVal
    totalDiff        = yTrailing - yLeading
    
    leadingRatio =
      case totalDiff == 0 of
        True -> 1
        False -> (totalDiff - trailingDiff)/totalDiff
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
      Left e -> Left $ "adjustCornerPoint1: " ++ e
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
      Left e -> Left $ "adjustCornerPoint2: " ++ e

adjustCornerPoint cPointsConstructor targetValue (Just(F3 (Point  leadingX leadingY leadingZ))) (Just(F2 (Point trailingX trailingY trailingZ))) =
  let
    leadingRatio = leadingRatioFromCPoint targetValue (F3 (Point  leadingX leadingY leadingZ)) (F2 (Point trailingX trailingY trailingZ))
  in
    case leadingRatio of 
      Right leadingRatio' -> Right $ Just $ cPointsConstructor $ Point
                                                                   (adjustAxis leadingRatio' leadingX trailingX)
                                                                   (adjustAxis leadingRatio' leadingY trailingY)
                                                                   (adjustAxis leadingRatio' leadingZ trailingZ)
      Left e -> Left $ "adjustCornerPoint3: " ++ e

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
      Left e -> Left $ "adjustCornerPoint4: " ++ e

adjustCornerPoint cPointsConstructor targetValue Nothing (Just (F2 (Point  trailingX trailingY trailingZ)))  =
  Right $ Just $ cPointsConstructor $ Point  trailingX trailingY trailingZ

adjustCornerPoint cPointsConstructor targetValue Nothing (Just (F3 (Point  trailingX trailingY trailingZ)))  =
  Right $ Just $ cPointsConstructor $ Point  trailingX trailingY trailingZ

adjustCornerPoint cPointsConstructor targetValue Nothing (Just unhandled)  =
  Left $ "adjustCornerPoint5: illegal or unhandled leadingCPoint: Nothing " ++ " and trailingCPoint: " ++ (TS.showConstructor unhandled)

adjustCornerPoint cPointsConstructor targetValue (Just unhandled) Nothing   =
  Left $ "adjustCornerPoint6: illegal or unhandled leadingCPoint: " ++ (TS.showConstructor unhandled) ++ " and trailingCPoint: Nothing" 


adjustCornerPoint cPointsConstructor targetValue Nothing Nothing  =
  Right $ Nothing

adjustCornerPoint _ _ (Just leadingCPoint) (Just trailingCPoint) =
  Left $ "adjustCornerPoint: illegal or unhandled leadingCPoint: " ++ (TS.showConstructor leadingCPoint) ++ " and trailingCPoint: " ++ (TS.showConstructor trailingCPoint)


