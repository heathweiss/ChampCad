module RadialLinesTest(radialLinesTestDo) where

import Test.HUnit
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.HorizontalFaces(createTopFacesVariableHeight, createBottomFacesVariableHeight)
import CornerPoints.Radius (Radius(..))
import Geometry.Angle(Angle(..))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceExtraction(extractFrontTopLine,  extractBottomFrontLine, extractF3, extractF2,)

import Data.List(find)

radialLinesTestDo = do
  putStrLn "\nRadialLinesTest"
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
  --getTrailingCPoint
  runTestTT getTrailingCPointExistsTest
  runTestTT getTrailingCPointExistsTest2
  runTestTT getTrailingCPointExistsTest3
  runTestTT getTrailingCPointExistsTest4
  ----------------------- removed items I should not need--------------------
  runTestTT containsYaxisValueNotTest
  runTestTT containsYaxisValueTrueTest
  runTestTT containsCpointWithYaxisValueLTTargetValFalseTest
  --runTestTT containsCpointWithYaxisValueLTTargetValTrueTest
  --runTestTT getLargestCpointWithLTEYvalExists
  --runTestTT getLargestCpointWithLTEYvalNotExists
  --runTestTT getLargestCpointWithLTEYvalExactlyExists
  runTestTT getMatchingCPointExistTest
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
  



--used only for testing. Should move it over to test unit.
leadingRadialShapeAsTopFrontPoints = filter (splitOnXaxis (>) 0) radialShapeAsTopFrontPoints

trailingRadialShapeAsTopFrontPoints =
  let list = filter (splitOnXaxis (<) 0) radialShapeAsTopFrontPoints
  in
    --(toF3 $ head list ) : (tail list)
    list

-----------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------get<Leading/Trailing>CPoint-----------------------------------------------------------------

--get the Maybe cpoint with greatest y-axis val that is < target val
--Left if unhandled cpoint constructor
getLeadingCPoint :: Double -> [CornerPoints] -> Either String (Maybe CornerPoints)
getLeadingCPoint _  [] = Left "getLeadingCPoint: empty [CornerPoints] passed in"
getLeadingCPoint targetVal  ((F2 (Point x y z)) : cpoints) =
  case  targetVal < y of
    True -> getLeadingCPoint' targetVal (Just $ F2 $ Point x y z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
getLeadingCPoint targetVal  ((F3 (Point x y z)) : cpoints) =
  case y < targetVal of
    True -> getLeadingCPoint' targetVal (Just $ F3 $ Point x y z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
getLeadingCPoint targetVal  (unhandledCPoint : cpoints) =  Left $ "getLeadingCPoint: unhandled or illegal CornerPoints: " ++ (show unhandledCPoint)
--recur for getLeadingCPoint
getLeadingCPoint' :: Double -> (Maybe CornerPoints) -> [CornerPoints] -> Either String (Maybe CornerPoints)
getLeadingCPoint' _ prevCPoint []  = Right prevCPoint
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y < targetVal of
    True ->
      Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right Nothing
----------------------------------------------------------------------------------------------------------------------------------------
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : cpoints) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : cpoints) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z)) : cpoints) =
  case current_y < targetVal of
    True ->
      getLeadingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
-----------------------------------------------------------------------------------------------------------------------------------------
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : cpoints) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : cpoints) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getLeadingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getLeadingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z)) : cpoints) =
  case current_y < targetVal of
    True ->
      getLeadingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getLeadingCPoint' targetVal Nothing cpoints
----------------------------------------------------------------------------------------------------------------------------------------------
getLeadingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y < targetVal of
    True ->
      case current_y < previous_y of
        True -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
getLeadingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y < targetVal of
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
  case  y > targetVal of
    True -> getTrailingCPoint' targetVal (Just $ F2 (Point x y z)) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints
getTrailingCPoint targetVal  ((F3 (Point x y z)) : cpoints) =
  case y > targetVal of
    True -> getTrailingCPoint' targetVal (Just $ F3 (Point x y z)) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints
getTrailingCPoint targetVal  (unhandledCPoint : cpoints) =  Left $ "getTrailingCPoint: unhandled or illegal CornerPoints: " ++ (show unhandledCPoint)
--recur for getTrailingCPoint
getTrailingCPoint' :: Double -> (Maybe CornerPoints) -> [CornerPoints] -> Either String (Maybe CornerPoints)
getTrailingCPoint' _ prevValidCPoint []  = Right prevValidCPoint
getTrailingCPoint' targetVal (Just ( F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> Right $ (Just $ F2 (Point previous_x previous_y previous_z))  
        False -> Right $ Just (F2 (Point current_x current_y current_z))
    False -> Right $ Just (F2 (Point previous_x previous_y previous_z))
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> Right $ (Just $ F3 (Point previous_x previous_y previous_z))  
        False -> Right $ Just (F2 (Point current_x current_y current_z))
    False -> Right $ Just (F3 (Point previous_x previous_y previous_z))
getTrailingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z)) : []) =
  case current_y > targetVal of
    True ->
      Right $ Just $ F2 $ Point current_x current_y current_z
    False -> Right Nothing
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> Right $ Just $ F3 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point current_x current_y current_z
getTrailingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> Right $ Just $ F2 $ Point previous_x previous_y previous_z
        False -> Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right $ Just $ F3 $ Point current_x current_y current_z
getTrailingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z)) : []) =
  case current_y > targetVal of
    True ->
      Right $ Just $ F3 $ Point current_x current_y current_z
    False -> Right Nothing
getTrailingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z))  : cpoints) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F2 (Point current_x current_y current_z))  : cpoints) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal Nothing  ((F2 (Point current_x current_y current_z))  : cpoints) =
  case current_y > targetVal of
    True ->
      getTrailingCPoint' targetVal (Just $ F2 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints
getTrailingCPoint' targetVal (Just (F2 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z))  : cpoints) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F2 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal (Just (F3 (Point previous_x previous_y previous_z)))  ((F3 (Point current_x current_y current_z))  : cpoints) =
  case current_y > targetVal of
    True ->
      case current_y > previous_y of
        True  -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
        False -> getTrailingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal (Just $ F3 $ Point previous_x previous_y previous_z) cpoints
getTrailingCPoint' targetVal Nothing  ((F3 (Point current_x current_y current_z))  : cpoints) =
  case current_y > targetVal of
    True ->
      getTrailingCPoint' targetVal (Just $ F3 $ Point current_x current_y current_z) cpoints
    False -> getTrailingCPoint' targetVal Nothing cpoints

--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------Tests----------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------- look at the [CornerPoints]----------------------------------------------------------------


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
--------
getLeadingCPointExistsTest6 = TestCase $ assertEqual
  "gets the last of many cpoints: getLeadingCPoint"
  (Right (Just (F2 {f2 = Point {x_axis = 1.7364817766693033, y_axis = 9.84807753012208, z_axis = 10.0}})))
  (getLeadingCPoint 50 leadingRadialShapeAsTopFrontPoints)

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
