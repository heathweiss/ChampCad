{- |
Do 2 lines intercept.
Either String Maybe Bool

If so, do they intercept on an endpoint (cornerPoint)
Either String Maybe Bool

If so, what is the point of interception
-Either String Maybe point, or cornerpoint?
 -A Face could intercept along a line, which would be CornerPoints
 -2 lines would intercept at a Pointx
 -maybe need separate modules for Point vs CornerPoints
-}
module Geometry.Intercept(getChangeInX, getChangeInY, yIntercept, topCoderAreTheParallel, topCoderXRayIntercept, lineIntersection, onTheLine,
                          segmentIntersection, segmentIntersectionBreakDown, legalIntersection, perimetersContainIllegalIntersection,
                          perimetersContainLegalIntersections, segmentIntersectionT, runSegmentIntersectionT) where

import CornerPoints.CornerPoints(CornerPoints(..), cpointType, (+++))
import CornerPoints.Points (Point(..))

import Math.Distance(DistanceA(..), calculateDistanceA, getOrderingA)

import Helpers.Applicative(extractE, extractMaybe)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

{-
calculate the slope of the line
-get range of x
-get range of y
 -polarity will matter, is y <in/de>creasing as x increases or stays the same
 -so always calculate in same way, front to back, btm to top, or left to right
  -some CornerPoints, such as a <Left/Right...>Face could be measured in more than 1 of these ways, so do it in the given order

-}
getChangeInX :: CornerPoints -> Either String DistanceA
getChangeInX (BottomLeftLine b1 f1) =
  Right $ DistanceA $ (x_axis f1) - (x_axis b1)

getChangeInX (BackBottomLine b1 b4) =
  Right $ DistanceA $ (x_axis b1) - (x_axis b4)

getChangeInX cpoint =
  Left $ "Geometry.Intercept.getChangeInX: illegal or missing pattern match for " ++ (cpointType cpoint)

getChangeInY :: CornerPoints -> Either String DistanceA
getChangeInY (BottomLeftLine b1 f1) =
  Right $ DistanceA $ (y_axis f1) - (y_axis b1)

getChangeInY (BackBottomLine b1 b4) =
  Right $ DistanceA $ (y_axis b1) - (y_axis b4)

getChangeInY cpoint =
  Left $ "Geometry.Intercept.getChangeInY: illegal or missing pattern match for " ++ (cpointType cpoint)

divDistanceA :: DistanceA -> DistanceA -> DistanceA 
divDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 / d2

multDistanceA :: DistanceA -> DistanceA -> DistanceA 
multDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 * d2

addDistanceA :: DistanceA -> DistanceA -> DistanceA 
addDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 + d2

subtractDistanceA :: DistanceA -> DistanceA -> DistanceA 
subtractDistanceA (DistanceA d1) (DistanceA d2) =  DistanceA $ d1 - d2

setX :: Point -> DistanceA -> Point
setX (Point x y z) (DistanceA d) = Point (x + d) y z
--geomalgorithms.com


--topCoder.com solution
--NFG: Maybe just don't understand what they are saying.
topCoderAreTheParallel :: CornerPoints -> CornerPoints -> Either String Bool
topCoderAreTheParallel (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let
    
    a1 = getChangeInY  (BottomLeftLine b1 f1)
    a2 = getChangeInY  (BackBottomLine b1' b4')
    b1Cx = getChangeInX  (BottomLeftLine b1 f1)
    b2Cx = getChangeInX (BackBottomLine b1' b4')

    a1xb2Cx = multDistanceA <$> a1 <*> b2Cx
    a2xb1Cx = multDistanceA <$> a2 <*> b1Cx
  in
    (==) <$> a1xb2Cx <*> a2xb1Cx 
      

--cx for intercept of 2 Rays (inifinite lines)
topCoderXRayIntercept :: CornerPoints -> CornerPoints -> Either String (Maybe CornerPoints)
topCoderXRayIntercept (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let
    a1 = getChangeInY  (BottomLeftLine b1 f1)
    a2 = getChangeInY  (BackBottomLine b1' b4')
    b1Cx = getChangeInX  (BottomLeftLine b1 f1)
    b2Cx = getChangeInX (BackBottomLine b1' b4')
    
    a1xb2Cx = multDistanceA <$> a1 <*> b2Cx
    a2xb1Cx = multDistanceA <$> a2 <*> b1Cx

    --C = A*x1+B*y1
    c1' = multDistanceA <$> a1 <*> Right (DistanceA $ x_axis b1)  
    c1'' = multDistanceA <$>  b1Cx <*> Right (DistanceA $ y_axis b1)
    c1 = addDistanceA <$> c1' <*> c1''

    c2' = multDistanceA <$> a2 <*> Right (DistanceA $ x_axis b1')  
    c2'' = multDistanceA <$>  b2Cx <*> Right (DistanceA $ y_axis b1')
    c2 = addDistanceA <$> c2' <*> c2''

    det =  subtractDistanceA <$> a1xb2Cx <*> a2xb1Cx

    {-
    temp' =  multDistanceA <$> b2Cx <*> c1
    temp'' = multDistanceA <$> b1Cx <*> c2
    temp  = addDistanceA <$> temp' <*> temp''-}

    --xIntercept = divDistanceA <$> temp <*> a1xb2Cx
    xIntercept = divDistanceA <$> (subtractDistanceA <$> (multDistanceA <$> b2Cx <*> c1)  <*> (multDistanceA <$> b1Cx <*> c2)) <*> a1xb2Cx
    yIntercept = divDistanceA <$> (subtractDistanceA <$> (multDistanceA <$> a1 <*> c2)  <*> (multDistanceA <$> a2 <*> c1)) <*> a1xb2Cx

    cpointSet :: (Point -> CornerPoints) -> DistanceA -> DistanceA  -> DistanceA -> CornerPoints
    cpointSet constructor (DistanceA z) (DistanceA x) (DistanceA y) =
      constructor $ Point x y z
    --the actual point of intersection, provided they are not parallel.
    --have not yet checked for segment length. This will be done in next fx
    p1 = cpointSet (F1) (DistanceA 0) <$> xIntercept <*> yIntercept
    --if not parallel, then p1 else nothing.
    p1det =
      case topCoderAreTheParallel (BottomLeftLine b1 f1) (BackBottomLine b1' b4') of
        Left e -> Left e
        Right True -> Right Nothing
        Right False ->
          case p1 of
            Left e -> Left e
            Right cpoints -> Right $ Just cpoints
  in
    p1det
    
      


--my algorithm which is nfg
yIntercept :: CornerPoints -> CornerPoints -> Either String Point
yIntercept (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let bllchangeInY = getChangeInY  (BottomLeftLine b1 f1)
      bllchangeInX = getChangeInX  (BottomLeftLine b1 f1)
  
      bblchangeInY = getChangeInY  (BackBottomLine b1' b4')
      bblchangeInX = getChangeInX  (BackBottomLine b1' b4')
  
      rateOfChange = divDistanceA <$>  bllchangeInY <*> bblchangeInY
      diffInY = (y_axis f1) - (y_axis b1')  
      -- x_f1 = 
   
      {-
      (rate of change * diff in y of bll and bbl ) + x_axis f1
       ================temp======================
      -}
      temp = (multDistanceA) <$> (Right (DistanceA diffInY)) <*> rateOfChange
      temp2 = (addDistanceA) <$> temp <*> (Right (DistanceA (x_axis f1)))
      temp3 = setX <$> Right (f1) <*> temp2 
      
  in temp3
      --Left "filler to compile"

test (BottomLeftLine b1 f1) (BackBottomLine b1' b4') =
  let bllchangeInY = getChangeInY  (BottomLeftLine b1 f1)
      bllchangeInX = getChangeInX  (BottomLeftLine b1 f1)

      bblchangeInY = getChangeInY  (BackBottomLine b1' b4')
      bblchangeInX = getChangeInX  (BackBottomLine b1' b4')
  in  divDistanceA <$>  bllchangeInY <*> bblchangeInY

-- =================================================================================================
-- https://rosettacode.org/wiki/Find_the_intersection_of_two_lines#Haskell
--good for intersection of lines, but not segments
--testing shows this fx is nfg. Can produce the wrong point of intersection. Their test works, but it is all positive cood's.
-- | Do 2 inifinite lines intersect.
lineIntersection :: CornerPoints -> CornerPoints -> Either String (Maybe Point)

--lineIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
--  lineIntersectionForGenericLine b1 f1 b1' b4

lineIntersection (BottomLeftLine (Point ax ay az) (Point bx by bz) ) (BackBottomLine (Point px py pz) (Point qx qy qz)) =
  let (pqDX, abDX) = (px - qx, ax - bx)
      (pqDY, abDY) = (py - qy, ay - by)
      determinant = abDX * pqDY - abDY * pqDX
      f pq ab =
        ((((ax * by) - (ay * bx)) * pq) - 
         (((px * qy) - (py * qx)) * ab)) /
        determinant
  in case determinant of
       0 -> Right Nothing
       _ -> Right $ Just (Point (f pqDX abDX) (f pqDY abDY) 0)

lineIntersection (BottomRightLine b4 f4 ) (BackBottomLine b1 b4') =
  lineIntersectionForGenericLine b4 f4 b1 b4'

lineIntersection line1 line2 =
  Left $ "Geometry.Intercept.lineIntersection has missing pattern match for: " ++ (cpointType line1) ++ " and " ++ (cpointType line2)

--all <backBottom/BackTop/...>lines are made of the same amount of points so make them generic.
lineIntersectionForGenericLine :: Point -> Point -> Point -> Point -> Either String (Maybe Point)
lineIntersectionForGenericLine (Point ax ay az) (Point bx by bz) (Point px py pz) (Point qx qy qz) =
  let (pqDX, abDX) = (px - qx, ax - bx)
      (pqDY, abDY) = (py - qy, ay - by)
      determinant = abDX * pqDY - abDY * pqDX
      f pq ab =
        ((((ax * by) - (ay * bx)) * pq) - 
         (((px * qy) - (py * qx)) * ab)) /
        determinant
  in case determinant of
       0 -> Right Nothing
       _ -> Right $ Just (Point (f pqDX abDX) (f pqDY abDY) 0)
  
-- | Do to Line Segments intersect.
-- This is only segments, not rays or infinite lines.
segmentIntersection :: CornerPoints -> --advancingCpoint
                            CornerPoints -> --perimeter
                            Either String Bool
segmentIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
  segmentIntersectionForGenericLine b1 f1 b1' b4

-- BottomRightLine and BackBottomLine
segmentIntersection (BottomRightLine b4 f4) (BackBottomLine b1 b4') =
  segmentIntersectionForGenericLine b4 f4 b1 b4'

segmentIntersection line1 line2 =
  Left $ "Geometry.Intercept.segmentIntersection missing pattern match for: " ++ (cpointType line1) ++ " and " ++ (cpointType line2)
  
--All <BackFront/BottomLeft/...>Line have the same point pattern, so process them with generic function
segmentIntersectionForGenericLine :: Point -> 
                                     Point ->
                                     Point ->
                                     Point -> 
                                     Either String Bool
segmentIntersectionForGenericLine b1 f1 b1' b4  = do
  
  case lineIntersection (BottomLeftLine b1 f1) (BackBottomLine b1' b4) of
    Left e -> Left $ "Geometry.Intercept.segmentIntersection got error from lineIntersection: " ++ e
    Right Nothing -> Right False --Right Nothing. I no longer return a Maybe
    Right (Just pointOfIntersection) ->
      let
        advancingCpointSegmentIntersectsPerimeterLine ::  Either String Bool --add Either String once converted to bool
        advancingCpointSegmentIntersectsPerimeterLine =
          let
            pointingInCorrectDirectionLine :: Either String Bool
            pointingInCorrectDirectionLine =
              case getOrderingA <$> calculateDistanceA  f1 pointOfIntersection   <*> --length of B to intersection
                                   calculateDistanceA b1 pointOfIntersection --A to intersection,
              of
                Right GT -> Right False
                Right LT -> Right True
                  
                Right EQ -> Right True --not sure about this one. EQ would make advancingCpoint a Point
                Left e -> Left $ "Geometry.Intercept.segmentIntersection.advancingCpointSegmentIntersectsPerimeterLine.pointingInCorrectDirectionLine had an error: " ++ e
          in
          case getOrderingA <$> calculateDistanceA  b1 f1   <*> --length of BLL
                               calculateDistanceA b1 pointOfIntersection --f1 of bll,
          of
            Right LT -> Right False
            Right GT -> --True
              pointingInCorrectDirectionLine
            Right EQ -> --True
              pointingInCorrectDirectionLine
            Left e   -> Left $ "Geometry.Intercept.segmentIntersection.advancingCpointSegmentIntersectsPerimeterLine had an error : " ++ e
        
        perimeterSegmentIntersectsAdvancingCpointLine :: Either String Bool --add either later
        perimeterSegmentIntersectsAdvancingCpointLine = --LT if not long enough
          let
            pointingInCorrectDirectionLine :: Either String Bool
            pointingInCorrectDirectionLine =
              case getOrderingA <$> calculateDistanceA  b4 pointOfIntersection   <*> --length of B to intersection
                                   calculateDistanceA b1' pointOfIntersection --A to intersection,
              of
                Right GT -> Right False
                Right LT -> Right True
                Right EQ -> Right True --not sure about this one. EQ would make advancingCpoint a Point
                Left e -> Left $ "Geometry.Intercept.segmentIntersection.perimeterSegmentIntersectsAdvancingCpointLine.pointingInCorrectDirectionLine had an error: " ++ e
          in
          case getOrderingA <$> calculateDistanceA  b1' b4   <*> --length of BLL
                           calculateDistanceA b1' pointOfIntersection --f1 of bll,
          of
            Right LT -> Right False
            Right GT -> --True
              pointingInCorrectDirectionLine
            Right EQ -> --True
              pointingInCorrectDirectionLine
            Left e -> Left $ "Geometry.Intercept.segmentIntersection.perimeterSegmentIntersectsAdvancingCpointLine had an error: " ++ e
        
        process :: Bool -> Bool -> Bool
        process False False = False
        process False _ = False
        process  _ False = False
        process _ _ = True
          
      
      in
        process <$> advancingCpointSegmentIntersectsPerimeterLine <*> perimeterSegmentIntersectsAdvancingCpointLine


----------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------segmentIntersectionT-------------------------------------------------------------
------------------------------------------------------------replacement for segmentIntersection-----------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
--should have been MaybeT 

data SegementIntersectionError = SegementIntersectionError {errMsg :: String }
  deriving Eq

type SegmentIntersectionType = ExceptT SegementIntersectionError (Maybe) Point
--type SegmentIntersectionType = MaybeT  (Except SegementIntersectionError) Point

segmentIntersectionErrorHandler :: SegementIntersectionError -> SegmentIntersectionType 
segmentIntersectionErrorHandler error = do
  throwE error

processSegment :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
processSegment  processLines extraMsg advancingCpoint perimeter =
  (processSegementOrFail  processLines extraMsg advancingCpoint perimeter) `catchError` segmentIntersectionErrorHandler

processSegementOrFail :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
processSegementOrFail processLines extraMsg advancingCpoint perimeter =
  --this is where I would process the lines and cx for errors
  case processLines advancingCpoint perimeter of
    Left e -> 
      throwE (SegementIntersectionError $ extraMsg ++ " " ++ e)
    Right (maybePoint) -> lift maybePoint

{-
checkForPointOfIntersection :: String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
checkForPointOfIntersection =
  processSegment lineIntersection
-}
segmentIntersectionT :: CornerPoints -> --advancingCpoint
                        CornerPoints -> --perimeter
                        SegmentIntersectionType

segmentIntersectionT advancingCpoint perimeterLine =  do
  --lineIntersection <- checkForPointOfIntersection "check for line intersection" advancingCpoint perimeterLine
  lineIntersection <- (processSegment lineIntersection) "check for line intersection" advancingCpoint perimeterLine
  
  advancingCpointSegmentIntersectsPerimeterLine <- checkThatSegmentIntersectsPerimeter
                                                   "advancingCpoint segment intersects perimeter line"
                                                   (F1 lineIntersection) advancingCpoint 
  return advancingCpointSegmentIntersectsPerimeterLine
  --return lineIntersection


--will it be needed, or will runSegmentIntersectionT be used.
--converts to Either String (Maybe Point) as that is what all other fx's are running
extractSegmentIntersectionT :: CornerPoints -> --advancingCpoint
                               CornerPoints -> --perimeter
                               Either String (Maybe Point)
extractSegmentIntersectionT advancingCpoint perimeter = --do
  case runExceptT $ segmentIntersectionT advancingCpoint perimeter of
    Just (Right a) -> Right $ Just a
    Just (Left (SegementIntersectionError e)) -> Left e
    Nothing -> Right Nothing
  --case segmentIntersectionT advancingCpoint perimeter of
  --  Left (SegementIntersectionError msg) -> Left msg

--will it be needed, or will extractSegmentIntersectionT be used.
--converts to Either String (Maybe Point) as that is what all other fx's are running
runSegmentIntersectionT :: SegmentIntersectionType -> Either String (Maybe Point)
runSegmentIntersectionT segmentIntersectionType = --do
  case runExceptT segmentIntersectionType of
    Just (Right a) -> Right $ Just a
    Just (Left (SegementIntersectionError e)) -> Left e
    Nothing -> Right Nothing
  --case segmentIntersectionT advancingCpoint perimeter of
  --  Left (SegementIntersectionError msg) -> Left msg


checkThatSegmentIntersectsPerimeter :: String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
checkThatSegmentIntersectsPerimeter  =
  processSegment segmentIntersectsPoint

--Use F1 to contain the pointOfIntersection, so types align for SegmentIntersectionType
segmentIntersectsPoint :: CornerPoints -> CornerPoints -> Either String (Maybe Point)
segmentIntersectsPoint (F1 pointOfIntersection) (BottomLeftLine b1 f1) = do
  segmentIntersectsPointGenericLine pointOfIntersection b1 f1

segmentIntersectsPoint pointOfIntersection segment =
  Left $ "Geometry.Intercept.segmentIntersectsPoint missing pattern match for: pointOfIntersection: " ++ (cpointType pointOfIntersection) ++
         " segment: " ++ (cpointType segment)

{-
Given: pointOfIntersection :: Point
Point at which the segment intersects if it is a line of infinite length.

Given: p1 p2 :: Point Point
Two endpoints of the segment.

Task:
Check if the segment contains the pointOfIntersection.

Return:
Left e if calculateDistanceA throws an error.

Right Nothing if segment does not contain the pointOfIntersection.

Right Just pointOfIntersection: The pointOfIntersection which is contained by segment.
-}
segmentIntersectsPointGenericLine :: Point -> Point -> Point -> Either String (Maybe Point)
segmentIntersectsPointGenericLine pointOfIntersection p1 p2 = do
  distanceP2ToPointOfIntersection <- calculateDistanceA  p2 pointOfIntersection
  distanceP1ToPointOfIntersection <- calculateDistanceA  p1 pointOfIntersection
  distnaceP2ToP1 <- calculateDistanceA  p2 p1 

  pointOfIntersectionNew <-
    case (distnaceP2ToP1 >= distanceP2ToPointOfIntersection) && (distanceP1ToPointOfIntersection <= distnaceP2ToP1) of
      True -> Right $ Just pointOfIntersection
      False -> Right Nothing

  return pointOfIntersectionNew

{-
data SegementIntersectionError = SegementIntersectionError {errMsg :: String }
  deriving Eq

type SegmentIntersectionType = ExceptT SegementIntersectionError (Maybe) Point

segmentIntersectionErrorHandler :: SegementIntersectionError -> SegmentIntersectionType 
segmentIntersectionErrorHandler error = do
  throwE error

processSegment :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
processSegment  processLines extraMsg advancingCpoint perimeter =
  (processSegementOrFail  processLines extraMsg advancingCpoint perimeter) `catchError` segmentIntersectionErrorHandler

processSegementOrFail :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
processSegementOrFail processLines extraMsg advancingCpoint perimeter =
  --this is where I would process the lines and cx for errors
  case processLines advancingCpoint perimeter of
    Left e -> 
      throwE (SegementIntersectionError $ extraMsg ++ " " ++ e)
    Right (maybePoint) -> lift maybePoint

checkForPointOfInterSection :: String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
checkForPointOfInterSection =
  processSegment lineIntersection

segmentIntersectionT :: CornerPoints -> --advancingCpoint
                        CornerPoints -> --perimeter
                        SegmentIntersectionType

segmentIntersectionT advancingCpoint perimeterLine =  do
  lineIntersection <- checkForPointOfInterSection "check for line intersection" advancingCpoint perimeterLine
  
  advancingCpointSegmentIntersectsPerimeterLine <- checkThatSegmentIntersectsPerimeter
                                                   "advancingCpoint segment intersects perimeter line"
                                                   advancingCpoint (F1 lineIntersection)
  return lineIntersection

checkThatSegmentIntersectsPerimeter :: String -> CornerPoints -> CornerPoints -> SegmentIntersectionType
checkThatSegmentIntersectsPerimeter  =
  processSegment segmentIntersectsPoint

--Use F1 to contain the pointOfIntersection, so types align for SegmentIntersectionType
segmentIntersectsPoint :: CornerPoints -> CornerPoints -> Either String (Maybe Point)
segmentIntersectsPoint (F1 pointOfIntersection) (BottomLeftLine b1 f1) = do
  segmentIntersectsPointGenericLine pointOfIntersection b1 f1

segmentIntersectsPoint pointOfIntersection segment =
  Left $ "Geometry.Intercept.segmentIntersectsPoint missing pattern match for: pointOfIntersection: " ++ (cpointType pointOfIntersection) ++
         " segment: " ++ (cpointType segment)
  
segmentIntersectsPointGenericLine :: Point -> Point -> Point -> Either String (Maybe Point)
segmentIntersectsPointGenericLine pointOfIntersection b1 f1 = do
  distanceF1ToPointOfIntersection <- calculateDistanceA  f1 pointOfIntersection
  distanceB1ToPointOfIntersection <- calculateDistanceA  b1 pointOfIntersection
  distnaceF1ToB1 <- calculateDistanceA  f1 b1 

  pointOfIntersectionNew <-
    case (distnaceF1ToB1 >= distanceF1ToPointOfIntersection) && (distanceF1ToPointOfIntersection > distanceB1ToPointOfIntersection) of
      True -> Right $ Just pointOfIntersection
      False -> Right Nothing

  return pointOfIntersectionNew

-}

----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------- MaybeT Excpept segmentIntersect--------------------------------------------------------------
--but does it make any sense to have the Either inside the Maybe.
type SegmentIntersectionTypeME = MaybeT (Either String) Point


{-
data SegementIntersectionErrorME = SegementIntersectionErrorME {errMsgME :: String }
  deriving Eq

segmentIntersectionME :: CornerPoints -> --advancingCpoint
                        CornerPoints -> --perimeter
                        SegmentIntersectionTypeME
segmentIntersectionErrorHandlerME :: SegementIntersectionErrorME -> SegmentIntersectionTypeME 
segmentIntersectionErrorHandlerME error = do
  throwE error

processSegmentME :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> SegmentIntersectionTypeME
processSegmentME  processLines extraMsg advancingCpoint perimeter =
  (processSegementOrFailME  processLines extraMsg advancingCpoint perimeter) `catchError` segmentIntersectionErrorHandlerME

processSegementOrFailME :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> SegmentIntersectionTypeME
processSegementOrFailME processLinesME extraMsg advancingCpoint perimeter =
  --this is where I would process the lines and cx for errors
  case processLinesME advancingCpoint perimeter of
    Left e -> 
      throwE (SegementIntersectionErrorME $ extraMsg ++ " " ++ e)
    Right (maybePoint) -> lift maybePoint




segmentIntersectionME :: CornerPoints -> CornerPoints -> SegmentIntersectionTypeME
segmentIntersectionME advancingCpoint perimeterLine =  do
  --lineIntersection <- checkForPointOfIntersection "check for line intersection" advancingCpoint perimeterLine
  lineIntersection <- processSegmentME $ lineIntersection advancingCpoint perimeterLine
  
  advancingCpointSegmentIntersectsPerimeterLine <- segmentIntersectsPoint advancingCpoint (F1 lineIntersection)
  return advancingCpointSegmentIntersectsPerimeterLine
-}  
  
{- |
If the new advancingCpoint does not intersect the perimeter, then it is legal.
If it does intersect, but on a vertice, then it is legal. Should be the perimeter from which the new advancing line it to be built.
If it does intersect, not on a vertice, thin it is illegal, as advancingCpoint is probably crossing this perimenter on it's way to another.

-}
legalIntersection :: CornerPoints -> --advancingCpoint may intersect 
                     CornerPoints -> --a perimeter cpoint
                     Either String (Bool) --with and erorr, or legally: no intersection, or intersection at a vertice
legalIntersection CornerPointsNothing _ = Right True
legalIntersection _ CornerPointsNothing = Right True
legalIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4) =
  let
    
    
    hasNoIntersectionOrIsOnVertice :: CornerPoints -> -- perimeter
                        Maybe Point -> --point of intersection
                        Bool  -> --is pointOfIntersection in perimeter segment
                        Either String Bool
    hasNoIntersectionOrIsOnVertice  _ _ False = Right True
    hasNoIntersectionOrIsOnVertice  (BackBottomLine b1' b4) pointOfIntersection intersectsPerimeter =
        case pointOfIntersection of
          Nothing -> Right True
          Just pointOfIntersection ->
            Right $ (pointOfIntersection ==  b1') || (pointOfIntersection ==  b4) 
      
  in 
    extractE $
     hasNoIntersectionOrIsOnVertice (BackBottomLine b1' b4) <$>
                                              lineIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4)  <*>
                                              segmentIntersection (BottomLeftLine b1 f1 ) (BackBottomLine b1' b4)

legalIntersection (BottomRightLine b4 f4) (BackBottomLine b1' b4') =
  let
    
    
    hasNoIntersectionOrIsOnVertice :: CornerPoints -> -- perimeter
                        Maybe Point -> --point of intersection
                        Bool  -> --is pointOfIntersection in perimeter segment
                        Either String Bool
    hasNoIntersectionOrIsOnVertice  _ _ False = Right True
    hasNoIntersectionOrIsOnVertice  (BackBottomLine b1' b4') pointOfIntersection intersectsPerimeter =
        case pointOfIntersection of
          Nothing -> Right True
          Just pointOfIntersection ->
            Right $ (pointOfIntersection ==  b1') || (pointOfIntersection ==  b4') 
      
  in 
    extractE $
     hasNoIntersectionOrIsOnVertice (BackBottomLine b1' b4') <$>
                                              lineIntersection (BottomRightLine b4 f4 ) (BackBottomLine b1' b4')  <*>
                                              segmentIntersection (BottomRightLine b4 f4 ) (BackBottomLine b1' b4')

legalIntersection advancingCpoint perimeter =
  Left $ "Geometry.Intercept.legalIntersection has missing or illegal pattern match for advancingCpoint: " ++ (cpointType advancingCpoint) ++ " and  perimeter: " ++ (cpointType perimeter)

perimetersContainIllegalIntersection :: [[CornerPoints]] -> CornerPoints -> Either String Bool
perimetersContainIllegalIntersection (p:perimeters) cpoint =
  let
    perimetersContainIllegalIntersection' :: [CornerPoints] -> CornerPoints -> Either String Bool
    perimetersContainIllegalIntersection' (p:perimeter) cpoint =
      case legalIntersection cpoint p of
        Left e -> Left e
        Right True -> perimetersContainIllegalIntersection' perimeter cpoint
        Right False -> Right True
    perimetersContainIllegalIntersection' ([]) _ = Right False

  in
    case perimetersContainIllegalIntersection' p cpoint of
      Left e -> Left e
      Right False ->
        perimetersContainIllegalIntersection perimeters cpoint
      Right True -> Right True

perimetersContainIllegalIntersection [] _ = Right False

perimetersContainLegalIntersections :: [[CornerPoints]] -> CornerPoints -> Either String Bool
perimetersContainLegalIntersections perimeters cpoint = do
  areTheyLegal <- perimetersContainIllegalIntersection perimeters cpoint
  return $ not areTheyLegal
  


--temp fx to look inside of segmentIntersection
segmentIntersectionBreakDown :: CornerPoints -> CornerPoints -> Either String Ordering
segmentIntersectionBreakDown (BottomLeftLine (Point ax ay az) (Point bx by bz) ) (BackBottomLine (Point px py pz) (Point qx qy qz)) =
  case lineIntersection (BottomLeftLine (Point ax ay az) (Point bx by bz) ) (BackBottomLine (Point px py pz) (Point qx qy qz)) of
    Right Nothing -> Left "got Nothing intersection"
    Right (Just (Point x y z)) ->
      let
        notLongEnoughToIntersect = --LT if not long enough
          getOrderingA <$> calculateDistanceA  (Point ax ay az) (Point bx by bz)   <*> --length of BLL
                           calculateDistanceA (Point ax ay az) (Point x y z) --f1 of bll,
        pointingInCorrectDirections =
          getOrderingA <$> calculateDistanceA  (Point bx by bz) (Point x y z)   <*> --length of B to intersection
                           calculateDistanceA (Point ax ay az) (Point x y z) --A to intersection,

        test =
          case notLongEnoughToIntersect of
            Right LT -> Just False
            Right EQ ->  
              case pointingInCorrectDirections of
                Right LT -> Just True
                --need to make sure that 2nd point is long enough
            Right GT ->  
              case pointingInCorrectDirections of
                Right LT -> Just True
                --need to make sure that 2nd point is long enough
      in
       --test --filler to compile
       notLongEnoughToIntersect


-- http://bit-player.org/wp-content/extras/bph-publications/BeautifulCode-2007-Hayes.pdf
--is this not intersection of lines again, without knowing about segements
  --Not even that, as it takes three point, and sees if they are on the same line. But::::

onTheLine :: Point -> Point -> Point -> Double
onTheLine (Point x1 y1 z1) (Point x2 y2 z2) (Point x3 y3 z3) =
  (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)
