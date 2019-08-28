module Geometry.PolarIntercept(lineIntersectionXY,
                               runPolarIntersect, lineIntersectionXYT, lineIntersectionViaSlopeLineXYT) where

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points (Point(..))
import qualified TypeClasses.Showable as TS
import Geometry.Rotation(rotatePointAroundZAxis, rotateCornerPointAroundZAxis)
import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), getXYAngle, getQuadrant, Quadrant(..))

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

-- | Do two infinite lines intersect on the x y plane.
-- If so, return the point, otherwise, nothing.
-- The only know way to fail, is to have missing pattern matches.



{-return maybe point for now, but see how better to fit it into the segment transformer monad-}
lineIntersectionXY :: CornerPoints -> CornerPoints -> Either String (Maybe Point)

lineIntersectionXY (BottomRightLine b4 f4 ) (BackBottomLine b1 b4') =
  lineIntersectionForGenericLineXY b4 f4 b1 b4'

lineIntersection line1 line2 =
  Left $ "Geometry.Intercept.lineIntersection has missing pattern match for: " ++ (TS.showConstructor line1) ++ " and " ++ (TS.showConstructor line2)

--all <backBottom/BackTop/...>lines are made of the same amount of points so make them generic.
--currently returns Nothing in Quad 4 for lines that should intercept
lineIntersectionForGenericLineXY :: Point -> Point -> Point -> Point -> Either String (Maybe Point)
lineIntersectionForGenericLineXY (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2a_x line2a_y line2a_z) (Point line2b_x line2b_y line2b_z) =
  let --line1XYangle  = getXYAngle (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z)
      --line1Quadrant = getQuadrant line1XYangle
      line1QAngle = getQuadrantAngle $ getXYAngle (Point line1b_x line1b_y line1b_z) (Point line1a_x line1a_y line1a_z)
      
      point2aAdjustedForAngleOfLine1 =
        rotatePointAroundZAxis (angle line1QAngle) (Point line1a_x line1a_y line1a_z) (Point line2a_x line2a_y line2a_z)
      

      point2bAdjustedForAngleOfLine1 =
        rotatePointAroundZAxis (angle line1QAngle) (Point line1a_x line1a_y line1a_z) (Point line2b_x line2b_y line2b_z) 

      point2aXYAngleInRelationToPoint1a = getXYAngle (Point line1a_x line1a_y line1a_z)  point2aAdjustedForAngleOfLine1
      point2aQuadrantInRelationToPoint1a = getQuadrant point2aXYAngleInRelationToPoint1a

      point2bXYAngleInRelationToPoint1a = getXYAngle (Point line1a_x line1a_y line1a_z) point2bAdjustedForAngleOfLine1
      point2bQuadrantInRelationToPoint1a = getQuadrant point2bXYAngleInRelationToPoint1a

      cxQuad :: (Quadrant, Quadrant) -> Either String (Maybe Point)
      cxQuad quads
        | quads == (Quadrant2, Quadrant3) = Right $ Just $ Point 0 0 0
        | otherwise = Right Nothing

      cxOnTheLine =
        (onTheLine (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2a_x line2a_y line2a_z) )
        ||
        ( onTheLine (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2b_x line2b_y line2b_z) )
  in
    case cxOnTheLine of
      True -> Right $ Just $ Point 0 0 0
      False ->
        cxQuad (point2aQuadrantInRelationToPoint1a,point2bQuadrantInRelationToPoint1a)



  
{-before going ExceptT Maybe Point
lineIntersectionForGenericLineXY :: Point -> Point -> Point -> Point -> Either String (Maybe Point)
lineIntersectionForGenericLineXY (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2a_x line2a_y line2a_z) (Point line2b_x line2b_y line2b_z) =
  let --line1XYangle  = getXYAngle (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z)
      --line1Quadrant = getQuadrant line1XYangle
      line1QAngle = getQuadrantAngle $ getXYAngle (Point line1b_x line1b_y line1b_z) (Point line1a_x line1a_y line1a_z)
      
      point2aAdjustedForAngleOfLine1 =
        rotatePointAroundZAxis (angle line1QAngle) (Point line1a_x line1a_y line1a_z) (Point line2a_x line2a_y line2a_z)
      

      point2bAdjustedForAngleOfLine1 =
        rotatePointAroundZAxis (angle line1QAngle) (Point line1a_x line1a_y line1a_z) (Point line2b_x line2b_y line2b_z) 

      point2aXYAngleInRelationToPoint1a = getXYAngle (Point line1a_x line1a_y line1a_z)  point2aAdjustedForAngleOfLine1
      point2aQuadrantInRelationToPoint1a = getQuadrant point2aXYAngleInRelationToPoint1a

      point2bXYAngleInRelationToPoint1a = getXYAngle (Point line1a_x line1a_y line1a_z) point2bAdjustedForAngleOfLine1
      point2bQuadrantInRelationToPoint1a = getQuadrant point2bXYAngleInRelationToPoint1a

      cxQuad :: (Quadrant, Quadrant) -> Either String (Maybe Point)
      cxQuad quads
        | quads == (Quadrant2, Quadrant3) = Right $ Just $ Point 0 0 0
        | otherwise = Right Nothing

      cxOnTheLine =
        (onTheLine (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2a_x line2a_y line2a_z) )
        ||
        ( onTheLine (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2b_x line2b_y line2b_z) )
  in
    case cxOnTheLine of
      True -> Right $ Just $ Point 0 0 0
      False ->
        cxQuad (point2aQuadrantInRelationToPoint1a,point2bQuadrantInRelationToPoint1a)

-}

{- |
See if 2 infinite lines intersect.
If yes, Right $ Just $ point of intersection
If no, Right Nothing
If error: PolarInterceptError
-}
lineIntersectionXYT :: CornerPoints -> CornerPoints -> PolarInterceptType
lineIntersectionXYT (BottomRightLine b4 f4 ) (BackBottomLine b1 b4') =
  lineIntersectionForGenericLineXYT b4 f4 b1 b4'

--cx for shared endpoints before analyzing deeper
lineIntersectionForGenericLineXYT :: Point -> Point -> Point -> Point -> PolarInterceptType
lineIntersectionForGenericLineXYT (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2a_x line2a_y line2a_z) (Point line2b_x line2b_y line2b_z) = do
  let
    p1a = Point line1a_x line1a_y 0
    p1b = Point line1b_x line1b_y 0
    p2a = Point line2a_x line2a_y 0
    p2b = Point line2b_x line2b_y 0

    temp =
      case p1a == p2a of
      True -> Just (Point line1a_x line1a_y line1a_z)
      False ->
        case p1a == p2b of
          True -> Just (Point line1a_x line1a_y line1a_z)
          False ->
            case p1b == p2a of
              True -> Just (Point line1b_x line1b_y line1b_z)
              False ->
                case p1b == p2b of
                  True -> Just (Point line1b_x line1b_y line1b_z)
                  False -> Nothing
  
  case temp of
      Nothing -> lineIntersectionForGenericLineXYT' (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2a_x line2a_y line2a_z) (Point line2b_x line2b_y line2b_z)
      Just point -> return point
      
  
lineIntersectionForGenericLineXYT' :: Point -> Point -> Point -> Point -> PolarInterceptType
lineIntersectionForGenericLineXYT' (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z) (Point line2a_x line2a_y line2a_z) (Point line2b_x line2b_y line2b_z) = do
  let
    line1QAngle = getQuadrantAngle $ getXYAngle  (Point line1a_x line1a_y line1a_z) (Point line1b_x line1b_y line1b_z)

    --Use the processLineIntercept in case this fails because of a missing pattern match.
    --The quadrant of the angle, dictates how the point will be rotated.
    --It gets rotated so the base line (line1QAngle) is on the y-axis
    rotatePointAroundZAxis' rotateAroundMe rotateMe ignoredCpoint ignoredCpoint2 =
        case line1QAngle of
          Quadrant2Angle angle ->
            Right $ Just $ rotatePointAroundZAxis (angle) rotateAroundMe rotateMe
          Quadrant4Angle angle ->
            Right $ Just $ rotatePointAroundZAxis (angle) rotateAroundMe rotateMe
          otherwise -> Left $ "Geometry.PolarIntercept.lineIntersectionXYT.lineIntersectionForGenericLineXYT missing pattern match for QuadrantAngle : " ++
                               (show line1QAngle)
    cxQuad :: Point -> Point -> Point -> CornerPoints -> CornerPoints -> Either String (Maybe Point)
    --cxQuad :: (Quadrant, Quadrant) -> Either String (Maybe Point)
    cxQuad point1 point2 point3 ignoredCpoint ignoredCpoint2 =
      case (getQuadrant $ getXYAngle point1 point2 , getQuadrant $ getXYAngle point1 point3) of
        --keep these aroung in case it does not work out
        --(Quadrant1, Quadrant3) -> Right $ Just $ Point 0 0 0 --this worked when shared end point.
        --(Quadrant1, Quadrant4) -> Right $ Just $ Point 0 0 0
        --(Quadrant2, Quadrant3) -> Right $ Just $ Point 0 0 0
        --(Quadrant3, Quadrant1) -> Right $ Just $ Point 0 0 0 --this worked when shared end point.
        --shared endpoints have already been taken care of so only these should not intersect.
        (Quadrant1, Quadrant1) -> Right Nothing
        (Quadrant2, Quadrant2) -> Right Nothing
        (Quadrant3, Quadrant3) -> Right Nothing
        (Quadrant4, Quadrant4) -> Right Nothing
        --and all else intersect
        (quadrantX, quadranX') -> Right $ Just $ Point 0 0 0 
        {-
        (quadrantX, quadranX') -> Left $ "missing pattern match for Geometry.PolarIntercept.lineIntersectionXYT.lineIntersectionForGenericLineXYT.cxQuad for: " ++
                                         (show (quadrantX, quadranX')) -}
    
  point2aAdjustedForAngleOfLine1 <- (processLineIntercept (rotatePointAroundZAxis'  (Point line1a_x line1a_y line1a_z) (Point line2a_x line2a_y line2a_z)))
                                    "point2aAdjustedForAngleOfLine1: "
                                    CornerPointsNothing CornerPointsNothing

  point2bAdjustedForAngleOfLine1 <- (processLineIntercept (rotatePointAroundZAxis'  (Point line1a_x line1a_y line1a_z) (Point line2b_x line2b_y line2b_z)))
                                    "point2bAdjustedForAngleOfLine1: "
                                    CornerPointsNothing CornerPointsNothing

  pointOfIntersectionAtOriginForNow <-
    (processLineIntercept (cxQuad (Point line1a_x line1a_y line1a_z) point2aAdjustedForAngleOfLine1 point2bAdjustedForAngleOfLine1 ))
    "pointOfIntersectionAtOriginForNow: "
    CornerPointsNothing CornerPointsNothing
  
  
        
  return pointOfIntersectionAtOriginForNow

----------------------------------------------------------- slope intercept ---------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------

lineIntersectionViaSlopeLineXYT :: CornerPoints -> CornerPoints -> PolarInterceptType
lineIntersectionViaSlopeLineXYT (BottomRightLine b4 f4 ) (BackBottomLine b1 b4') =
  lineIntersectionViaSlopeLineXYT' (Line b4 f4) (Line b1 b4')

lineIntersectionViaSlopeLineXYT a b =
  throwE (PolarInterceptError $ "Geometry.PolarIntercept.lineIntersectionViaSlopeLineXYT missing pattern match for " ++ (show a) ++ " and " ++ (show b))

--Use Point.Line so as to be generic for any 2 lines
lineIntersectionViaSlopeLineXYT' :: Point -> Point ->  PolarInterceptType
lineIntersectionViaSlopeLineXYT' (Line p1 p2)(Line q1 q2) = do
  
     let
       line1QAngle = getQuadrantAngle $ getXYAngle  p1 p2
     
       --Use the processLineIntercept in case this fails because of a missing pattern match.
       --The quadrant of the angle, dictates how the point will be rotated.
       --It gets rotated so the base line (line1QAngle) is on the y-axis. Should be handled in Geometry.Rotations
       rotatePointAroundZAxis' :: Angle -> Point -> Point -> CornerPoints -> CornerPoints -> Either String (Maybe Point)
       rotatePointAroundZAxis' qAngle rotateAroundMe (Point x y z) ignoredCpoint ignoredCpoint2 =
           case qAngle of
             Angle angle' ->
               Right $ Just $ rotatePointAroundZAxis (angle') rotateAroundMe (Point x y z)
             Quadrant2Angle angle' ->
               Right $ Just $ rotatePointAroundZAxis (angle') rotateAroundMe (Point x y z)
             Quadrant4Angle angle' ->
               Right $ Just $ rotatePointAroundZAxis (angle') rotateAroundMe (Point x y z)
             otherwise -> Left $ "Geometry.PolarIntercept.lineIntersectionViaSlopeLineXYT'.rotatePointAroundZAxis missing pattern match for QuadrantAngle : " ++
                                  (show line1QAngle)
      
       rotatePointAroundZAxis' qAngle rotateAroundMe (Line p1 p2) ignoredCpoint ignoredCpoint2 =
           case qAngle of
             Angle angle' ->
               Right $ Just $ Line (rotatePointAroundZAxis angle' rotateAroundMe p1) (rotatePointAroundZAxis angle' rotateAroundMe p2)
             Quadrant2Angle angle' ->
               Right $ Just $ Line (rotatePointAroundZAxis angle' rotateAroundMe p1) (rotatePointAroundZAxis angle' rotateAroundMe p2)
             Quadrant4Angle angle' ->
               Right $ Just $ Line (rotatePointAroundZAxis angle' rotateAroundMe p1) (rotatePointAroundZAxis angle' rotateAroundMe p2)
             otherwise -> Left $ "Geometry.PolarIntercept.lineIntersectionViaSlopeLineXYT'.rotatePointAroundZAxis missing pattern match for QuadrantAngle : " ++
                                 (show line1QAngle)

       --
       calcYintercept :: Point -> CornerPoints -> CornerPoints -> Either String (Maybe Point)
       calcYintercept (Line (Point x1 y1 _) (Point x2 y2 _)) _ _ =
         Right $ Just $ Point 0 (calcYintercept' x1 y1 x2 y2) 0
       calcYintercept' :: Double -> Double -> Double -> Double -> Double
       calcYintercept' x1 y1 x2 y2  =
         y1 - (((y2 - y1)/(abs(x2 - x1))) * (abs x1))
     
     line1AlignedToYaxis <- (processLineIntercept (rotatePointAroundZAxis' line1QAngle p1 (Line p1 p2)))
                            "line1AlignedToYaxis"
                            CornerPointsNothing CornerPointsNothing
     
     line2AlignedToYaxis <- (processLineIntercept (rotatePointAroundZAxis' line1QAngle p1 (Line q1 q2)))
                            "line2AlignedToYaxis"
                            CornerPointsNothing CornerPointsNothing

     intercept <- 
                   (processLineIntercept (calcYintercept line2AlignedToYaxis))
                   "intercept"
                    CornerPointsNothing CornerPointsNothing

     interceptRotatedBack <- (processLineIntercept (rotatePointAroundZAxis' (Angle $ 360 - (angle line1QAngle)) p1 intercept))
                            "interceptRotatedBack"
                            CornerPointsNothing CornerPointsNothing

     
     let linesAreBothOnSameYaxis = ((x_axis $ _p1 line2AlignedToYaxis) == (x_axis $ _p2 line2AlignedToYaxis)) &&
                         ((x_axis $ _p1 line1AlignedToYaxis) == (x_axis $ _p2 line2AlignedToYaxis))
         linesAreParallelAndSoWillNeverIntersect = ((x_axis $ _p1 line2AlignedToYaxis) == (x_axis $ _p2 line2AlignedToYaxis))
     --return  line2AlignedToYaxis
     case linesAreBothOnSameYaxis of
       True -> return (Line p1 p2)
       False ->
         case linesAreParallelAndSoWillNeverIntersect of
           True -> lift Nothing--return noIntercept
           False -> return interceptRotatedBack --Now find the intercept using slope

{- |
Given:
 CornerPoints x 2
   See if these 2 cornerpoints intersect.
   If yes return the point of intersection.
   If not, return Nothing
-}
segmentIntersectViaSlopeXYT :: CornerPoints -> CornerPoints -> PolarInterceptType
segmentIntersectViaSlopeXYT (BottomRightLine b4 f4 ) (BackBottomLine b1 b4') =
  segmentIntersectViaSlopeXYT' (Line b4 f4) (Line b1 b4')

segmentIntersectViaSlopeXYT a b = 
  throwE (PolarInterceptError $ "Geometry.PolarIntercept.segmentIntersectViaSlopeXYT missing pattern match for " ++ (show a) ++ " and " ++ (show b))
  
segmentIntersectViaSlopeXYT' :: Point -> Point -> PolarInterceptType
segmentIntersectViaSlopeXYT' (Line p1 p2) (Line q1 q2) = do
  pointOfIntersection <- lineIntersectionViaSlopeLineXYT' (Line p1 p2) (Line q1 q2)

  let inRange :: Point -> Point -> Bool 
      inRange (Point x_pntOfInt _ _) (Line (Point x1 _ _)(Point x2 _ _)) =
        (((x_pntOfInt <= x1) && (x_pntOfInt >= x2)) || ((x_pntOfInt >= x1) && (x_pntOfInt <= x2))) 
         
  case inRange pointOfIntersection (Line q1 q2) of
    True -> return pointOfIntersection
    False -> lift Nothing
  --return pointOfIntersection
  


-- http://bit-player.org/wp-content/extras/bph-publications/BeautifulCode-2007-Hayes.pdf
--is this not intersection of lines again, without knowing about segements
  --Not even that, as it takes three point, and sees if they are on the same line. But::::
--order does not matter, is checking if they are all in a line, in any order
--return 0 if true(all in a line) else some other value
onTheLine :: Point -> Point -> Point -> Bool
onTheLine (Point x1 y1 z1) (Point x2 y2 z2) (Point x3 y3 z3) =
   ((x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)) == 0 
    
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------- got ExceptT Maybe Point ------------------------------------------------------------------------
data PolarInterceptError = PolarInterceptError {errMsg :: String }
  deriving Eq

type PolarInterceptType = ExceptT PolarInterceptError (Maybe) Point

polarInterceptErrorHandler :: PolarInterceptError -> PolarInterceptType 
polarInterceptErrorHandler error = do
  throwE error

processLineIntercept :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> PolarInterceptType
processLineIntercept  processLines extraMsg line1 line2 =
  (processLineInterceptOrFail  processLines extraMsg line1 line2) `catchError` polarInterceptErrorHandler

processLineInterceptOrFail :: (CornerPoints -> CornerPoints -> Either String (Maybe Point)) -> String -> CornerPoints -> CornerPoints -> PolarInterceptType
processLineInterceptOrFail processLines extraMsg line1 line2 =
  --this is where I would process the lines and cx for errors
  case processLines line1 line2 of
    Left e -> 
      throwE (PolarInterceptError $ extraMsg ++ " " ++ e)
    Right (maybePoint) -> lift maybePoint

extractLineIntersect :: (CornerPoints -> CornerPoints -> PolarInterceptType) ->
                         CornerPoints -> --advancingCpoint
                         CornerPoints -> --perimeter
                         Either String (Maybe Point)
extractLineIntersect lineIntersection line1 line2 = 
  case runExceptT $ lineIntersection line1 line2 of
    Just (Right a) -> Right $ Just a
    Just (Left (PolarInterceptError e)) -> Left e
    Nothing -> Right Nothing

-- | Converts from PolarInterceptType to Either Maybe Point
runPolarIntersect :: (CornerPoints -> CornerPoints -> PolarInterceptType) ->
                         CornerPoints -> --advancingCpoint
                         CornerPoints -> --perimeter
                         Either String (Maybe Point)
runPolarIntersect polarIntersect line1 line2 = 
  case runExceptT $ polarIntersect line1 line2 of
    Just (Right a) -> Right $ Just a
    Just (Left (PolarInterceptError e)) -> Left e
    Nothing -> Right Nothing
