{-# LANGUAGE TemplateHaskell #-}
{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}
module Joiners.Delaunay(delaunay, removeIfUsed) where

import Data.Typeable

import CornerPoints.CornerPoints(CornerPoints(..),(+++),(++++), center, (<-|->), cpointType)
import CornerPoints.FaceExtraction(extractB1,extractFrontLeftLine, extractF1, extractBackLeftLine, contains )
import CornerPoints.FaceConversions(toLeftFace, raisedTo, toBottomLeftLine)

import Math.Distance(Distance(..), Distant, calculateDistance)


data Terminator =
 Early -- ^ Terminate as soon as 1 list has ended
 |
 Late  -- ^ Keep running till all lists have ended.

-- | Join together 2 [CornerPoints]
delaunay :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints]
delaunay [] _ _ _ = [CornerPointsError "empty outer points passed into delaunay"]
delaunay  _ [] [] [] = [CornerPointsError "at least 1 inner points required into delaunay"]
--use the head of first list and head of another list to create the intial back <Face/line>.
delaunay (i:is) (x:xs) [] [] =
  delaunay' (Right (is)) (Right (xs)) (Right []) (Right []) (i +++ x) ((i +++ x):[])
  

--Run (i:is) and at least 1 other list to the end of both lists.
--(i:is) is the outer CornerPoints defining the shape into which all the others will be inserted.
--delaunay' :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] ->  CornerPoints -> [CornerPoints] -> [CornerPoints]
delaunay' :: (Either String [CornerPoints]) -> (Either String [CornerPoints]) -> (Either String [CornerPoints]) -> (Either String [CornerPoints]) ->  CornerPoints -> [CornerPoints] -> [CornerPoints] 


delaunay'    (Left e) _ _ _ _ cpointsJoined =
  reverse $ (CornerPointsError e : cpointsJoined)
delaunay'    _ (Left e) _ _ _ cpointsJoined =
  reverse $ (CornerPointsError e : cpointsJoined)
delaunay'    _ _ (Left e) _ _ cpointsJoined =
  reverse $ (CornerPointsError e : cpointsJoined)
delaunay'    _  _ _ (Left e) _ cpointsJoined =
  reverse $ (CornerPointsError e : cpointsJoined)

delaunay'    (Right (i:is))            (Right (x:xs))            (Right [])                (Right [])                 currLine        cpointsJoined =
  let
    avgDistance = getAvgDistance (i:is)            (x:xs)            []                []                 currLine 
    newCurrLine = buildLine
                    currLine
                      (getOrderedCPoint
                        avgDistance
                        currLine
                        i
                      )
                      (getOrderedCPoint
                        avgDistance
                        currLine
                        x
                       )
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
  in
    case newCurrLine of
      Right newCurrLine' ->
        delaunay' (removeIfUsed (i:is) (newCurrLine')) (removeIfUsed (x:xs) (newCurrLine')) (Right []) (Right []) newCurrLine' (newCurrLine':cpointsJoined)
        
      Left e -> reverse $ (CornerPointsError e) : cpointsJoined

delaunay'  (Right (i:is)) (Right []) (Right []) (Right []) currLine cpointsJoined =
  let
    avgDistance = getAvgDistance (i:is)            []            []                []                 currLine 
    newCurrLine = buildLine
                    currLine
                      (getOrderedCPoint avgDistance currLine i)
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
  in
    case newCurrLine of
      Right newCurrLine' ->
        delaunay' (Right is) (Right []) (Right []) (Right []) newCurrLine' (newCurrLine':cpointsJoined)
        
      Left e -> reverse $ (CornerPointsError e) : cpointsJoined

delaunay' (Right []) (Right (x:xs)) (Right []) (Right []) currLine cpointsJoined =
  let
    avgDistance = getAvgDistance []            (x:xs)            []                []                 currLine 
    newCurrLine = buildLine
                    currLine
                      (getOrderedCPoint
                        avgDistance
                        currLine 
                        CornerPointsNothing) 
                      (getOrderedCPoint
                         avgDistance
                         currLine
                         x
                      )
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
  in
    case newCurrLine of
      Right newCurrLine' ->
        delaunay' (Right []) (Right xs) (Right []) (Right []) newCurrLine' (newCurrLine':cpointsJoined)
      Left e -> reverse $ (CornerPointsError e) : cpointsJoined

delaunay'  (Right []) (Right []) (Right []) (Right []) _ cpointsJoined =  reverse cpointsJoined


delaunay' _ _ _ _ _ cpointsJoined =
  reverse $ (CornerPointsError "delaunay cathcall used"):cpointsJoined

buildLine :: CornerPoints ->            OrderedCornerPoint -> OrderedCornerPoint -> OrderedCornerPoint -> OrderedCornerPoint ->  Either  String CornerPoints
{-           currLine                   i                     x                      y                    z                    -}

-- ======================================= Right<Point/Line> ====================================================
buildLine    ((BottomRightLine b4 f4))   (LTCPoint (F1 f1))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  (F1 f1) `raisedTo` (toBottomLeftLine (BottomRightLine b4 f4))

buildLine    ((RightFace b3 b4 f3 f4))   (LTCPoint (FrontLeftLine f1 f2))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  (FrontLeftLine f1 f2) `raisedTo` (toLeftFace $ RightFace b3 b4 f3 f4)
  
buildLine    ((BottomRightLine b4 f4))   (GTCPoint _)          (LTCPoint (B1 b1))     (GTCPoint _)         (GTCPoint _)    =
  (B1 b1) `raisedTo` (toBottomLeftLine $ BottomRightLine b4 f4)

buildLine    (RightFace b3 b4 f3 f4)   (GTCPoint _)          (LTCPoint (BackLeftLine b1 b2))     (GTCPoint _)         (GTCPoint _)    =
  (BackLeftLine b1 b2) `raisedTo` (toLeftFace $ RightFace b3 b4 f3 f4 )
  
buildLine    ((BottomRightLine b4 f4))   (GTCPoint _)          (EQCPoint (B1 b1))     (GTCPoint _)         (GTCPoint _)    =
  (B1 b1) `raisedTo` (toBottomLeftLine $ BottomRightLine b4 f4)

buildLine    (RightFace b3 b4 f3 f4)   (GTCPoint _)          (EQCPoint (BackLeftLine b1 b2))     (GTCPoint _)         (GTCPoint _)    =
  (BackLeftLine b1 b2) `raisedTo` (toLeftFace $ RightFace b3 b4 f3 f4)
  
buildLine    ((BottomRightLine b4 f4))   (EQCPoint (F1 f1))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  --Right $ BottomLeftLine b4 f1
  (F1 f1) `raisedTo`  (toBottomLeftLine $ BottomRightLine b4 f4)
  
buildLine    ((RightFace b3 b4 f3 f4))   (EQCPoint (FrontLeftLine f1 f2))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  (FrontLeftLine f1 f2) `raisedTo` (toLeftFace $ RightFace b3 b4 f3 f4)
  
  -- ================================ Left<Line/Face>======================================
buildLine    ((BottomLeftLine b1 f1)) (GTCPoint _)       (LTCPoint (B1 b1'))         (GTCPoint _)              (GTCPoint _)    =
  (B1 b1') `raisedTo` (BottomLeftLine b1 f1)

buildLine    (LeftFace b1 b2 f1 f2)    (GTCPoint _)       (LTCPoint (BackLeftLine b1' b2'))         (GTCPoint _)              (GTCPoint _)    =
  (BackLeftLine b1' b2') `raisedTo` (LeftFace b1 b2 f1 f2) 

buildLine    ((BottomLeftLine b1 f1)) (LTCPoint (F1 f1')) (GTCPoint _)                (GTCPoint _)              (GTCPoint _)    =
  (F1 f1') `raisedTo` (BottomLeftLine b1 f1)

buildLine    (LeftFace b1 b2 f1 f2)      (LTCPoint (FrontLeftLine f1' f2'))  (GTCPoint _)        (GTCPoint _)              (GTCPoint _)    =
  (FrontLeftLine f1' f2') `raisedTo` (LeftFace b1 b2 f1 f2)

buildLine    ((BottomLeftLine b1 f1))    (EQCPoint (F1 f1'))        (GTCPoint _)                (GTCPoint _)              (GTCPoint _)    =
  (F1 f1') `raisedTo` (BottomLeftLine b1 f1)

buildLine    (LeftFace b1 b2 f1 f2)      (EQCPoint (FrontLeftLine f1' f2'))  (GTCPoint _)        (GTCPoint _)              (GTCPoint _)    =
  (FrontLeftLine f1' f2') `raisedTo` (LeftFace b1 b2 f1 f2)
  
buildLine    ((BottomLeftLine b1 f1)) (GTCPoint _) (EQCPoint (B1 b1')) (GTCPoint _)  (GTCPoint _)   =
  (B1 b1') `raisedTo` (BottomLeftLine b1 f1)

buildLine    (LeftFace b1 b2 f1 f2)    (GTCPoint _)       (EQCPoint (BackLeftLine b1' b2'))         (GTCPoint _)              (GTCPoint _)    =
  (BackLeftLine b1' b2') `raisedTo` (LeftFace b1 b2 f1 f2) 

--look for the missing pattern match
buildLine currLine i x y z =
  let
    currLineType = "currLine: " ++ ( cpointType currLine)
    iType = " i: " ++ (cpointType $ fromOrderedCornerPoint i)
    xType = " x: " ++ (cpointType $ fromOrderedCornerPoint x)
    yType = " y: " ++ (cpointType $ fromOrderedCornerPoint y)
    zType = " z: " ++ (cpointType $ fromOrderedCornerPoint z)
    
  in
    Left $ currLineType ++ iType ++ xType ++ yType ++ zType

getAvgDistance :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> CornerPoints -> Distance
getAvgDistance    (i:is)            (x:xs)            []                []                currLine        =
  let
    iDist = calculateDistance (center i) (center currLine)
    xDist = calculateDistance (center x) (center currLine)
  in
    case iDist of
      (Distance d) ->
        case xDist of
          (Distance d') -> Distance $ (d + d')/2
          NoDistance -> NoDistance
      NoDistance -> NoDistance

getAvgDistance    []            (x:xs)            []                []                currLine        =
  calculateDistance (center x) (center currLine)
getAvgDistance  (i:is)  []          []                []                currLine        =
  calculateDistance (center i) (center currLine)

getOrderedCPoint :: Distance -> CornerPoints -> CornerPoints -> OrderedCornerPoint
getOrderedCPoint    avgDist     advanceFromThisLine        orderThisCpoint =
  let
    myDist = calculateDistance
             (center advanceFromThisLine)
             (center orderThisCpoint)
    getAvgDistanceStatus :: Distance    -> Distance -> Ordering
    getAvgDistanceStatus _ NoDistance = GT
    getAvgDistanceStatus NoDistance _ = GT
    getAvgDistanceStatus    (Distance avgDistance)    (Distance dist)
      | (dist) > (avgDistance)  = GT
      | (dist) < (avgDistance)  = LT
      | otherwise = EQ
  in
    case getAvgDistanceStatus avgDist myDist of
      GT -> GTCPoint orderThisCpoint
      LT -> LTCPoint orderThisCpoint
      EQ -> EQCPoint orderThisCpoint

{-
--should be able to replace the front/back versions
cpoint: The CornerPoint that was created during the join.
(x:xs): One of the [CornerPoints] that are to be joined.

If cpoint `contains` x, then x was used and gets removed from the list as it has now been 'joined'.
-}
removeIfUsed :: [CornerPoints] -> CornerPoints -> Either String [CornerPoints]
removeIfUsed (x:xs) cpoint =
  case ( cpoint `contains` (x)) of
    Right True ->  Right xs
    Right False -> Right (x:xs)
    Left e -> Left e
{-
removeHeadIfFrontUsed :: [CornerPoints] -> CornerPoints -> Either String [CornerPoints]
removeHeadIfFrontUsed ((F1 f1'):xs) (BottomLeftLine b1 f1) =
  case ((F1 f1') == (extractF1 (BottomLeftLine b1 f1))) of
    True -> (Right xs)
    False -> (Right $ (F1 f1'):xs)
removeHeadIfFrontUsed ((FrontLeftLine f1' f2'):xs) (LeftFace b1 b2 f1 f2) =
  case ((FrontLeftLine f1' f2') == (extractFrontLeftLine )(LeftFace b1 b2 f1 f2)) of
    True -> Right xs
    False -> Right $ (FrontLeftLine f1' f2'):xs
--the orig that worked with bottom points
--now make it an error
removeHeadIfFrontUsed (x:xs) currLine =
  Left $ "unmatched removeHeadIfFrontUsed: list: " ++ ( cpointType x) ++ " currLine: " ++ ( cpointType currLine)

removeHeadIfBackUsed :: [CornerPoints] -> CornerPoints -> Either String [CornerPoints]
removeHeadIfBackUsed ((B1 b1'):xs) (BottomLeftLine b1 f1) =
  case ((B1 b1') == (extractB1 (BottomLeftLine b1 f1))) of
    True -> (Right xs)
    False -> (Right $ (B1 b1'):xs)
removeHeadIfBackUsed ((BackLeftLine b1' f1'):xs) (LeftFace b1 b2 f1 f2) =
  case ((BackLeftLine b1' f1') == (extractBackLeftLine )(LeftFace b1 b2 f1 f2)) of
    True -> (Right xs)
    False -> Right $ (BackLeftLine b1' f1'):xs
--the orig that worked with bottom points

removeHeadIfBackUsed (x:xs) currLine =
  Left $ "unmatched removeHeadIfBackUsed: list: " ++ ( cpointType x) ++ " currLine: " ++ ( cpointType currLine)

-}  
--Associate a CornerPoints with Ordering.
--Used by buildLine pattern matching to decide which CornerPoint is to be used to advance.
data OrderedCornerPoint =
  LTCPoint {ltCpoint :: CornerPoints}
  |
  EQCPoint {eqCpoint :: CornerPoints}
  |
  GTCPoint {gtCpoint :: CornerPoints}


fromOrderedCornerPoint :: OrderedCornerPoint -> CornerPoints
fromOrderedCornerPoint (LTCPoint cpoint) = cpoint
fromOrderedCornerPoint (EQCPoint cpoint) = cpoint
fromOrderedCornerPoint (GTCPoint cpoint) = cpoint

