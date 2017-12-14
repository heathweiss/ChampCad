{-# LANGUAGE TemplateHaskell #-}
{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}
module Joiners.Delaunay(delaunay) where

import Data.Typeable

import CornerPoints.CornerPoints(CornerPoints(..),(+++), center, (<-|->), cpointType)
import CornerPoints.FaceExtraction(extractB1,extractFrontLeftLine, extractF1, extractBackLeftLine )

import Math.Distance(Distance(..), Distant, calculateDistance)

import Control.Lens

makeLenses ''Distance

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
  delaunay' (is) (xs) [] [] (i +++ x) ((i +++ x):[])
  

--Run (i:is) and at least 1 other list to the end of both lists.
--(i:is) is the outer CornerPoints defining the shape into which all the others will be inserted.
delaunay' :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] ->  CornerPoints -> [CornerPoints] -> [CornerPoints] 
delaunay'    (i:is)            (x:xs)            []                []                 currLine        cpointsJoined =
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
    --delaunay' (removeHeadIfFrontUsed (i:is) (F1 $ f1 newCurrLine)) (removeHeadIfBackUsed (x:xs) (B1 $ b1 newCurrLine)) [] [] newCurrLine (newCurrLine:cpointsJoined)
    case newCurrLine of
      Right newCurrLine' ->
        delaunay' (removeHeadIfFrontUsed (i:is) (newCurrLine')) (removeHeadIfBackUsed (x:xs) (newCurrLine')) [] [] newCurrLine' (newCurrLine':cpointsJoined)
        
      Left e -> reverse $ (CornerPointsError e) : cpointsJoined



delaunay'  (i:is) [] [] [] currLine cpointsJoined =
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
        delaunay' is [] [] [] newCurrLine' (newCurrLine':cpointsJoined)
        
      Left e -> reverse $ (CornerPointsError e) : cpointsJoined

delaunay' [] (x:xs) [] [] currLine cpointsJoined =
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
        delaunay' [] xs [] [] newCurrLine' (newCurrLine':cpointsJoined)
      Left e -> reverse $ (CornerPointsError e) : cpointsJoined

delaunay'  [] [] [] [] _ cpointsJoined =  reverse cpointsJoined

buildLine :: CornerPoints ->            OrderedCornerPoint -> OrderedCornerPoint -> OrderedCornerPoint -> OrderedCornerPoint ->  Either  String CornerPoints
{-           currLine                   i                     x                      y                    z                    -}

-- ======================================= Right<Point/Line> ====================================================
buildLine    ((BottomRightLine b4 _))   (LTCPoint (F1 f1))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  Right $ BottomLeftLine b4 f1
--add face
buildLine    ((RightFace b3 b4 _ _))   (LTCPoint (FrontLeftLine f1 f2))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  Right $ LeftFace b4 b3 f1 f2
  --Left "stopped ((RightFace b3 b4 _ _))   (LTCPoint (FrontLeftLine f1 f2))  (GTCPoint _) "

buildLine    ((BottomRightLine _ f4))   (GTCPoint _)          (LTCPoint (B1 b1))     (GTCPoint _)         (GTCPoint _)    =
  Right $ BottomLeftLine  b1 f4
--add face
buildLine    (RightFace _ _ f3 f4)   (GTCPoint _)          (LTCPoint (BackLeftLine b1 b2))     (GTCPoint _)         (GTCPoint _)    =
  Right $ LeftFace b1 b2 f4 f3
        --LeftFace b1 b2 f1 f2
  --Left "stopped (RightFace _ _ f3 f4)   (GTCPoint _)          (LTCPoint (BackLeftLine b1 b2))"
  
buildLine    ((BottomRightLine _ f4))   (GTCPoint _)          (EQCPoint (B1 b1))     (GTCPoint _)         (GTCPoint _)    =
  Right $ BottomLeftLine b1 f4
--add face
buildLine    (RightFace _ _ f3 f4)   (GTCPoint _)          (EQCPoint (BackLeftLine b1 b2))     (GTCPoint _)         (GTCPoint _)    =
  Right $ LeftFace b1 b2 f4 f3
  --Left "stopped (RightFace _ _ f3 f4)   (GTCPoint _)          (EQCPoint (BackLeftLine b1 b2)) "

buildLine    ((BottomRightLine b4 _))   (EQCPoint (F1 f1))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  Right $ BottomLeftLine b4 f1
--add face
buildLine    ((RightFace b3 b4 _ _))   (EQCPoint (FrontLeftLine f1 f2))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  Right $ LeftFace b4 b3 f1 f2
  --Left "stopped ((RightFace b3 b4 _ _))   (EQCPoint (FrontLeftLine f1 f2))    (GTCPoint _) "

  -- ================================ Left<Line/Face>======================================
buildLine    ((BottomLeftLine _ f1)) (GTCPoint _)       (LTCPoint (B1 b1))         (GTCPoint _)              (GTCPoint _)    =
  Right $ BottomLeftLine b1 f1
 
--add face
buildLine    (LeftFace _ _ f1 f2)    (GTCPoint _)       (LTCPoint (BackLeftLine b1 b2))         (GTCPoint _)              (GTCPoint _)    =
  Right $ LeftFace b1 b2 f1 f2
  --Left "stopped (LeftFace _ _ f1 f2)    (GTCPoint _)       (LTCPoint (BackLeftLine b1 b2))   "

buildLine    ((BottomLeftLine b1 _)) (LTCPoint (F1 f1)) (GTCPoint _)                (GTCPoint _)              (GTCPoint _)    =
  Right $ BottomLeftLine b1 f1
--add face
buildLine    (LeftFace b1 b2 _ _)      (LTCPoint (FrontLeftLine f1 f2))  (GTCPoint _)        (GTCPoint _)              (GTCPoint _)    =
  Right $ LeftFace b1 b2 f1 f2
  --Left "stopped (LeftFace b1 b2 _ _)      (LTCPoint (FrontLeftLine f1 f2))  (GTCPoint _)"

buildLine    ((BottomLeftLine b1 _))    (EQCPoint (F1 f1))        (GTCPoint _)                (GTCPoint _)              (GTCPoint _)    =
  Right $ BottomLeftLine b1 f1
--add face
buildLine    (LeftFace b1 b2 _ _)      (EQCPoint (FrontLeftLine f1 f2))  (GTCPoint _)        (GTCPoint _)              (GTCPoint _)    =
  Right $ LeftFace b1 b2 f1 f2
  --Left "stopped (LeftFace b1 b2 _ _)      (EQCPoint (FrontLeftLine f1 f2))  (GTCPoint _)"
  
buildLine    ((BottomLeftLine _ f1)) (GTCPoint _) (EQCPoint (B1 b1)) (GTCPoint _)  (GTCPoint _)   =
  Right $ BottomLeftLine b1 f1
--add face
buildLine    (LeftFace _ _ f1 f2)    (GTCPoint _)       (EQCPoint (BackLeftLine b1 b2))         (GTCPoint _)              (GTCPoint _)    =
  Right $ LeftFace b1 b2 f1 f2
  --Left "stopped (LeftFace _ _ f1 f2)    (GTCPoint _)       (EQCPoint (BackLeftLine b1 b2)) "

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

{-
buildLine (RightFace _ _ f3 f4) i (GTCPoint _) (GTCPoint _) (GTCPoint _) =
  CornerPointsError "(RightFace _ _ f3 f4) i"
  --untested

buildLine (BottomRightLine b4 _) i (GTCPoint _) (GTCPoint _) (GTCPoint _) =
  CornerPointsError "(BottomRightLine b4 _) i"
  --untested

buildLine (LeftFace _ _ f1 f2) i (GTCPoint _) (GTCPoint _) (GTCPoint _) =
  CornerPointsError "(LeftFace _ _ f1 f2) i"
  --untested

buildLine (BottomLeftLine _ f1) i (GTCPoint _) (GTCPoint _) (GTCPoint _) =
  CornerPointsError "(BottomLeftLine _ f1) i"
  --untested

buildLine (CornerPointsNothing) i (GTCPoint _) (GTCPoint _) (GTCPoint _) =
  CornerPointsError "(CornerPointsNothing) i"

buildLine (CornerPointsError m) i (GTCPoint _) (GTCPoint _) (GTCPoint _) =
  CornerPointsError "(CornerPointsError) i"

buildLine testCurrLine i (GTCPoint _) (GTCPoint _) (GTCPoint _) =
  CornerPointsError "testCurrLine i"
  --got non-exhaustive in fx center.

buildLine testCurrLine i x y z =
  CornerPointsError "testCurrLine"
  --had to kill
-}
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

removeHeadIfFrontUsed :: [CornerPoints] -> CornerPoints -> [CornerPoints]
removeHeadIfFrontUsed ((F1 f1'):xs) (BottomLeftLine b1 f1) =
  case ((F1 f1') == (extractF1 (BottomLeftLine b1 f1))) of
    True -> xs
    False -> (F1 f1'):xs
removeHeadIfFrontUsed ((FrontLeftLine f1' f2'):xs) (LeftFace b1 b2 f1 f2) =
  case ((FrontLeftLine f1' f2') == (extractFrontLeftLine )(LeftFace b1 b2 f1 f2)) of
    True -> xs
    False -> (FrontLeftLine f1' f2'):xs
--the orig that worked with bottom points
--now make it an error
removeHeadIfFrontUsed (x:xs) cpoint =
  case (x == cpoint) of
    True -> [CornerPointsError "removeHeadIfFrontUsed generic true"] -- xs
    False -> [CornerPointsError "removeHeadIfFrontUsed generic false"] -- x:xs

removeHeadIfBackUsed :: [CornerPoints] -> CornerPoints -> [CornerPoints]
removeHeadIfBackUsed ((B1 b1'):xs) (BottomLeftLine b1 f1) =
  case ((B1 b1') == (extractB1 (BottomLeftLine b1 f1))) of
    True -> xs
    False -> (B1 b1'):xs
removeHeadIfBackUsed ((BackLeftLine b1' f1'):xs) (LeftFace b1 b2 f1 f2) =
  case ((BackLeftLine b1' f1') == (extractBackLeftLine )(LeftFace b1 b2 f1 f2)) of
    True -> xs
    False -> (BackLeftLine b1' f1'):xs
--the orig that worked with bottom points
removeHeadIfBackUsed (x:xs) cpoint =
  case (x == cpoint) of
    True -> [CornerPointsError "removeHeadIfBackUsed generic true"]  -- xs
    False -> [CornerPointsError "removeHeadIfBackUsed generic false"]  -- x:xs
  
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
