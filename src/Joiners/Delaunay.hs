{-# LANGUAGE TemplateHaskell #-}
module Joiners.Delaunay(delaunay) where

{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}

import CornerPoints.CornerPoints(CornerPoints(..),(+++), center, (<-|->))

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
--find the first available to create the intial back line
--start with the 1st one for now. Will need better pattern matching
--and to see with array has the closest.
delaunay (i:is) (x:xs) [] [] =
  delaunay' (is) (xs) [] [] (i +++ x) ((i +++ x):[])

--Run 2 lists to the end of both lists.
--
delaunay' :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] ->  CornerPoints -> [CornerPoints] -> [CornerPoints] 
delaunay'    (i:is)            (x:xs)            []                []                 currLine        cpointsJoined =
  --temp to compile
    --cpointsJoined 
  let
    avgDistance = getAvgDistance (i:is)            (x:xs)            []                []                 currLine 
    newCurrLine = buildLine
                    currLine
                      (getLTECpoint
                       (getAvgDistanceStatus
                                       avgDistance
                                       (calculateDistance (center currLine) (center i))
                       )
                       i)
                      (getLTECpoint
                       (getAvgDistanceStatus
                          avgDistance
                          (calculateDistance
                            (center currLine)
                            (center x)
                          )
                          
                       )
                       x
                      )
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
  in
    delaunay' (removeHeadIfUsed (i:is) (F1 $ f1 newCurrLine)) (removeHeadIfUsed (x:xs) (B1 $ b1 newCurrLine)) [] [] newCurrLine (newCurrLine:cpointsJoined)

delaunay'  (i:is) [] [] [] currLine cpointsJoined =
  let
    avgDistance = getAvgDistance (i:is)            []            []                []                 currLine 
    newCurrLine = buildLine
                    currLine
                      (getLTECpoint
                       (getAvgDistanceStatus
                                       avgDistance
                                       (calculateDistance (center currLine) (center i))
                       )
                       i)
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
  in
    delaunay' is [] [] [] newCurrLine (newCurrLine:cpointsJoined)
  

delaunay' [] (x:xs) [] [] currLine cpointsJoined =
  let
    avgDistance = getAvgDistance []            (x:xs)            []                []                 currLine 
    newCurrLine = buildLine
                    currLine
                      (getLTECpoint
                       (getAvgDistanceStatus
                                       avgDistance
                                       (Distance 100000000.0) --just give a big # for now
                       )
                       CornerPointsNothing) --just give it Nothing for now
                      (getLTECpoint
                       (getAvgDistanceStatus
                          avgDistance
                          (calculateDistance
                            (center currLine)
                            (center x)
                          )
                          
                       )
                       x
                      )
                      (GTCPoint CornerPointsNothing)
                      (GTCPoint CornerPointsNothing)
  in
    delaunay' [] xs [] [] newCurrLine (newCurrLine:cpointsJoined)
  
delaunay'  [] [] [] [] _ cpointsJoined =  reverse cpointsJoined

buildLine :: CornerPoints ->            OrderedCornerPoint -> OrderedCornerPoint -> OrderedCornerPoint -> OrderedCornerPoint -> CornerPoints
{-           currLine                   i                     x                      y                    z                    -}
buildLine    ((BottomRightLine b4 _))   (LTCPoint (F1 f1))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  BottomLeftLine b4 f1                                                                                                       
buildLine    ((BottomRightLine _ f4))   (GTCPoint _)          (LTCPoint (B1 b1))     (GTCPoint _)         (GTCPoint _)    =
  BottomLeftLine  b1 f4
buildLine    ((BottomRightLine _ f4))   (GTCPoint _)          (EQCPoint (B1 b1))     (GTCPoint _)         (GTCPoint _)    =
  BottomLeftLine b1 f4
buildLine    ((BottomRightLine b4 _))   (EQCPoint (F1 f1))    (GTCPoint _)           (GTCPoint _)         (GTCPoint _)    =
  BottomLeftLine b4 f1    
                                                                                                                          
buildLine    ((BottomLeftLine _ f1))    (GTCPoint _)               (LTCPoint (B1 b1))         (GTCPoint _)              (GTCPoint _)    =
  BottomLeftLine b1 f1                                                                                                       
buildLine    ((BottomLeftLine b1 _))    (LTCPoint (F1 f1))        (GTCPoint _)                (GTCPoint _)              (GTCPoint _)    =
  BottomLeftLine b1 f1
buildLine    ((BottomLeftLine b1 _))    (EQCPoint (F1 f1))        (GTCPoint _)                (GTCPoint _)              (GTCPoint _)    =
  BottomLeftLine b1 f1
--need 2 equals
buildLine    ((BottomLeftLine _ f1)) (GTCPoint _) (EQCPoint (B1 b1)) (GTCPoint _)  (GTCPoint _)   =
  BottomLeftLine b1 f1

getAvgDistance :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> CornerPoints -> Distance
getAvgDistance    (i:is)            (x:xs)            []                []                currLine        =
  let
    iDist = calculateDistance (center i) (center currLine)
    xDist = calculateDistance (center x) (center currLine)
  in
    Distance $ ((iDist^.distance) + (xDist^.distance))/2
getAvgDistance    []            (x:xs)            []                []                currLine        =
  calculateDistance (center x) (center currLine)
getAvgDistance  (i:is)  []          []                []                currLine        =
  calculateDistance (center i) (center currLine)

getAvgDistanceStatus :: Distance    -> Distance -> Ordering
getAvgDistanceStatus    avgDistance    (dist)
  | (dist^.distance) > (avgDistance^.distance)  = GT
  | (dist^.distance) < (avgDistance^.distance)  = LT
  | otherwise = EQ
  


getLTECpoint :: Ordering -> CornerPoints -> OrderedCornerPoint
getLTECpoint (LT) cpoint = LTCPoint cpoint
getLTECpoint (EQ) cpoint = EQCPoint cpoint
getLTECpoint (GT) cpoint = GTCPoint cpoint

removeHeadIfUsed :: [CornerPoints] -> CornerPoints -> [CornerPoints]
removeHeadIfUsed (x:xs) cpoint =
  case (x == cpoint) of
    True -> xs
    False -> x:xs
  

data OrderedCornerPoint =
  LTCPoint {ltCpoint :: CornerPoints}
  |
  EQCPoint {eqCpoint :: CornerPoints}
  |
  GTCPoint {gtCpoint :: CornerPoints}
