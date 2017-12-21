{-# LANGUAGE TemplateHaskell #-}
{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}
module Joiners.Delaunay(delaunay, delaunayA, delaunayB, removeIfUsed, ensureGoodHeadDistance, orderByDistance, removeOneOrBothIfUsed, extractI) where

import Data.Typeable

import CornerPoints.CornerPoints(CornerPoints(..),(+++),(++++), cpointType)
import CornerPoints.FaceExtraction(extractB1,extractFrontLeftLine, extractF1, extractBackLeftLine, contains )
import CornerPoints.FaceConversions(toLeftFace, raisedTo, toBottomLeftLine)

import Math.Distance(Distance(..), Distant, calculateDistance, DistanceA(..), DistantA, calculateDistanceA
                    , center, (<-|->), centerA, (<-||->))

import Helpers.List(removeEmpty, safeTail, safeHead)
import Helpers.Applicative(extractE)

import Data.List(filter, sortOn)

{- |
joinedCpoints:
 The built up [CornerPoints] that is the result of joining

perim cpoints:
 The [CornerPoints] that is the perimter.
 All inner cpoints are inside of this.
 Every join will include a cpoint from this [CornerPoints]

inner cpoints:
 The [[CornerPoints]] which contains all the [CornerPoints] which make up the inner cpoints.
 Only one of these will be part of each join.
-}
delaunayB ::  [CornerPoints] -> --outer perim cpoints
              [[CornerPoints]] -> --inner perim cpoints
              [CornerPoints]
              
delaunayB     [] _ =
  [CornerPointsError "Joiners.Delaunay.delaunayB: empty perimeter [CornerPoints]"]

delaunayB      _ [] =
  [CornerPointsError "Joiners.Delaunay.delaunayB: empty inner [[CornerPoints]]"]

delaunayB  (o:outerPerimeters) innerPerimeters =
  
  let 
      --remove any [] from innerPerimeters
      --order the inner perims lists by head distance from the advancing line
      --For this first call, advancing line will be head of outer perims
      orderedInnerPerims = orderByDistance (removeEmpty innerPerimeters) o
      
      advancingCpoint = ((\x -> o +++ (head $ head x))) <$> orderedInnerPerims

      --remove the used cpoint from the perims.
      --todo: it should cx all cpoints in all perimeters.
      innerOuterRemoved = extractE( removeOneOrBothIfUsed (o:outerPerimeters) <$> (fmap safeHead orderedInnerPerims) <*> advancingCpoint)
      
      
  in
  case delaunayB' <$> 
        ( extractOuterPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved ) <*>
          --If used to build advancing cpoint, should have cpoint removed.
        
        --(extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap tail orderedInnerPerims ) <*>
        (removeEmpty <$> (extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap safeTail orderedInnerPerims )) <*>
        --If used to build advancing cpoint, should have cpoint removed.
        --Why using fmap tail orderedInnerPerims. Because it appends the head that was used, onto the unused tail.
        
        advancingCpoint <*>                             --The advancing Cpoint just created.
        --appendAdvancingCpointToJoinedCpoints advancingCpoint  --joined cpoints with the advancing cpoint added to it.
        (appendAdvancingCpointToJoinedCpoints <$> advancingCpoint <*> Right [])  --joined cpoints with the advancing cpoint added to it.
        
  of
    Left e -> [CornerPointsError $ "Joiners.DeluanayB(Left e): " ++  e]
    Right (Left e) -> [CornerPointsError $ "Joiners.DeluanayB(Righ(Left e)): " ++ e]
    Right (Right val) -> val
    
  --[CornerPointsError "filler to compile"]

delaunayB' ::  [CornerPoints] -> --outer perim cpoints with cpoint removed if used to create advancing cpoint.
               [[CornerPoints]] -> -- inner cpoints with cpoint removed, if it was used to create advancing cpoint.
               CornerPoints -> -- advancing cpoint. This has to be appended to joinedCpoints inside delaunayB'
               [CornerPoints] ->   -- joinedCpoints. Should move this after advancing point.
               Either String [CornerPoints] -- joined cpoints or error

delaunayB' (o:outerPerimeters) (i:innerPerimeters) advancingCpoint joinedCpoints =
  let
    orderedNonEmptyInnerPerimsE = orderByDistance (removeEmpty (i:innerPerimeters)) advancingCpoint

    --need to: first cx for distance to decide if outer/inner perim point is used to build advancingCpointNewE
    --advancingCpointNewE = ((\x -> (head $ head x) `raisedTo` advancingCpoint )) <$> orderedNonEmptyInnerPerimsE
    advancingCpointNewE = newAdvancingCpointE <$>
                           calculateDistanceA advancingCpoint o <*>
                           (extractE ((\is ->  calculateDistanceA advancingCpoint $ head $ head is) <$> orderedNonEmptyInnerPerimsE)) <*>
                           Right o <*>
                           orderedNonEmptyInnerPerimsE <*>
                           Right advancingCpoint
                           


    
    --remove the used cpoint from the perims.
    --todo: it should cx all cpoints in all perimeters.
    innerOuterRemoved = extractE( removeOneOrBothIfUsed (o:outerPerimeters) <$> (fmap head orderedNonEmptyInnerPerimsE) <*> (extractE advancingCpointNewE))

  in
    --Right [CornerPointsError "filler to compile"]
    extractE
    (delaunayB' <$> 
        (extractOuterPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved ) <*>
          --If used to build advancing cpoint, should have cpoint removed.
        
        --(extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap tail orderedNonEmptyInnerPerimsE ) <*>
        (removeEmpty <$> (extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap safeTail orderedNonEmptyInnerPerimsE )) <*>
        --If used to build advancing cpoint, should have cpoint removed.

        
        (extractE advancingCpointNewE) <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpoints <$> (extractE advancingCpointNewE) <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
    )
        
  

delaunayB' [] (i:innerPerimeters) advancingCpoint joinedCpoints =
    let
    orderedNonEmptyInnerPerimsE = orderByDistance (removeEmpty (i:innerPerimeters)) advancingCpoint

    --need to: first cx for distance to decide if outer/inner perim point is used to build advancingCpointNewE
    --advancingCpointNewE = ((\x -> (head $ head x) `raisedTo` advancingCpoint )) <$> orderedNonEmptyInnerPerimsE
    advancingCpointNewE = newAdvancingCpointE <$>
                           --calculateDistanceA advancingCpoint o <*>
                           (Right $ DistanceA 100000000.0) <*> --need a Maybe Distance a
                           (extractE ((\is ->  calculateDistanceA advancingCpoint $ head $ head is) <$> orderedNonEmptyInnerPerimsE)) <*>
                           --Right o <*>
                           Right CornerPointsNothing <*> --need a Maybe?
                           orderedNonEmptyInnerPerimsE <*>
                           Right advancingCpoint
                           


    
    --remove the used cpoint from the perims.
    --todo: it should cx all cpoints in all perimeters.
    --innerOuterRemoved = extractE( removeOneOrBothIfUsed (o:outerPerimeters) <$> (fmap head orderedNonEmptyInnerPerimsE) <*> (extractE advancingCpointNewE))
    innerOuterRemoved = extractE( removeOneOrBothIfUsed [] <$> (fmap head orderedNonEmptyInnerPerimsE) <*> (extractE advancingCpointNewE))
      --will this work with [] passed in for outer perims

  in
    --Right [CornerPointsError "filler to compile"]
    extractE
    (delaunayB' <$> 
        (extractOuterPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved ) <*>
          --If used to build advancing cpoint, should have cpoint removed.
        
        --(extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap tail orderedNonEmptyInnerPerimsE ) <*>
        (removeEmpty <$> (extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap safeTail orderedNonEmptyInnerPerimsE )) <*>
        --If used to build advancing cpoint, should have cpoint removed.

        
        (extractE advancingCpointNewE) <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpoints <$> (extractE advancingCpointNewE) <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
    )

delaunayB' (o:outerPerimeters) [] advancingCpoint joinedCpoints =
    let
    --orderedNonEmptyInnerPerimsE = orderByDistance (removeEmpty (i:innerPerimeters)) advancingCpoint
    orderedNonEmptyInnerPerimsE = Right []

    --need to: first cx for distance to decide if outer/inner perim point is used to build advancingCpointNewE
    --advancingCpointNewE = ((\x -> (head $ head x) `raisedTo` advancingCpoint )) <$> orderedNonEmptyInnerPerimsE
    advancingCpointNewE = newAdvancingCpointE <$>
                           calculateDistanceA advancingCpoint o <*>
                           --(extractE ((\is ->  calculateDistanceA advancingCpoint $ head $ head is) <$> orderedNonEmptyInnerPerimsE)) <*>
                           (Right $ DistanceA 100000000.0) <*> --need a Maybe Distance a
                           Right o <*>
                           orderedNonEmptyInnerPerimsE <*>
                           Right advancingCpoint
                           


    
    --remove the used cpoint from the perims.
    --todo: it should cx all cpoints in all perimeters.
    innerOuterRemoved = extractE( removeOneOrBothIfUsed (o:outerPerimeters) <$> (Right []) <*> (extractE advancingCpointNewE))
    --innerOuterRemoved = extractE( removeOneOrBothIfUsed [] <$> (fmap head orderedNonEmptyInnerPerimsE) <*> (extractE advancingCpointNewE))
      --will this work with [] passed in for inner perims

  in
    --Right [CornerPointsError "filler to compile"]
    extractE
    (delaunayB' <$> 
        (extractOuterPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved ) <*>
          --If used to build advancing cpoint, should have cpoint removed.
        
        --(extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap tail orderedNonEmptyInnerPerimsE ) <*>
        (removeEmpty <$> (extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap safeTail orderedNonEmptyInnerPerimsE )) <*>
        --If used to build advancing cpoint, should have cpoint removed.

        
        (extractE advancingCpointNewE) <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpoints <$> (extractE advancingCpointNewE) <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
    )
  
{-
delaunayB' [] [] advancingCpoint joinedCpoints =
-}

delaunayB' [] [] _ joinedCpoints = Right $ reverse joinedCpoints

--extract the first cpoint from the first list of inner perims.
--do i have enough patter matches. Could go with length checks
--this could be in Helpers.List
--could I do this with safeHead or make a safeHeadHead
extractI :: [[CornerPoints]] -> Either String CornerPoints
extractI [] = Left "Joiners.Delaunay.extractI: attempt to get i from []"
extractI [[]] = Left "Joiners.Delaunay.extractI: attempt to get i from [[]]"
--extractI [[[]:xs]] = Left "Joiners.Delaunay.extractI: attempt to get i from [[]:xs]"
extractI (x:xs) =
  case (length x) == 0 of
    True -> Left "Joiners.Delaunay.extractI: attempt to get i from [[]:xs]"
    False -> Right $ head x
  
--extractI [(x:xs):ys] = Right x
--extractI [(x:xs):[]] = Right x
  

newAdvancingCpointE :: DistanceA -> --from center of advancing cpoint to head of outer perims
                            DistanceA -> --from center of advancing cpoint to head of head of orderedNonEmptyInnerPerims
                            CornerPoints -> --The head $ outer perims
                            [[CornerPoints]] -> --[[]] of all inner perimters
                            CornerPoints -> --the current advancingCpoint, from which advancingCpointNew is build from.
                            Either String CornerPoints
                            --Right: the new advancing cpoint build from advancingCpoint and closes perim cpoint
                            --Left: the eror
newAdvancingCpointE outerDistance innerDistance outerCpoint innerPerimeters advancingCpoint =
      case outerDistance <= innerDistance of
        True -> outerCpoint `raisedTo` advancingCpoint
        --False -> extractE (((\i' advancingCpoint' -> (head $ head i') `raisedTo` advancingCpoint' )) <$> orderedNonEmptyInnerPerimsE <*> Right advancingCpoint)
        --False -> extractE (((\i' advancingCpoint' -> (head $ head i') `raisedTo` advancingCpoint' )) <$> Right innerPerimeters <*> Right advancingCpoint)
        False -> (head $ head innerPerimeters) `raisedTo` advancingCpoint
      {-
      case (<=) <$> (calculateDistanceA advancingCpoint o ) <*> (extractE(calculateDistanceA <$> Right advancingCpoint <*>  (fmap (head $ head) orderedNonEmptyInnerPerimsE))) of
        Left e -> Left e
        Right True -> Right o
        Right False -> (fmap (head $ head) orderedNonEmptyInnerPerimsE)
     -}


appendAdvancingCpointToJoinedCpoints advancingCpoint joinedCpoints = advancingCpoint : joinedCpoints

--extract the outer perimeters from the innerOuterRemoved 
extractOuterPerimsWithAdvancingCpointRemoved (inner,outer) = outer -- [[CornerPointsError "inner perims with used cpoint removed filler"]]

----extract the inner perimeters from the innerOuterRemoved. Then append them back onto $ tail orderedInnerPerims 
extractInnerPerimsWithAdvancingCpointRemoved :: ([CornerPoints],[CornerPoints]) -> [[CornerPoints]] -> [[CornerPoints]] 
extractInnerPerimsWithAdvancingCpointRemoved (inner,outer) innerPerimsTail = inner : innerPerimsTail -- [[CornerPointsError "inner perims with used cpoint removed filler"]]
      --extractInnerPerimsWithAdvancingCpointRemoved (Left e) _ = Left e

--remove the head cpoint if used in the advancing line.
  --can be 1 or both lists
--ensures that at least 1 is removed
--error if none are removed
removeOneOrBothIfUsed :: [CornerPoints] -> --the outer list
                         [CornerPoints] -> --the inner list
                         CornerPoints ->   --advancing cpoint
                         Either String ([CornerPoints],[CornerPoints]) --the 2 lists, one with a cpoint removed
                         --left if no cpoints, or 2 cpoints removed
removeOneOrBothIfUsed (o:outerCpoints) (i:innerCpoints) advancingPoint =
  let
    process :: Bool -> --outer perims
               Bool -> --inner perims
               Either String ([CornerPoints],[CornerPoints]) --the inner list
               
    process True True =
      Right (outerCpoints, innerCpoints)
    process True False =
      Right (outerCpoints, i:innerCpoints)
    process False True =
      Right (o:outerCpoints, innerCpoints)
    process False False =
      Left $ "Joiners.Delaunay.removeIfUsed: did not find a used cpoint in inner/outer perimeters for: \n advancingCpoint: " ++
             (show advancingPoint) ++
             "\n outer cpoint: " ++ (show o) ++
             "\n inner cpoint: " ++ (show i)
      
  in
    case process <$>  advancingPoint `contains` o <*> advancingPoint `contains` i of
      Right (Left e) -> Left e
      Right val -> val
      Right (Right val) -> Right val
      Left e -> Left $ "Joiners.Delaunay.removeIfUsed: threw error from 'contains': " ++ e

removeOneOrBothIfUsed (o:outerCpoints) [] advancingPoint =
  let
    process :: Bool -> --outer perims
               Either String ([CornerPoints],[CornerPoints]) --the inner list
               
    process True =
      Right (outerCpoints, [])
    process False  =
      Left $ "Joiners.Delaunay.removeIfUsed: did not find a used cpoint in outer perimeters for: " ++ (show advancingPoint) ++ " containing " ++ (show o)
    
      
      
  in
    case process <$>  advancingPoint `contains` o  of
      Right (Left e) -> Left e
      Right val -> val
      Left e -> Left $ "Joiners.Delaunay.removeIfUsed: threw error from 'contains': " ++ e

removeOneOrBothIfUsed [] (i:innerCpoints) advancingPoint =
  let
    process :: Bool -> --outer perims
               Either String ([CornerPoints],[CornerPoints]) --the inner list
               
    process True =
      Right ([], innerCpoints)
    process False  =
      Left $ "Joiners.Delaunay.removeIfUsed: did not find a used cpoint in inner perimeters for: " ++ (show advancingPoint) ++ " containing " ++ (show i)
    
      
      
  in
    case process <$>  advancingPoint `contains` i  of
      Right (Left e) -> Left e
      Right val -> val
      Left e -> Left $ "Joiners.Delaunay.removeIfUsed: threw error from 'contains': " ++ e

removeOneOrBothIfUsed [] [] _ =
  Right ([], [])
      
-- |
-- Make sure that all the inner perimters will have a valid DistanceA from the advancing cpoints calc' from outer perim cpoints and head of each [inner perims] .
-- Return a [[CornerPoints]] containing inner perimters where heads are gauranteed to get a valid distance from the advancing cpoint. 
ensureGoodHeadDistance ::  [[CornerPoints]] -> -- [[inner perimter CornerPoints]] which have no [] so head will not fail
                           [[CornerPoints]] -> -- [[good inner perim distance CornerPoints]]. This will be the return list if all passes.
                           CornerPoints  ->    -- this is the current outer perim cpoint, which gets used to figure out advancing cpoint
                           Either String [[CornerPoints]]
ensureGoodHeadDistance (i:innerPerimeters) cleanedPerimeters outerPerimCpoint =
  
  let advancingCpoint =
        
        case (head i) `raisedTo` ((head i) +++ outerPerimCpoint) of
          Left e -> Left e
          Right val -> Right val
  in
  
   case calculateDistanceA <$> advancingCpoint <*> Right (head i) of
    Left e -> Left e
    Right (Right goodDistance) ->
      ensureGoodHeadDistance  innerPerimeters (i:cleanedPerimeters) outerPerimCpoint
    Right (Left e) -> Left e
  
ensureGoodHeadDistance  [] cleanedPerimeters _ =
  Right $ reverse cleanedPerimeters
  

-- | Order the innerPermiters by distance to the advancing Cpoint
      
orderByDistance :: [[CornerPoints]] -> -- [[good inner perim distance CornerPoints]]. 
                    CornerPoints ->    -- The outer perim cpoint
                    Either String [[CornerPoints]]    -- [[good inner perim distance CornerPoints]] ordered by distance to advancing line

orderByDistance  [] _ =
  --Left $ "Joiners.Deluanay.orderByDistance: empty [[inner perimeters]] passed in"
  Right []
  
orderByDistance  (innerPerimeters) outerPerimCpoint = 
     let sorter j =
          calculateDistanceA <$>
              (head j) `raisedTo` ((head j) +++ outerPerimCpoint) <*> Right (head j)
          
           
     in
     --Right $ sortOn (sorter) innerPerimeters
     (sortOn sorter <$>  Right innerPerimeters)

{-keep this orig until tested.
orderByDistance  (innerPerimeters) outerPerimCpoint = 
     let sorter j =
          case calculateDistanceA <$>
              (head j) `raisedTo` ((head j) +++ outerPerimCpoint) <*>
              Right (head j)
          of
           Left e ->  DistanceA 100000000.0
           Right (Right distanceA) -> distanceA
           Right (Left e) -> DistanceA 100000000.0
           --Right distanceA ->  distanceA
           
     in
     Right $ sortOn (sorter) innerPerimeters

-}
-- ============================================= delaunay and delauanayA=====================================
-- | Join together 2 [CornerPoints]
delaunay :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints]
delaunay [] _ _ _ = [CornerPointsError "empty outer points passed into delaunay"]
delaunay  _ [] [] [] = [CornerPointsError "at least 1 inner points required into delaunay"]
--use the head of first list and head of another list to create the intial back <Face/line>.
delaunay (i:is) (x:xs) [] [] =
  --to use Applicative everthing will have to be Either.
    --(i +++ x) would be (i ++++ x)
  --((i +++ x):[]) needs a append fx that takes an Either str cpoint and a [cpoint] -> Either str [cpoint]
  delaunay' (Right (is)) (Right (xs)) (Right []) (Right []) (i +++ x) ((i +++ x):[])

--applicative version of delaunay which needs to call delaunay'A
delaunayA :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints]
delaunayA (i:is) (x:xs) [] [] =
  
  let --Need an Either way to build joined cpoints so everything is applicative.
      --This fx will need to be moved up in scope. Perhaps in Helpers.List
      {- moved up to global in Deluanay for now
      append :: Either String CornerPoints -> [CornerPoints] -> Either String [CornerPoints]
      append (Right cpoint) cpoints = Right $ cpoint : cpoints
      append (Left e) cpoints = Left e
      -}
  in
    case (Right delaunayA' <*> (i ++++ x) <*> (Right (is)) <*> (Right (xs)) <*> (Right []) <*> (Right [])  <*> (append (i ++++ x) [])) of
      Left e -> [CornerPointsError $ "Joiners.Delaunay.delaunayA: " ++ e]
      Right cpoints -> cpoints
  
--applicative version of delaunay'
delaunayA' :: CornerPoints
              ->  [CornerPoints]
              ->  [CornerPoints]
              ->  [CornerPoints]
              ->  [CornerPoints]
              ->  [CornerPoints]
              ->  [CornerPoints]



delaunayA' currLine (i:is) (x:xs) [] []  cpointsJoined =
  --can't get the Applicative to compile.
  --Need to simplify by getting rid of getAvgDistance and OrderedCpoints.
  let avgDistanceA = "won't compile as applicative"
        --(Right( getAvgDistanceA))
                       --( Right (getAvgDistanceA)) <*> (Right (centerA i) )
                            --DistanceA -> DistanceA -> DistanceA -> DistanceA  -> Either String DistanceA
                       -- <*> ((Right(calculateDistanceA)) <*> ((centerA currLine)) <*> ((centerA i)))
                       -- <*> ( (Right(centerA i)))
                         --should be: CornerPoints.Points.Point -> CornerPoints.Points.Point -> DistanceA
                         --got:       CornerPoints.Points.Point -> CornerPoints.Points.Point -> Either String DistanceA
                     {-
                     <*> ((Right calculateDistanceA) <*> ((centerA currLine)) <*> ((centerA x)))
                     <*> (Right(DistanceA 0.0))
                     <*> (Right (DistanceA 0.0))
-}
      {-
      iOrdered = (Right getOrderedCPointA) <*> avgDistanceA <*> ((Right calculateDistanceA) <*> (Right currLine) <*> (Right i)) <*> (Right i)
      xOrdered = (Right getOrderedCPointA) <*> avgDistanceA <*> ((Right calculateDistanceA) <*> (Right currLine) <*> (Right x)) <*> (Right x)
      builtLine = (Right buildLine)
                  <*> (Right currLine)
                  <*> iOrdered
                  <*> xOrdered
                  <*> (Right (GTCPoint CornerPointsNothing))
                  <*> (Right (GTCPoint CornerPointsNothing))
      {-
      builtLine = (Right buildLine)
                   <*> (Right currLine)
                   <*> ((Right getOrderedCPointA) <*> avgDistanceA <*> ((Right calculateDistanceA) <*> (Right currLine) <*> (Right i)) <*> (Right i))
                   <*> xOrdered
                   <*> (Right (GTCPoint CornerPointsNothing))
                   <*> (Right (GTCPoint CornerPointsNothing))
      -}
      extractedBuiltLine =
        case builtLine of
          Left e -> Left e
          Right builtLine' -> builtLine'
      -}
  in
    [CornerPointsError "filler to get it to compile"]
    {-
    case ((Right delaunayA') <*> extractedBuiltLine <*> (Right (is)) <*> (Right (xs)) <*> (Right []) <*> (Right [])  <*> (append extractedBuiltLine [])) of
      Right cpoints -> cpoints
      Left e        -> reverse $ (CornerPointsError e) : cpointsJoined
    -}
delaunayA' _ _ _ _ _ _ = [CornerPointsError "filler to get it to compile"]

--Run (i:is) and at least 1 other list to the end of both lists.
--(i:is) is the outer CornerPoints defining the shape into which all the others will be inserted.
--This is the return value for delaunay, and so can't return Either as it will be input into Builder.Monad.
--Builder.Monad will find an error by looking for a CornerPointsError.
delaunay' :: (Either String [CornerPoints])
             -> (Either String [CornerPoints])
             -> (Either String [CornerPoints])
             -> (Either String [CornerPoints])
             ->  CornerPoints -> [CornerPoints]
             -> [CornerPoints] 

--all these Left checks will be removed as Applicative will do this in delaunayA'
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

{------------------------------------------- applicative version of build ----------------------------------------------------------
Why do I need this?
All I am doing is changing it to return a CornerPoints instead of: Either  String CornerPoints
-}
buildLineA ::
     CornerPoints
  -> OrderedCornerPoint
  -> OrderedCornerPoint
  -> OrderedCornerPoint
  -> OrderedCornerPoint
  -> CornerPoints

{-
Temp:
Put a wrapper around buildLine to see what happens.
Problem is that I am not using the whole applicative system which gets rid of things like:
NoDistance and CornerPointsError.
However:
buildLine only uses `raisedTo` which is an Either so all the applicative
action should be used above the level of buildLine.
Also:
buildLine should be used as an applicative input in delaunayA'.
-}
buildLineA currLine is xs ys zs =
  case buildLine currLine is xs ys zs of
    Right val -> val
    Left e    -> CornerPointsError $ "Joiners.Delaunay.buildLineA error: " ++ e

buildLineA _ _ _ _ _ = CornerPointsError "Just a holder to make it compile"
  
{-           currLine                   i                     x                      y                    z                    -}


------------------------------------------------ end applicative buildLine ------------------------------------------------------
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


getAvgDistanceA :: DistanceA -> DistanceA -> DistanceA -> DistanceA  -> DistanceA
getAvgDistanceA    (DistanceA i) (DistanceA x) (DistanceA y) (DistanceA z)        =
  DistanceA $ (i + x + y + z)/4


{-
getAvgDistanceA :: [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> CornerPoints -> Either String Distance
getAvgDistanceA    (i:is)            (x:xs)            []                []                currLine        =
  let
    iDist = calculateDistance (center i) (center currLine)
    xDist = calculateDistance (center x) (center currLine)
  in
    case iDist of
      (Distance d) ->
        case xDist of
          (Distance d') -> Right $ Distance $ (d + d')/2
          NoDistance -> Left "Delaunay.getAverageDistanceA xDist error"--NoDistance
      NoDistance -> Left "Delaunay.getAverageDistanceA iDist error" --NoDistance

getAvgDistanceA    []            (x:xs)            []                []                currLine        =
  Right $ calculateDistance (center x) (center currLine)
getAvgDistanceA  (i:is)  []          []                []                currLine        =
  Right $ calculateDistance (center i) (center currLine)
-}

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

--Compare myDist to the center of the advancing Line/Face, to that
--of the average distance from the advancing Line/Face to all the next available CornerPoints.
getOrderedCPointA :: DistanceA -> DistanceA -> CornerPoints -> OrderedCornerPoint
getOrderedCPointA    avgDist     myDist      orderThisCpoint =
  let
    
    getAvgDistanceStatus :: DistanceA    -> DistanceA -> Ordering
    --getAvgDistanceStatus _ NoDistance = GT
    --getAvgDistanceStatus NoDistance _ = GT
    getAvgDistanceStatus    (DistanceA avgDistance)    (DistanceA dist)
      | (dist) > (avgDistance)  = GT
      | (dist) < (avgDistance)  = LT
      | otherwise = EQ
  in
    case getAvgDistanceStatus avgDist myDist of
      GT -> GTCPoint orderThisCpoint
      LT -> LTCPoint orderThisCpoint
      EQ -> EQCPoint orderThisCpoint

{-
getOrderedCPointA :: Distance -> CornerPoints -> CornerPoints -> Either String OrderedCornerPoint
getOrderedCPointA    avgDist     advanceFromThisLine        orderThisCpoint =
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
      GT -> Right $ GTCPoint orderThisCpoint
      LT -> Right $ LTCPoint orderThisCpoint
      EQ -> Right $ EQCPoint orderThisCpoint
-}
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

removeIfUsedA :: (Either String [CornerPoints]) -> (Either String CornerPoints) -> Either String [CornerPoints]
removeIfUsedA (Right (x:xs)) (Right cpoint) =
  case ( cpoint `contains` (x)) of
    Right True ->  Right xs
    Right False -> Right (x:xs)
    Left e -> Left e

{-wasRemoved:
See if one of the CornerPoints is contained by the advancing line/face.
This is to ensure that the point that was used to advance, was removed from the lists.
Failure to do this will result in delaunayA not exiting as there is always another point to use.
-}
wasRemoved :: CornerPoints ->  [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> [CornerPoints] -> Either String CornerPoints
         --advancing line/face   (i:is)            (x:xs)             (y:ys)            (z:zs)           (removed cpoint)
wasRemoved advancer (i:is) [] [] [] =
  case advancer `contains` i of
    Right True  -> Right i
    Right False -> Left $ "Joiners.Delaunay.wasRemoved: The advancer cpoint " ++ (show advancer) ++ " did not contain i: " ++ (show i)
    Left e      -> Left $  "Joiners.Delaunay.wasRemoved: The advancer cpoint " ++ (show advancer) ++ " produced a contains error while" ++
                         "checking " ++ (show i) ++ " :" ++ e

  
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

--Need an Either way to build joined cpoints so everything is applicative.
--This fx will need to be moved up in scope. Perhaps in Helpers.List
append :: Either String CornerPoints -> [CornerPoints] -> Either String [CornerPoints]
append (Right cpoint) cpoints = Right $ cpoint : cpoints
append (Left e) cpoints = Left e
