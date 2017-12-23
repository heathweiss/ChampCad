{-# LANGUAGE TemplateHaskell #-}
{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}
module Joiners.Delaunay(delaunay, delaunayA, delaunayB, removeIfUsed, ensureGoodHeadDistance, orderByDistance, extractI) where

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

delaunayB  outerPerimeters innerPerimeters =
  
  let --As this is the very 1st AdvancingCPoint, build it from head OuterPermiter, and the nearest head InnerPerimiter
      initialAdvancingCpoint :: Maybe Perimeters -> Maybe Perimeters -> Either String AdvancingCPoint
      initialAdvancingCpoint innerPerims outerPerims =
        let
          createCpoint :: Maybe Perimeters -> Maybe Perimeters  -> Either String AdvancingCPoint
          createCpoint (Just(InnerPerimeters (i:is))) (Just (OuterPerimeter (o:os))) = Right $ AdvancingCPoint $ o +++ (head i)
          createCpoint _ Nothing = Left "Joiners.Delaunay.deluanayB was given an empty outer perimeter"
          createCpoint Nothing _ = Left "Joiners.Delaunay.deluanayB was given an empty inner perimeters"
            
        in
        createCpoint  innerPerims outerPerims

      advancingCpoint :: Either String AdvancingCPoint
      advancingCpoint = initialAdvancingCpoint
                         (justifyPerimeters' $ InnerPerimeters $ removeEmpty innerPerimeters) --justifiedInnerPerims
                         (justifyPerimeters' $ OuterPerimeter outerPerimeters) --justifiedOuterPerims
      
      perimsWithAdvancingCpointBldrRemoved =
                           extractE
                             (innerOuterRemoved' <$>
                                                 orderedInnerPerims'
                                                   (justifyPerimeters' $ OuterPerimeter outerPerimeters)
                                                   (justifyPerimeters' $ InnerPerimeters $ removeEmpty innerPerimeters) <*>  
                                                 Right (justifyPerimeters' $ OuterPerimeter outerPerimeters) <*>
                                                 advancingCpoint
                             )

      
  in
  case delaunayB' <$> 
        (fst <$> perimsWithAdvancingCpointBldrRemoved) <*>
        (snd <$> perimsWithAdvancingCpointBldrRemoved) <*>
        advancingCpoint <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpointsE <$> advancingCpoint <*> Right [])  --joined cpoints with the advancing cpoint added to it.
        
  of
    Left e -> [CornerPointsError $ "Joiners.DeluanayB(Left e): " ++  e]
    Right (Left e) -> [CornerPointsError $ "Joiners.DeluanayB(Righ(Left e)): " ++ e]
    Right (Right val) -> val
    

delaunayB' ::  --[CornerPoints] -> --outer perim cpoints with cpoint removed if used to create advancing cpoint.
               Maybe Perimeters -> --inner
               
               
               --[[CornerPoints]] -> -- inner cpoints with cpoint removed, if it was used to create advancing cpoint.
               Maybe Perimeters -> --outer
               
               AdvancingCPoint -> -- advancing cpoint. This has to be appended to joinedCpoints inside delaunayB'
               [CornerPoints] ->   -- joinedCpoints. Should move this after advancing point.
               Either String [CornerPoints] -- joined cpoints or error

delaunayB' Nothing Nothing _ joinedCpoints = Right $ reverse joinedCpoints

delaunayB' innerPerimeters outerPerimeters  advancingCpoint joinedCpoints =
  let
    safeO :: Maybe Perimeters -> Maybe CornerPoints
    safeO Nothing = Nothing
    safeO (Just (OuterPerimeter outerPerimeter)) =
      case (length outerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ head outerPerimeter
    --orderedNonEmptyInnerPerimsE = orderByDistance (removeEmpty (i:innerPerimeters)) advancingCpoint
    orderedNonEmptyInnerPerimsE = orderByDistance innerPerimeters advancingCpoint
    
    --need to: first cx for distance to decide if outer/inner perim point is used to build advancingCpointNewE
    --advancingCpointNewE = ((\x -> (head $ head x) `raisedTo` advancingCpoint )) <$> orderedNonEmptyInnerPerimsE
    advancingCpointNewE = newAdvancingCpointE <$>
                           (calculateJustDistance advancingCpoint  orderedNonEmptyInnerPerimsE) <*>
                           (calculateJustDistance advancingCpoint (Right outerPerimeters)) <*>
                           (Right $ safeO outerPerimeters) <*>
                           orderedNonEmptyInnerPerimsE <*>
                           (Right advancingCpoint)
                           
    --advancingCpointNewE = advancingCpoint

    
    --remove the used cpoint from the perims.
    --todo: it should cx all cpoints in all perimeters.
    --innerOuterRemoved = extractE( removeOneOrBothIfUsed (o:outerPerimeters) <$> (fmap head orderedNonEmptyInnerPerimsE) <*> (extractE advancingCpointNewE))
    --innerOuterRemoved = extractE (innerOuterRemoved' innerPerimeters outerPerimeters <$> extractE advancingCpointNewE )
    innerOuterRemoved = extractE (innerOuterRemoved' <$> orderedNonEmptyInnerPerimsE <*> Right outerPerimeters <*> extractE advancingCpointNewE )
  in
    --Right [CornerPointsError "filler to compile"]
    extractE
    (delaunayB' <$> 
        (fst <$> innerOuterRemoved ) <*>
          --If used to build advancing cpoint, should have cpoint removed.
        
        --(extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap tail orderedNonEmptyInnerPerimsE ) <*>
        --(removeEmpty <$> (extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap safeTail orderedNonEmptyInnerPerimsE )) <*>
        (snd <$> innerOuterRemoved ) <*>
        --If used to build advancing cpoint, should have cpoint removed.

        
        (extractE advancingCpointNewE) <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpointsE <$> (extractE advancingCpointNewE) <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
    )
        
  

--delaunayB' _ _ _  = Right [CornerPointsError "filler to compile"]
{-
--delaunayB' (o:outerPerimeters) (i:innerPerimeters) advancingCpoint joinedCpoints =
delaunayB' (Just (o:outerPerimeters)) (Just(i:innerPerimeters)) advancingCpoint joinedCpoints =
  let
    --orderedNonEmptyInnerPerimsE = orderByDistance (removeEmpty (i:innerPerimeters)) advancingCpoint
    orderedNonEmptyInnerPerimsE = orderByDistance (Just (i:innerPerimeters)) advancingCpoint

    --need to: first cx for distance to decide if outer/inner perim point is used to build advancingCpointNewE
    --advancingCpointNewE = ((\x -> (head $ head x) `raisedTo` advancingCpoint )) <$> orderedNonEmptyInnerPerimsE
    advancingCpointNewE =
      newAdvancingCpointE <$>
                           --calculateDistanceA advancingCpoint o <*>
                           calculateJustDistance advancingCpoint (Right $ Just (o:outerPerimeters)) <*>
                           --(extractE ((\is ->  calculateDistanceA advancingCpoint $ head $ head is) <$> orderedNonEmptyInnerPerimsE)) <*>
                           (extractE ((calculateJustDistance advancingCpoint)<$> orderedNonEmptyInnerPerimsE)) <*>
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
        
  

--delaunayB' [] (i:innerPerimeters) advancingCpoint joinedCpoints =
delaunayB' Nothing (Just(i:innerPerimeters)) advancingCpoint joinedCpoints =
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

--delaunayB' (o:outerPerimeters) [] advancingCpoint joinedCpoints =
delaunayB' (Just(o:outerPerimeters)) Nothing advancingCpoint joinedCpoints =
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

--delaunayB' [] [] _ joinedCpoints = Right $ reverse joinedCpoints
delaunayB' Nothing Nothing _ joinedCpoints = Right $ reverse joinedCpoints
-}
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

orderedInnerPerims' :: Maybe Perimeters ->  Maybe Perimeters ->  Either String (Maybe Perimeters)
orderedInnerPerims'     (Just (InnerPerimeters justifiedInnerPerims)) (Just (OuterPerimeter (o:outerPerims)))  =
            orderByDistance (Just(InnerPerimeters justifiedInnerPerims)) (AdvancingCPoint o)
orderedInnerPerims' Nothing _ = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty outer perims passed in"
orderedInnerPerims' _ Nothing = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty inner perims passed in"


        
--extractI [(x:xs):ys] = Right x
--extractI [(x:xs):[]] = Right x
data Perimeters =
  OuterPerimeter {_outerPerimeter :: [CornerPoints]}
  |
  InnerPerimeters {_innerPerimeters :: [[CornerPoints]]}
  |
  InnerPerimeter {_innerPerimeter :: [CornerPoints]}

data AdvancingCPoint =
 AdvancingCPoint {_advancingCpoint :: CornerPoints}


calculateJustDistance :: AdvancingCPoint -> Either String (Maybe Perimeters) -> Either String (Maybe DistanceA)
calculateJustDistance _ (Left e) = Left e
calculateJustDistance _ (Right Nothing) = Right Nothing
--calculateJustDistance advancingCpoint' (Right (Just (InnerPerimeters innerPerims))) = Right $ Just $  calculateDistanceA advancingCpoint' $ head $ head innerPerims
calculateJustDistance (AdvancingCPoint advancingCpoint) (Right (Just (InnerPerimeters (innerPerim:innerPerims)))) =
  case calculateDistanceA advancingCpoint $ head innerPerim of
    Left e -> Left e
    Right distanceA -> Right $ Just distanceA
--calculateJustDistance advancingCpoint' (Right (Just (OuterPerimeter outerPerim))) = Right $ Just $  calculateDistanceA advancingCpoint' $ head outerPerim
calculateJustDistance (AdvancingCPoint advancingCpoint) (Right (Just (OuterPerimeter outerPerim))) =
  case calculateDistanceA advancingCpoint $ head outerPerim of
    Left e -> Left e
    Right distanceA -> Right $ Just distanceA

{-
calculateJustDistance :: CornerPoints -> Either String (Maybe [[CornerPoints]]) -> Either String (Maybe [[CornerPoints]])
calculateJustDistance advancingCpoint' (Left e) = Left e
calculateJustDistance advancingCpoint' (Right Nothing) = Right Nothing
calculateJustDistance advancingCpoint' (Right (Just innerPerims)) = Right $ Just $  calculateDistanceA advancingCpoint' $ head $ head innerPerims
-}

newAdvancingCpointE :: Maybe DistanceA -> --from center of advancing cpoint to head of innerPerim
                       Maybe DistanceA -> --from center of advancing cpoint to outerPerim
                       Maybe CornerPoints -> --The head $ outer perims
                       Maybe Perimeters -> --[[]] of all inner perimters
                       AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                       Either String AdvancingCPoint
                            --Right: the new advancing cpoint build from advancingCpoint and closes perim cpoint
                            --Left: the eror
newAdvancingCpointE (Just innerDistance) (Just outerDistance)  (Just outerCpoint) (Just(InnerPerimeter innerPerimeter)) (AdvancingCPoint advancingCpoint) =
      case outerDistance <= innerDistance of
        True ->
          case outerCpoint `raisedTo` advancingCpoint of
            Left e -> Left e
            Right cpoint -> Right $ AdvancingCPoint cpoint
        False ->
          case (head innerPerimeter) `raisedTo` advancingCpoint of
            Left e -> Left e
            Right cpoint -> Right $ AdvancingCPoint cpoint
--should not need
--newAdvancingCpointE _ (Just outerDistance)  Nothing _  advancingCpoint =
  --Left "Joiners.Delaunay.newAdvancingCpointE shows an outer distance, but there is no outer cpoint"

--should not need
--newAdvancingCpointE _ (Just innerDistance) _ Nothing advancingCpoint =
--  Left "Joiners.Delaunay.newAdvancingCpointE shows an inner distance, but there are no innerPerimeters"
      
newAdvancingCpointE (Just innerDistance) Nothing  _ (Just(InnerPerimeter innerPerimeter)) (AdvancingCPoint advancingCpoint) =
  case (head innerPerimeter) `raisedTo` advancingCpoint of
    Left e -> Left e
    Right cpoint -> Right $ AdvancingCPoint cpoint

newAdvancingCpointE  Nothing (Just outerDistance) (Just outerCpoint) _ (AdvancingCPoint advancingCpoint) =
  case outerCpoint `raisedTo` advancingCpoint of
    Left e -> Left e
    Right cpoint -> Right $ AdvancingCPoint cpoint
 
{-
newAdvancingCpointE :: Maybe DistanceA -> --from center of advancing cpoint to head of outer perims
                       Maybe DistanceA -> --from center of advancing cpoint to head of head of orderedNonEmptyInnerPerims
                       Maybe CornerPoints -> --The head $ outer perims
                       --Maybe [[CornerPoints]] -> --[[]] of all inner perimters
                       Maybe Perimeters -> --[[]] of all inner perimters
                       CornerPoints -> --the current advancingCpoint, from which advancingCpointNew is build from.
                       Either String CornerPoints
                            --Right: the new advancing cpoint build from advancingCpoint and closes perim cpoint
                            --Left: the eror
newAdvancingCpointE (Just outerDistance) (Just innerDistance) (Just outerCpoint) (Just (InnerPerimeters innerPerimeters)) advancingCpoint =
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

newAdvancingCpointE (Just outerDistance) _ Nothing _  advancingCpoint =
  Left "Joiners.Delaunay.newAdvancingCpointE shows an outer distance, but there is no outer cpoint"

newAdvancingCpointE _ (Just innerDistance) _ Nothing advancingCpoint =
  Left "Joiners.Delaunay.newAdvancingCpointE shows an inner distance, but there are no innerPerimeters"
      
newAdvancingCpointE (Nothing) (Just innerDistance) _ (Just(InnerPerimeters innerPerimeters)) advancingCpoint =
  (head $ head innerPerimeters) `raisedTo` advancingCpoint 

newAdvancingCpointE (Just outerDistance) Nothing (Just outerCpoint) _ advancingCpoint =
  outerCpoint `raisedTo` advancingCpoint


-}
--version used prior to delaunayB
appendAdvancingCpointToJoinedCpoints advancingCpoint joinedCpoints = advancingCpoint : joinedCpoints

--version used in delaunayB
appendAdvancingCpointToJoinedCpointsE :: AdvancingCPoint -> [CornerPoints] -> [CornerPoints]
appendAdvancingCpointToJoinedCpointsE (AdvancingCPoint advancingCpoint) joinedCpoints = advancingCpoint : joinedCpoints

--extract the outer perimeters from the innerOuterRemoved 
extractOuterPerimsWithAdvancingCpointRemoved (inner,outer) = outer -- [[CornerPointsError "inner perims with used cpoint removed filler"]]

----extract the inner perimeters from the innerOuterRemoved. Then append them back onto $ tail orderedInnerPerims 
extractInnerPerimsWithAdvancingCpointRemoved :: ((Maybe [CornerPoints]), a) -> Maybe [[CornerPoints]] -> Maybe [[CornerPoints]] 
extractInnerPerimsWithAdvancingCpointRemoved ((Just inner, _)) (Just innerPerimsTail) = Just $ inner : innerPerimsTail -- [[CornerPointsError "inner perims with used cpoint removed filler"]]
extractInnerPerimsWithAdvancingCpointRemoved ((Nothing), _) (Just innerPerimsTail) = Just innerPerimsTail
extractInnerPerimsWithAdvancingCpointRemoved ((Nothing), _) (Nothing) = Nothing
      --extractInnerPerimsWithAdvancingCpointRemoved (Left e) _ = Left e

{-
extractInnerPerimsWithAdvancingCpointRemoved :: ([CornerPoints],[CornerPoints]) -> [[CornerPoints]] -> [[CornerPoints]] 
extractInnerPerimsWithAdvancingCpointRemoved (inner,outer) innerPerimsTail = inner : innerPerimsTail -- [[CornerPointsError "inner perims with used cpoint removed filler"]]
      --extractInnerPerimsWithAdvancingCpointRemoved (Left e) _ = Left e
-}

headOfOrderedInnerPerims :: Maybe Perimeters  -> Maybe Perimeters
headOfOrderedInnerPerims (Just (InnerPerimeters(x:xs))) = Just $ InnerPerimeter x
headOfOrderedInnerPerims (Nothing) = Nothing


innerOuterRemoved' :: (Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))
innerOuterRemoved' (Just (InnerPerimeters(innerPerimeter:innerPerimeters))) (Just (OuterPerimeter (o:outerPerimeter ))) advancingCpoint =
  let
    --remove the head cpoint if used in the advancing line.
    --can be 1 or both lists
    --ensures that at least 1 is removed
    --error if none are removed
    removeOneOrBothIfUsed :: Maybe Perimeters -> --inner perims
                         Maybe Perimeters -> --outer perims
                         AdvancingCPoint ->   --advancing cpoint
                         Either String ((Maybe Perimeters),(Maybe Perimeters))
                         --left if no cpoints, or 2 cpoints removed
    removeOneOrBothIfUsed  (Just (InnerPerimeter(i:innerPerimeter))) (Just (OuterPerimeter(o:outerPerimeter))) (AdvancingCPoint advancingPoint) =
     let
      
      
      process :: Bool -> --outer perims
               Bool -> --inner perims
               --Either String ([CornerPoints],[CornerPoints]) --the inner list
               Either String ((Maybe Perimeters),(Maybe Perimeters))
               
      process True True =
       --Right (justifyPerimeters outerPerimeter, justifyPerimeters innerPerimeter)
       Right (justifyPerimeters' $ InnerPerimeter innerPerimeter, justifyPerimeters' $ OuterPerimeter outerPerimeter)
      
      process True False =
       --Right (justifyPerimeters outerPerimeter, justifyPerimeters $ i:innerPerimeter)
       Right (justifyPerimeters' $ InnerPerimeter $ i:innerPerimeter, justifyPerimeters' $ OuterPerimeter outerPerimeter)
      
      process False True =
       --Right (justifyPerimeters $ o:outerPerimeter, justifyPerimeters innerPerimeter)
       Right (justifyPerimeters' $ InnerPerimeter innerPerimeter , justifyPerimeters' $ OuterPerimeter $ o:outerPerimeter)
      
      process False False =
       Left $ "Joiners.Delaunay.removeIfUsed: did not find a used cpoint in inner/outer perimeters for: \n advancingCpoint: " ++
             (show advancingPoint) ++
             "\n outer cpoint: " ++ (show o) ++
             "\n inner cpoint: " ++ (show i)
      
     in
     case process <$>   advancingPoint `contains` i <*> advancingPoint `contains` o  of
      Right (Left e) -> Left e
      --Right (Right val) -> Right val
      Right val -> val
      Left e -> Left $ "Joiners.Delaunay.removeIfUsed: threw error from 'contains': " ++ e
    
    removeOneOrBothIfUsed Nothing (Just (OuterPerimeter(o:outerPerimeter)))  (AdvancingCPoint advancingPoint) =
     let
      process :: Bool -> --outer perims
               --Either String ([CornerPoints],[CornerPoints]) --the inner list
               Either String (Maybe Perimeters,Maybe Perimeters) --the inner list
               
      process True =
       --Right (outerCpoints, []) justifyPerimeters
       Right (Nothing, justifyPerimeters' $  OuterPerimeter outerPerimeter)
      process False  =
       Left $ "Joiners.Delaunay.removeIfUsed: did not find a used cpoint in outer perimeters for: " ++ (show advancingPoint) ++ " containing " ++ (show o)
      
      
      
     in
     case process <$>  advancingPoint `contains` o  of
      Right (Left e) -> Left e
      Right val -> val
      Left e -> Left $ "Joiners.Delaunay.removeIfUsed: threw error from 'contains': " ++ e
    
    removeOneOrBothIfUsed (Just (InnerPerimeter(i:innerPerimeter))) Nothing  (AdvancingCPoint advancingPoint) =
     let
      process :: Bool -> --outer perims
               --Either String ([CornerPoints],[CornerPoints]) --the inner list
               Either String (Maybe Perimeters,Maybe Perimeters) --the inner list
               
      process True =
       --Right ([], innerCpoints)
       Right (Nothing, justifyPerimeters' $ InnerPerimeter innerPerimeter)
      process False  =
       Left $ "Joiners.Delaunay.removeIfUsed: did not find a used cpoint in inner perimeters for: " ++ (show advancingPoint) ++ " containing " ++ (show i)
      
      
      
     in
     case process <$>  advancingPoint `contains` i  of
      Right (Left e) -> Left e
      Right val -> val
      Left e -> Left $ "Joiners.Delaunay.removeIfUsed: threw error from 'contains': " ++ e
    
    removeOneOrBothIfUsed Nothing Nothing _ =
     Right (Nothing, Nothing)
  
  
  in
        case (removeOneOrBothIfUsed (Just (InnerPerimeters(innerPerimeter:innerPerimeters))) (Just (OuterPerimeter (o:outerPerimeter ))) advancingCpoint) of
          Left e -> Left e
          --Right (Left e) -> Left e
          (Right (outerPerimeter,Just (InnerPerimeter innerPerimeter))) ->
            Right (outerPerimeter,(Just $ InnerPerimeters (innerPerimeter : innerPerimeters)))
          (Right (outerPerimeter,Nothing)) ->
            (Right (outerPerimeter,Nothing))

{-
innerOuterRemoved' :: (Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))
innerOuterRemoved' (Just (InnerPerimeters(innerPerimeter:innerPerimeters))) (Just (OuterPerimeter (o:outerPerimeter ))) advancingCpoint =
        case (removeOneOrBothIfUsed (Just (InnerPerimeters(innerPerimeter:innerPerimeters))) (Just (OuterPerimeter (o:outerPerimeter ))) advancingCpoint) of
          Left e -> Left e
          --Right (Left e) -> Left e
          (Right (outerPerimeter,Just (InnerPerimeter innerPerimeter))) ->
            Right (outerPerimeter,(Just $ InnerPerimeters (innerPerimeter : innerPerimeters)))
          (Right (outerPerimeter,Nothing)) ->
            (Right (outerPerimeter,Nothing))
-}

justifyPerimeters :: [a] -> Maybe [a]
justifyPerimeters list =
  case (length list) == 0 of
    True -> Nothing
    False -> Just list

justifyPerimeters' :: Perimeters -> Maybe Perimeters
justifyPerimeters' (OuterPerimeter outerPerimeter) =
      case (length outerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ OuterPerimeter outerPerimeter
justifyPerimeters' (InnerPerimeter innerPerimeter) =
      case (length innerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ InnerPerimeter innerPerimeter
justifyPerimeters' (InnerPerimeters innerPerimeters) =
      case (length innerPerimeters) == 0 of
        True -> Nothing
        False -> Just $ InnerPerimeters innerPerimeters
        
      
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
      
orderByDistance ::  Maybe Perimeters ->
                    AdvancingCPoint ->    -- The outer perim cpoint
                    Either String (Maybe Perimeters)    -- [[good inner perim distance CornerPoints]] ordered by distance to advancing line
{-
orderByDistance  Nothing _ =
  --Left $ "Joiners.Deluanay.orderByDistance: empty [[inner perimeters]] passed in"
  Right Nothing
-}
orderByDistance  (Just(InnerPerimeters innerPerimeters)) (AdvancingCPoint distanceFromThisCpoint) = 
     let sorter innerPerimeter =
          calculateDistanceA <$>
              (head innerPerimeter) `raisedTo` ((head innerPerimeter) +++ distanceFromThisCpoint) <*> Right (head innerPerimeter)
     in
     --Right $ sortOn (sorter) innerPerimeters
     case (length $ removeEmpty innerPerimeters) == 0 of
       True -> Right Nothing
       False ->
         case (sortOn sorter <$>  Right innerPerimeters) of
           Left e -> Left e
           Right val -> Right $ Just $ InnerPerimeters val 

orderByDistance Nothing _ = Right Nothing
orderByDistance  (Just(InnerPerimeter _)) _ = Left "Deluanay.orderByDistance only works on InnerPerimeters but had an InnerPerimeter passed in "
orderByDistance  (Just(OuterPerimeter _)) _ = Left "Deluanay.orderByDistance only works on InnerPerimeters but had an OuterPerimeter passed in " 
{-
orderByDistance :: --[[CornerPoints]] -> -- [[good inner perim distance CornerPoints]].
                    Maybe [[CornerPoints]] ->
                    CornerPoints ->    -- The outer perim cpoint
                    --Either String [[CornerPoints]]    -- [[good inner perim distance CornerPoints]] ordered by distance to advancing line
                    Either String (Maybe [[CornerPoints]])    -- [[good inner perim distance CornerPoints]] ordered by distance to advancing line

orderByDistance  Nothing _ =
  --Left $ "Joiners.Deluanay.orderByDistance: empty [[inner perimeters]] passed in"
  Right Nothing
  
orderByDistance  (Just innerPerimeters) outerPerimCpoint = 
     let sorter j =
          calculateDistanceA <$>
              (head j) `raisedTo` ((head j) +++ outerPerimCpoint) <*> Right (head j)
     in
     --Right $ sortOn (sorter) innerPerimeters
     Just (sortOn sorter <$>  Right innerPerimeters)

-}

{-
orderByDistance  (innerPerimeters) outerPerimCpoint = 
     let sorter j =
          calculateDistanceA <$>
              (head j) `raisedTo` ((head j) +++ outerPerimCpoint) <*> Right (head j)
     in
     --Right $ sortOn (sorter) innerPerimeters
     (sortOn sorter <$>  Right innerPerimeters)
-}

{-keep this orig until tested as it no longer used the DistanceA 100000000.0
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
