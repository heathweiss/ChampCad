{-# LANGUAGE TemplateHaskell #-}
{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}
module Joiners.Delaunay(delaunayBCurried, delaunayB, orderedInnerPerims', advancingCpointFromHeadOfInnerPerims, advancingCpointFromHeadOfOuterPerims,
                        orderInnerPerimsByDistanceFromHead, extractI, removeAdvCPointFromIOPerims, removeContainedCPointFromHeadOfPerims, Perimeters(..), AdvancingCPoint(..)) where

import Data.Typeable

import CornerPoints.CornerPoints(CornerPoints(..),(+++),(++++))
import CornerPoints.FaceExtraction(extractB1,extractFrontLeftLine, extractF1, extractBackLeftLine, contains )
import CornerPoints.FaceConversions(toLeftFace, raisedTo, toBottomLeftLine)

import Math.Distance(Distance(..), Distant, calculateDistance, DistanceA(..), DistantA, calculateDistanceA
                    , center, (<-|->), centerA, (<-||->))

import Helpers.List(removeEmpty, safeTail, safeHead)
import Helpers.Applicative(extractE)
import qualified TypeClasses.Showable as TS
import Data.List(filter, sortOn)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------depracated-----------------------------------------------------------------------------------
{-This entire module is depracated. Keep it around till I know I don't need any of the original delaunayB code.
Replaced with Joiners.Advance, and it's support modules used to break up this monstrosity. -}


--delaunayBCurried = delaunayBase (removeContainedCPointFromHeadOfPerims) (newAdvancingCpointE)
delaunayBCurried = delaunayBase
                    (removeContainedCPointFromHeadOfPerims)
                    (advancingCpointFromHeadOfInnerPerims)
                    advancingCpointFromHeadOfOuterPerims
                    --advancingCpointFromDoublePerimsUsingCenterOfAdvancingLines
                    advancingCpointFromDoublePerimsUsingDistanceToCpoints

delaunayBase ::  ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                   --removeAdvancingCPointFromPerimeters
                 (Maybe Perimeters -> --[[]] of all inner perimters
                  AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                  Either String AdvancingCPoint) ->
                    --removeAdvancingCPointFromPerimeters
                 (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                   --advancingCpointFromHeadOfOuterPerims
                 -- =======================================================================================
                 (
                  ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                    --remove advancingCpoints from perimeters
                  (
                   Maybe Perimeters -> --[[]] of all inner perimters
                   AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                   Either String AdvancingCPoint) ->
                    --removeAdvancingCPointFromPerimeters
                   (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                    --advancingCpointFromHeadOfOuterPerims
                   Maybe Perimeters -> --inner
                   Maybe Perimeters -> --outer
                   AdvancingCPoint -> -- advancing cpoint. This has to be appended to joinedCpoints inside delaunayB'
                   [CornerPoints] ->   -- joinedCpoints. Should move this after advancing point.
                   Either String [CornerPoints] -- joined cpoints or error
                 ) ->
               -- ========================================================================================
                 [[CornerPoints]] -> --inner perim cpoints
                 [CornerPoints] -> --outer perim cpoints
                 [CornerPoints]
delaunayBase   removeAdvancingCPointFromPerimeters
               createAdvancingCpointFromInnerPerimeters
               advancingCpointFromOuterPerims
               doublePerimDecision
               innerPerimeters outerPerimeters  =
  
  let --As this is the very 1st AdvancingCPoint, build it from head OuterPermiter, and the nearest head InnerPerimiter
      {-
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
                         (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters) --justifiedInnerPerims
                         (justifyPerimeters $ OuterPerimeter outerPerimeters) --justifiedOuterPerims
      -}
      advancingCpoint :: Either String AdvancingCPoint
      advancingCpoint = createAdvancingCpointFromInnerPerimeters (Just $ InnerPerimeters innerPerimeters) (AdvancingCPoint $ head outerPerimeters)

        
      perimsWithAdvancingCpointBldrRemoved =
                           extractE
                             (removeAdvancingCPointFromPerimeters 
                                 (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters)
                                 (justifyPerimeters $ OuterPerimeter outerPerimeters) <$>
                                 advancingCpoint
                             )

  in
  case delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims  doublePerimDecision<$> 
        (fst <$> perimsWithAdvancingCpointBldrRemoved) <*>
        (snd <$> perimsWithAdvancingCpointBldrRemoved) <*>
        advancingCpoint <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpointsE <$> advancingCpoint <*> Right [])  --joined cpoints with the advancing cpoint added to it.
        
  of
    Left e -> [CornerPointsError $ "Joiners.DeluanayB(Left e): " ++  e]
    Right (Left e) -> [CornerPointsError $ "Joiners.DeluanayB(Righ(Left e)): " ++ e]
    Right (Right val) -> val
{-
delaunayBase ::  ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                   --remove advancingCpoints from perimeters
                 (--Maybe DistanceA -> --from center of advancing cpoint to head of innerPerim
                  --Maybe DistanceA -> --from center of advancing cpoint to outerPerim
                  --Maybe CornerPoints -> --The head $ outer perims
                  Maybe Perimeters -> --[[]] of all inner perimters
                  AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                  Either String AdvancingCPoint) ->
                 [[CornerPoints]] -> --inner perim cpoints
                 [CornerPoints] -> --outer perim cpoints
                 [CornerPoints]
delaunayBase   removeAdvancingCPointFromPerimeters createNewAdvancingCPoint innerPerimeters outerPerimeters  =
  
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
                         (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters) --justifiedInnerPerims
                         (justifyPerimeters $ OuterPerimeter outerPerimeters) --justifiedOuterPerims
      
      perimsWithAdvancingCpointBldrRemoved =
                           extractE
                             (removeAdvancingCPointFromPerimeters 
                                 (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters)
                                 (justifyPerimeters $ OuterPerimeter outerPerimeters) <$>
                                 advancingCpoint
                             )

  in
  case delaunayBase' removeAdvancingCPointFromPerimeters createNewAdvancingCPoint <$> 
        (fst <$> perimsWithAdvancingCpointBldrRemoved) <*>
        (snd <$> perimsWithAdvancingCpointBldrRemoved) <*>
        advancingCpoint <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpointsE <$> advancingCpoint <*> Right [])  --joined cpoints with the advancing cpoint added to it.
        
  of
    Left e -> [CornerPointsError $ "Joiners.DeluanayB(Left e): " ++  e]
    Right (Left e) -> [CornerPointsError $ "Joiners.DeluanayB(Righ(Left e)): " ++ e]
    Right (Right val) -> val
-}

delaunayBase' ::
               ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                   --remove advancingCpoints from perimeters
               (
                Maybe Perimeters -> --[[]] of all inner perimters
                AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                Either String AdvancingCPoint) ->
               --removeAdvancingCPointFromPerimeters
               (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
               -- =======================================================================================
               (
                 ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                       --remove advancingCpoints from perimeters
                       (
                        Maybe Perimeters -> --[[]] of all inner perimters
                        AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                        Either String AdvancingCPoint) ->
                        --removeAdvancingCPointFromPerimeters
                        (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                        --advancingCpointFromHeadOfOuterPerims
                        Maybe Perimeters -> --inner
                        Maybe Perimeters -> --outer
                        AdvancingCPoint -> -- advancing cpoint. This has to be appended to joinedCpoints inside delaunayB'
                        [CornerPoints] ->   -- joinedCpoints. Should move this after advancing point.
                        Either String [CornerPoints] -- joined cpoints or error
               ) ->
               -- ========================================================================================
                   --advancingCpointFromHeadOfOuterPerims
               Maybe Perimeters -> --inner
               Maybe Perimeters -> --outer
               
               AdvancingCPoint -> -- advancing cpoint. This has to be appended to joinedCpoints inside delaunayB'
               [CornerPoints] ->   -- joinedCpoints. Should move this after advancing point.
               Either String [CornerPoints] -- joined cpoints or error

delaunayBase' removeAdvancingCPointFromPerimeters
              createAdvancingCpointFromInnerPerimeters
              advancingCpointFromOuterPerims
              doublePerimDecision
              Nothing Nothing _ joinedCpoints = Right $ reverse joinedCpoints
--the following 2 should have been taken car of innerOuterRemoved
delaunayBase' removeAdvancingCPointFromPerimeters
              createAdvancingCpointFromInnerPerimeters
              advancingCpointFromOuterPerims
              doublePerimDecision
              (Just(InnerPerimeters [])) Nothing _ joinedCpoints = Right $ reverse joinedCpoints
delaunayBase' removeAdvancingCPointFromPerimeters
              createAdvancingCpointFromInnerPerimeters
              advancingCpointFromOuterPerims
              doublePerimDecision
              (Just(InnerPerimeters [[]])) Nothing _ joinedCpoints = Right $ reverse joinedCpoints



delaunayBase' removeAdvancingCPointFromPerimeters
              createAdvancingCpointFromInnerPerimeters
              advancingCpointFromOuterPerims
              doublePerimDecision
              innerPerimeters outerPerimeters  advancingCpointIn joinedCpoints =
  let
    safeO :: Maybe Perimeters -> Maybe CornerPoints
    safeO Nothing = Nothing
    safeO (Just (OuterPerimeter outerPerimeter)) =
      case (length outerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ head outerPerimeter
    
    
  in
    --justify both perims to make sure still have some
    case innerPerimeters of
      Nothing ->
        --call again with InnerPerimeters [] so can use logic for that 
        delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision (Just $ InnerPerimeters []) outerPerimeters  advancingCpointIn joinedCpoints
      --re-call with InnerPerimeters
      Just (InnerPerimeter innerPerimeter') ->
        delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision (Just $ InnerPerimeters [innerPerimeter']) outerPerimeters  advancingCpointIn joinedCpoints
      
      --has InnerPerimeters
      Just (InnerPerimeters innerPerimeters') ->
        case justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters' of
          --Inner: empty
          Nothing ->
            case (outerPerimeters) of
              --inner empty, outer empty
              Nothing ->
                delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision Nothing Nothing  advancingCpointIn joinedCpoints
              --inner empty, outer good but not justified
              (Just (OuterPerimeter outerPerimeter')) ->
                case justifyPerimeters (OuterPerimeter outerPerimeter') of
                  --inner: empty, outer: empty
                  Nothing -> delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision Nothing Nothing  advancingCpointIn joinedCpoints
                  --inner: empty, outer: good
                  Just (OuterPerimeter outerPerimeter') -> 
                    let --advancing point from outerPerimeter' only
                        advancingCpointNewE :: Either String AdvancingCPoint
                        -- ==========================================================this should be using outerPerimeters, which will require a diff fx to create advancint cpoint=====================
                        --advancingCpoint = createAdvancingCpointFromInnerPerimeters (Just $ InnerPerimeters innerPerimeters') (AdvancingCPoint $ head outerPerimeter')
                        advancingCpointNewE = advancingCpointFromOuterPerims (Just $ OuterPerimeter outerPerimeter') (advancingCpointIn)

                        perimsWithAdvancingCpointBldrRemoved =
                           extractE
                             (removeAdvancingCPointFromPerimeters 
                                 Nothing 
                                 (Just $ OuterPerimeter outerPerimeter') <$>
                                 advancingCpointNewE
                             )
                    in  --Left "inner: empty, outer: good filled to compile"
                      extractE $
                         delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision<$> 
                           (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                           (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                           advancingCpointNewE <*>                             --The advancing Cpoint just created.
                           (appendAdvancingCpointToJoinedCpointsE <$> advancingCpointNewE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
                      
          --Inner: good    
          Just (InnerPerimeters innerPerimeters') ->
            case (outerPerimeters) of
              --inner empty, outer empty
              Nothing -> delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision Nothing Nothing  advancingCpointIn joinedCpoints
              --inner good, outer good but not justified
              (Just (OuterPerimeter outerPerimeter')) ->
                case justifyPerimeters (OuterPerimeter outerPerimeter') of
                  --Inner: good, outer: empty
                  Nothing -> 
                    let
                      advancingCpointNewE = createAdvancingCpointFromInnerPerimeters (Just $ InnerPerimeters innerPerimeters') advancingCpointIn
                      innerOuterRemoved = extractE (removeAdvancingCPointFromPerimeters (Just $ InnerPerimeters innerPerimeters') outerPerimeters <$> advancingCpointNewE )
                    in
                      --Left "inner: good, outer: empty filled to compile"
                      
                      extractE $
                         delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision<$> 
                           (fst <$> innerOuterRemoved ) <*>
                           (snd <$> innerOuterRemoved ) <*>
                           advancingCpointNewE <*>                             --The advancing Cpoint just created.
                           (appendAdvancingCpointToJoinedCpointsE <$> advancingCpointNewE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
                  --Inner: good, outer: good ============================== filled to compile
                  Just (OuterPerimeter outerPerimeter') ->
                    --Left "doublePerimDecision will go here"
                    doublePerimDecision
                      removeAdvancingCPointFromPerimeters
                      createAdvancingCpointFromInnerPerimeters
                      advancingCpointFromOuterPerims
                      (Just (InnerPerimeters innerPerimeters'))
                      (Just (OuterPerimeter outerPerimeter'))
                      advancingCpointIn
                      joinedCpoints
                    {-
                    let getAdvancingCpoint (AdvancingCPoint advancingCpoint') = advancingCpoint'
                        
                        advancingInnerCpointE =
                          createAdvancingCpointFromInnerPerimeters (Just $ InnerPerimeters innerPerimeters') (advancingCpointIn)
                        advancingInnerCpointEAsCpoint =
                          case advancingInnerCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
                        distanceToInnerE :: Either String DistanceA
                        distanceToInnerE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpointIn) <$> advancingInnerCpointEAsCpoint

                        advancingOuterCpointE =
                          advancingCpointFromOuterPerims (Just $ OuterPerimeter outerPerimeter') (advancingCpointIn)
                        advancingOuterCpointEAsCpoint =
                          case advancingOuterCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
                        distanceToOuterE :: Either String DistanceA
                        distanceToOuterE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpointIn) <$> advancingOuterCpointEAsCpoint
                        isOuterDistanceLTInnerDistance' :: DistanceA -> DistanceA -> Bool
                        isOuterDistanceLTInnerDistance'  (DistanceA innerDistance) (DistanceA outerDistance ) =
                          outerDistance < innerDistance
            
                        isOuterDistanceLTInnerDistance :: Either String Bool
                        isOuterDistanceLTInnerDistance = isOuterDistanceLTInnerDistance' <$> distanceToInnerE <*> distanceToOuterE  
                        --get outer advancing cpoint 
                        --get inner advancing cpoint
                        --if distanct curr advancing cpoint  to outer advancing cpoint
                        -- <
                        -- distanct curr advancing cpoint  to inner advancing cpoint
                        --then use outer advancing cpoint
                        --else use inner advancing cpoint
                        
                    in
                      --Left "inner: good, outer: good
                      --This should be a drop in fx so this decision system can be swapped out.
                      --Can try the original distance to points instead of this distance to advancing lines.
                      case isOuterDistanceLTInnerDistance of
                        Left e -> Left $ "Joiners.Delaunay.delaunayBase' failed for good inner/outer perims, while finding distances because: " ++ e
                        --outer distance <
                        Right True ->
                          let perimsWithAdvancingCpointBldrRemoved =
                               extractE
                                (removeAdvancingCPointFromPerimeters 
                                 (Just $ InnerPerimeters innerPerimeters') 
                                 (Just $ OuterPerimeter outerPerimeter') <$>
                                 advancingOuterCpointE
                                )
                          in
                            extractE $
                              delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims <$> 
                              (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              advancingOuterCpointE <*>                             --The advancing Cpoint just created.
                              (appendAdvancingCpointToJoinedCpointsE <$> advancingOuterCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
                        --outer distance >=
                        Right False ->
                          let perimsWithAdvancingCpointBldrRemoved =
                               extractE
                                (removeAdvancingCPointFromPerimeters 
                                 (Just $ InnerPerimeters innerPerimeters') 
                                 (Just $ OuterPerimeter outerPerimeter') <$>
                                 advancingInnerCpointE
                                )
                          in
                            extractE $
                              delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims <$> 
                              (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              advancingInnerCpointE <*>                             --The advancing Cpoint just created.
                              (appendAdvancingCpointToJoinedCpointsE <$> advancingInnerCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
-}

{-
Figure out which Cpoint to use when there is both a inner and outer perimetere available.
Create an advancing line for both of them, then check the distance to each of these, using the shorter distance.

ToDo:
Solve the problem of the closer advancing line crossing the inner perimeter.
-}
advancingCpointFromDoublePerimsUsingCenterOfAdvancingLines :: ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                       --remove advancingCpoints from perimeters
                       (
                        Maybe Perimeters -> --[[]] of all inner perimters
                        AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                        Either String AdvancingCPoint) ->
                        --removeAdvancingCPointFromPerimeters
                        (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                        --advancingCpointFromHeadOfOuterPerims
                        Maybe Perimeters -> --inner
                        Maybe Perimeters -> --outer
                        AdvancingCPoint -> -- advancing cpoint. This has to be appended to joinedCpoints inside delaunayB'
                        [CornerPoints] ->   -- joinedCpoints. Should move this after advancing point.
                        Either String [CornerPoints] -- joined cpoints or error
advancingCpointFromDoublePerimsUsingCenterOfAdvancingLines removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims innerPerimeters' outerPerimeter'  advancingCpointIn joinedCpoints =
                    let getAdvancingCpoint (AdvancingCPoint advancingCpoint') = advancingCpoint'
                        
                        advancingInnerCpointE =
                          createAdvancingCpointFromInnerPerimeters innerPerimeters' (advancingCpointIn)
                        advancingInnerCpointEAsCpoint =
                          case advancingInnerCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
                        distanceToInnerE :: Either String DistanceA
                        distanceToInnerE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpointIn) <$> advancingInnerCpointEAsCpoint

                        advancingOuterCpointE =
                          advancingCpointFromOuterPerims outerPerimeter' (advancingCpointIn)
                        advancingOuterCpointEAsCpoint =
                          case advancingOuterCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
                        distanceToOuterE :: Either String DistanceA
                        distanceToOuterE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpointIn) <$> advancingOuterCpointEAsCpoint
                        isOuterDistanceLTInnerDistance' :: DistanceA -> DistanceA -> Bool
                        isOuterDistanceLTInnerDistance'  (DistanceA innerDistance) (DistanceA outerDistance ) =
                          outerDistance < innerDistance
            
                        isOuterDistanceLTInnerDistance :: Either String Bool
                        isOuterDistanceLTInnerDistance = isOuterDistanceLTInnerDistance' <$> distanceToInnerE <*> distanceToOuterE  
                        --get outer advancing cpoint 
                        --get inner advancing cpoint
                        --if distanct curr advancing cpoint  to outer advancing cpoint
                        -- <
                        -- distanct curr advancing cpoint  to inner advancing cpoint
                        --then use outer advancing cpoint
                        --else use inner advancing cpoint
                        
                    in
                      --Left "inner: good, outer: good
                      --This should be a drop in fx so this decision system can be swapped out.
                      --Can try the original distance to points instead of this distance to advancing lines.
                      case isOuterDistanceLTInnerDistance of
                        Left e -> Left $ "Joiners.Delaunay.delaunayBase' failed for good inner/outer perims, while finding distances because: " ++ e
                        --outer distance <
                        Right True ->
                          let perimsWithAdvancingCpointBldrRemoved =
                               extractE
                                (removeAdvancingCPointFromPerimeters 
                                 innerPerimeters' 
                                 outerPerimeter' <$>
                                 advancingOuterCpointE
                                )
                          in
                            extractE $
                              delaunayBase' removeAdvancingCPointFromPerimeters
                                            createAdvancingCpointFromInnerPerimeters
                                            advancingCpointFromOuterPerims
                                            advancingCpointFromDoublePerimsUsingCenterOfAdvancingLines <$> 
                              (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              advancingOuterCpointE <*>                             --The advancing Cpoint just created.
                              (appendAdvancingCpointToJoinedCpointsE <$> advancingOuterCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
                        --outer distance >=
                        Right False ->
                          let perimsWithAdvancingCpointBldrRemoved =
                               extractE
                                (removeAdvancingCPointFromPerimeters 
                                 innerPerimeters'
                                 outerPerimeter' <$>
                                 advancingInnerCpointE
                                )
                          in
                            extractE $
                              delaunayBase'
                                removeAdvancingCPointFromPerimeters
                                createAdvancingCpointFromInnerPerimeters
                                advancingCpointFromOuterPerims
                                advancingCpointFromDoublePerimsUsingCenterOfAdvancingLines <$> 
                              (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                              advancingInnerCpointE <*>                             --The advancing Cpoint just created.
                              (appendAdvancingCpointToJoinedCpointsE <$> advancingInnerCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.



{-
Figure out which Cpoint to use when there is both a inner and outer perimetere available.
Get distance to advancing line for both of them, then check the distance to each of these, using the shorter distance.


-}
advancingCpointFromDoublePerimsUsingDistanceToCpoints :: ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                       --remove advancingCpoints from perimeters
                       (
                        Maybe Perimeters -> --[[]] of all inner perimters
                        AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                        Either String AdvancingCPoint) ->
                        --removeAdvancingCPointFromPerimeters
                        (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                        --advancingCpointFromHeadOfOuterPerims
                        Maybe Perimeters -> --inner
                        Maybe Perimeters -> --outer
                        AdvancingCPoint -> -- advancing cpoint. This has to be appended to joinedCpoints inside delaunayB'
                        [CornerPoints] ->   -- joinedCpoints. Should move this after advancing point.
                        Either String [CornerPoints] -- joined cpoints or error

advancingCpointFromDoublePerimsUsingDistanceToCpoints
  removeAdvancingCPointFromPerimeters
  createAdvancingCpointFromInnerPerimeters
  advancingCpointFromOuterPerims
  innerPerimeters'
  outerPerimeter'
  advancingCpointIn
  joinedCpoints =
     let getAdvancingCpoint (AdvancingCPoint advancingCpoint') = advancingCpoint'
         getInnerPerimeterHead :: (Maybe (Perimeters )) -> [CornerPoints]
         getInnerPerimeterHead  (Just (InnerPerimeters (i:is))) =
           i
         getOuterPerimeterHead :: (Maybe (Perimeters )) -> CornerPoints
         getOuterPerimeterHead (Just (OuterPerimeter (o:os))) =
           o
                
         advancingInnerCpointE =
           createAdvancingCpointFromInnerPerimeters innerPerimeters' (advancingCpointIn)
         advancingInnerCpointEAsCpoint =
           case advancingInnerCpointE of
             Left e -> Left e
             Right (AdvancingCPoint cpoint) -> Right cpoint
         distanceToInnerE :: Either String DistanceA
         --distanceToInnerE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpointIn) <$> advancingInnerCpointEAsCpoint
         distanceToInnerE = calculateDistanceA (getAdvancingCpoint advancingCpointIn) (head $ getInnerPerimeterHead innerPerimeters')

         
         advancingOuterCpointE =
           advancingCpointFromOuterPerims outerPerimeter' (advancingCpointIn)
         advancingOuterCpointEAsCpoint =
           case advancingOuterCpointE of
             Left e -> Left e
             Right (AdvancingCPoint cpoint) -> Right cpoint
                            
         distanceToOuterE :: Either String DistanceA
         --distanceToOuterE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpointIn) <$> advancingOuterCpointEAsCpoint
         distanceToOuterE = calculateDistanceA (getAdvancingCpoint advancingCpointIn) (getOuterPerimeterHead outerPerimeter')

         isOuterDistanceLTInnerDistance' :: DistanceA -> DistanceA -> Bool
         isOuterDistanceLTInnerDistance'  (DistanceA innerDistance) (DistanceA outerDistance ) =
           outerDistance < innerDistance
            
         isOuterDistanceLTInnerDistance :: Either String Bool
         isOuterDistanceLTInnerDistance = isOuterDistanceLTInnerDistance' <$> distanceToInnerE <*> distanceToOuterE  
                        --get outer advancing cpoint 
                        --get inner advancing cpoint
                        --if distanct curr advancing cpoint  to outer advancing cpoint
                        -- <
                        -- distanct curr advancing cpoint  to inner advancing cpoint
                        --then use outer advancing cpoint
                        --else use inner advancing cpoint
                        
     in
     --Left "inner: good, outer: good
     --This should be a drop in fx so this decision system can be swapped out.
     --Can try the original distance to points instead of this distance to advancing lines.
      case isOuterDistanceLTInnerDistance of
        Left e -> Left $ "Joiners.Delaunay.delaunayBase' failed for good inner/outer perims, while finding distances because: " ++ e
        --outer distance <
        Right True ->
          let perimsWithAdvancingCpointBldrRemoved =
                extractE
                 (removeAdvancingCPointFromPerimeters 
                  innerPerimeters' 
                  outerPerimeter' <$>
                    advancingOuterCpointE
                 )
          in
            extractE $
              delaunayBase' removeAdvancingCPointFromPerimeters
                            createAdvancingCpointFromInnerPerimeters
                            advancingCpointFromOuterPerims
                            advancingCpointFromDoublePerimsUsingDistanceToCpoints <$> 
                             (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                             (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                             advancingOuterCpointE <*>                             --The advancing Cpoint just created.
                             (appendAdvancingCpointToJoinedCpointsE <$> advancingOuterCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
                        --outer distance >=
        Right False ->
          let perimsWithAdvancingCpointBldrRemoved =
                extractE
                  (removeAdvancingCPointFromPerimeters 
                   innerPerimeters'
                   outerPerimeter' <$>
                     advancingInnerCpointE
                  )
          in
            extractE $
              delaunayBase'
                removeAdvancingCPointFromPerimeters
                createAdvancingCpointFromInnerPerimeters
                advancingCpointFromOuterPerims
                advancingCpointFromDoublePerimsUsingDistanceToCpoints <$> 
                  (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                  (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                  advancingInnerCpointE <*>                             --The advancing Cpoint just created.
                  (appendAdvancingCpointToJoinedCpointsE <$> advancingInnerCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.




  
    {-
    extractE
    (delaunayBase' removeAdvancingCPointFromPerimeters createNewAdvancingCPoint <$> 
        (fst <$> innerOuterRemoved ) <*>
          --If used to build advancing cpoint, should have cpoint removed.
        
        --(extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap tail orderedNonEmptyInnerPerimsE ) <*>
        --(removeEmpty <$> (extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap safeTail orderedNonEmptyInnerPerimsE )) <*>
        (snd <$> innerOuterRemoved ) <*>
        --If used to build advancing cpoint, should have cpoint removed.

        
        advancingCpointNewE <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpointsE <$> advancingCpointNewE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.-}
    

{-
delaunayBase' removeAdvancingCPointFromPerimeters createNewAdvancingCPoint innerPerimeters outerPerimeters  advancingCpoint joinedCpoints =
  let
    safeO :: Maybe Perimeters -> Maybe CornerPoints
    safeO Nothing = Nothing
    safeO (Just (OuterPerimeter outerPerimeter)) =
      case (length outerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ head outerPerimeter
    
    --orderedNonEmptyInnerPerimsE = orderInnerPerimsByDistanceFromHead innerPerimeters advancingCpoint
    {-
    advancingCpointNewE = createNewAdvancingCPoint <$>
                           (calculateJustDistance advancingCpoint  orderedNonEmptyInnerPerimsE) <*>
                           (calculateJustDistance advancingCpoint (Right outerPerimeters)) <*>
                           (Right $ safeO outerPerimeters) <*>
                           orderedNonEmptyInnerPerimsE <*>
                           (Right advancingCpoint)

-}
    advancingCpointNewE = createNewAdvancingCPoint (innerPerimeters) (advancingCpoint)
    --if distanceToAdvancingCPoint  advancingInnerCpointNewE < distanceToAdvancingCPoint advancingOuterCpointNewE
      --then advancingInnerCpointNewE
      --else advancingOuterCpointNewE
                           
    --remove the used cpoint from the perims.
    --has a problem of return [] or [[]] instead of Nothing, and so DelaunayB' has to gaurd against that.
    --innerOuterRemoved = extractE (removeAdvancingCPointFromPerimeters innerPerimeters outerPerimeters <$> extractE advancingCpointNewE )
    innerOuterRemoved = extractE (removeAdvancingCPointFromPerimeters innerPerimeters outerPerimeters <$> advancingCpointNewE )
  in
    extractE
    (delaunayBase' removeAdvancingCPointFromPerimeters createNewAdvancingCPoint <$> 
        (fst <$> innerOuterRemoved ) <*>
          --If used to build advancing cpoint, should have cpoint removed.
        
        --(extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap tail orderedNonEmptyInnerPerimsE ) <*>
        --(removeEmpty <$> (extractInnerPerimsWithAdvancingCpointRemoved <$> innerOuterRemoved <*> fmap safeTail orderedNonEmptyInnerPerimsE )) <*>
        (snd <$> innerOuterRemoved ) <*>
        --If used to build advancing cpoint, should have cpoint removed.

        
        advancingCpointNewE <*>                             --The advancing Cpoint just created.
        (appendAdvancingCpointToJoinedCpointsE <$> advancingCpointNewE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
    )

-}

-- =================================================================orig delaunayB===================================================================
-- =================================================================orig delaunayB===================================================================
-- =================================================================orig delaunayB===================================================================
-- =================================================================orig delaunayB===================================================================

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
                         (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters) --justifiedInnerPerims
                         (justifyPerimeters $ OuterPerimeter outerPerimeters) --justifiedOuterPerims
      
      perimsWithAdvancingCpointBldrRemoved =
                           extractE
                             (removeAdvCPointFromIOPerims <$>
                                                 orderedInnerPerims
                                                   (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters)
                                                   (justifyPerimeters $ OuterPerimeter outerPerimeters)  <*>
                                                 Right (justifyPerimeters $ OuterPerimeter outerPerimeters) <*>
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
--the following 2 should have been taken car of innerOuterRemoved
delaunayB' (Just(InnerPerimeters [])) Nothing _ joinedCpoints = Right $ reverse joinedCpoints
delaunayB' (Just(InnerPerimeters [[]])) Nothing _ joinedCpoints = Right $ reverse joinedCpoints

delaunayB' innerPerimeters outerPerimeters  advancingCpoint joinedCpoints =
  let
    safeO :: Maybe Perimeters -> Maybe CornerPoints
    safeO Nothing = Nothing
    safeO (Just (OuterPerimeter outerPerimeter)) =
      case (length outerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ head outerPerimeter
    
    orderedNonEmptyInnerPerimsE = orderInnerPerimsByDistanceFromHead innerPerimeters advancingCpoint
    
    advancingCpointNewE = newAdvancingCpointE <$>
                           (calculateJustDistance advancingCpoint  orderedNonEmptyInnerPerimsE) <*>
                           (calculateJustDistance advancingCpoint (Right outerPerimeters)) <*>
                           (Right $ safeO outerPerimeters) <*>
                           orderedNonEmptyInnerPerimsE <*>
                           (Right advancingCpoint)
                           
    --remove the used cpoint from the perims.
    --has a problem of return [] or [[]] instead of Nothing, and so DelaunayB' has to gaurd against that.
    innerOuterRemoved = extractE (removeAdvCPointFromIOPerims <$> orderedNonEmptyInnerPerimsE <*> Right outerPerimeters <*> extractE advancingCpointNewE )
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

--version for delaunayB
orderedInnerPerims :: Maybe Perimeters ->  Maybe Perimeters ->  Either String (Maybe Perimeters)
orderedInnerPerims     (Just (InnerPerimeters justifiedInnerPerims)) (Just (OuterPerimeter (o:outerPerims)))  =
            orderInnerPerimsByDistanceFromHead (Just(InnerPerimeters justifiedInnerPerims)) (AdvancingCPoint o)
orderedInnerPerims     (Just (InnerPerimeter justifiedInnerPerims)) (Just (OuterPerimeter (o:outerPerims)))  =
            orderInnerPerimsByDistanceFromHead (Just(InnerPerimeters [justifiedInnerPerims])) (AdvancingCPoint o)
orderedInnerPerims Nothing _ = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty outer perims passed in"
orderedInnerPerims _ Nothing = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty inner perims passed in"
orderedInnerPerims perim1 perim2 =
  Left $ "Joiners.Deluany.orderedInnerPerims has invalid Perimeter types for:" ++ (show perim1) ++ " and : " ++ (show perim2)

--version for delaunayBase 
orderedInnerPerims' :: Maybe Perimeters ->  AdvancingCPoint ->  Either String (Maybe Perimeters)
orderedInnerPerims'     (Just (InnerPerimeters justifiedInnerPerims))  advancingCPoint  =
            orderInnerPerimsByDistanceFromHead (Just(InnerPerimeters justifiedInnerPerims)) advancingCPoint
orderedInnerPerims'     (Just (InnerPerimeter justifiedInnerPerims)) advancingCPoint  =
            orderInnerPerimsByDistanceFromHead (Just(InnerPerimeters [justifiedInnerPerims])) advancingCPoint
orderedInnerPerims' Nothing _ = Right Nothing
  --Left "Joiners.Delaunay.DelauanayB orderedInnerPerims' has empty perims passed in"
orderedInnerPerims' perim1  _ =
  Left $ "Joiners.Deluany.orderedInnerPerims has invalid Perimeter type for:" ++ (show perim1) 

{-
--version for delaunayBase 
orderedInnerPerims' :: Maybe Perimeters ->  Maybe Perimeters -> AdvancingCPoint ->  Either String (Maybe Perimeters)
orderedInnerPerims'     (Just (InnerPerimeters justifiedInnerPerims)) (Just (OuterPerimeter (o:outerPerims))) advancingCPoint  =
            orderByDistance (Just(InnerPerimeters justifiedInnerPerims)) advancingCPoint
orderedInnerPerims'     (Just (InnerPerimeter justifiedInnerPerims)) (Just (OuterPerimeter (o:outerPerims))) advancingCPoint  =
            orderByDistance (Just(InnerPerimeters [justifiedInnerPerims])) advancingCPoint
orderedInnerPerims' Nothing _ _ = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty outer perims passed in"
orderedInnerPerims' _ Nothing _ = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty inner perims passed in"
orderedInnerPerims' perim1 perim2 _ =
  Left $ "Joiners.Deluany.orderedInnerPerims has invalid Perimeter types for:" ++ (show perim1) ++ " and : " ++ (show perim2)
-}

        
--extractI [(x:xs):ys] = Right x
--extractI [(x:xs):[]] = Right x
data Perimeters =
  OuterPerimeter {_outerPerimeter :: [CornerPoints]}
  |
  InnerPerimeters {_innerPerimeters :: [[CornerPoints]]}
  |
  InnerPerimeter {_innerPerimeter :: [CornerPoints]}
  deriving (Show, Eq)

data AdvancingCPoint =
 AdvancingCPoint {_advancingCpoint :: CornerPoints}
  deriving (Show, Eq)

calculateJustDistance :: AdvancingCPoint -> Either String (Maybe Perimeters) -> Either String (Maybe DistanceA)
calculateJustDistance _ (Left e) = Left e
calculateJustDistance _ (Right Nothing) = Right Nothing
calculateJustDistance (AdvancingCPoint advancingCpoint) (Right (Just (InnerPerimeters ([])))) =
  Right Nothing
calculateJustDistance (AdvancingCPoint advancingCpoint) (Right (Just (InnerPerimeters ([]:innerPerimeters)))) =
  Right Nothing
  
calculateJustDistance (AdvancingCPoint advancingCpoint) (Right (Just (InnerPerimeters (innerPerim:innerPerims)))) =
  case calculateDistanceA advancingCpoint $ head innerPerim of
    Left e -> Left e
    Right distanceA -> Right $ Just distanceA
--calculateJustDistance advancingCpoint' (Right (Just (OuterPerimeter outerPerim))) = Right $ Just $  calculateDistanceA advancingCpoint' $ head outerPerim
calculateJustDistance (AdvancingCPoint advancingCpoint) (Right (Just (OuterPerimeter outerPerim))) =
  case calculateDistanceA advancingCpoint $ head outerPerim of
    Left e -> Left e
    Right distanceA -> Right $ Just distanceA

newAdvancingCpointE :: Maybe DistanceA -> --from center of advancing cpoint to head of innerPerim
                       Maybe DistanceA -> --from center of advancing cpoint to outerPerim
                       Maybe CornerPoints -> --The head $ outer perims
                       Maybe Perimeters -> --[[]] of all inner perimters
                       AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                       Either String AdvancingCPoint
                            --Right: the new advancing cpoint build from advancingCpoint and closes perim cpoint
                            --Left: the eror

newAdvancingCpointE Nothing Nothing Nothing Nothing advancingCPoint =
  Left $ "Joiners.Deluanay.newAdvancingCpointE had Nothing passed in for all parameters except: " ++ (show advancingCPoint)

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

newAdvancingCpointE (Just innerDistance) Nothing  _ (Just(InnerPerimeter innerPerimeter)) (AdvancingCPoint advancingCpoint) =
  case (head innerPerimeter) `raisedTo` advancingCpoint of
    Left e -> Left e
    Right cpoint -> Right $ AdvancingCPoint cpoint

newAdvancingCpointE  Nothing (Just outerDistance) (Just outerCpoint) _ (AdvancingCPoint advancingCpoint) =
  case outerCpoint `raisedTo` advancingCpoint of
    Left e -> Left e
    Right cpoint -> Right $ AdvancingCPoint cpoint


newAdvancingCpointE  innerDistance outerDistance o (Just(InnerPerimeters (innerPerimeter:innerPerimeters))) advancingCPoint =
  newAdvancingCpointE innerDistance outerDistance o (justifyPerimeters (InnerPerimeter innerPerimeter)) advancingCPoint

newAdvancingCpointE innerDistance outerDistance o innerPerimeters advancingCPoint =
  Left $ "Joiners.Delaunay.newAdvancingCpointE: had missing pattern match for: " ++
         "inner distance: " ++ (show innerDistance) ++
         " outer distance: " ++ (show outerDistance) ++
         " head of outer perimeters: " ++ (show o) ++
         " innerPerimeters: " ++ (show innerPerimeters) ++
         " advancingCPoint: " ++ (show advancingCPoint)

{-
cleaned up version of newAdvancingCpointE
todo: take on the applicative duties of delaunay<Base/Base'> except the advancing cpoint.
order the perims
calculate distances

-}
advancingCpointFromHeadOfInnerPerims ::
                       --Maybe DistanceA -> --from center of advancing cpoint to head of innerPerim
                       --Maybe DistanceA -> --from center of advancing cpoint to outerPerim
                       --Maybe CornerPoints -> --The head $ outer perims
                       --Maybe advancingCpointFromHeadOfPerims -> --[[]] of all inner perimters
                       Maybe Perimeters -> --the inner perimeters
                       AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                       Either String AdvancingCPoint
                            --Right: the new advancing cpoint build from advancingCpoint and closes perim cpoint
                            --Left: the eror

advancingCpointFromHeadOfInnerPerims (Just (InnerPerimeters innerPerimeters)) (AdvancingCPoint advancingCPoint) =
  let --order the perimeters by the by distance from head to advancing cpoint
      orderedPerimetersEM = orderInnerPerimsByDistanceFromHead (Just (InnerPerimeters innerPerimeters)) (AdvancingCPoint advancingCPoint) 
      safeHeadOfOrderedPerimetersEM =
        case orderedPerimetersEM of
          Left e -> Left e
          Right (Nothing) -> Left "Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: has no head $ headOfOrderedPerimetersEM"
          Right (Just (InnerPerimeters (innerPerimeter:innerPerimeters))) ->
            Right $ head innerPerimeter
  in
  case justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters of
    Nothing ->
      Left "Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: had empty inner perimeters passed in"
    (Just (InnerPerimeters innerPerimeters)) ->
      case extractE $ raisedTo <$> safeHeadOfOrderedPerimetersEM <*> Right advancingCPoint of
        Left e -> Left e
        Right cpoint -> Right $  AdvancingCPoint cpoint
    
  
advancingCpointFromHeadOfInnerPerims Nothing (AdvancingCPoint advancingCPoint) =
  Left $ "Joiners.Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: has Nothing passed in for inner perimeters"

advancingCpointFromHeadOfInnerPerims innerPerimeters advancingCpoint =
  Left $ "Joiners.Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: had unmatched pattern matching for innerPerimeters: " ++
         (show innerPerimeters) ++ " and advancingCpoint: " ++ (show advancingCpoint)

advancingCpointFromHeadOfOuterPerims :: Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint
advancingCpointFromHeadOfOuterPerims (Just (InnerPerimeters _)) _ =
  Left "Joiners.Delaunay.advancingCpointFromHeadOfOuterPerims: was passed an InnerPerimeters, but only OuterPerimeters are allowed."
advancingCpointFromHeadOfOuterPerims (Just (InnerPerimeter _)) _ =
  Left "Joiners.Delaunay.advancingCpointFromHeadOfOuterPerims: was passed an InnerPerimeter, but only OuterPerimeters are allowed."
advancingCpointFromHeadOfOuterPerims Nothing  _ =
  Left "Joiners.Delaunay.advancingCpointFromHeadOfOuterPerims: was passed Nothing, but only OuterPerimeters are allowed."
advancingCpointFromHeadOfOuterPerims (Just (OuterPerimeter outerPerimeter)) (AdvancingCPoint advancingCPoint) =
  case justifyPerimeters $ OuterPerimeter outerPerimeter of
    Nothing -> Left "Joiners.Delaunay.advancingCpointFromHeadOfOuterPerims: was passed [], but only valid OuterPerimeters are allowed."
    Just (OuterPerimeter (o:outerPerimeter)) ->
      case o `raisedTo` advancingCPoint of
        Left e -> Left $ "Joiners.Delaunay.advancingCpointFromHeadOfOuterPerims: had error when o `raisedTo` advancingCPoint because: " ++ e
        Right (cpoint) -> Right $ AdvancingCPoint cpoint


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

justifyPerimeters :: Perimeters -> Maybe Perimeters
justifyPerimeters (OuterPerimeter outerPerimeter) =
      case (length outerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ OuterPerimeter outerPerimeter
justifyPerimeters (InnerPerimeter innerPerimeter) =
      case (length innerPerimeter) == 0 of
        True -> Nothing
        False -> Just $ InnerPerimeter innerPerimeter
justifyPerimeters (InnerPerimeters innerPerimeters) =
      case (length innerPerimeters) == 0 of
        True -> Nothing
        False -> Just $ InnerPerimeters innerPerimeters



{-
removes AdvancingCPoint from head of outerPerimeters and or head $ head inner perimeters.
If nothing removed, return Left e, as it must be contained in one of them.
The new  version for delaunayBase.
refactor step 1========================
innerPerimeters needs to be ordered internally, 
-}
removeContainedCPointFromHeadOfPerims :: (Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))
removeContainedCPointFromHeadOfPerims innerPerimetersUnOrdered outerPerimeters (AdvancingCPoint advancingCpoint) =
  let
   processInnerPerimeters :: Maybe Perimeters -> Either String ((Maybe Perimeters),(Maybe Perimeters))
   processInnerPerimeters innerPerimeters = 
    case innerPerimeters of
     --inner nothing
     Nothing -> 
      case outerPerimeters of
        --inner nothing, outer nothing
        Nothing -> Left $ "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims InnerPerimeter = Nothing, OuterPerimter was Nothing: did not remove the advancingCpoint: " ++ (show advancingCpoint)
        (Just(OuterPerimeter outerPerimeter)) ->
          case justifyPerimeters (OuterPerimeter outerPerimeter) of
            --inner nothing, outer nothing
            Nothing -> Left $ "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims InnerPerimeter = Nothing, OuterPerimter was []: did not remove the advancingCpoint: " ++ (show advancingCpoint)
            --inner nothing, outer good
            (Just(OuterPerimeter (o:os))) -> 
              case advancingCpoint `contains` o of
               Left e -> Left $ "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims: InnerPerimter = Nothing, OuterPermeter = "
                             ++ (show o) ++ " :os did not remove advancingCpoint: " ++ (show advancingCpoint) ++ " because: " ++ e
               Right True -> Right (Nothing, justifyPerimeters $ OuterPerimeter os)
               Right False -> Left $ "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims: InnerPerimter = Nothing, OuterPermeter = "
                             ++ (show o) ++ " :os did not remove advancingCpoint: " ++ (show advancingCpoint) ++ " because it was not contained in o "
     --inner is a InnerPerimeter <no s>
     (Just (InnerPerimeter innerPerimeter)) -> 
       case justifyPerimeters (InnerPerimeter innerPerimeter) of
        --inner nothing
        Nothing ->  processInnerPerimeters Nothing
        --inner good
        Just (InnerPerimeter (innerPerimeter)) ->
          processInnerPerimeters $ Just $ InnerPerimeters [innerPerimeter]
          
     (Just (InnerPerimeters innerPerimeters )) ->
        case justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters of
          --inner nothing
          Nothing -> processInnerPerimeters Nothing
          --inner good
          (Just (InnerPerimeters (innerPerimeter:innerPerimeters))) ->
            case outerPerimeters of
              --inner good, outer nothing
              Nothing ->
                case advancingCpoint `contains` (head innerPerimeter) of
                  Left e -> Left $ "Joiners.Deluanay.removeContainedCPointFromHeadOfPerims did not contain the advancing cpoint because: " ++ e
                  Right True ->
                    Right $  (Just $ InnerPerimeters $ (tail innerPerimeter):innerPerimeters, Nothing)
                  Right False -> Left  "Joiners.Deluanay.removeContainedCPointFromHeadOfPerims did not contain the advancing cpoint in inner/outer perims"
              --inner good, possible good
              (Just (OuterPerimeter outerPerimeter)) ->
                case justifyPerimeters (OuterPerimeter outerPerimeter) of
                  --inner good, outer nothing
                  Nothing ->
                     case advancingCpoint `contains` (head innerPerimeter) of
                       Left e -> Left $ "Joiners.Deluanay.removeContainedCPointFromHeadOfPerims did not contain the advancing cpoint because: " ++ e
                       Right True ->
                         Right $  (Just $ InnerPerimeters $ (tail innerPerimeter):innerPerimeters, Nothing)
                       Right False -> Left  "Joiners.Deluanay.removeContainedCPointFromHeadOfPerims did not contain the advancing cpoint in inner/outer perims"
                  --inner good, outer good
                  Just (OuterPerimeter (o:outerPerimeter)) ->
                    case advancingCpoint `contains` (head innerPerimeter) of
                      Left e -> Left $ "fdkjkfds " ++ e
                      Right True ->
                        case advancingCpoint `contains` o of
                          Left e -> Left $ "ouiiopipo" ++ e
                          Right True -> Right  (Just $ InnerPerimeters $ (tail innerPerimeter):innerPerimeters, Just $ OuterPerimeter outerPerimeter)
                          Right False -> Right (Just $ InnerPerimeters $ (tail innerPerimeter):innerPerimeters, Just $ OuterPerimeter $ o:outerPerimeter)
                      Right False ->
                         case advancingCpoint `contains` o of
                          Left e -> Left $ "ouiiopipo" ++ e
                          Right True -> Right  (Just $ InnerPerimeters $ (innerPerimeter):innerPerimeters, Just $ OuterPerimeter outerPerimeter)
                          Right False -> Left "Joiners.Deluanay.removeContainedCPointFromHeadOfPerims did not contain advancingCpoint in inner/outer perimeters"
  in
     case
      extractE (processInnerPerimeters <$> (orderedInnerPerims'
                                           (innerPerimetersUnOrdered)
                                           (AdvancingCPoint advancingCpoint)
                                          )
       
              ) of
       Left e -> Left $ "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims had following erorr: " ++ e
       Right val -> Right val
{-
removes AdvancingCPoint from head of outerPerimeters and or head $ head inner perimeters.
If nothing removed, return Left e, as it must be contained in one of them.
The original version for delaunayB
-}
removeAdvCPointFromIOPerims :: (Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))
removeAdvCPointFromIOPerims innerPerimeters outerPerimeters (AdvancingCPoint advancingCpoint) =
  let fillerToCompile = Right $ (Nothing, Nothing)
  in
  case innerPerimeters of
    Nothing -> 
      case outerPerimeters of
        Nothing -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = Nothing, OuterPerimter was Nothing: did not remove the advancingCpoint: " ++ (show advancingCpoint)
        (Just(OuterPerimeter [])) -> 
          Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = Nothing, OuterPerimter was []: did not remove the advancingCpoint: " ++ (show advancingCpoint)
        (Just(OuterPerimeter (o:os))) -> 
          case advancingCpoint `contains` o of
            Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims: InnerPerimter = Nothing, OuterPermeter = "
                             ++ (show o) ++ " :os did not remove advancingCpoint: " ++ (show advancingCpoint) ++ " because: " ++ e
            Right True -> Right (Nothing, justifyPerimeters $ OuterPerimeter os)
            Right False -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims: InnerPerimter = Nothing, OuterPermeter = "
                             ++ (show o) ++ " :os did not remove advancingCpoint: " ++ (show advancingCpoint) ++ " because it was not contained in o "
    (Just (InnerPerimeter [])) -> 
      case outerPerimeters of
        Nothing -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = [], OuterPerimeter = Nothing: did not remove the advancingCpoint: " ++ (show advancingCpoint)
        (Just(OuterPerimeter [])) -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = [], OuterPerimter was []: did not remove the advancingCpoint: " ++ (show advancingCpoint)
        (Just(OuterPerimeter (o:os))) ->
          case advancingCpoint `contains` o of
            Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims: InnerPerimter = Nothing, OuterPermeter = "
                             ++ (show o) ++ " :os did not remove advancingCpoint: " ++ (show advancingCpoint) ++ " because: " ++ e
            Right True -> Right (Nothing, justifyPerimeters $ OuterPerimeter os)
            Right False -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = [], OuterPerimeter = "
                                  ++ (show o) ++ ": os did not remove the advancingCpoint: " ++ (show advancingCpoint)
    (Just (InnerPerimeter (i:is))) ->
      case outerPerimeters of
        Nothing -> 
          case advancingCpoint `contains` i of
            Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims: InnerPerimter = " ++ (show i) ++ "and outerPermeter = Nothing did not remove advancingCpoint because " ++ e
            Right True -> Right (justifyPerimeters $ InnerPerimeters [is], Nothing)
            Right False -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = " ++
                                  (show i) ++ ":is, OuterPerimeter = Nothing did not remove the advancingCpoint: " ++ (show advancingCpoint)
        (Just(OuterPerimeter [])) ->
          case advancingCpoint `contains` i of
            Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims: InnerPerimter = " ++ (show i) ++ "and outerPermeter = [] did not remove advancingCpoint because " ++ e
            Right True -> Right (justifyPerimeters $ InnerPerimeters [is], Nothing)
            Right False -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = " ++
                                  (show i) ++ ":is, OuterPerimeter = [] did not remove the advancingCpoint: " ++ (show advancingCpoint)
        (Just(OuterPerimeter (o:os))) ->
          case advancingCpoint `contains` i of
            Right True ->
              case advancingCpoint `contains` o of
                Right True -> Right (justifyPerimeters $ InnerPerimeters [is], justifyPerimeters $ OuterPerimeter os)
                Right False -> Right (justifyPerimeters $ InnerPerimeters [is], justifyPerimeters $ OuterPerimeter (o:os))
                Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = " ++
                                  (show i) ++ ":is, OuterPerimeter = " ++ (show o) ++ ":os did not remove the advancingCpoint: " ++ (show advancingCpoint) ++
                                  "because advancingCpoint `contains` o threw error: " ++ e
            Right False ->
              case advancingCpoint `contains` o of
                Right True -> Right (justifyPerimeters $ InnerPerimeters [i:is], justifyPerimeters $ OuterPerimeter os)
                Right False -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = " ++
                                  (show i) ++ ":is, OuterPerimeter = " ++ (show o) ++ ":os did not remove the advancingCpoint: " ++ (show advancingCpoint) ++
                                  "because advancingCpoint because it was not contained in either one."
                Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = " ++
                                  (show i) ++ ":is, OuterPerimeter = " ++ (show o) ++ ":os did not remove the advancingCpoint: " ++ (show advancingCpoint) ++
                                  "because advancingCpoint `contains` o threw error: " ++ e
            Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = " ++
                                  (show i) ++ ":is, OuterPerimeter = " ++ (show o) ++ ":os did not remove the advancingCpoint: " ++ (show advancingCpoint) ++
                                  "because advancingCpoint `contains` i threw error: " ++ e
    (Just (InnerPerimeters [])) ->
      removeAdvCPointFromIOPerims Nothing outerPerimeters (AdvancingCPoint advancingCpoint)
    (Just (InnerPerimeters (i:[]))) ->
      removeAdvCPointFromIOPerims (Just $ InnerPerimeter i) outerPerimeters (AdvancingCPoint advancingCpoint)
    (Just (InnerPerimeters (i:is))) ->
      case removeAdvCPointFromIOPerims (Just $ InnerPerimeter i) outerPerimeters (AdvancingCPoint advancingCpoint) of
        Left e -> Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims head $ InnerPerimeters = " ++
                                  (show i) ++ ":is " ++ "did not remove the advancingCpoint because : " ++ e
        Right (Nothing, outerPerimeter) ->
          Right (justifyPerimeters $ InnerPerimeters is, outerPerimeter)
        Right (Just (InnerPerimeters (innerPerimeter:innerPerimeters)), outerPerimeter) ->
          Right (justifyPerimeters $ InnerPerimeters (innerPerimeter:is), outerPerimeter)
    (Just innerPerimeters) ->
      Left $ "Joiners.Delaunay.removeAdvCPointFromIOPerims has missing pattern match for innerPerimeters: " ++ (show innerPerimeters)


  
-- | Order the innerPermiters by distance form head to the advancing Cpoint
      
orderInnerPerimsByDistanceFromHead ::  Maybe Perimeters ->
                    AdvancingCPoint ->    -- The outer perim cpoint
                    Either String (Maybe Perimeters)    -- [[good inner perim distance CornerPoints]] ordered by distance to advancing line
orderInnerPerimsByDistanceFromHead  (Just(InnerPerimeter _)) _ = Left "Deluanay.orderInnerPerimsByDistanceFromHead only works on InnerPerimeters but had an InnerPerimeter passed in "

orderInnerPerimsByDistanceFromHead  (Just(OuterPerimeter _)) _ = Left "Deluanay.orderInnerPerimsByDistanceFromHead only works on InnerPerimeters but had an OuterPerimeter passed in "

orderInnerPerimsByDistanceFromHead (Just(InnerPerimeters innerPerimeters)) (AdvancingCPoint distanceFromThisCpoint) =
  case justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters of
    Nothing -> Right Nothing
    (Just (InnerPerimeters (innerPerimeter:[]))) ->
      extractE $ orderInnerPerimsByDistanceFromHeadRecur (justifyPerimeters $ InnerPerimeters []) (AdvancingCPoint distanceFromThisCpoint)  (InnerPerimeters [innerPerimeter]) <$>
                 (calculateDistanceA (head innerPerimeter) distanceFromThisCpoint)
    (Just (InnerPerimeters (innerPerimeter:innerPerimeters))) ->
      extractE $ orderInnerPerimsByDistanceFromHeadRecur (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters) (AdvancingCPoint distanceFromThisCpoint)  (InnerPerimeters [innerPerimeter]) <$>
                 (calculateDistanceA (head innerPerimeter) distanceFromThisCpoint)

orderInnerPerimsByDistanceFromHead Nothing _ = Right Nothing

orderInnerPerimsByDistanceFromHeadRecur :: Maybe Perimeters ->
                        AdvancingCPoint ->
                        Perimeters -> --the ordered Perimeters
                        DistanceA -> --current distance val to compare to
                        Either String (Maybe Perimeters)

orderInnerPerimsByDistanceFromHeadRecur Nothing _ sortedInnerPerimeters _ = Right $ Just sortedInnerPerimeters

orderInnerPerimsByDistanceFromHeadRecur (Just(InnerPerimeters (innerPerimeter:innerPerimeters))) (AdvancingCPoint distanceFromThisCpoint) (InnerPerimeters (sortedInnerPerimeter:sortedInnerPerimeters)) (previousDistance) =
  let distanceToCurrInnerPerim =
        calculateDistanceA (head innerPerimeter) (distanceFromThisCpoint) 
      
  in
    case ((previousDistance < ) <$> distanceToCurrInnerPerim )  of
     Left e -> Left e
     Right (True) ->
       orderInnerPerimsByDistanceFromHeadRecur (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters) (AdvancingCPoint distanceFromThisCpoint)
                                  (InnerPerimeters $ sortedInnerPerimeter:innerPerimeter:sortedInnerPerimeters) previousDistance
     Right (False) ->
       extractE $ (orderInnerPerimsByDistanceFromHeadRecur (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters) (AdvancingCPoint distanceFromThisCpoint) 
                                       (InnerPerimeters $ innerPerimeter:sortedInnerPerimeter:sortedInnerPerimeters)) <$> distanceToCurrInnerPerim
