{- |
----------------------------------Terms:------------------------------------
advancing cpoint(CornerPoints):
Like a delaunay mesh, it advances from existing CornerPoints.
As it is joining [[CornerPoints]] of inner perimeters, to a single outer perimeter of
[CornerPoints], the is only a single advancing cpoint at any time.

Perimeter:
These are [CornerPoints] which define a shape which is part of the joining process.

The OuterPerimeter is a single [CornerPoints] which contains all other shapes.
At this time it does not handle shapes which are not fully contained insid of it.
These will be Front cornerpoints such as Front<Face/LeftLine/RightLine/TopLine...> or F<1,2,3,4>

The InnerPerimeters can be made of 1 or more [CornerPoints] shapes, each of which
gets removed from the interior of the OuterPerimeter.
These will be Back cornerpoints such as Back<Face/LeftLine/RightLine/TopLine...> or B<1,2,3,4>

-}
module Joiners.AdvanceBase(delaunayBase, delaunayBase', delaunayBaseNM, delaunayBaseNM') where

import Joiners.AdvanceSupport(Perimeters(..), AdvancingCPoint(..), ExtraLists(..),
                              justifyPerimeters, appendAdvancingCpointToJoinedCpointsE)
import Joiners.AdvanceToHeadOfPerimeters(orderInnerPerimsByDistanceFromHead, orderedInnerPerims, removeContainedCPointFromHeadOfPerims,
                                         advancingCpointFromHeadOfInnerPerims, advancingCpointFromHeadOfOuterPerims)
                                         

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.FaceConversions(raisedTo)

import Math.Distance(DistanceA(..), calculateDistanceA)

import Helpers.List(removeEmpty)
import Helpers.Applicative(extractE)

import Data.List(filter, sortOn)


delaunayBase ::  --removeAdvancingCPointFromPerimeters
                 ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->

                 --removeAdvancingCPointFromPerimeters
                 (Maybe Perimeters -> --inner perimters
                  AdvancingCPoint -> --the current advancingCpoint
                  Either String AdvancingCPoint) ->

                 --build advancingCpoint from perimeters.
                 --Uses the next fx, when > 1 perimeter is available
                 (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                   
                 --build advancingCpoint from perimeters, when > 1 perimeter is available
                   --has the same signature as delaunayBase, except it does not need this fx again.
                 (((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                  (
                   Maybe Perimeters -> 
                   AdvancingCPoint -> 
                   Either String AdvancingCPoint) ->
                   (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                   Maybe Perimeters -> 
                   Maybe Perimeters -> 
                   AdvancingCPoint -> 
                   [CornerPoints] -> 
                   Either String [CornerPoints] 
                 ) ->
                 --end of curried functions.
                 --start of actual data being passed.
                 [[CornerPoints]] -> --inner perim cpoints
                 [CornerPoints] -> --outer perim cpoints
                 [CornerPoints]
delaunayBase   removeAdvancingCPointFromPerimeters
               createAdvancingCpointFromInnerPerimeters
               advancingCpointFromOuterPerims
               doublePerimDecision
               innerPerimeters outerPerimeters  =
  
  let --As this is the very first advancingCpoint, build it from head outerPerimeters, and innerPerimeters
      advancingCpoint :: Either String AdvancingCPoint
      advancingCpoint = createAdvancingCpointFromInnerPerimeters (Just $ InnerPerimeters innerPerimeters) (AdvancingCPoint $ head outerPerimeters)

      --remove whichever Cpoints were used, from the inner/outer Perimeters  
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


------------------------------------------------------------------ delaunayBase'----------------------------------------------------------------------
------------------------------------------------------------------ delaunayBase'----------------------------------------------------------------------
------------------------------------------------------------------ delaunayBase'----------------------------------------------------------------------
------------------------------------------------------------------ delaunayBase'----------------------------------------------------------------------
------------------------------------------------------------------ delaunayBase'----------------------------------------------------------------------
--was not able to reuse anywhere else so far.
type DelaunayBase'Signature =
               --start of curried functions. Same as delaunayBase so refer to it for parameter info.
               ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
               (
                Maybe Perimeters -> 
                AdvancingCPoint -> 
                Either String AdvancingCPoint) ->
               (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
               (
                 ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))) ->
                       (
                        Maybe Perimeters -> 
                        AdvancingCPoint -> 
                        Either String AdvancingCPoint) ->
                        (Maybe Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                        Maybe Perimeters -> 
                        Maybe Perimeters -> 
                        AdvancingCPoint -> 
                        [CornerPoints] ->  
                        Either String [CornerPoints] 
               ) ->
               --start of data being passed
               Maybe Perimeters -> --inner perimeters
               Maybe Perimeters -> --outer perimeters
               AdvancingCPoint -> -- The current advancing cpoint
               [CornerPoints] ->   --joinedCpoints. The built up [CornerPoints] which gets joined radially with +++> to form the final shape.
                                   --Each recursive call of delaunayBase' adds onto this list. 
               Either String [CornerPoints] --The built up [CornerPoints] which gets joined radially with +++> to form the final shape.
                                            --delaunayBase will convert from this Either to a [CornerPoints] which shows errors via CornerPointsError
                                            --as that is how Builder.Monad handles it.
                                            --That is bad design decision that needs to be changed. Get rid of CornerPointsError. Will be a big job. :( 


delaunayBase' :: DelaunayBase'Signature
delaunayBase' removeAdvancingCPointFromPerimeters
              createAdvancingCpointFromInnerPerimeters
              advancingCpointFromOuterPerims
              doublePerimDecision
              Nothing Nothing _ joinedCpoints = Right $ reverse joinedCpoints
--the following 2 should have been taken car of innerOuterRemoved. Need to remove and test.
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


--Everyting is good with perimeters, so build advancing cpoints, and adjust perimeters.
delaunayBase' removeAdvancingCPointFromPerimeters
              createAdvancingCpointFromInnerPerimeters
              advancingCpointFromOuterPerims
              doublePerimDecision
              innerPerimeters outerPerimeters  advancingCpointIn joinedCpoints =

    let
      delaunayBase'' = delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision
    in
    --Cx all possible combinations of Perimeters. Also check for Perimeters containing [].
    --ToDo: See if there is a cleaner way to do this with something like K-Arrows, monads, applicative.
    case innerPerimeters of
      Nothing ->
        --call again with InnerPerimeters [] to keep it DRY. The next section "Just (InnerPerimeter innerPerimeter')" will use justifyPerimeters to see that is is really Nothing.
        --delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision (Just $ InnerPerimeters []) outerPerimeters  advancingCpointIn joinedCpoints
        delaunayBase'' (Just $ InnerPerimeters []) outerPerimeters  advancingCpointIn joinedCpoints
      --re-call with InnerPerimeters
      Just (InnerPerimeter innerPerimeter') ->
        delaunayBase'' (Just $ InnerPerimeters [innerPerimeter']) outerPerimeters  advancingCpointIn joinedCpoints
        {-delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims doublePerimDecision (Just $ InnerPerimeters [innerPerimeter']) outerPerimeters  advancingCpointIn joinedCpoints-}
      
      --has InnerPerimeters
      Just (InnerPerimeters innerPerimeters') ->
        case justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters' of
          --Inner: empty
          Nothing ->
            case (outerPerimeters) of
              --inner empty, outer empty
              Nothing ->
                delaunayBase'' Nothing Nothing  advancingCpointIn joinedCpoints
              --inner empty, outer good but not justified
              (Just (OuterPerimeter outerPerimeter')) ->
                case justifyPerimeters (OuterPerimeter outerPerimeter') of
                  --inner: empty, outer: empty
                  Nothing -> delaunayBase'' Nothing Nothing  advancingCpointIn joinedCpoints
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
                         delaunayBase'' <$> 
                           (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                           (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                           advancingCpointNewE <*>                             --The advancing Cpoint just created.
                           (appendAdvancingCpointToJoinedCpointsE <$> advancingCpointNewE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
                      
          --Inner: good    
          Just (InnerPerimeters innerPerimeters') ->
            case (outerPerimeters) of
              --inner empty, outer empty
              Nothing -> delaunayBase'' Nothing Nothing  advancingCpointIn joinedCpoints
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
                         delaunayBase'' <$> 
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
This seems like it should be in Joiners.AdvanceToHeadOfPerimeters module, but it makes a call to delaunayBase', which would make it circular.
Maybe it needs to take delaunayBase' as a param, then curry it in here.

There is a new NM version in Joiners.AdvanceToHeadOfPerimeters
-This 1 can be deleted when non-NM versions are removed
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
     let
         delaunayBase'' = delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters
                                        advancingCpointFromOuterPerims advancingCpointFromDoublePerimsUsingDistanceToCpoints
                          
         getAdvancingCpoint (AdvancingCPoint advancingCpoint') = advancingCpoint'
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
            
              delaunayBase''  <$> 
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
              delaunayBase'' <$> 
                  (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                  (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                  advancingInnerCpointE <*>                             --The advancing Cpoint just created.
                  (appendAdvancingCpointToJoinedCpointsE <$> advancingInnerCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.



-- ======================================================================= NM versions ===============================================================================
type DelaunayBaseNM'Signature =
               --start of curried functions. Same as delaunayBase so refer to it for parameter info.
  
               (Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters)) ->
                 --removeAdvancingCPointFromPerimeters
                 --this has to remain Mabye perims going in, as there may only be 1 perimeter left to go into the removing fx 

               (Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                  --createAdvancingCpointFromInnerPerimeters

               (Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                 --advancingCpointFromOuterPerims

               ((Perimeters) -> (Perimeters) -> [[CornerPoints]] -> AdvancingCPoint -> Either String AdvancingCPoint ) -> 
                 --doublePerimDecision, both perims have cpoints in them, so decide which to make advancingCpoint from.

               --need the original raw inner perimeters as [[CornerPoints]] so can cx for legalIntersection.
               --should make a newType, to try them out. May be a better choice for other perimeters as well.
                 --could make a class of Intercept, so that [[]] [] and the newtype of raw perimeters would all use it.
                --There really is no reason have fx's that process both inner/outer perimeters.
                --Is there any need for the InnerPerimeter.
                --for now, use [[CornerPoints]] as that is what Intercept module already uses
               [[CornerPoints]] ->

               --start of data being passed
               Maybe Perimeters -> --inner perimeters
               Maybe Perimeters -> --outer perimeters

               

               AdvancingCPoint -> -- The current advancing cpoint
               [CornerPoints] ->   --joinedCpoints. The built up [CornerPoints] which gets joined radially with +++> to form the final shape.
                                   --Each recursive call of delaunayBase' adds onto this list. 
               Either String [CornerPoints] --The built up [CornerPoints] which gets joined radially with +++> to form the final shape.
                                            --delaunayBase will convert from this Either to a [CornerPoints] which shows errors via CornerPointsError
                                            --as that is how Builder.Monad handles it.
                                            --That is bad design decision that needs to be changed. Get rid of CornerPointsError. Will be a big job. :( 


--Everyting is good with perimeters, so build advancing cpoints, and adjust perimeters.
delaunayBaseNM' removeAdvancingCPointFromPerimeters
                createAdvancingCpointFromInnerPerimeters
                advancingCpointFromOuterPerims
                doublePerimDecision
                rawInnerPerimeters
                innerPerimeters outerPerimeter
                advancingCpointIn joinedCpoints = do

    let
      delaunayBaseNM'' = delaunayBaseNM' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters
                                         advancingCpointFromOuterPerims doublePerimDecision rawInnerPerimeters
    --in
    --Cx all possible combinations of Perimeters. Also check for Perimeters containing [].
    --ToDo: See if there is a cleaner way to do this with something like K-Arrows, monads, applicative.
    justifiedInnerPerimeters <-
      case innerPerimeters of
       (Just(OuterPerimeter _)) ->
         Left "delaunayNM' had OuterPerimeter passed in for innerPerimeters"
       (Just(InnerPerimeter _)) ->
         Left "delaunayNM' had InnerPerimeter passed in for innerPerimeters"
       Nothing -> Right Nothing 
       (Just(InnerPerimeters (i:innerPerimeters))) ->
         case (length i) == 0 of
           True ->
             case (length $ removeEmpty innerPerimeters) == 0 of
               True ->
                 Right Nothing
               False -> Right $ Just $ InnerPerimeters innerPerimeters
           False ->
             Right $ Just $ InnerPerimeters (i:(removeEmpty innerPerimeters))
           
    justifiedOuterPerimeter <-
      case outerPerimeter of
        (Just(InnerPerimeter _)) -> Left "delaunayNM has InnerPerimeter passed in as OuterPerimeter"
        (Just(InnerPerimeters _)) -> Left "delaunayNM has InnerPerimeters passed in as OuterPerimeter"
        Nothing ->
          Right Nothing
        (Just(OuterPerimeter outerPerimeter)) ->
          case (length outerPerimeter) == 0 of
            True -> Right Nothing
            False -> Right $ Just $ OuterPerimeter outerPerimeter


    returnVal <-
      case (justifiedInnerPerimeters, justifiedOuterPerimeter) of
        (Nothing,Nothing) -> Right $ reverse joinedCpoints
        (Nothing,(Just (OuterPerimeter(outerPerimeter)))) -> do
          advancingCpointNew <- advancingCpointFromOuterPerims (OuterPerimeter outerPerimeter) (advancingCpointIn)
            
          perimsWithAdvancingCpointBldrRemoved <-
                      (removeAdvancingCPointFromPerimeters 
                       Nothing 
                        (Just $ OuterPerimeter outerPerimeter) advancingCpointNew
                   )
          returnVal <-
            delaunayBaseNM'' 
                           (fst perimsWithAdvancingCpointBldrRemoved )
                           (snd perimsWithAdvancingCpointBldrRemoved ) 
                           advancingCpointNew                              --The advancing Cpoint just created.
                           (appendAdvancingCpointToJoinedCpointsE advancingCpointNew joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
          return returnVal
        ((Just (InnerPerimeters innerPerimeters)),Nothing) -> do
          advancingCpointNew <- createAdvancingCpointFromInnerPerimeters (InnerPerimeters innerPerimeters) (advancingCpointIn)
            
          perimsWithAdvancingCpointBldrRemoved <-
                      (removeAdvancingCPointFromPerimeters 
                        (Just $ InnerPerimeters innerPerimeters) Nothing advancingCpointNew
                         
                   )
          returnVal <-
            delaunayBaseNM'' 
                           (fst perimsWithAdvancingCpointBldrRemoved )
                           (snd perimsWithAdvancingCpointBldrRemoved ) 
                           advancingCpointNew                              --The advancing Cpoint just created.
                           (appendAdvancingCpointToJoinedCpointsE advancingCpointNew joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
          return returnVal
        ((Just (InnerPerimeters innerPerimeters)),(Just (OuterPerimeter(outerPerimeter)))) -> do
          advancingCpointNew <- doublePerimDecision (InnerPerimeters innerPerimeters) (OuterPerimeter outerPerimeter) rawInnerPerimeters (advancingCpointIn)
            
          perimsWithAdvancingCpointBldrRemoved <-
                      (removeAdvancingCPointFromPerimeters 
                        (Just $ InnerPerimeters innerPerimeters) (Just $ OuterPerimeter outerPerimeter) advancingCpointNew
                         
                   )
          returnVal <-
            delaunayBaseNM'' 
                           (fst perimsWithAdvancingCpointBldrRemoved )
                           (snd perimsWithAdvancingCpointBldrRemoved ) 
                           advancingCpointNew                              --The advancing Cpoint just created.
                           (appendAdvancingCpointToJoinedCpointsE advancingCpointNew joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
          return returnVal
          
    return returnVal       
    

-- ===================================================================================================================================================
delaunayBaseNM ::  
                 ((Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String (Maybe Perimeters, Maybe Perimeters)) ->
                 --removeAdvancingCPointFromPerimeters

                 (Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                 --createAdvancingCpointFromInnerPerimeters

                 (Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint) ->
                 --advancingCpointFromOuterPerims
                   
                 ((Perimeters) -> (Perimeters) -> [[CornerPoints]] -> AdvancingCPoint -> Either String AdvancingCPoint ) -> 
                 --doublePerimDecision, both perims have cpoints in them, so decide which to make advancingCpoint from.

                 [[CornerPoints]] -> --raw inner perims
                 
                 --end of curried functions.
                 --start of actual data being passed.
                 [[CornerPoints]] -> --inner perim cpoints
                 [CornerPoints] -> --outer perim cpoints
                 [CornerPoints]
delaunayBaseNM removeAdvancingCPointFromPerimeters
               createAdvancingCpointFromInnerPerimeters
               advancingCpointFromOuterPerims
               doublePerimDecision
               rawInnerPerimeters
               innerPerimeters outerPerimeter  =
  
  let
    delaunayBaseNM'' = delaunayBaseNM'
                         removeAdvancingCPointFromPerimeters
                         createAdvancingCpointFromInnerPerimeters advancingCpointFromOuterPerims  doublePerimDecision rawInnerPerimeters 
    process :: Either String [CornerPoints]
    process = do
      --As this is the very first advancingCpoint, build it from head outerPerimeters, and innerPerimeters
      (OuterPerimeter (o: justifiedOuterPerimeter)) <-
        case (length  outerPerimeter) == 0 of
          True -> Left "empty outerPerimeters passed into delaunayBaseNM"
          False -> Right $ OuterPerimeter outerPerimeter

      (InnerPerimeters (i:innerPerimeters)) <-
        case (length $ removeEmpty innerPerimeters) == 0 of
          True -> Left "deluanaBaseNM has empty InnerPerimeters"
          False -> Right $ InnerPerimeters innerPerimeters

      advancingCpoint <- createAdvancingCpointFromInnerPerimeters (InnerPerimeters (i:innerPerimeters)) (AdvancingCPoint o)

      perimsWithAdvancingCpointBldrRemoved <-
        (removeAdvancingCPointFromPerimeters 
            (Just $ (InnerPerimeters (i:innerPerimeters)))  (Just $ OuterPerimeter outerPerimeter) advancingCpoint)

      appended <- Right $ appendAdvancingCpointToJoinedCpointsE advancingCpoint  []

      delaunay <-
        delaunayBaseNM'' 
        (fst perimsWithAdvancingCpointBldrRemoved)
        (snd perimsWithAdvancingCpointBldrRemoved)
        advancingCpoint 
        appended
      
      return delaunay
      
  in
  case process  of
    Left e -> [CornerPointsError $ "Joiners.DeluanayB(Left e): " ++  e]
    Right val -> val









