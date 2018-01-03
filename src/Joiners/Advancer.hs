{- |
This is where deluanayBase gets functions curried in to build advancing joiners.
As the functions passed in to delaunayBase need to match up to work properly, it is best to have it done here,
instead of the user trying to mix and match. To be successful at that, you need to know how the functions work together.
Then again, mixing and matching randomly may be a good way to get new results as required.
-}
module Joiners.Advancer(advanceFromHeadUsingDistanceToCornerPoint) where

import Joiners.AdvanceBase(delaunayBase, delaunayBase', DelaunayBase'Signature)
import Joiners.AdvanceSupport(Perimeters(..), AdvancingCPoint(..), justifyPerimeters, appendAdvancingCpointToJoinedCpointsE)
import Joiners.AdvanceToHeadOfPerimeters(orderInnerPerimsByDistanceFromHead, orderedInnerPerims, removeContainedCPointFromHeadOfPerims, advancingCpointFromHeadOfInnerPerims,
                                         advancingCpointFromHeadOfOuterPerims)

import CornerPoints.CornerPoints(CornerPoints(..))

import  Geometry.Intercept(legalIntersection)

import Math.Distance(DistanceA(..), calculateDistanceA)

import Helpers.Applicative(extractE)

-- | All advancing is done from the head of perimeters.
-- The tail of perimeters are never checked for Distance, to see if one of them
-- would be a valid point to advance to.
advanceFromHeadUsingDistanceToCornerPoint
  = delaunayBase
      (removeContainedCPointFromHeadOfPerims)
      (advancingCpointFromHeadOfInnerPerims)
      advancingCpointFromHeadOfOuterPerims
      advancingCpointFromDoublePerimsUsingDistanceToCpoints

{-
This seems like it should be in Joiners.AdvanceToHeadOfPerimeters module, but it makes a call to delaunayBase', which would make it circular.
Could make pass in delaunayB', but the complexity of passing in functions is getting too much. Maybe there should be a separate module for
functions such as this. If more come up, will make one.
-}
advancingCpointFromDoublePerimsUsingDistanceToCpoints :: 
  
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

advancingCpointFromDoublePerimsUsingDistanceToCpoints
  removeAdvancingCPointFromPerimeters
  createAdvancingCpointFromInnerPerimeters
  advancingCpointFromOuterPerims
  innerPerimeters'
  outerPerimeter'
  advancingCpointIn
  joinedCpoints =
     let delaunayBase'' = delaunayBase' removeAdvancingCPointFromPerimeters createAdvancingCpointFromInnerPerimeters
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
              delaunayBase'' <$> 
                             (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                             (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                             advancingOuterCpointE <*>                             --The advancing Cpoint just created.
                             (appendAdvancingCpointToJoinedCpointsE <$> advancingOuterCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
                        --outer distance >=
        Right False -> --so inner perim is closer, but is it legal. If not, go with the outer perimeter
          let perimsWithAdvancingCpointBldrRemoved =
                extractE
                  (removeAdvancingCPointFromPerimeters 
                   innerPerimeters'
                   outerPerimeter' <$>
                     advancingInnerCpointE
                  )
          in
            let
              -- =====================================================================================================================================================================
              --check the new advancing cpont and cx for legal
              
              --make this wrapper because legalIntersection takes a Cpoint instead of an AdvancingCPoint
              legalIntersection' :: AdvancingCPoint -> 
                                            (CornerPoints -> --a perimeter cpoint
                                             Either String Bool)
              legalIntersection' (AdvancingCPoint advancingCPoint) =
                legalIntersection advancingCPoint

              -- ============================================================================================================================================================
              --nfg: need to cx all original innerperimeters, before extraction, for legality, instead of (head $ getInnerPerimeterHead innerPerimeters').
              --Will need to make a datatype, perhaps can contain the running advancing cpoints, and the [[inner perim]]
              -- ==============================================================================================================================================================
              isNewAdvancingCpointLegal  = extractE $ legalIntersection' <$>  advancingInnerCpointE <*> Right (head $ getInnerPerimeterHead innerPerimeters')

              

              makeFromInner =
                extractE $
                  delaunayBase'' <$> 
                      (fst <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                      (snd <$> perimsWithAdvancingCpointBldrRemoved ) <*>
                      advancingInnerCpointE <*>                             --The advancing Cpoint just created.
                      (appendAdvancingCpointToJoinedCpointsE <$> advancingInnerCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.
            in
              makeFromInner
              {-get rid of legal for now till i clean up my mess.
              case isNewAdvancingCpointLegal of
               Right True -> makeFromInner
               Right False -> --use the code to build from the outerPerimeter. Should be a fuction to keep dry as this was copied from above.
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
               Left e -> Left $ "opewrilsdfklsljf" ++ e
-}
          {-
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
                  (appendAdvancingCpointToJoinedCpointsE <$> advancingInnerCpointE <*> Right joinedCpoints)  --joined cpoints with the advancing cpoint added to it.-}

