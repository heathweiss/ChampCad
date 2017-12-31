module Joiners.AdvanceToHeadOfPerimeters(orderInnerPerimsByDistanceFromHead, orderedInnerPerims, removeContainedCPointFromHeadOfPerims,
                                        advancingCpointFromHeadOfInnerPerims, advancingCpointFromHeadOfOuterPerims) where

import Joiners.AdvanceSupport(Perimeters(..), AdvancingCPoint(..), justifyPerimeters, appendAdvancingCpointToJoinedCpointsE)

import Helpers.List(removeEmpty)
import Helpers.Applicative(extractE)

import Math.Distance(DistanceA(..), calculateDistanceA)

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.FaceExtraction(contains)
import CornerPoints.FaceConversions(raisedTo)


orderedInnerPerims :: Maybe Perimeters ->  AdvancingCPoint ->  Either String (Maybe Perimeters)
orderedInnerPerims     (Just (InnerPerimeters justifiedInnerPerims))  advancingCPoint  =
            orderInnerPerimsByDistanceFromHead (Just(InnerPerimeters justifiedInnerPerims)) advancingCPoint
orderedInnerPerims     (Just (InnerPerimeter justifiedInnerPerims)) advancingCPoint  =
            orderInnerPerimsByDistanceFromHead (Just(InnerPerimeters [justifiedInnerPerims])) advancingCPoint
orderedInnerPerims Nothing _ = Right Nothing
  --Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty perims passed in"
orderedInnerPerims perim1  _ =
  Left $ "Joiners.Deluany.orderedInnerPerims has invalid Perimeter type for:" ++ (show perim1) 



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
      extractE (processInnerPerimeters <$> (orderedInnerPerims
                                           (innerPerimetersUnOrdered)
                                           (AdvancingCPoint advancingCpoint)
                                          )
       
              ) of
       Left e -> Left $ "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims had following erorr: " ++ e
       Right val -> Right val

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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



---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
This seems like it should be in Joiners.AdvanceToHeadOfPerimeters module, but it makes a call to delaunayBase', which would make it circular.
Maybe it needs to take delaunayBase' as a param, then curry it in here.


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

-}


