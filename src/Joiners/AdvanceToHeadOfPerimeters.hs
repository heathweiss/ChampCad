module Joiners.AdvanceToHeadOfPerimeters(orderInnerPerimsByDistanceFromHead, orderedInnerPerims,
                                         removeContainedCPointFromHeadOfPerims, removeContainedCPointFromHeadOfPerimsNM,
                                         advancingCpointFromHeadOfInnerPerims, advancingCpointFromHeadOfInnerPerimsNM,
                                         advancingCpointFromHeadOfOuterPerims, advancingCpointFromHeadOfOuterPerimsNM,
                                         advancingCpointFromDoublePerimsUsingDistanceToHeadOfPerimsCpointsNM) where

import Joiners.AdvanceSupport(Perimeters(..), AdvancingCPoint(..), justifyPerimeters, appendAdvancingCpointToJoinedCpointsE,
                              )
import Helpers.List(removeEmpty)
import Helpers.Applicative(extractE)

import Math.Distance(DistanceA(..), calculateDistanceA)

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.FaceExtraction(contains)
import CornerPoints.FaceConversions(raisedTo)

import Geometry.Intercept(legalIntersection, perimetersContainIllegalIntersection, perimetersContainLegalIntersections)


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

-- ====================================================== version of orderInnerPerimsByDistanceFromHeadRecur w/o Maybe=========================
orderInnerPerimsByDistanceFromHeadNM ::  Perimeters ->
                    AdvancingCPoint ->    -- The outer perim cpoint
                    Either String ({-Maybe-} Perimeters)    -- [[good inner perim distance CornerPoints]] ordered by distance to advancing line
orderInnerPerimsByDistanceFromHeadNM  ((InnerPerimeter _)) _ = Left "Deluanay.orderInnerPerimsByDistanceFromHead only works on InnerPerimeters but had an InnerPerimeter passed in "

orderInnerPerimsByDistanceFromHeadNM  ((OuterPerimeter _)) _ = Left "Deluanay.orderInnerPerimsByDistanceFromHead only works on InnerPerimeters but had an OuterPerimeter passed in "

orderInnerPerimsByDistanceFromHeadNM ((InnerPerimeters innerPerimeters)) (AdvancingCPoint distanceFromThisCpoint) =
  {-
  case justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters of
    Nothing -> Right Nothing
    (Just (InnerPerimeters (innerPerimeter:[]))) ->
      extractE $ orderInnerPerimsByDistanceFromHeadRecurNM (justifyPerimeters $ InnerPerimeters []) (AdvancingCPoint distanceFromThisCpoint)  (InnerPerimeters [innerPerimeter]) <$>
                 (calculateDistanceA (head innerPerimeter) distanceFromThisCpoint)
    (Just (InnerPerimeters (innerPerimeter:innerPerimeters))) -> -}
      extractE $ orderInnerPerimsByDistanceFromHeadRecurNM (InnerPerimeters innerPerimeters) (AdvancingCPoint distanceFromThisCpoint)  (InnerPerimeters [head innerPerimeters]) <$>
                 (calculateDistanceA (head $ head innerPerimeters) distanceFromThisCpoint)

--orderInnerPerimsByDistanceFromHead Nothing _ = Right Nothing

orderInnerPerimsByDistanceFromHeadRecurNM ::
                        Perimeters ->
                        AdvancingCPoint ->
                        Perimeters -> --the ordered Perimeters
                        DistanceA -> --current distance val to compare to
                        Either String ({-Maybe-} Perimeters)


orderInnerPerimsByDistanceFromHeadRecurNM
  ((InnerPerimeters (innerPerimeter:innerPerimeters)))
  (AdvancingCPoint distanceFromThisCpoint)
  (InnerPerimeters (sortedInnerPerimeter:sortedInnerPerimeters))
  (previousDistance) =
  
  let distanceToCurrInnerPerim =
        calculateDistanceA (head innerPerimeter) (distanceFromThisCpoint) 
      
  in
    case ((previousDistance < ) <$> distanceToCurrInnerPerim )  of
     Left e -> Left e
     Right (True) ->
       orderInnerPerimsByDistanceFromHeadRecurNM (InnerPerimeters  innerPerimeters) (AdvancingCPoint distanceFromThisCpoint)
                                  (InnerPerimeters $ sortedInnerPerimeter:innerPerimeter:sortedInnerPerimeters) previousDistance
     Right (False) ->
       extractE $ (orderInnerPerimsByDistanceFromHeadRecurNM (InnerPerimeters  innerPerimeters) (AdvancingCPoint distanceFromThisCpoint) 
                                       (InnerPerimeters $ innerPerimeter:sortedInnerPerimeter:sortedInnerPerimeters)) <$> distanceToCurrInnerPerim

orderInnerPerimsByDistanceFromHeadRecurNM (InnerPerimeters []) _  sortedInnerPerimeters _ = Right sortedInnerPerimeters

orderInnerPerimsByDistanceFromHeadRecurNM perims _ _ _ =
  Left $ "orderInnerPerimsByDistanceFromHeadRecurNM missing pattern match: InnerPerimeters: " ++ (show perims)
-- ====================================================== end: version of orderInnerPerimsByDistanceFromHeadRecur w/o Maybe=========================

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
--------------------------------------------------------------NM of removeContainedCPointFromHeadOfPerims -----------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
Task:
Remvove the head of outerperimeters and head of each [innnerperimeters], if that CornerPoints `contains` the advancingCpoint

Given:
innerPerimetersUnOrdered:
InnerPerimeters (which is a [[CornerPoints]])
Must be passed in as justified(not empty, nor contains any empty lists, as that should be Nothing) an so will not fail with Head

outerPerimeters:
OuterPerimeter which is Nothing or contains at least 1 value so will not fail with Head
-}
--ToDo: Perhaps use Preluse.Safe or some other such package to ensure no failure on Head
removeContainedCPointFromHeadOfPerimsNM :: (Maybe Perimeters) -> (Maybe Perimeters) -> AdvancingCPoint -> Either String ((Maybe Perimeters),(Maybe Perimeters))
removeContainedCPointFromHeadOfPerimsNM innerPerimetersUnOrdered outerPerimeter (AdvancingCPoint advancingCpoint) =
  let
    process :: (Maybe Perimeters) -> (Maybe Perimeters) -> Either String ((Maybe Perimeters),(Maybe Perimeters))
    process Nothing Nothing = Right ((Nothing),(Nothing))
    process Nothing (Just(OuterPerimeter (o:outerPerimeter))) =
      case advancingCpoint `contains` o of
               Left e -> Left $ "Joiners.AdvanceToHeadOfPerimeters.removeContainedCPointFromHeadOfPerimsNM: InnerPerimter = Nothing, OuterPermeter = "
                             ++ (show o) ++ " :os did not remove advancingCpoint: " ++ (show advancingCpoint) ++ " because: " ++ e
               Right True -> Right (Nothing, justifyPerimeters $ OuterPerimeter outerPerimeter)
               Right False -> Left $ "Joiners.AdvanceToHeadOfPerimeters.removeContainedCPointFromHeadOfPerimsNM: InnerPerimter = Nothing, OuterPermeter = "
                             ++ (show o) ++ " :os did not remove advancingCpoint: " ++ (show advancingCpoint) ++ " because it was not contained in head OuterPerimeter "
    process (Just(InnerPerimeters innerPerimeters)) Nothing =
      let
        --previousInnerListThatWasProcessed is passed in and appended so as to be leftist, such as foldl/foldr
        --Is that more efficient?
        processList :: (Maybe [CornerPoints]) -> [[CornerPoints]] ->   [[CornerPoints]] -> Either String [[CornerPoints]]
        processList    (previousInnerListThatWasProcessed)                    ((i:innerPerimeters)) joinedPerims =
          case advancingCpoint `contains` (head i) of
            Left e -> Left e
            Right True ->
              case previousInnerListThatWasProcessed of
                Just previousInnerListThatWasProcessed ->
                  processList (Just $ tail i) innerPerimeters (previousInnerListThatWasProcessed:joinedPerims)
                Nothing ->
                  processList (Just $ tail i) innerPerimeters (joinedPerims)
            Right False ->
              case previousInnerListThatWasProcessed of
                Just previousInnerListThatWasProcessed ->
                  processList (Just i) innerPerimeters (previousInnerListThatWasProcessed:joinedPerims)
                Nothing ->
                  processList (Just i) innerPerimeters (joinedPerims)
        processList (Just previousInnerListThatWasProcessed) [] joinedPerims  =
          Right $ removeEmpty (previousInnerListThatWasProcessed:joinedPerims)
        processList Nothing [] joinedPerims  =
          Right $ (( joinedPerims))
             
      in
        case processList Nothing innerPerimeters [] of
          Left e -> Left e
          Right innerPerims ->
            Right $ ((justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerims),Nothing)
        

    process (Just(InnerPerimeters innerPerimeters)) (Just(OuterPerimeter (o:outerPerimeter))) =
      --to keep it dry, call process for both inner/outer perims, with the other being Nothing,
      --then combine the results.
      let innerPerims = process (Just(InnerPerimeters innerPerimeters)) Nothing
          outerPerims = process Nothing (Just(OuterPerimeter (o:outerPerimeter)))
      in
      case innerPerims of
        Left e -> Left e
        (Right (innerPerims,Nothing)) ->
          case outerPerims of
            Left e -> Left e
            Right (i, outerPerims) ->
              Right (innerPerims, outerPerims)

       

          
      
      
    process innerPerim outerPerim = Left $ "AdvanceToHeadOfInnerPerimeters.removeContainedCPointFromHeadOfPerimsNM.proesss: missing pattern match for inner perims: " ++
                                           (show innerPerim) ++ " and outer perims: " ++ (show outerPerim)
  in
   
    process innerPerimetersUnOrdered outerPerimeter



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
--does not get used internally
--Used in Advancer, Delaunay<Test>
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

--replaced with the next fx, which using monad. Did it improve
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

-- ================================================== NM versions of advancingCpointFromHeadOfInnerPerims =============================================
--no Maybe inputs or returned.
advancingCpointFromHeadOfInnerPerimsNM ::
                       Perimeters -> --the inner perimeters
                       AdvancingCPoint -> --the current advancingCpoint, from which advancingCpointNew is build from.
                       Either String AdvancingCPoint
                            --Right: the new advancing cpoint build from advancingCpoint and closes perim cpoint
                            --Left: the eror

advancingCpointFromHeadOfInnerPerimsNM (InnerPerimeters innerPerimeters) (AdvancingCPoint advancingCPoint) = do
  let getHead :: Perimeters -> Either String CornerPoints
      getHead (InnerPerimeters (innerPerimeter:innerPerimeters)) =
        case (length ((innerPerimeter:innerPerimeters))) == 0 of
          True -> Left "Joiners.AdvanceToHeadOfPerimeters.advancingCpointFromHeadOfInnerPerims has empty InnerPerimeters"
          False -> case (length innerPerimeter) == 0 of
            True -> Left "Joiners.AdvanceToHeadOfPerimeters.advancingCpointFromHeadOfInnerPerims has empty InnerPerimeters"
            False -> Right $ head innerPerimeter
        
  justifiedPerimeters <- case justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters of
                      Nothing -> Left "inner perimeters are empty"
                      (Just (InnerPerimeters innerPerimeters)) -> Right $ ({-Just-} (InnerPerimeters innerPerimeters))  
  orderedPerimeters <- orderInnerPerimsByDistanceFromHeadNM justifiedPerimeters (AdvancingCPoint advancingCPoint)
  headOfOrderedPerims <- getHead orderedPerimeters
  val <- raisedTo {-safeHeadOfOrderedPerimetersEM-}headOfOrderedPerims advancingCPoint
  return $ AdvancingCPoint val
  



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


advancingCpointFromHeadOfOuterPerimsNM :: Perimeters -> AdvancingCPoint -> Either String AdvancingCPoint
advancingCpointFromHeadOfOuterPerimsNM (OuterPerimeter (o:outerPerimeter)) (AdvancingCPoint advancingCPoint) =
  case o `raisedTo` advancingCPoint of
        Left e -> Left $ "Joiners.Delaunay.advancingCpointFromHeadOfOuterPerims: had error when o `raisedTo` advancingCPoint because: " ++ e
        Right (cpoint) -> Right $ AdvancingCPoint cpoint



---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
See which is closest base on advancingCpoint to the head cpoint, and build an  cpoint from it.
-If it has legalIntersection use it
-If not legal
 build cpoint from other perim and cx for legal
 -if not legal, it is an error
 -if legal use it
-}
advancingCpointFromDoublePerimsUsingDistanceToHeadOfPerimsCpointsNM :: Perimeters -> Perimeters -> [[CornerPoints]] -> AdvancingCPoint -> Either String AdvancingCPoint 
advancingCpointFromDoublePerimsUsingDistanceToHeadOfPerimsCpointsNM innerPerimeters (OuterPerimeter(o:outerPerimeters)) rawInnerPerimeters (AdvancingCPoint advancingCpoint) = do
  distanceToInnerCpoint <- do -- :: Either String DistanceA
    --(InnerPerimeters(i:orderedInnerPerimeters)) <- orderInnerPerimsByDistanceFromHeadNM innerPerimeters (AdvancingCPoint advancingCpoint)
    orderedInnerPerimeters <- orderInnerPerimsByDistanceFromHeadNM innerPerimeters (AdvancingCPoint advancingCpoint)
    --calculateDistanceA advancingCpoint $ head i    -- :: Either String Perimeters
    let deref :: Perimeters -> [CornerPoints]
        deref (InnerPerimeters(i:orderedInnerPerimeters)) = i 
    calculateDistanceA advancingCpoint $ head $ deref orderedInnerPerimeters   -- :: Either String Perimeters
    -- :: Either String DistanceA

  distanceToOuterCpoint <- calculateDistanceA advancingCpoint o

  (AdvancingCPoint advancingInnerCpoint) <- advancingCpointFromHeadOfInnerPerimsNM innerPerimeters (AdvancingCPoint advancingCpoint)
  isLegalInnerCpoint <- perimetersContainLegalIntersections rawInnerPerimeters  advancingInnerCpoint

  (AdvancingCPoint advancingOuterCpoint) <- advancingCpointFromHeadOfOuterPerimsNM (OuterPerimeter(o:outerPerimeters)) (AdvancingCPoint advancingCpoint)
  isLegalOuterCpoint <- perimetersContainLegalIntersections rawInnerPerimeters  advancingOuterCpoint

  advancingCpointNew <- do
    case distanceToInnerCpoint <= distanceToOuterCpoint of
      True ->
        case isLegalInnerCpoint of
          True -> return $ AdvancingCPoint advancingInnerCpoint
          False -> 
            case isLegalOuterCpoint of
              True -> return $ AdvancingCPoint advancingOuterCpoint
              False -> Left "both inner and outer cpoint intersections were illegallllll"
      False ->
        case isLegalOuterCpoint of
          True -> return $ AdvancingCPoint advancingOuterCpoint
          False -> 
            case isLegalInnerCpoint of
              True -> return $ AdvancingCPoint advancingInnerCpoint
              False -> Left "both inner and outer cpoint intersections were illegal"
  
  return $ advancingCpointNew
 
{-
advancingCpointFromDoublePerimsUsingDistanceToHeadOfPerimsCpointsNM :: Perimeters -> Perimeters -> [[CornerPoints]] -> AdvancingCPoint -> Either String AdvancingCPoint 
advancingCpointFromDoublePerimsUsingDistanceToHeadOfPerimsCpointsNM innerPerimeters (OuterPerimeter(o:outerPerimeters)) rawInnerPerimeters (AdvancingCPoint advancingCpoint) = do
  distanceToInnerCpoint <- do
    (InnerPerimeters(i:orderedInnerPerimeters)) <- orderInnerPerimsByDistanceFromHeadNM innerPerimeters (AdvancingCPoint advancingCpoint)
    calculateDistanceA advancingCpoint $ head i
    

  distanceToOuterCpoint <- calculateDistanceA advancingCpoint o

  (AdvancingCPoint advancingInnerCpoint) <- advancingCpointFromHeadOfInnerPerimsNM innerPerimeters (AdvancingCPoint advancingCpoint)
  isLegalInnerCpoint <- perimetersContainLegalIntersections rawInnerPerimeters  advancingInnerCpoint

  (AdvancingCPoint advancingOuterCpoint) <- advancingCpointFromHeadOfOuterPerimsNM (OuterPerimeter(o:outerPerimeters)) (AdvancingCPoint advancingCpoint)
  isLegalOuterCpoint <- perimetersContainLegalIntersections rawInnerPerimeters  advancingOuterCpoint

  advancingCpointNew <- do
    case distanceToInnerCpoint <= distanceToOuterCpoint of
      True ->
        case isLegalInnerCpoint of
          True -> return $ AdvancingCPoint advancingInnerCpoint
          False -> 
            case isLegalOuterCpoint of
              True -> return $ AdvancingCPoint advancingOuterCpoint
              False -> Left "both inner and outer cpoint intersections were illegallllll"
      False ->
        case isLegalOuterCpoint of
          True -> return $ AdvancingCPoint advancingOuterCpoint
          False -> 
            case isLegalInnerCpoint of
              True -> return $ AdvancingCPoint advancingInnerCpoint
              False -> Left "both inner and outer cpoint intersections were illegal"
  
  return $ advancingCpointNew

-}







getHead :: Perimeters -> Either String CornerPoints
getHead (InnerPerimeters (innerPerimeter:innerPerimeters)) =
        case (length ((innerPerimeter:innerPerimeters))) == 0 of
          True -> Left "Joiners.AdvanceToHeadOfPerimeters.advancingCpointFromHeadOfInnerPerims has empty InnerPerimeters"
          False -> case (length innerPerimeter) == 0 of
            True -> Left "Joiners.AdvanceToHeadOfPerimeters.advancingCpointFromHeadOfInnerPerims has empty InnerPerimeters"
            False -> Right $ head innerPerimeter



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


