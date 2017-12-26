{-# LANGUAGE TemplateHaskell #-}
{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}
module Joiners.Delaunay({-delaunay, delaunayA, ,-}delaunayB, 
                        orderByDistance, extractI, removeAdvCPointFromIOPerims, Perimeters(..), AdvancingCPoint(..)) where

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
                         (justifyPerimeters $ InnerPerimeters $ removeEmpty innerPerimeters) --justifiedInnerPerims
                         (justifyPerimeters $ OuterPerimeter outerPerimeters) --justifiedOuterPerims
      
      perimsWithAdvancingCpointBldrRemoved =
                           extractE
                             (removeAdvCPointFromIOPerims <$>
                                                 orderedInnerPerims'
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
    
    orderedNonEmptyInnerPerimsE = orderByDistance innerPerimeters advancingCpoint
    
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

orderedInnerPerims' :: Maybe Perimeters ->  Maybe Perimeters ->  Either String (Maybe Perimeters)
orderedInnerPerims'     (Just (InnerPerimeters justifiedInnerPerims)) (Just (OuterPerimeter (o:outerPerims)))  =
            orderByDistance (Just(InnerPerimeters justifiedInnerPerims)) (AdvancingCPoint o)
orderedInnerPerims' Nothing _ = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty outer perims passed in"
orderedInnerPerims' _ Nothing = Left "Joiners.Delaunay.DelauanayB orderedInnerPerims has empty inner perims passed in"
orderedInnerPerims' perim1 perim2 =
  Left $ "Joiners.Deluany.orderedInnerPerims' has invalid Perimeter types for:" ++ (show perim1) ++ " and : " ++ (show perim2)

        
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


  
-- | Order the innerPermiters by distance to the advancing Cpoint
      
orderByDistance ::  Maybe Perimeters ->
                    AdvancingCPoint ->    -- The outer perim cpoint
                    Either String (Maybe Perimeters)    -- [[good inner perim distance CornerPoints]] ordered by distance to advancing line
orderByDistance  (Just(InnerPerimeters ([]:innerPerimeters))) (AdvancingCPoint distanceFromThisCpoint) =
  orderByDistance (Just (InnerPerimeters (innerPerimeters))) (AdvancingCPoint distanceFromThisCpoint) 
  
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
