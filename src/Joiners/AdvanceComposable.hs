{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-- |
Create a version of Advancers that are totally composable.

Have a single function that runs the composers. Do not have a wrapper such as Advancer which wrapped Advancer'.
It will take a datatype that is passed in from Builder.Monad(or where ever) allowing it to be recursive from the start.

Internal functions, such as get advancing cpoint from outer/inner perims will take/return that same datatype.
Have a chooser fx that takes/returns 2(or a list) of same datatype, for each potential advancing cpoint, and the perimeters lists
which have already been modified for that advancing cpoint.

Have an extractor fx that checks for Either status, and converst Right results to the [CornerPoints] used by Builder.Monad.
Could have other extractor fx's that extract it differently, such as a Builder.Monad that does not use CornerPointsError.
-}
module Joiners.AdvanceComposable(Advancer(..), OuterAdvancerOutput(..), InnerAdvancerOutput(..), naiveAdvCpointFromInnerPerims, naiveAdvCPointFromOuterPerims, advancerRecur,
                                 advCPointFromClosestInnerOuterAdvCPoint, extractAdvCPointsFromAdvancer, advCPointFromClosestInnerOuterUsedCPoint,
                                 createAdvCPointFromInnerPerimsCheckLegalIntersection, outerAdvancerOutPutHasLegalIntersections, checkInnerAdvCPtForLegality) where

import Joiners.AdvanceSupport(justifyPerimeters)

import Helpers.List(removeEmpty)

import Math.Distance(DistanceA(..), calculateDistanceA)

import Data.Maybe(fromJust)

import CornerPoints.FaceConversions(raisedTo)
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@),
                                cornerPointsError, isCubePoints, isCubePointsList)
import CornerPoints.Points (Point(..), )
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction(extractB1, extractB2, extractB3, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
                                  extractFrontLeftLine, extractFrontRightLine, extractBackBottomLine, extractBackFace)

import Geometry.Intercept(perimetersContainLegalIntersections)

import Primitives.Cylindrical.Walled(cylinder)

import Geometry.Angle(Angle(..))

import Control.Lens

import Test.HUnit

type InnerPerimeters = [[CornerPoints]]
type OuterPerimeter = [CornerPoints]
type InnerPerimetersBeforeExtraction = [[CornerPoints]]
type AdvancingCPoints = [CornerPoints]
type AdvancingCPoint = CornerPoints

-- |
-- This is the master datatype that holds all the data, and what will be used for recursive calls.
data Advancer =
  Advancer
    {_innerPerimeters :: Maybe InnerPerimeters,
     _innerPerimetersBeforeExtraction :: Maybe InnerPerimetersBeforeExtraction,
     _outerPerimeterA  :: Maybe OuterPerimeter,
     _advancingCPointA :: Maybe AdvancingCPoint,
     _advancingCpoints :: AdvancingCPoints
    }
  deriving (Eq)

instance Show Advancer where
  show (Advancer innerPerimeters _ outerPerimeter (Just advCPoint) _) =
    "Advancer " ++ (show innerPerimeters) ++ (show outerPerimeter) ++ " advCPointA: " ++ (show advCPoint)
  show (Advancer innerPerimeters _ outerPerimeter Nothing _) =
    "Advancer " ++ (show innerPerimeters) ++ (show outerPerimeter) ++ " advCPointA: Nothing"
  show advancer = "Advancer: missing pattern match for show"

makeLenses ''Advancer

-- |
-- The output datatype of the function which creates a new AdvancingCPoint using InnerPerimeters and current AdvancingCPoint.
data InnerAdvancerOutput =
  InnerAdvancerOutput
    {_innerPerimetersI  :: Maybe InnerPerimeters, -- | The remaining InnerPerimeters, after possibly removing the CornerPoint from which the just built AdvancintCPoint was made from.
     _advancingCPointI  :: Maybe AdvancingCPoint, -- | The new AdvancingCPoint, which may have been built from the previous AdvancingCPoint and InnerPerimeters.
     _usedCPointI       :: Maybe CornerPoints,    -- | The perimeter CornerPoint, which was used to build the current  AdvancingCPoint,.
     _advancingCPointsI :: AdvancingCPoints       -- | The [AdvancingCPoint] which may or may not have a new AdvancingCPoint appended onto it.
    }
  deriving (Eq)



instance Show InnerAdvancerOutput where
  show (InnerAdvancerOutput (Just innerPerims) (Just advCPoint) (Just usedCPoint) advCPoints) =
          " innerAdvancer: innerPerimeters length = " ++ (show $ length innerPerims) ++ (show innerPerims) ++ " advCPoint: " ++ (show advCPoint) ++ " used cpoint: " ++ (show usedCPoint)
          ++ " advCPoints length: " ++ (show $ length advCPoints)
  show (InnerAdvancerOutput Nothing (Just advCPoint) (Just usedCPoint) advCPoints) =
          " innerAdvancer: innerPerims: Nothing" ++ " advCPoint: " ++ (show advCPoint) ++ " used cpoint: " ++ (show usedCPoint)
          ++ " advCPoints length: " ++ (show $ length advCPoints)
  show (InnerAdvancerOutput (Just innerPerims) Nothing Nothing advCPoints) =
          " innerAdvancer: innerPerimeters length and content of head = " ++ (show $ length $ head innerPerims) ++ (show innerPerims) ++  " advCPoint: Nothing; usedCPoint: Nothing"
          ++ " advCPoints length: " ++ (show $ length advCPoints)
  show (InnerAdvancerOutput Nothing Nothing Nothing advCPoints) =
          " innerAdvancer: innerPerimeters Nothing; advancingCPoint Nothing; usedCPoint Nothing"
          ++ " advCPoints length: " ++ (show $ length advCPoints)
  show innerAdvancer = "missing pattern match to show InnerAdvancerOutput " 
  

-- |
-- The output datatype of the function which creates a new AdvancingCPoint using OuterPerimeter and current AdvancingCPoint.
data OuterAdvancerOutput =
  OuterAdvancerOutput
    {_outerPerimeterO   :: Maybe  OuterPerimeter, -- | The remaining OuterPerimeter, after removing the CornerPoint from which the just built AdvancintCPoint was made from.
     _advancingCPointO  :: Maybe AdvancingCPoint, -- | The new AdvancingCPoint, which was built from the previous AdvancingCPoint and OuterPerimeter.
     _usedCPointO  :: Maybe AdvancingCPoint, -- | The CPoint, which was use to build the new advancingCPoint.
     _advancingCPointsO :: AdvancingCPoints -- | The [AdvancingCPoint] which may or may not have a new AdvancingCPoint appended onto it.
    }
    deriving (Eq)

instance Show OuterAdvancerOutput where
  show (OuterAdvancerOutput (Just outerPerimeter) (Just advCPoint) (Just usedCPoint) advCPoints) =
    "OuterAdvancerOutput: outerPerimeter length == " ++ (show $ length outerPerimeter) ++ " advCPoint: " ++ (show advCPoint)
    ++ " used cpoint: " ++ (show usedCPoint) ++ " advCPoints length: " ++ (show $ length advCPoints)
  show (OuterAdvancerOutput Nothing (Just advCPoint) (Just usedCPoint) advCPoints) =
    "OuterAdvancerOutput: outerPerimeter Nothing;  advCPoint: " ++ (show advCPoint) ++ " used cpoint: "
    ++ (show usedCPoint) ++ " advCPoints length: " ++ (show $ length advCPoints)
  show (OuterAdvancerOutput (Just outerPerimeter) Nothing Nothing advCPoints) =
    --"OuterAdvancerOutput: outerPerimeter length == " ++ (show $ length outerPerimeter) ++ " advCPoint: Nothing; useCPoint Nothing"
    "OuterAdvancerOutput: outerPerimeter  == " ++ (show outerPerimeter) ++ " advCPoint: Nothing; useCPoint Nothing"
    ++ " advCPoints length: " ++ (show $ length advCPoints)
  show (OuterAdvancerOutput Nothing Nothing Nothing advCPoints) =
    "OuterAdvancerOutput: outerPerimeter Nothing  advCPoint: Nothing; usedCPoint Nothing" ++ " advCPoints length: " ++ (show $ length advCPoints)
  show outerAdvancerOutput = "missing pattern match to show OuterAdvancerOutput"



justifyMaybeNestedLists :: Maybe [[CornerPoints]] -> Maybe [[CornerPoints]]
justifyMaybeNestedLists Nothing = Nothing
justifyMaybeNestedLists (Just ([])) = Nothing
justifyMaybeNestedLists (Just ([[]])) = Nothing
justifyMaybeNestedLists (Just innerPerimeters) =
  let emptiesRemoved = removeEmpty innerPerimeters
  in
  case (length emptiesRemoved) == 0 of
    True -> Nothing
    False -> Just   emptiesRemoved

justifyNestedLists :: [[CornerPoints]] -> Maybe [[CornerPoints]]
justifyNestedLists [] = Nothing
justifyNestedLists [[]] = Nothing
justifyNestedLists innerPerimeters =
  let emptiesRemoved = removeEmpty innerPerimeters
  in
  case (length emptiesRemoved) == 0 of
    True -> Nothing
    False -> Just   emptiesRemoved

-- | Extract the advancing cpoints from the Advancer.
-- Use to extract the advancing cPoints to a [CornerPoints] when the joiner is done running.
-- Left e will be converted into [CornerPointsError e] as required by Builders.Monad
extractAdvCPointsFromAdvancer :: (Either String Advancer) -> [CornerPoints]
extractAdvCPointsFromAdvancer (Left e) = [CornerPointsError e]
extractAdvCPointsFromAdvancer (Right (Advancer _ _ _ _ advCPoints)) = advCPoints

                               --         iP iPBE oP              advC    advCs
naiveAdvCPointFromOuterPerims :: Advancer -> Either String OuterAdvancerOutput
naiveAdvCPointFromOuterPerims (Advancer _  _    Nothing         Nothing advancingCPoints)  = Right $ OuterAdvancerOutput Nothing Nothing Nothing advancingCPoints
naiveAdvCPointFromOuterPerims (Advancer _  _    Nothing         _       advancingCPoints)  = Right $ OuterAdvancerOutput Nothing Nothing Nothing advancingCPoints
naiveAdvCPointFromOuterPerims (Advancer _  _    (Just [])       _       advancingCPoints)  = Right $ OuterAdvancerOutput Nothing Nothing Nothing advancingCPoints
  --get overlapped patterns when outerPerims is Nothing or (Just []). But if I don't, get failing pattern match when testing
  --In order to get rid of this, would need to cx for Just [] in the next fx.
naiveAdvCPointFromOuterPerims (Advancer _  _    (Just(o:outerPerim)) (Just prevAdvCPoint) advancingCPoints) = 
        let newAdvancingCPoint = o `raisedTo` prevAdvCPoint
        in
        case newAdvancingCPoint of
          Left e -> Left $ "Joiners.AdvanceComposable.advancingCpointFromHeadOfOuterPerims: had error when o `raisedTo` advancingCPoint because: " ++ e
          Right ( newAdvancingCPoint) ->
            case (length outerPerim) == 0 of
              True -> Right $ OuterAdvancerOutput Nothing (Just newAdvancingCPoint) (Just o) (newAdvancingCPoint:advancingCPoints)
              False -> Right $ OuterAdvancerOutput (Just outerPerim) (Just newAdvancingCPoint) (Just o) (newAdvancingCPoint:advancingCPoints)

naiveAdvCPointFromOuterPerims (Advancer _  _    outerPerimeters Nothing advancingCPoints)  = Right $ OuterAdvancerOutput outerPerimeters Nothing Nothing advancingCPoints
naiveAdvCPointFromOuterPerims advancer = Left $ "Joiners.AdvanceComposable.naiveAdvCPointFromOuterPerims missing pattern match for: " ++ (show advancer)

{-
                               --         iP iPBE oP              advC    advCs
naiveAdvCPointFromOuterPerims :: Advancer -> Either String OuterAdvancerOutput
naiveAdvCPointFromOuterPerims (Advancer _  _    Nothing         Nothing advancingCPoints)  = Right $ OuterAdvancerOutput Nothing Nothing advancingCPoints
naiveAdvCPointFromOuterPerims (Advancer _  _    Nothing         _       advancingCPoints)  = Right $ OuterAdvancerOutput Nothing Nothing advancingCPoints
naiveAdvCPointFromOuterPerims (Advancer _  _    (Just [])       _       advancingCPoints)  = Right $ OuterAdvancerOutput Nothing Nothing advancingCPoints
  --get overlapped patterns when outerPerims is Nothing or (Just []). But if I don't, get failing pattern match when testing
  --In order to get rid of this, would need to cx for Just [] in the next fx.
naiveAdvCPointFromOuterPerims (Advancer _  _    (Just(o:outerPerim)) (Just prevAdvCPoint) advancingCPoints) = 
        let newAdvancingCPoint = o `raisedTo` prevAdvCPoint
        in
        case newAdvancingCPoint of
          Left e -> Left $ "Joiners.AdvanceComposable.advancingCpointFromHeadOfOuterPerims: had error when o `raisedTo` advancingCPoint because: " ++ e
          Right ( newAdvancingCPoint) ->
            case (length outerPerim) == 0 of
              True -> Right $ OuterAdvancerOutput Nothing (Just newAdvancingCPoint) (newAdvancingCPoint:advancingCPoints)
              False -> Right $ OuterAdvancerOutput (Just outerPerim) (Just newAdvancingCPoint) (newAdvancingCPoint:advancingCPoints)

naiveAdvCPointFromOuterPerims (Advancer _  _    outerPerimeters Nothing advancingCPoints)  = Right $ OuterAdvancerOutput outerPerimeters Nothing advancingCPoints
naiveAdvCPointFromOuterPerims advancer = Left $ "Joiners.AdvanceComposable.naiveAdvCPointFromOuterPerims missing pattern match for: " ++ (show advancer)

-}


naiveAdvCpointFromInnerPerims :: Advancer -> Either String InnerAdvancerOutput
naiveAdvCpointFromInnerPerims (Advancer Nothing _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing Nothing advancingCPoints
naiveAdvCpointFromInnerPerims (Advancer Nothing _ _ _ advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing Nothing advancingCPoints
naiveAdvCpointFromInnerPerims (Advancer innerPerimeters _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput innerPerimeters Nothing Nothing advancingCPoints
naiveAdvCpointFromInnerPerims advancer = do
  case (justifyMaybeNestedLists(advancer^.innerPerimeters)) of
    Nothing -> Right $ InnerAdvancerOutput Nothing Nothing Nothing (advancer^.advancingCpoints)
    Just (i:innerPerimeters') ->
          case (head i) `raisedTo` (fromJust (advancer^.advancingCPointA)) of
            Left e -> Left $ "Joiners.AdvanceComposable.naiveAdvCpointFromInnerPerims: raise error: " ++ e
            --Right advCPoint -> Right $ InnerAdvancerOutput (Just innerPerimeters) (Just advCPoint) (advCPoint:(advancer^.advancingCpoints))
              --Above caused it to have a single point as innerPerim
              
            --Right advCPoint -> Right $ InnerAdvancerOutput (Just $ ((tail i) : innerPerimeters')) (Just advCPoint) (Just $ head i) (advCPoint:(advancer^.advancingCpoints))
            Right advCPoint -> Right $ InnerAdvancerOutput (justifyNestedLists $ ((tail i) : innerPerimeters')) (Just advCPoint) (Just $ head i) (advCPoint:(advancer^.advancingCpoints))
{-
naiveAdvCpointFromInnerPerims :: Advancer -> Either String InnerAdvancerOutput
naiveAdvCpointFromInnerPerims (Advancer Nothing _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing advancingCPoints
naiveAdvCpointFromInnerPerims (Advancer Nothing _ _ _ advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing advancingCPoints
naiveAdvCpointFromInnerPerims (Advancer innerPerimeters _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput innerPerimeters Nothing advancingCPoints
naiveAdvCpointFromInnerPerims advancer = do
  case (justifyMaybeNestedLists(advancer^.innerPerimeters)) of
    Nothing -> Right $ InnerAdvancerOutput Nothing Nothing (advancer^.advancingCpoints)
    Just (i:innerPerimeters') ->
          case (head i) `raisedTo` (fromJust (advancer^.advancingCPointA)) of
            Left e -> Left $ "Joiners.AdvanceComposable.naiveAdvCpointFromInnerPerims: raise error: " ++ e
            --Right advCPoint -> Right $ InnerAdvancerOutput (Just innerPerimeters) (Just advCPoint) (advCPoint:(advancer^.advancingCpoints))
              --Above caused it to have a single point as innerPerim
              
            Right advCPoint -> Right $ InnerAdvancerOutput (Just $ ((tail i) : innerPerimeters')) (Just advCPoint) (advCPoint:(advancer^.advancingCpoints))
-}
-- | Create advancing cpoint from inner perimeters.
-- If no previous advancing cpoint exist, return nothing.
-- If created adv. cpoint is illegal, return nothing.
-- If created adv. cpoint is legal:
--  retun new adv cpoint
--  remove the used inner cpoint from inner perims
--  add new adv. cpoint to the advancing cpoints
-- If and error is thrown, return Left with error info.

{-
To do:
  Choose the inner perim cpoint to use. This one should be the head of the inner list which is closest.
-}
createAdvCPointFromInnerPerimsCheckLegalIntersection :: Advancer -> Either String InnerAdvancerOutput
createAdvCPointFromInnerPerimsCheckLegalIntersection (Advancer Nothing _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing Nothing advancingCPoints
createAdvCPointFromInnerPerimsCheckLegalIntersection (Advancer Nothing _ _ _ advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing Nothing advancingCPoints
createAdvCPointFromInnerPerimsCheckLegalIntersection (Advancer innerPerimeters _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput innerPerimeters Nothing Nothing advancingCPoints
createAdvCPointFromInnerPerimsCheckLegalIntersection (Advancer (Just innerPerims) (Just innerPerimsNoExt) _ (Just advCPoint) advCPoints) = do
  case (justifyNestedLists innerPerims) of
    Nothing -> Right $ InnerAdvancerOutput Nothing Nothing Nothing advCPoints
    Just (i:innerPerimeters) ->
          case (head i) `raisedTo` advCPoint of
            Left e -> Left $ "Joiners.AdvanceComposable.naiveAdvCpointFromInnerPerims: raise error: " ++ e
            Right advCPoint ->
              case perimetersContainLegalIntersections innerPerimsNoExt advCPoint of
                Right True ->
                  --Right $ InnerAdvancerOutput (justifyNestedLists innerPerimeters) (Just advCPoint) (Just $ head i) (advCPoint:advCPoints)
                  Right $ InnerAdvancerOutput (justifyNestedLists ((tail i) : innerPerimeters)) (Just advCPoint) (Just $ head i) (advCPoint:advCPoints)
                Right False -> Right $ InnerAdvancerOutput (Just innerPerims) Nothing Nothing advCPoints
                Left e -> Left $ "Joiners.AdvanceComposable.createAdvCPointFromInnerPerimsCheckLegalIntersection.perimetersContainLegalIntersections threw error: " ++ e
createAdvCPointFromInnerPerimsCheckLegalIntersection advancer = Left $ "Joiners.AdvanceComposable.createAdvCPointFromInnerPerimsCheckLegalIntersection: missing pattern match for: " ++ (show advancer)

-- | Check if the advCPt in an InnerAdvancerOuput is legal.
-- Takes both an InnerAdvancer, and the inner perims before extraction. Should I pass in the entire Advancer instead of just inner perims before ext.? 
checkInnerAdvCPtForLegality :: InnerAdvancerOutput -> Maybe InnerPerimeters -> Either String InnerAdvancerOutput
checkInnerAdvCPtForLegality (InnerAdvancerOutput (Just (i:innerPerimeters)) (Just advCPt) (Just usedCPt) advCPts) (Just innerPBE) =
  case perimetersContainLegalIntersections innerPBE advCPt of
                Right True ->
                  Right $ InnerAdvancerOutput (Just (i:innerPerimeters)) (Just advCPt) (Just usedCPt) advCPts
                Right False -> Right $ InnerAdvancerOutput (Just ((usedCPt : i) : innerPerimeters)) Nothing Nothing (tail advCPts)
                Left e -> Left $ "Joiners.AdvanceComposable.createAdvCPointFromInnerPerimsCheckLegalIntersection.perimetersContainLegalIntersections threw error: " ++ e

checkInnerAdvCPtForLegality (InnerAdvancerOutput innerAdvancerOutput advCPt usedCPt advCPts) innerPBE =
  Left $ "Joiners.AdvanceComposable.checkInnerAdvCPtForLegality has missing pattern match for innerAdvancerOutput: " ++ (show innerAdvancerOutput)
         ++ "advCPnt: " ++ (show advCPt) ++ "usedCPt: " ++ (show usedCPt) ++ "advCPts: no need to show"

--this one fails when run in deluanayView, while the closed used cpoint works. Perhaps needs to be used with legal intersections.
-- |
-- Create a new AdvancingCPoint if one is not already supplied by <Inner/Outer>AdvancerOutput
-- If AdvancingCPoint is suppied by both <Inner/Outer>AdvancerOutput, choose which to use
-- If AdvancingCPoint is supplied by only 1 of <Inner/Outer>AdvancerOutput, use that 1
advCPointFromClosestInnerOuterAdvCPoint :: InnerAdvancerOutput -> OuterAdvancerOutput -> Advancer -> Either String Advancer


-- Is the intial advCPoint, as there is not AdvCPoint supplied by Inner/Outer AdvancerOutput
advCPointFromClosestInnerOuterAdvCPoint
  (InnerAdvancerOutput (Just (i:innerPerimeters)) Nothing Nothing _ )
  (OuterAdvancerOutput (Just (o:outerPerimeter)) Nothing Nothing _)
  (Advancer _ (Just innerPerimsBeforeExrtnA) _ Nothing advCPointsA)
  =
  --take it from head of first innerP for now
  let
    advCPointNew = o +++ (head i)
  in
  Right $
    Advancer
      (Just ((tail i):innerPerimeters))
      (Just innerPerimsBeforeExrtnA)
      (Just outerPerimeter)
      (Just advCPointNew)
      (advCPointNew : advCPointsA)

--pattern match for the results of last test where inner/outer advancers both have candidate adv CPoints.
advCPointFromClosestInnerOuterAdvCPoint 
  (InnerAdvancerOutput innerPerimetersI (Just advancingCPointI) (Just usedCPoint) advancingCPointsI) --innerA
  (OuterAdvancerOutput outerPerimeterO (Just advancingCPointO) (Just usedCPointO) advancingCPointsO)  --outerA
  (Advancer innerPerimetersA innerPerimetersBeforeExtraction outerPerimeterA (Just advancingCPointA) _)  --advancer
  = do
  iDistance <- calculateDistanceA advancingCPointA advancingCPointI
  oDistance <- calculateDistanceA advancingCPointA advancingCPointO
  case iDistance <= oDistance of
    True  -> Right $ Advancer innerPerimetersI innerPerimetersBeforeExtraction outerPerimeterA (Just advancingCPointI) advancingCPointsI
    False -> Right $ Advancer innerPerimetersA innerPerimetersBeforeExtraction outerPerimeterO (Just advancingCPointO) advancingCPointsO
  

--pattern match for the <inner/outer>Perims are Nothing but the outerPerims has produced an advCPoint.
advCPointFromClosestInnerOuterAdvCPoint
  --innerA
  (InnerAdvancerOutput Nothing Nothing Nothing _)
  --outerA
  (OuterAdvancerOutput Nothing (Just advCPointO) (Just usedCPointO) advCPointsO)
  --advancer
  (Advancer Nothing  innerPerimetersBeforeExtraction (Just outerPerimeterA) (Just advCPointA) advCPointsA) =
  Right $ Advancer Nothing innerPerimetersBeforeExtraction Nothing (Just advCPointO) advCPointsO

--pattern match for the innerPerims are done(so Nothing) but the outerPerims are still going.
advCPointFromClosestInnerOuterAdvCPoint
  --innerA
  (InnerAdvancerOutput Nothing Nothing Nothing _)
  --outerA
  --(OuterAdvancerOutput Nothing (Just advCPointO) advCPointsO)
  (OuterAdvancerOutput outerPerimeterO (Just advCPointO) (Just usedCPointO) advCPointsO)  --outerA
  --advancer
  (Advancer Nothing  innerPerimetersBeforeExtraction (Just outerPerimeterA) (Just advCPointA) advCPointsA) =
  Right $ Advancer Nothing innerPerimetersBeforeExtraction  outerPerimeterO (Just advCPointO) advCPointsO


  
advCPointFromClosestInnerOuterAdvCPoint innerAdvancer outerAdvancer advancer =
  
  Left $ "Joiners.AdvanceComposable.advCPointFromClosestInnerOuterAdvCPoint: missing pattern matche for innerAdvancer: " ++ (show innerAdvancer) ++
         " innerPerimetersBeforeExtraction: no need to show them" ++
         " outer advancer: " ++ (show outerAdvancer) ++
         " original Advancer passed into this fx call: " ++ (show advancer)

-- | Check for legal intersections.
-- If any are illegal, reset the advancingCPoint to Nothing
outerAdvancerOutPutHasLegalIntersections :: OuterAdvancerOutput -> Maybe InnerPerimetersBeforeExtraction -> Either String OuterAdvancerOutput
outerAdvancerOutPutHasLegalIntersections (OuterAdvancerOutput outerPerimeter Nothing usedCPoint advancingCPoints) _ =
  Right $ (OuterAdvancerOutput outerPerimeter Nothing usedCPoint advancingCPoints)
outerAdvancerOutPutHasLegalIntersections (OuterAdvancerOutput (Just outerPerimeter) (Just advCPoint) (Just usedCPoint) advancingCPoints) (Just innerPerimetersBeforeExtraction) =
  case perimetersContainLegalIntersections innerPerimetersBeforeExtraction advCPoint of
    Right True  -> Right (OuterAdvancerOutput (Just outerPerimeter) (Just advCPoint) (Just usedCPoint) advancingCPoints)
    Right False -> Right (OuterAdvancerOutput (Just $ usedCPoint : outerPerimeter) Nothing Nothing (tail advancingCPoints))
    Left e      -> Left $ "outerAdvancerOutPutHasLegalIntersections error: " ++ e
outerAdvancerOutPutHasLegalIntersections (OuterAdvancerOutput Nothing (Just advCPoint) (Just usedCPoint) advancingCPoints) (Just innerPerimetersBeforeExtraction) =
  case perimetersContainLegalIntersections innerPerimetersBeforeExtraction advCPoint of
    Right True  -> Right (OuterAdvancerOutput Nothing (Just advCPoint) (Just usedCPoint) advancingCPoints)
    Right False -> Right (OuterAdvancerOutput (Just [usedCPoint]) Nothing Nothing (tail advancingCPoints))
    Left e      -> Left $ "outerAdvancerOutPutHasLegalIntersections error: " ++ e
  
outerAdvancerOutPutHasLegalIntersections outerAdvancer _ =
  Left $ "outerAdvancerOutPutHasLegalIntersections: missing pattern match for " ++ (show outerAdvancer)

-- |
-- Create a new AdvancingCPoint if one is not already supplied by <Inner/Outer>AdvancerOutput
-- If AdvancingCPoint is suppied by both <Inner/Outer>AdvancerOutput, choose the one which had the closes used cpoint
-- If AdvancingCPoint is supplied by only 1 of <Inner/Outer>AdvancerOutput, use that 1
advCPointFromClosestInnerOuterUsedCPoint :: InnerAdvancerOutput -> OuterAdvancerOutput -> Advancer -> Either String Advancer


-- Is the intial advCPoint, as there is no AdvCPoint supplied, but there are perims to build from. 
advCPointFromClosestInnerOuterUsedCPoint
  (InnerAdvancerOutput (Just (i:innerPerimeters)) Nothing _ _ )
  (OuterAdvancerOutput (Just (o:outerPerimeter))  Nothing _ _)
  (Advancer _ innerPBE _ Nothing advCPointsA)
  =
  --Build advCPt from head of first innerP and head of outerP for now.
  --May need a smarter way to do this.
  let
    advCPointNew = o +++ (head i)
  in
  Right $
    Advancer
      --(Just ((tail i):innerPerimeters))
      (justifyNestedLists ((tail i):innerPerimeters))
      innerPBE
      (Just outerPerimeter)
      (Just advCPointNew)
      (advCPointNew : advCPointsA)

--innerP built new advCPt(s) so pass them on. Nothing left to build from so advanceRecur must exit. 
advCPointFromClosestInnerOuterUsedCPoint
  (InnerAdvancerOutput innerP (Just advCPtI) _ advCPtsI )
  (OuterAdvancerOutput outerP Nothing       _ _       )
  (Advancer _ innerPBE _ _ _)
  =
  Right $ Advancer innerP innerPBE outerP (Just advCPtI) advCPtsI


--pattern match for the <inner/outer>Perims are Nothing but the outerPerims has produced an advCPoint.
advCPointFromClosestInnerOuterUsedCPoint
  (InnerAdvancerOutput innerP Nothing _  _) 
  (OuterAdvancerOutput outerP (Just advCPtO) _ advCPtsO) 
  (Advancer _  innerPBE _ _ _) 
   = 
  Right $ Advancer innerP innerPBE outerP (Just advCPtO) advCPtsO

--pattern match for the results of last test where inner/outer advancers both have candidate adv CPoints.
--Both inner/outer perims produced advCPt so decide which to use and pass it on.
--Pass on inner/outer perims and let advanceRecur see if anything left to build from.
advCPointFromClosestInnerOuterUsedCPoint 
  (InnerAdvancerOutput innerPerimetersI (Just advancingCPointI) (Just usedCPointI) advancingCPointsI) --innerA
  (OuterAdvancerOutput outerPerimeterO (Just advancingCPointO) (Just usedCPointO) advancingCPointsO)  --outerA
  (Advancer innerPerimetersA innerPerimetersBeforeExtraction outerPerimeterA (Just advancingCPointA) _)  --advancer
  = do
  iDistance <- calculateDistanceA advancingCPointA usedCPointI
  oDistance <- calculateDistanceA advancingCPointA usedCPointO
  --case iDistance <= oDistance of
  case iDistance <= (DistanceA $ (distance oDistance) * 1.0) of
    True  -> Right $ Advancer innerPerimetersI innerPerimetersBeforeExtraction outerPerimeterA (Just advancingCPointI) advancingCPointsI
    False -> Right $ Advancer innerPerimetersA innerPerimetersBeforeExtraction outerPerimeterO (Just advancingCPointO) advancingCPointsO
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--No advancingCPnt was build, even though there are innerP's still available. This is not the initial advCPnt as Advancer already has one.
advCPointFromClosestInnerOuterUsedCPoint 
  (InnerAdvancerOutput (Just innerPerimetersI) Nothing _ _) 
  (OuterAdvancerOutput _ Nothing _ _) 
  (Advancer _ _ _ (Just advCPtA) _)
  =
  Left $ "No non-intial advCPnt created when there are still innerPerimeters available. Prev advCPt: " ++ (show advCPtA) ++
         " InnerPerimeters: " ++ (show innerPerimetersI)
  
advCPointFromClosestInnerOuterUsedCPoint innerAdvancer outerAdvancer advancer =
  
  Left $ "Joiners.AdvanceComposable.advCPointFromClosestInnerOuterUsedCPoint: missing pattern matche for innerAdvancer: " ++ (show innerAdvancer) ++
         " outer advancer: " ++ (show outerAdvancer) ++
         " original Advancer passed into this fx call: " ++ (show advancer)

--not sure about this one. See where it fits in when done
{-
advCPointFromClosestInnerOuterUsedCPoint
  (InnerAdvancerOutput innerPerimeters Nothing Nothing _ )
  (OuterAdvancerOutput Nothing Nothing Nothing _)
  (Advancer _ (Just innerPerimsBeforeExrtnA) _ (Just advCPointA) advCPointsA) =
    Right $ Advancer Nothing  (Just innerPerimsBeforeExrtnA) Nothing Nothing advCPointsA
-}

--No advCPt was built and nothing left to build from. advanceRecur must exit
--Do I really need this one.
{-
advCPointFromClosestInnerOuterUsedCPoint
  (InnerAdvancerOutput Nothing Nothing Nothing _ )
  (OuterAdvancerOutput Nothing Nothing Nothing _)
  (Advancer Nothing Nothing Nothing Nothing advCPointsA) =
    Right $ Advancer Nothing Nothing Nothing Nothing advCPointsA
-}

{-
--pattern match for the innerPerims are done(so Nothing) but the outerPerims are still going.
advCPointFromClosestInnerOuterUsedCPoint
  --innerA
  (InnerAdvancerOutput Nothing Nothing Nothing  _)
  --outerA
  --(OuterAdvancerOutput Nothing (Just advCPointO) advCPointsO)
  (OuterAdvancerOutput outerPerimeterO (Just advCPointO) _ advCPointsO)  --outerA
  --advancer
  (Advancer Nothing  innerPerimetersBeforeExtraction (Just outerPerimeterA) (Just advCPointA) advCPointsA) =
  Right $ Advancer Nothing innerPerimetersBeforeExtraction  outerPerimeterO (Just advCPointO) advCPointsO
-}




{-Pattern Matches
          perimsI perimsIBE perimsO advCPt  advCPts
--exit as there is nothing but orig inner perims. Done
Advancer  Nothing Just PIBE Nothing Nothing advCPts
-}

-- | Check a Advancer to see if it has any <Inner/Outer>Perimeters left.
-- If yes, make a recursive call to the function that contains this call of advancerRecur.
-- If no, return the current Advancer.
advancerRecur :: (Advancer -> Either String Advancer) -> Advancer -> Either String Advancer
--Nothing left to build advCPts from, so exit. Whatever the advCPt is does not matter, but pass it on just in case want to look at it.
advancerRecur _ (Advancer Nothing _ Nothing advCPt advCPts) =
  Right $ Advancer Nothing Nothing Nothing advCPt $ reverse advCPts
--all out of perimeters
advancerRecur _ (Advancer Nothing innerPerimetersBeforeExtraction Nothing advancingCPoint advancingCpoints) =
  Right (Advancer Nothing innerPerimetersBeforeExtraction Nothing advancingCPoint (reverse advancingCpoints))
--out of innerPerimeters. Check length of outerPerimeters continue if not 0
advancerRecur me (Advancer Nothing innerPerimetersBeforeExtraction (Just outerPerimeter) advancingCPoint advancingCpoints) =
  case (length outerPerimeter) == 0 of
    True -> Right $ (Advancer Nothing innerPerimetersBeforeExtraction Nothing advancingCPoint (reverse advancingCpoints))
    False -> me (Advancer Nothing innerPerimetersBeforeExtraction (Just outerPerimeter) advancingCPoint advancingCpoints)
--out of outerPerimeters. Check remove empty innerPerimeters and continue.
advancerRecur me (Advancer (Just innerPerimeters) innerPerimetersBeforeExtraction Nothing advancingCPoint advancingCpoints) =
  case justifyNestedLists innerPerimeters of
    Nothing  -> Right $ (Advancer Nothing innerPerimetersBeforeExtraction Nothing advancingCPoint (reverse advancingCpoints))
    Just innerPerimeters' -> me (Advancer (Just innerPerimeters') innerPerimetersBeforeExtraction Nothing advancingCPoint advancingCpoints)
--have both perimeters, justify them and continue ==============================================================================================================================
advancerRecur me (Advancer (Just innerPerimeters) innerPerimetersBeforeExtraction (Just outerPerimeter) advancingCPoint advancingCpoints) =
  case justifyNestedLists innerPerimeters of
    Nothing  ->
      case (length outerPerimeter) == 0 of
        True -> Right $ (Advancer Nothing innerPerimetersBeforeExtraction Nothing advancingCPoint (reverse advancingCpoints))
        False -> me (Advancer Nothing innerPerimetersBeforeExtraction (Just outerPerimeter) advancingCPoint advancingCpoints)
    Just innerPerimeters -> 
      case (length outerPerimeter) == 0 of
        True  -> me (Advancer (Just innerPerimeters) innerPerimetersBeforeExtraction Nothing advancingCPoint advancingCpoints)
        False -> me (Advancer (Just innerPerimeters) innerPerimetersBeforeExtraction (Just outerPerimeter) advancingCPoint advancingCpoints)
advancerRecur _ advancer =
  Left $ "Joiners.AdvanceComposable.advanceRecur missing pattern match for: " ++ (show advancer)
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
--The inner perims for upper/lower sections. Like the top/bottom layers of a scan
--innerTestCylinder' = cylinder [Radius 30 | r <- [1..]] [Radius 150 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
innerTestCylinder' = cylinder [Radius 3 | r <- [1..]] [Radius 3 | r <- [1..]] ([Angle a | a <- [0,10..360]] ) (Point 0 0 0) 10
innerTestCylinder = [innerTestCylinder']
--lower section extracted as Vertices, as would be used by the Joiner
innerTestCylinderBtmExtractedAsVertices' =  (extractB4 $ head innerTestCylinder') : (map (extractB1) innerTestCylinder')
innerTestCylinderBtmExtractedAsVertices = [innerTestCylinderBtmExtractedAsVertices']
--lower section extracted as Lines, as is needed to check for legal intersections
innerTestCylinderBtmExtractedAsBackBtmLines' =  (map extractBackBottomLine innerTestCylinder') 
innerTestCylinderBtmExtractedAsBackBtmLines = [innerTestCylinderBtmExtractedAsBackBtmLines']


--upper section
innerTestCylinderTopExtracted' =  (extractB3 $ head innerTestCylinder') : (map (extractB2) innerTestCylinder')
innerTestCylinderTopExtracted = [innerTestCylinderTopExtracted']

--outerTestCylinderBtm = cylinder [Radius 60 | r <- [1..]] [Radius 80 | r <- [1..]] [Angle a | a <- [0,10..360]] (Point 0 0 0) 10
outerTestCylinderBtm = cylinder [Radius 9 | r <- [1..]] [Radius 9 | r <- [1..]] [Angle a | a <- [0,20..360]] (Point 0 0 0) 10
outerTestCylinderBtmExtracted = (extractF4 $ head outerTestCylinderBtm) : (map (extractF1)  outerTestCylinderBtm)

advancingCPointTest = TestCase $ assertEqual
  "advancingCpointFromHeadOfOuterPerims: with OuterPerimeters == Nothing"
  (Right (OuterAdvancerOutput {_outerPerimeterO = Nothing, _advancingCPointO = Nothing, _usedCPointO = Nothing, _advancingCPointsO = []}))
  (let advancer = Advancer Nothing Nothing Nothing (Just $ CornerPointsError "just a filler") ([])
   in
   naiveAdvCPointFromOuterPerims advancer
  )

run_advancingCPointTest = do
  runTestTT advancingCPointTest

advancingCPointTestWithJustEmptyOPerims = TestCase $ assertEqual
  "advancingCpointFromHeadOfOuterPerims: with OuterPerimeters == Nothing"
  (Right (OuterAdvancerOutput {_outerPerimeterO = Nothing, _advancingCPointO = Nothing, _usedCPointO = Nothing, _advancingCPointsO = []}))
  (let advancer = Advancer Nothing Nothing (Just []) (Just $ CornerPointsError "just a filler") ([])
   in
   naiveAdvCPointFromOuterPerims advancer
  )

run_advancingCPointTestWithJustEmptyOPerims = do
  runTestTT advancingCPointTestWithJustEmptyOPerims

advancingCPointTest2 = TestCase $ assertEqual
  "advancingCpointFromHeadOfOuterPerims: with good OuterPerimeter, AdvancingCPoint == Nothing"
  (Right (OuterAdvancerOutput {_outerPerimeterO = (Just [CornerPointsError "just a filler"]), _advancingCPointO = Nothing, _usedCPointO = Nothing, _advancingCPointsO = []}))
  (let advancer = Advancer Nothing Nothing (Just [CornerPointsError "just a filler"]) Nothing []
   in
   naiveAdvCPointFromOuterPerims advancer
  )
  
run_advancingCPointTest2 = do
  runTestTT advancingCPointTest2


advancingCPointTest3 = TestCase $ assertEqual
  "advancingCpointFromHeadOfOuterPerims: with good <Inner/Outer>Perimeter<s>, AdvancingCPoint == Nothing"
  (Right (OuterAdvancerOutput
          {_outerPerimeterO = (Just $ [CornerPointsError "just a filler"]),
           _advancingCPointO = Nothing,
           _usedCPointO = Nothing,
           _advancingCPointsO = []
          }
         )
  )
  (let advancer = Advancer (Just (innerTestCylinderBtmExtractedAsVertices::InnerPerimeters)) (Just (innerTestCylinderBtmExtractedAsBackBtmLines::InnerPerimetersBeforeExtraction))
                            (Just ([CornerPointsError "just a filler"]::OuterPerimeter)) Nothing ([]::AdvancingCPoints)
   in
   naiveAdvCPointFromOuterPerims advancer
  )


advancingCPointTest4 = TestCase $ assertEqual
  "naiveAdvCpointFromInnerPerims: with good <Inner/Outer>Perimeter<s>, AdvancingCPoint == Nothing, therefore is the intial AdvCPoint"
  (Right Nothing)
  
  (let advancer = Advancer (Just (innerTestCylinderBtmExtractedAsVertices::InnerPerimeters)) (Just (innerTestCylinderBtmExtractedAsBackBtmLines::InnerPerimetersBeforeExtraction))
                            (Just ([CornerPointsError "just a filler"]::OuterPerimeter)) Nothing ([]::AdvancingCPoints)
   in
   case naiveAdvCpointFromInnerPerims advancer of
     Left e -> Left e
     Right (InnerAdvancerOutput _ advCPoint _ _) -> Right advCPoint 
  )

initialAdvCPointBuiltManually = TestCase $ assertEqual
  "have a look at initial adv cpoint"
  --(BottomRightLine {b4 = Point 0.0 (-30.0)  0.0, f4 = Point 0.0 (-80.0) 0.0})
  (BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -3.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -9.0, z_axis = 0.0}})
  (let
     outsidePoint = head outerTestCylinderBtmExtracted
     insidePoint  = head $ head innerTestCylinderBtmExtractedAsVertices
   in
   outsidePoint +++ insidePoint
  )

initialAdvCPointBuiltWithNaiveBuilder  = TestCase $ assertEqual
  "advCPointFromClosestInnerOuterAdvCPoint: build the initial adv cpoint with naive builders"
  --(Right (BottomRightLine {b4 = Point 0.0 (-30.0) 0.0, f4 = Point 0.0 (-80.0) 0.0}))
  (Right $ BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -3.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -9.0, z_axis = 0.0}})
  (let
    process = 
     do
      let
       innerAdvancer =
         Advancer
          (Just innerTestCylinderBtmExtractedAsVertices)
          (Just innerTestCylinder)
          (Just outerTestCylinderBtmExtracted) Nothing ([])
     
      initialAdvCPointFromInner <- naiveAdvCpointFromInnerPerims innerAdvancer
      initialAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims innerAdvancer
      advCPointFromClosestInnerOuterAdvCPoint initialAdvCPointFromInner initialAdvCPointFromOuter innerAdvancer
     
   in
     case process of
       Left e -> Left e
       Right (Advancer _ _ _ (Just advCPoint) _) -> Right advCPoint
       Right advancer -> Left "unmatched pattern in case process"
  )

--value looks right, though should physically confirm it.
--
secondAdvCPointBuiltWithNaiveBuilder = TestCase $ assertEqual
  "advCPointFromClosestInnerOuterAdvCPoint: build the initial adv cpoint plus the next with naive builders"
  (Right
   [
     BottomLeftLine {b1 = Point {x_axis = 0.520944533000791, y_axis = -2.954423259036624, z_axis = 0.0}, f1 = Point {x_axis = 0.0, y_axis = -9.0, z_axis = 0.0}},
     BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -3.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -9.0, z_axis = 0.0}}
   ]
  )
  
  (let
    process = 
     do
      let
       innerAdvancer =
         Advancer
          (Just ([take 2 $ head innerTestCylinderBtmExtractedAsVertices]))
          (Just ([take 2 $ head innerTestCylinderBtmExtractedAsBackBtmLines]))
          (Just $ take 2 outerTestCylinderBtmExtracted) Nothing []
     
      initialAdvCPointFromInner <- naiveAdvCpointFromInnerPerims innerAdvancer
      initialAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims innerAdvancer
      advancerAfterInitialAdvCPoint <-  advCPointFromClosestInnerOuterAdvCPoint initialAdvCPointFromInner initialAdvCPointFromOuter innerAdvancer
      secondAdvCPointFromInner <- naiveAdvCpointFromInnerPerims advancerAfterInitialAdvCPoint
      secondAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims advancerAfterInitialAdvCPoint
      advCPointFromClosestInnerOuterAdvCPoint secondAdvCPointFromInner secondAdvCPointFromOuter advancerAfterInitialAdvCPoint
     
   in
     case process of
       Left e -> Left e
       Right (Advancer _ _ _ _ advCPoints) -> Right advCPoints
       Right advancer -> Left "unmatched pattern in case process"
  )

processPerimetersBothLength2WithRecur = TestCase $ assertEqual 
  "advancerRecur: build the adv cpoints with naive builders using recur function"
  (
   [BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -3.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -9.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 0.520944533000791, y_axis = -2.954423259036624, z_axis = 0.0}, f1 = Point {x_axis = 0.0, y_axis = -9.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 0.520944533000791, y_axis = -2.954423259036624, z_axis = 0.0}, f1 = Point {x_axis = 3.0781812899310186, y_axis = -8.457233587073176, z_axis = 0.0}}
   ]
  )
  (let
     recurProcessor :: Advancer -> Either String Advancer
     recurProcessor advancer = do
                   {- Advancer
                     (innerPerimeter)
                     (innerPerimetersBeforeExtraction)
                     (outerPerimeter) advCPoint advCPoint =-}
       advCPointFromInner <- naiveAdvCpointFromInnerPerims advancer
       advCPointFromOuter <- naiveAdvCPointFromOuterPerims advancer
       newAdvancer       <- advCPointFromClosestInnerOuterAdvCPoint advCPointFromInner advCPointFromOuter advancer
       advancerRecur recurProcessor newAdvancer
   in
   {-
   case recurProcessor $
          Advancer
            (Just ([take 2 $ head innerTestCylinderBtmExtractedAsVertices]))
            (Just ([take 2 $ head innerTestCylinderBtmExtractedAsBackBtmLines]))
            (Just $ take 2 outerTestCylinderBtmExtracted) Nothing [] of
     Left e -> Left e
     Right (Advancer _ _ _ _ advCPoints) -> Right advCPoints-}
   extractAdvCPointsFromAdvancer $ 
     recurProcessor $
          Advancer
            (Just ([take 2 $ head innerTestCylinderBtmExtractedAsVertices]))
            (Just ([take 2 $ head innerTestCylinderBtmExtractedAsBackBtmLines]))
            (Just $ take 2 outerTestCylinderBtmExtracted) Nothing []

  )

outerAdvancerOutPutHasLegalIntersectionsTest = TestCase $ assertEqual
  "outerAdvancerOutPutHasLegalIntersections: has legal intersections"
  (Left "filler")
  --outerAdvancerOutPutHasLegalIntersections
  (let
    process = 
     do
      let
       innerAdvancer =
         Advancer
          (Just ([take 2 $ head innerTestCylinderBtmExtractedAsVertices]))
          (Just ([take 2 $ head innerTestCylinderBtmExtractedAsBackBtmLines]))
          (Just $ take 2 outerTestCylinderBtmExtracted) Nothing []
     
      initialAdvCPointFromInner <- naiveAdvCpointFromInnerPerims innerAdvancer
      initialAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims innerAdvancer
      advancerAfterInitialAdvCPoint <-  advCPointFromClosestInnerOuterAdvCPoint initialAdvCPointFromInner initialAdvCPointFromOuter innerAdvancer
      secondAdvCPointFromInner <- naiveAdvCpointFromInnerPerims advancerAfterInitialAdvCPoint
      secondAdvCPointFromOuter <- naiveAdvCPointFromOuterPerims advancerAfterInitialAdvCPoint
      --advCPointFromClosestInnerOuterAdvCPoint secondAdvCPointFromInner secondAdvCPointFromOuter advancerAfterInitialAdvCPoint
      legalizedSecondAdvCPointFromOuter <- outerAdvancerOutPutHasLegalIntersections secondAdvCPointFromOuter (Just ([take 2 $ head innerTestCylinderBtmExtractedAsBackBtmLines]))
      return legalizedSecondAdvCPointFromOuter
     
   in
     case process of
       Left e -> Left e
       Right (OuterAdvancerOutput _ advCPoint _ _) -> Right advCPoint
       Right advancer -> Left "unmatched pattern in case process"
  )

--createAdvCPointFromInnerPerimsCheckLegalIntersection
createAdvCPointFromInnerPerimsCheckLegalIntersectionGermanHikers = TestCase $ assertEqual
  "see why german hikers never build from inner perims"
  (Right $
    InnerAdvancerOutput 
      Nothing 
      (Just (TopLeftLine {b2 = Point {x_axis = 1.7, y_axis = -1.9, z_axis = 10.0},
                          f2 = Point {x_axis = 0.0, y_axis = -11.3, z_axis = 18.75}}))
      (Just (B2 {b2 = Point {x_axis = 1.7, y_axis = -1.9, z_axis = 10.0}}))
      [TopLeftLine {b2 = Point {x_axis = 1.7, y_axis = -1.9, z_axis = 10.0},
                          f2 = Point {x_axis = 0.0, y_axis = -11.3, z_axis = 18.75}}]
  )
  --(Left "german hikers filler")
  (let
      innerPBE = Just [[BackTopLine (Point 1.7 (-1.9) 10) (Point 0 (-2) 10)]]
      innerP   = Just [[B2 (Point 1.7 (-1.9) 10), B3 (Point 0 (-2) 10)]]
      outerP   = Just [F3 $ Point 0 (-11.3) 18.75, F2 $ Point 4.6 (-11.3) 18.75 ]
      initAdvCPt = Just $ TopRightLine (Point 0 (-2) 10) (Point 0 (-11.3) 18.75)
  
   in    
   createAdvCPointFromInnerPerimsCheckLegalIntersection $ Advancer innerP innerPBE outerP initAdvCPt []
  )

runAllAdvancingCPointTests = do
  runTestTT advancingCPointTest
  runTestTT advancingCPointTest2
  runTestTT advancingCPointTest3
  runTestTT advancingCPointTest4
  runTestTT advancingCPointTestWithJustEmptyOPerims
  runTestTT initialAdvCPointBuiltManually
  runTestTT initialAdvCPointBuiltWithNaiveBuilder
  runTestTT secondAdvCPointBuiltWithNaiveBuilder
  runTestTT processPerimetersBothLength2WithRecur
  runTestTT outerAdvancerOutPutHasLegalIntersectionsTest
  runTestTT createAdvCPointFromInnerPerimsCheckLegalIntersectionGermanHikers
