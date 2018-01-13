{-# LANGUAGE TemplateHaskell #-}
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
module Joiners.AdvanceComposable() where

import Joiners.AdvanceSupport(justifyPerimeters)

import Helpers.List(removeEmpty)

import Data.Maybe(fromJust)

import CornerPoints.FaceConversions(raisedTo)
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@),
                                cornerPointsError, isCubePoints, isCubePointsList)
import CornerPoints.Points (Point(..), )
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
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
     _innerPerimetersBeforeExtractionA :: Maybe InnerPerimetersBeforeExtraction,
     _outerPerimeterA  :: Maybe OuterPerimeter,
     _advancingCPointA :: Maybe AdvancingCPoint,
     _advancingCpoints :: AdvancingCPoints
    }
  deriving (Eq, Show)
makeLenses ''Advancer

-- |
-- The output datatype of the function which creates a new AdvancingCPoint using OuterPerimeter and current AdvancingCPoint.
data OuterAdvancerOutput =
  OuterAdvancerOutput
    {_outerPerimeterO   :: Maybe  OuterPerimeter, -- | The remaining OuterPerimeter, after removing the CornerPoint from which the just built AdvancintCPoint was made from.
     _advancingCPointO  :: Maybe AdvancingCPoint, -- | The new AdvancingCPoint, which was built from the previous AdvancingCPoint and OuterPerimeter.
     _advancingCPointsO :: AdvancingCPoints -- | The [AdvancingCPoint] which may or may not have a new AdvancingCPoint appended onto it.
    }
    deriving (Eq, Show)


data InnerAdvancerOutput =
  InnerAdvancerOutput
    {_innerPerimetersI  :: Maybe InnerPerimeters, -- | The remaining InnerPerimeters, after possibly removing the CornerPoint from which the just built AdvancintCPoint was made from.
     _advancingCPointI  :: Maybe AdvancingCPoint, -- | The new AdvancingCPoint, which may have been built from the previous AdvancingCPoint and InnerPerimeters.
     _advancingCPointsI :: AdvancingCPoints       -- | The [AdvancingCPoint] which may or may not have a new AdvancingCPoint appended onto it.
    }
  deriving (Eq, Show)


justifyInnerPerimeters :: Maybe [[CornerPoints]] -> Maybe [[CornerPoints]]
justifyInnerPerimeters Nothing = Nothing
justifyInnerPerimeters (Just ([])) = Nothing
justifyInnerPerimeters (Just ([[]])) = Nothing
justifyInnerPerimeters (Just innerPerimeters) =
  let emptiesRemoved = removeEmpty innerPerimeters
  in
  case (length emptiesRemoved) == 0 of
    True -> Nothing
    False -> Just   emptiesRemoved

justify :: [[CornerPoints]] -> Maybe [[CornerPoints]]
justify [] = Nothing
justify [[]] = Nothing
justify innerPerimeters =
  let emptiesRemoved = removeEmpty innerPerimeters
  in
  case (length emptiesRemoved) == 0 of
    True -> Nothing
    False -> Just   emptiesRemoved


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
          Right ( newAdvancingCPoint) -> Right $ OuterAdvancerOutput (Just outerPerim) (Just newAdvancingCPoint) (newAdvancingCPoint:advancingCPoints)

naiveAdvCPointFromOuterPerims (Advancer _  _    outerPerimeters Nothing advancingCPoints)  = Right $ OuterAdvancerOutput outerPerimeters Nothing advancingCPoints
naiveAdvCPointFromOuterPerims advancer = Left $ "Joiners.AdvanceComposable.naiveAdvCPointFromOuterPerims missing pattern match for: " ++ (show advancer)



naiveAdvCpointFromInnerPerims :: Advancer -> Either String InnerAdvancerOutput
naiveAdvCpointFromInnerPerims (Advancer Nothing _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing advancingCPoints
naiveAdvCpointFromInnerPerims (Advancer Nothing _ _ _ advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing advancingCPoints
naiveAdvCpointFromInnerPerims (Advancer innerPerimeters _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput innerPerimeters Nothing advancingCPoints
naiveAdvCpointFromInnerPerims advancer = do
  case (justifyInnerPerimeters(advancer^.innerPerimeters)) of
    Nothing -> Right $ InnerAdvancerOutput Nothing Nothing (advancer^.advancingCpoints)
    Just (i:innerPerimeters) ->
          case (head i) `raisedTo` (fromJust (advancer^.advancingCPointA)) of
            Left e -> Left $ "Joiners.AdvanceComposable.naiveAdvCpointFromInnerPerims: raise error: " ++ e
            Right advCPoint -> Right $ InnerAdvancerOutput (Just innerPerimeters) (Just advCPoint) (advCPoint:(advancer^.advancingCpoints))

--perimetersContainLegalIntersections
legalOnInnerPerimsAdvCpointFromInnerPerims :: Advancer -> Either String InnerAdvancerOutput
legalOnInnerPerimsAdvCpointFromInnerPerims (Advancer Nothing _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing advancingCPoints
legalOnInnerPerimsAdvCpointFromInnerPerims (Advancer Nothing _ _ _ advancingCPoints)  = Right $ InnerAdvancerOutput Nothing Nothing advancingCPoints
legalOnInnerPerimsAdvCpointFromInnerPerims (Advancer innerPerimeters _ _ Nothing advancingCPoints)  = Right $ InnerAdvancerOutput innerPerimeters Nothing advancingCPoints
legalOnInnerPerimsAdvCpointFromInnerPerims (Advancer (Just innerPerims) (Just innerPerimsNoExt) (Just outerPerim) (Just advCPoint) advCPoints) = do
  case (justify innerPerims) of
    Nothing -> Right $ InnerAdvancerOutput Nothing Nothing advCPoints
    Just (i:innerPerimeters) ->
          case (head i) `raisedTo` advCPoint of
            Left e -> Left $ "Joiners.AdvanceComposable.naiveAdvCpointFromInnerPerims: raise error: " ++ e
            Right advCPoint ->
              case perimetersContainLegalIntersections innerPerimsNoExt advCPoint of
                Right True ->
                  Right $ InnerAdvancerOutput (justify innerPerimeters) (Just advCPoint) (advCPoint:advCPoints)
                Right False -> Right $ InnerAdvancerOutput (Just innerPerims) Nothing advCPoints
                Left e -> Left $ "Joiners.AdvanceComposable.legalOnInnerPerimsAdvCpointFromInnerPerims threw error: " ++ e
legalOnInnerPerimsAdvCpointFromInnerPerims advancer = Left $ "oiners.AdvanceComposable.legalOnInnerPerimsAdvCpointFromInnerPerims: missing pattern match for: " ++ (show advancer)

-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------
-------------------------------------------------------- testing 1 2 3----------------------------------------------------------------------

innerTestCylinder' = cylinder [Radius 30 | r <- [1..]] [Radius 150 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
innerTestCylinder = [innerTestCylinder']
innerTestCylinderExtracted' =  (extractB4 $ head innerTestCylinder') : (map (extractB1) innerTestCylinder')
innerTestCylinderExtracted = [innerTestCylinderExtracted']
outerTestCylinder = cylinder [Radius 60 | r <- [1..]] [Radius 80 | r <- [1..]] [Angle a | a <- [0,10..360]] (Point 0 0 0) 10
outerTestCylinderExtracted' = (extractF4 $ head outerTestCylinder) : (map (extractF1)  outerTestCylinder)
outerTestCylinderExtracted = [outerTestCylinderExtracted']

advancingCPointTest = TestCase $ assertEqual
  "advancingCpointFromHeadOfOuterPerims: with OuterPerimeters == Nothing"
  (Right (OuterAdvancerOutput {_outerPerimeterO = Nothing, _advancingCPointO = Nothing, _advancingCPointsO = []}))
  (let advancer = Advancer Nothing Nothing Nothing (Just $ CornerPointsError "just a filler") ([])
   in
   naiveAdvCPointFromOuterPerims advancer
  )

run_advancingCPointTest = do
  runTestTT advancingCPointTest

advancingCPointTestWithJustEmptyOPerims = TestCase $ assertEqual
  "advancingCpointFromHeadOfOuterPerims: with OuterPerimeters == Nothing"
  (Right (OuterAdvancerOutput {_outerPerimeterO = Nothing, _advancingCPointO = Nothing, _advancingCPointsO = []}))
  (let advancer = Advancer Nothing Nothing (Just []) (Just $ CornerPointsError "just a filler") ([])
   in
   naiveAdvCPointFromOuterPerims advancer
  )

run_advancingCPointTestWithJustEmptyOPerims = do
  runTestTT advancingCPointTestWithJustEmptyOPerims

advancingCPointTest2 = TestCase $ assertEqual
  "advancingCpointFromHeadOfOuterPerims: with good OuterPerimeter, AdvancingCPoint == Nothing"
  (Right (OuterAdvancerOutput {_outerPerimeterO = (Just [CornerPointsError "just a filler"]), _advancingCPointO = Nothing, _advancingCPointsO = []}))
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
           _advancingCPointsO = []
          }
         )
  )
  (let advancer = Advancer (Just (innerTestCylinderExtracted::InnerPerimeters)) (Just (innerTestCylinder::InnerPerimetersBeforeExtraction))
                            (Just ([CornerPointsError "just a filler"]::OuterPerimeter)) Nothing ([]::AdvancingCPoints)
   in
   naiveAdvCPointFromOuterPerims advancer
  )


run_all_advancingCPointTests = do
  runTestTT advancingCPointTest
  runTestTT advancingCPointTest2
  runTestTT advancingCPointTest3
  runTestTT advancingCPointTestWithJustEmptyOPerims
