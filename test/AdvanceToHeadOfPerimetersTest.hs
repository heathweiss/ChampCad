module AdvanceToHeadOfPerimetersTest(advanceToHeadOfPerimetersTestDo) where

import Test.HUnit

import Joiners.AdvanceToHeadOfPerimeters(orderInnerPerimsByDistanceFromHead, orderedInnerPerims,
                                         removeContainedCPointFromHeadOfPerims, removeContainedCPointFromHeadOfPerimsNM,
                                         advancingCpointFromHeadOfInnerPerims, advancingCpointFromHeadOfInnerPerimsNM,
                                         advancingCpointFromHeadOfOuterPerims, advancingCpointFromHeadOfOuterPerimsNM,
                                         advancingCpointFromDoublePerimsUsingDistanceToHeadOfPerimsCpointsNM)

import Joiners.AdvanceSupport(Perimeters(..), AdvancingCPoint(..), justifyPerimeters, appendAdvancingCpointToJoinedCpointsE)

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
                                  extractFrontLeftLine, extractFrontRightLine, extractBackBottomLine, extractBackFace)

import CornerPoints.Points (Point(..))
import CornerPoints.Radius(Radius(..))

import Geometry.Angle(Angle(..))

import Primitives.Cylindrical.Walled(cylinder)

advanceToHeadOfPerimetersTestDo = do
  putStrLn "\nAdvanceToHeadOfPerimetersTest"
  runTestTT seeInnerCylinderTest
  runTestTT seeOuterCylinderTest
  runTestTT removeContainedCPointFromHeadOfPerimsNMTest
  runTestTT removeContainedCPointFromHeadOfPerimsNMTest2
  runTestTT removeContainedCPointFromHeadOfPerimsNMTest3
  runTestTT removeContainedCPointFromHeadOfPerimsNMTest4
  runTestTT removeContainedCPointFromHeadOfPerimsNMTest5
  runTestTT removeContainedCPointFromHeadOfPerimsNMTest6
  runTestTT removeContainedCPointFromHeadOfPerimsNMTest7


removeContainedCPointFromHeadOfPerimsNMTest = TestCase $ assertEqual
  "removeContainedCPointFromHeadOfPerimsNM: remove advancingCpoint from outerPerimeters(length == 1) and Nothing inner perims"
   (Right (Nothing,Nothing))
   (let
       outer = Just $ OuterPerimeter [F1 $ Point 0 0 0]
       inner = Nothing --Just $ InnerPerimeters [[F1 $ Point 0 0 0, F1 $ Point 1 0 0]]
    in
    removeContainedCPointFromHeadOfPerimsNM  inner outer $ AdvancingCPoint $ F1 $ Point 0 0 0 
   )


removeContainedCPointFromHeadOfPerimsNMTest2 = TestCase $ assertEqual
  "removeContainedCPointFromHeadOfPerimsNM: remove advancingCpoint from outerPerimeters(length > 1) and Nothing inner perims"
   (Right (Nothing,Just (OuterPerimeter {_outerPerimeter = [F1 {f1 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}]})))
   (let
       
       outer = Just $ OuterPerimeter [F1 $ Point 0 0 0, F1 $ Point 1 0 0]
       inner = Nothing --Just $ InnerPerimeters [[F1 $ Point 0 0 0, F1 $ Point 1 0 0]]
    in
    removeContainedCPointFromHeadOfPerimsNM  inner outer $ AdvancingCPoint $ F1 $ Point 0 0 0 
   )
removeContainedCPointFromHeadOfPerimsNMTest3 = TestCase $ assertEqual
  "removeContainedCPointFromHeadOfPerimsNM: remove advancingCpoint from Nothing InnerPerimeters[[(length == 1)]]"
   (Right (Nothing,Nothing))
   (let
       outer = Nothing 
       inner = Just $ InnerPerimeters [[F1 $ Point 0 0 0]]
    in  
    removeContainedCPointFromHeadOfPerimsNM  inner outer $ AdvancingCPoint $ F1 $ Point 0 0 0 
   )

removeContainedCPointFromHeadOfPerimsNMTest4 = TestCase $ assertEqual
  "removeContainedCPointFromHeadOfPerimsNM: remove advancingCpoint from Nothing InnerPerimeters[[(length == 2)]]"
   (Right (Just (InnerPerimeters {_innerPerimeters = [[F1 {f1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}}]]}),Nothing))
   (let
       outer = Nothing 
       inner = Just $ InnerPerimeters [[F1 $ Point 0 0 0, F1 $ Point 0 0 1]]
    in  
    removeContainedCPointFromHeadOfPerimsNM  inner outer $ AdvancingCPoint $ F1 $ Point 0 0 0 
   )


removeContainedCPointFromHeadOfPerimsNMTest5 = TestCase $ assertEqual
  "removeContainedCPointFromHeadOfPerimsNM: remove advancingCpoint from OuterPerimeters where OuterPerimeters[length = 1] InnerPerimeters[[(length == 2)]]"
   (Right (Just (InnerPerimeters {_innerPerimeters = [[F1 {f1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 2.0}},F1 {f1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 3.0}}]]}),Just (OuterPerimeter {_outerPerimeter = [F1 {f1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}}]})))
   (let
       outer = Just $ OuterPerimeter [F1 $ Point 0 0 0, F1 $ Point 0 0 1]
       inner = Just $ InnerPerimeters [[F1 $ Point 0 0 2, F1 $ Point 0 0 3]]
    in  
    removeContainedCPointFromHeadOfPerimsNM  inner outer $ AdvancingCPoint $ F1 $ Point 0 0 0 
   )
innerCylinderRaw = cylinder [Radius 30 | r <- [1..]] [Radius 150 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
innerCylinderExtracted = (extractB4 $ head innerCylinderRaw) : (map (extractB1)  innerCylinderRaw)
outerCylinderRaw = cylinder [Radius 60 | r <- [1..]] [Radius 80 | r <- [1..]] [Angle a | a <- [0,10..360]] (Point 0 0 0) 10
outerCylinderExtracted = (extractF4 $ head innerCylinderRaw) : (map (extractF1) innerCylinderRaw)

seeInnerCylinderTest  = TestCase $ assertEqual
  "see inner cylinder"
  ([])
  (take 2 innerCylinderExtracted)

seeOuterCylinderTest  = TestCase $ assertEqual
  "see outer cylinder"
  ([])
  (take 2 $ outerCylinderExtracted)

removeContainedCPointFromHeadOfPerimsNMTest6 = TestCase $ assertEqual
  "removeContainedCPointFromHeadOfPerimsNM: remove advancingCpoint from OuterPerimeters where OuterPerimeters[length = 2] InnerPerimeters[[(length == 2)]]"
   (Right
    (Just
     (InnerPerimeters
      {_innerPerimeters =
       [
         [B4 {b4 = Point {x_axis = 0.0, y_axis = -30.0, z_axis = 0.0}},
          B1 {b1 = Point {x_axis = 2.614672282429745, y_axis = -29.885840942752367, z_axis = 0.0}}]]}),
      Just
      (OuterPerimeter
       {_outerPerimeter =
        [F1 {f1 = Point {x_axis = 13.073361412148724, y_axis = -149.42920471376183, z_axis = 0.0}}]})))
   (let
       inner = Just $ InnerPerimeters $ [take 2 innerCylinderExtracted]
       outer = Just $ OuterPerimeter $ take 2 outerCylinderExtracted
    in  
    removeContainedCPointFromHeadOfPerimsNM  inner outer $ AdvancingCPoint $ BottomRightLine {f4 = Point {x_axis = 0.0, y_axis = -150.0, z_axis = 0.0},
                                                                                             b4 = Point {x_axis = 10.0, y_axis = -1150.0, z_axis = 10.0}}
   )

removeContainedCPointFromHeadOfPerimsNMTest7 = TestCase $ assertEqual
  "removeContainedCPointFromHeadOfPerimsNM: remove advancingCpoint from OuterPerimeters where OuterPerimeters[length = 2] InnerPerimeters[[(length == 2)]]"
   ( Left "filler")
   (let
       inner = InnerPerimeters [take 2 innerCylinderExtracted]
       outer = take 2 outerCylinderExtracted

       deluanayNMAppended = do
         advancingCpoint <- advancingCpointFromHeadOfInnerPerimsNM (inner) (AdvancingCPoint $ head outer)
         perimsWithAdvancingCpointBldrRemoved <-
          (removeContainedCPointFromHeadOfPerimsNM 
            (Just $ ( inner))  (Just $ OuterPerimeter outer) advancingCpoint)
         
         appended <- Right $ appendAdvancingCpointToJoinedCpointsE advancingCpoint  []
         return appended
       
       deluanayNMAdvancingCpoint = do
         advancingCpoint <- advancingCpointFromHeadOfInnerPerimsNM (inner) (AdvancingCPoint $ head outer)
         perimsWithAdvancingCpointBldrRemoved <-
          (removeContainedCPointFromHeadOfPerimsNM 
            (Just $ ( inner))  (Just $ OuterPerimeter outer) advancingCpoint)
         
         appended <- Right $ appendAdvancingCpointToJoinedCpointsE advancingCpoint  []
         return advancingCpoint
       
       deluanayNMPerimsRemoved = do
         advancingCpoint <- advancingCpointFromHeadOfInnerPerimsNM (inner) (AdvancingCPoint $ head outer)
         perimsWithAdvancingCpointBldrRemoved <-
          (removeContainedCPointFromHeadOfPerimsNM 
            (Just $ ( inner))  (Just $ OuterPerimeter outer) advancingCpoint)
         
         appended <- Right $ appendAdvancingCpointToJoinedCpointsE advancingCpoint  []
         return perimsWithAdvancingCpointBldrRemoved
         
    in  
    deluanayNMAppended
    
   )
