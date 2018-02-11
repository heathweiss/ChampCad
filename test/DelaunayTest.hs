module DelaunayTest(delaunayTestDo) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@),
                                cornerPointsError, isCubePoints, isCubePointsList)
import CornerPoints.Points (Point(..), )
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
                                  extractFrontLeftLine, extractFrontRightLine, extractBackBottomLine, extractBackFace)

import Joiners.Advancer(advanceToHeadCPointDistanceNoIntersectionTest, advanceToHeadCPointDistanceNoIntersectionTestNM)
import Joiners.AdvanceToHeadOfPerimeters(orderInnerPerimsByDistanceFromHead, orderedInnerPerims, removeContainedCPointFromHeadOfPerims,
                                        advancingCpointFromHeadOfInnerPerims, advancingCpointFromHeadOfOuterPerims)
import qualified Joiners.AdvanceComposable as AC (Advancer(..), OuterAdvancerOutput(..), InnerAdvancerOutput(..),naiveAdvCpointFromInnerPerims, naiveAdvCPointFromOuterPerims, advancerRecur,
                                 advCPointFromClosestInnerOuterAdvCPoint, extractAdvCPointsFromAdvancer, advCPointFromClosestInnerOuterUsedCPoint,
                                 createAdvCPointFromInnerPerimsCheckLegalIntersection, outerAdvancerOutPutHasLegalIntersections, checkInnerAdvCPtForLegality)

import Joiners.AdvanceSupport(Perimeters(..), AdvancingCPoint(..), justifyPerimeters, appendAdvancingCpointToJoinedCpointsE)

import Math.Distance(Distance(..), Distant, calculateDistance, DistanceA(..), DistantA, calculateDistanceA
                    , center, (<-|->), centerA, (<-||->))

import Helpers.List(removeEmpty, safeHead )
import Helpers.Applicative(extractE)

import Primitives.Cylindrical.Walled(cylinder)

import Geometry.Angle(Angle(..))
import Geometry.Intercept(perimetersContainLegalIntersections)

delaunayTestDo = do

  putStrLn ""
  putStrLn "delaunayTestDo"
  
  runTestTT orderInnerPerimsByDistanceFromHeadTest
  runTestTT orderInnerPerimsByDistanceFromHeadTest1
  runTestTT orderInnerPerimsByDistanceFromHeadTest1_1
  runTestTT orderInnerPerimsByDistanceFromHeadTest2
  runTestTT orderInnerPerimsByDistanceFromHeadTest3
  runTestTT orderInnerPerimsByDistanceFromHeadTest4
  runTestTT orderInnerPerimsByDistanceFromHeadTest5
  runTestTT orderInnerPerimsByDistanceFromHeadTest6
  
  
  runTestTT orderedInnerPerimsTest1
  runTestTT advancingCpointTest1

  runTestTT orderedInnerPerimsTest1'
  
  runTestTT removeAdvancingCpointFromPerimsTest1
  runTestTT removeAdvancingCpointFromPerimsTest1'
  runTestTT removeAdvancingCpointFromPerimsTest2
  runTestTT removeAdvancingCpointFromPerimsTest3
  runTestTT removeAdvancingCpointFromPerimsTest4
  
  runTestTT advancingCpointTest2

  runTestTT advancingCpointFromHeadOfPerimsTest1
  runTestTT advancingCpointFromHeadOfPerimsTest2
  runTestTT advancingCpointFromHeadOfPerimsTest2b
  runTestTT advancingCpointFromHeadOfPerimsTest3
  runTestTT advancingCpointFromHeadOfPerimsTest4
  runTestTT advancingCpointFromHeadOfPerimsTest5

  runTestTT createCpointByDistanceTest1
  runTestTT createCpointByDistanceTest1a
  runTestTT createCpointByDistanceTest2

  runTestTT safeListTest
  --runTestTT safeListTest1

  runTestTT seeInitialAdvancingCpoint
  runTestTT see2ndAdvancingCpoint

  runTestTT outerPerimCrossesInnerPerimsTest
  runTestTT outerPerimCrossesInnerPerimsTest_checkLegalIntersections
--------------------------------------------------------- advanceComposable-------------------------------------------------------
--Fails:
  --Sees that it is illegal, but puts back in the wrong z_axis value for the advCPt.
outerPerimCrossesInnerPerimsTest = TestCase $ assertEqual
  "outerAdvancerOutPutHasLegalIntersections: outerPerim crosses innerPerim illegally"
  (Right $ AC.OuterAdvancerOutput (Just [F2 $ Point (-5) 2 10]) Nothing Nothing [])
    --got back the original outerAdv, but it should have been modified because of illegal intersection
  --(Left "got back the original outerAdv, but it should have been modified because of illegal intersection")
  (let
      advCPt = TopLeftLine {b2=Point 8 (-11) 10, f2=Point (-5) 2 10}
      innerPBE = [[BackTopLine {b2=Point (-8) 6 0, b3=Point (-8) 11 0}, BackTopLine {b2=Point (-5) 2 0, b3=Point (-8) 6 0}]]
      outerAdv = AC.OuterAdvancerOutput Nothing (Just advCPt) (Just (F2 $ Point (-5) 2 10)) [advCPt]
   in
   AC.outerAdvancerOutPutHasLegalIntersections outerAdv (Just innerPBE) 
  )

{-
OuterAdvancerOutput: outerPerimeter  == [F2 {f2 = Point {x_axis = -5.0, y_axis = 2.0, z_axis = 10.0}}] advCPoint: Nothing; useCPoint Nothing advCPoints length: 0
OuterAdvancerOutput: outerPerimeter  == [F2 {f2 = Point {x_axis = -5.0, y_axis = 2.0, z_axis = 0.0}}] advCPoint: Nothing; useCPoint Nothing advCPoints length: 0

-}

--Fails: manually cx for legalIntersections from previous failing tests:
--Use these values in InterceptTest module to see why it fails.
outerPerimCrossesInnerPerimsTest_checkLegalIntersections = TestCase $ assertEqual
  "perimetersContainLegalIntersections: outerPerim crosses innerPerim illegally"
  (Right False)
  (let
      advCPt = TopLeftLine {b2=Point 8 (-11) 10, f2=Point (-5) 2 10}
      innerPBE = [[BackTopLine {b2=Point (-8) 6 0, b3=Point (-8) 11 0}, BackTopLine {b2=Point (-5) 2 0, b3=Point (-8) 6 0}]]
   in
     perimetersContainLegalIntersections innerPBE advCPt
  )
-- ====================================================== delaunay viewer builder test =============================================
{-
Cx the shapes in delaunay viewr as they are failing somewhere in : bottomPointsBuilder with both inner and outer cpoint intersections were illegal
See InterceptTest for intercepts
-}

--create the inner/outer cylinders
innerCylinder = cylinder [Radius 10 | r <- [1..]] [Radius 10 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
 
innerCylinderPoints = (extractB4 $ head innerCylinder) : (map (extractB1) innerCylinder)
innerCylinderLines =
  map (extractBackBottomLine) innerCylinder
  
outerCylinder =  cylinder [Radius 20 | r <- [1..]] [Radius 20 | r <- [1..]] ([Angle a | a <- [0,10..360]]) (Point 0 0 0) 10
outerCylinderPoints = (extractF4 $ head outerCylinder) : (map (extractF1)  outerCylinder)



seeInitialAdvancingCpoint = TestCase $ assertEqual
  "the first advancing point, hand made"
  ([BottomRightLine
    {
      b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0},
      f4 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0}
    }
   ])
  (advanceToHeadCPointDistanceNoIntersectionTestNM
    [[head innerCylinderLines]] [[head innerCylinderPoints]]  [head outerCylinderPoints]
  )

see2ndAdvancingCpoint = TestCase $ assertEqual
  "the first advancing point, hand made"
  ([BottomRightLine
    {b4 = Point {x_axis = 0.0, y_axis = -30.0, z_axis = 0.0},
     f4 = Point {x_axis = 0.0, y_axis = -80.0, z_axis = 0.0}
    }
   ])
  (advanceToHeadCPointDistanceNoIntersectionTestNM
    [take 2 innerCylinderLines] [take 2 innerCylinderPoints]  $ take 2 outerCylinderPoints
  )

{-
[BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -30.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -80.0, z_axis = 0.0}},
BottomLeftLine {b1 = Point {x_axis = 2.614672282429745, y_axis = -29.885840942752367, z_axis = 0.0}, f1 = Point {x_axis = 0.0, y_axis = -80.0, z_axis = 0.0}}]
-}
  
-- ====================================================== end: delaunay viewer builder test =============================================
  
leftFace =
  LeftFace
    {b1 = Point 0 0 0,
     b2 = Point 0 0 1,
     f1 = Point 1 1 0,
     f2 = Point 1 1 1
    }

frontLeftLine =
  FrontLeftLine
      {f1 = Point 1 1 0,
       f2= Point 1 1 1
      }

rightFace =
  RightFace
      {b3 = Point 0 0 1,
       b4 = Point 0 0 0,
       f3 = Point 0 (-1) 1,
       f4 = Point 0 (-1) 0
      }



inner1a =
  [B4 $ Point 0 (-7) 0,
   B1 $ Point 2 (-5) 0
  ]

inner1b =
  [B4 $ Point 0 4 1,
   B1 $ Point 3 6 1
  ]
  
outer1 =
  [F4 $ Point 0 (-19) 0,
   F1 $ Point 6 (-17) 0
  ]

-- ==================================================== handle empty lists, or lists with [[]] [[],[]] and Nothing==========================
--does delaunay allow any form of emty lists
--this gets caught in AdvanceToHeadOfPerimeters advancingCpointFromHeadOfInnerPerims line 191
  --get rid of justify perims with some better method.
safeListTest = TestCase $ assertEqual
  "[] [] into delaunay"
  ([CornerPointsError {errMessage = "Joiners.DeluanayB(Left e): Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: had empty inner perimeters passed in"}])
  (advanceToHeadCPointDistanceNoIntersectionTest [] [])

--this one cause Prelude.head empty list error. Don't run it for now.
safeListTest1 = TestCase $ assertEqual
  "[inner1a] [] into delaunay"
  ([CornerPointsError {errMessage = "Joiners.DeluanayB(Left e): Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: had empty inner perimeters passed in"}])
  (advanceToHeadCPointDistanceNoIntersectionTest [inner1a] [])
  

-- ---------------------------------------- orderInnerPerimsByDistanceFromHead tests ========================================
orderInnerPerimsByDistanceFromHeadTest = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest"
   (Right $ Just $ InnerPerimeters  [[B1 $ Point 0 0 10]])
   (orderInnerPerimsByDistanceFromHead (Just $ InnerPerimeters [[B1 $ Point 0 0 10]]) (AdvancingCPoint (F1 (Point 0 0 0))) )

orderInnerPerimsByDistanceFromHeadTest1 = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest1"
   (Right $ Just $ InnerPerimeters  [[B1 $ Point 0 0 0], [B1 $ Point 0 0 10]])
   (orderInnerPerimsByDistanceFromHead (Just $ InnerPerimeters [[B1 $ Point 0 0 10], [B1 $ Point 0 0 0]]) (AdvancingCPoint (F1 (Point 0 0 0))) )
{-
but got: Right (Just (InnerPerimeters {_innerPerimeters =
[[B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}}]]}))
-}

orderInnerPerimsByDistanceFromHeadTest1_1 = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest1_1"
   (Right $ Just $ InnerPerimeters  [[B1 $ Point 0 0 0], [B1 $ Point 0 0 10]])
   (orderInnerPerimsByDistanceFromHead (Just $ InnerPerimeters [[B1 $ Point 0 0 0], [B1 $ Point 0 0 10]]) (AdvancingCPoint (F1 (Point 0 0 0))) )

orderInnerPerimsByDistanceFromHeadTest2 = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest2"
   (Right $ Just $ InnerPerimeters [[(B1 $ Point 0 0 0), (B1 $ Point 0 0 110)], [(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)]])
   (orderInnerPerimsByDistanceFromHead  (Just $ InnerPerimeters [[(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)], [(B1 $ Point 0 0 0), (B1 $ Point 0 0 110)]]) (AdvancingCPoint (F1 (Point 0 0 0))) )

orderInnerPerimsByDistanceFromHeadTest3 = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest3"
   (Right Nothing)
   (orderInnerPerimsByDistanceFromHead  (Just $ InnerPerimeters []) (AdvancingCPoint (F1 (Point 10 0 0))) )

orderInnerPerimsByDistanceFromHeadTest4 = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest4"
   (Right Nothing)
   (orderInnerPerimsByDistanceFromHead  (Just $ InnerPerimeters [[]]) (AdvancingCPoint (F1 (Point 10 0 0))) )

orderInnerPerimsByDistanceFromHeadTest5 = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest5"
   (Right (Just (InnerPerimeters {_innerPerimeters = [[B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}},B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 110.0}}]]})))
   (orderInnerPerimsByDistanceFromHead  (Just $ InnerPerimeters [[],[(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)]]) (AdvancingCPoint (F1 (Point 10 0 0))) )

orderInnerPerimsByDistanceFromHeadTest6 = TestCase $ assertEqual
  "orderInnerPerimsByDistanceFromHeadTest6"
   (Right (Just $ InnerPerimeters [inner1b, inner1a]))
   (orderInnerPerimsByDistanceFromHead  (Just $ InnerPerimeters [inner1a, inner1b]) (AdvancingCPoint (head inner1b)) )




------------------- delaunayB: from set points --------------------------------
orderedInnerPerimsTest1 = TestCase $ assertEqual
  "orderedInnerPerimsTest1"
   (Right $ Just $ InnerPerimeters [inner1a, inner1b])
   (orderInnerPerimsByDistanceFromHead  (Just $ InnerPerimeters [inner1a, inner1b]) (AdvancingCPoint (head outer1)) )


--build the initial advancingCpoint in delaunayB from:
  --head outer1Perims
  --head orderdInnerPerims
advancingCpointTest1 = TestCase $ assertEqual
  "advancingCpointTest1"
   (Right $ BottomRightLine {f4=(Point 0 (-19) 0), b4=(Point 0 (-7) 0)})
   (let
       orderedInnerPerims = orderInnerPerimsByDistanceFromHead (Just $ InnerPerimeters [inner1a, inner1b]) (AdvancingCPoint $ head outer1 )
       o = F4 $ Point 0 (-19) 0
    in
      ((\(Just (InnerPerimeters innerPerimeters)) -> o +++ (head $ head innerPerimeters))) <$> orderedInnerPerims
   )

-- ================================================orderedInnerPerims'========================================================
orderedInnerPerimsTest1' = TestCase $ assertEqual
  "orderedInnerPerimsTest1'"
  (Right (Just (InnerPerimeters [inner1b, inner1a])))
  (orderedInnerPerims (Just $ (InnerPerimeters [inner1a, inner1b]))  (AdvancingCPoint $ head inner1b))
  --was orderedInnerPerims'
{-
Right (Just (InnerPerimeters {_innerPerimeters = [[B4 {b4 = Point {x_axis = 0.0, y_axis = 4.0, z_axis = 1.0}},B1 {b1 = Point {x_axis = 3.0, y_axis = 6.0, z_axis = 1.0}}],[B4 {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}},B1 {b1 = Point {x_axis = 2.0, y_axis = -5.0, z_axis = 0.0}}]]}))

-}

-- ================================================removeAdvancingCpointFromPerims==================================================

--build the initial advancingCpoint in delaunayB from:
  --head outer1Perims
  --head orderdInnerPerims
--now remove the used cpoints from inner/outer lists
  --because this is initial call, there should be on removed from inner and outer perims
removeAdvancingCpointFromPerimsTest1 = TestCase $ assertEqual
  "can't remove advancingCpoint which does not exist in Perimeters"
  (Left "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims had following erorr: Joiners.Deluanay.removeContainedCPointFromHeadOfPerims did not contain advancingCpoint in inner/outer perimeters")

  
  (let
     advancingCpoint = AdvancingCPoint $ F1 $ Point 11 22 33 --a point that is not part of Perimeters
      
     orderedInnerPerims :: Either String (Maybe Perimeters)
     orderedInnerPerims = Right (Just $ (InnerPerimeters [inner1a, inner1b]))
        --distance from o, as that is the initial advancing line.
      
   in
      extractE (removeContainedCPointFromHeadOfPerims <$> orderedInnerPerims <*>  (Right (Just (OuterPerimeter outer1))) <*> Right advancingCpoint)
  )



--build the initial advancingCpoint in delaunayB from:
  --head outer1Perims
  --head orderdInnerPerims
--now remove the used cpoints from inner/outer lists
  --because this is initial call, there should be on removed from inner and outer perims
removeAdvancingCpointFromPerimsTest1' = TestCase $ assertEqual
  "can't remove advancingCpoint which does not exist in Perimeters"
  (Left "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims had following erorr: Joiners.Deluanay.removeContainedCPointFromHeadOfPerims did not contain advancingCpoint in inner/outer perimeters")
  ( removeContainedCPointFromHeadOfPerims (Just $ (InnerPerimeters [inner1a, inner1b]))  (Just (OuterPerimeter outer1)) (AdvancingCPoint $ F1 $ Point 11 22 33))


--do an orderInnerPerimsByDistanceFromHead on (Just $ InnerPerimeters [])
--now try to remove the advancingCpoint from outer list, where it does not exist.
removeAdvancingCpointFromPerimsTest2 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest2"
  (Left "Joiners.Delaunay.removeContainedCPointFromHeadOfPerims had following erorr: Joiners.Delaunay.removeContainedCPointFromHeadOfPerims: InnerPerimter = Nothing, OuterPermeter = F1 {f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}} :os did not remove advancingCpoint: F4 {f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}} because it was not contained in o ")
  (let
      orderedInnerPerims = orderInnerPerimsByDistanceFromHead (Just $ InnerPerimeters []) advancingCpoint
      advancingCpoint = AdvancingCPoint $ head outer1
   in
      extractE (removeContainedCPointFromHeadOfPerims  <$> orderedInnerPerims <*> Right(Just(OuterPerimeter $ tail outer1))  <*> Right advancingCpoint)
  )

--give an advancingCpoint that is in outerPerimemeter, while inner perims is []
--now remove the used cpoints from outerPerimeter
removeAdvancingCpointFromPerimsTest3 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest3"
  (Right (Nothing,Just (OuterPerimeter {_outerPerimeter = [F1 {f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}}]})))
  (let
      advancingCpoint = AdvancingCPoint $ head outer1
        --1st advancing line so we know it is head outer perimeters
      orderedInnerPerims = orderInnerPerimsByDistanceFromHead (Just $ InnerPerimeters []) advancingCpoint
      
   in
     --because it is used applicatively, it will return a Right Right ...
     extractE (removeContainedCPointFromHeadOfPerims  <$> orderedInnerPerims <*> Right (Just $ OuterPerimeter outer1) <*> Right advancingCpoint) 
  )


--give an advancingCpoint that should be in inner perims, while outer perims is []
--now remove the used cpoints from outer list
removeAdvancingCpointFromPerimsTest4 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest4"
  (Right
   (
    Just $ InnerPerimeters [tail inner1a, inner1b],
    Nothing
   )
  )
  (let
      o = AdvancingCPoint $ head outer1
        --1st advancing line so we know it is head outer perimeters
      orderedInnerPerims = orderInnerPerimsByDistanceFromHead (Just $ InnerPerimeters [inner1a, inner1b]) o
      --B4 {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}}
      advancingCpoint = AdvancingCPoint $ BottomRightLine {f4=Point 0 (-19) 0, b4=Point 0 (-7) 0} 
        --should contain o  
      
   in
     extractE (removeContainedCPointFromHeadOfPerims <$> orderedInnerPerims <*> Right Nothing <*> Right advancingCpoint)
  )

outer2 = outer1 ++ [F1 $ Point 10 (-11) 0]

--advanceToHeadCPointDistanceNoIntersectionTest:
--Uses shortest distance from advancingCpoint to head inner1<a/b>
--Uses head outer2
advancingCpointTest2 = TestCase $ assertEqual
  "advancingCpointTest2"
  --([CornerPointsError "just a filler"])
  (
   [BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}, f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 2.0, y_axis = -5.0, z_axis = 0.0}, f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 2.0, y_axis = -5.0, z_axis = 0.0}, f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 0.0, y_axis = 4.0, z_axis = 1.0}, f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}}]
  )
  (advanceToHeadCPointDistanceNoIntersectionTest [inner1a, inner1b] outer2  )
-- ========================================================advancingCpointFromHeadOfInnerPerims===============================================
advancingCpointFromHeadOfPerimsTest1 = TestCase $ assertEqual
  "got the head of first innerPerimeter"
  (Right $ AdvancingCPoint $ BottomRightLine {b4=(Point 0 (-7) 0), f4=(Point 0 (-19) 0)})
  (advancingCpointFromHeadOfInnerPerims (Just $ InnerPerimeters [inner1a, inner1b]) (AdvancingCPoint $ head outer1) )

advancingCpointFromHeadOfPerimsTest2 = TestCase $ assertEqual
  "get the head of second inner perimeter"
  (Right (AdvancingCPoint {_advancingCpoint = BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = 4.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 4.0, z_axis = 1.0}}}))
  (advancingCpointFromHeadOfInnerPerims (Just $ InnerPerimeters [inner1a, inner1b]) (AdvancingCPoint $ F4 $ Point 1 4 1) )

advancingCpointFromHeadOfPerimsTest2b = TestCase $ assertEqual
  "get the head of 4th inner perimeter, when 1st/3rd is []"
  (Right (AdvancingCPoint {_advancingCpoint = BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = 4.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 4.0, z_axis = 1.0}}}))
  (advancingCpointFromHeadOfInnerPerims (Just $ InnerPerimeters [[],inner1a, [], inner1b]) (AdvancingCPoint $ F4 $ Point 1 4 1) )

advancingCpointFromHeadOfPerimsTest3 = TestCase $ assertEqual
  "pass in inner perims: Nothing"
  (Left "Joiners.Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: has Nothing passed in for inner perimeters")
  (advancingCpointFromHeadOfInnerPerims Nothing (AdvancingCPoint $ F4 $ Point 1 4 1) )

advancingCpointFromHeadOfPerimsTest4 = TestCase $ assertEqual
  "pass in inner perims: InnerPerimeters []"
  (Left "Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: had empty inner perimeters passed in")
  (advancingCpointFromHeadOfInnerPerims (Just $ InnerPerimeters []) (AdvancingCPoint $ F4 $ Point 1 4 1) )

advancingCpointFromHeadOfPerimsTest5 = TestCase $ assertEqual
  "pass in inner perims: InnerPerimeters [[]]"
  (Left "Joiners.Delaunay.advancingCpointFromHeadOfInnerPerims: had empty inner perimeters passed in")
  (advancingCpointFromHeadOfInnerPerims (Just $ InnerPerimeters [[]]) (AdvancingCPoint $ F4 $ Point 1 4 1) )

--------------------------------------------- create advancing cpoint by comparing distances ---------------------------
inner3a =
  [B1 $ Point 2 (-5) 0,
   B1 $ Point 1 (-2) 0
  ]

inner3b =
  [B1 $ Point 3 6 1,
   B1 $ Point 1 9 1
  ]
  
outer3 =
  [F1 $ Point 6 (-17) 0,
   F1 $ Point 10 (-11) 0
  ]

createCpointByDistanceTest1 = TestCase $ assertEqual
  "createCpointByDistanceTest1"
  (Right (Just (InnerPerimeters {_innerPerimeters = [[B1 {b1 = Point {x_axis = 1.0, y_axis = -2.0, z_axis = 0.0}}],[B1 {b1 = Point {x_axis = 3.0, y_axis = 6.0, z_axis = 1.0}},B1 {b1 = Point {x_axis = 1.0, y_axis = 9.0, z_axis = 1.0}}]]}),Just (OuterPerimeter {_outerPerimeter = [F1 {f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}},F1 {f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}}]})))
  (let
      getAdvancingCpoint (AdvancingCPoint advancingCpoint') = advancingCpoint'

      
      advancingCpoint = AdvancingCPoint $ BottomRightLine {b4=Point 0 (-7) 0, f4=Point 0 (-19) 0}
      innerPerimeters = InnerPerimeters [inner3a, inner3b]
      outerPerimeter = OuterPerimeter outer3
      
      advancingInnerCpointE  =
        advancingCpointFromHeadOfInnerPerims (Just $ innerPerimeters) (advancingCpoint)
      advancingInnerCpointEAsCpoint =
                          case advancingInnerCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
      distanceToInnerE :: Either String DistanceA
      distanceToInnerE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpoint) <$> advancingInnerCpointEAsCpoint

      advancingOuterCpointE =
        advancingCpointFromHeadOfOuterPerims (Just outerPerimeter) (advancingCpoint)
      advancingOuterCpointEAsCpoint =
                          case advancingOuterCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
      distanceToOuterE :: Either String DistanceA
      distanceToOuterE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpoint) <$> advancingOuterCpointEAsCpoint
      isOuterDistanceLTInnerDistance' :: DistanceA -> DistanceA -> Bool
      isOuterDistanceLTInnerDistance'  (DistanceA innerDistance) (DistanceA outerDistance ) =
        outerDistance < innerDistance

      isOuterDistanceLTInnerDistance :: Either String Bool
      isOuterDistanceLTInnerDistance = isOuterDistanceLTInnerDistance' <$> distanceToInnerE <*> distanceToOuterE
                        
   in
     case isOuterDistanceLTInnerDistance of
            Left e -> Left $ "err1 " ++ e
            --outer distance <
            Right True ->
              
                   extractE
                     (removeContainedCPointFromHeadOfPerims 
                      (Just innerPerimeters) 
                      (Just outerPerimeter) <$>
                      advancingOuterCpointE
                     )
                   
                        --outer distance >=
            Right False ->
              
                               extractE
                                (removeContainedCPointFromHeadOfPerims 
                                 (Just innerPerimeters) 
                                 (Just outerPerimeter) <$>
                                 advancingInnerCpointE
                                )
  )



createCpointByDistanceTest1a = TestCase $ assertEqual
  "look at various values"
  (
   [BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}, f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 2.0, y_axis = -5.0, z_axis = 0.0}, f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 2.0, y_axis = -5.0, z_axis = 0.0}, f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}},
    BottomLeftLine {b1 = Point {x_axis = 1.0, y_axis = -2.0, z_axis = 0.0}, f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}}
   ]
  )
  (let 
    inner4a =
     [B4 $ Point 0 (-7) 0,
      B1 $ Point 2 (-5) 0,
      B1 $ Point 1 (-2) 0
     ]
    
    inner4b =
     [B4 $ Point 0 4 1,
      B1 $ Point 3 6 1,
      B1 $ Point 1 9 1
     ]
    
    outer4 =
     [F4 $ Point 0 (-19) 0,
      F1 $ Point 6 (-17) 0,
      F1 $ Point 10 (-11) 0
     ]

   in
    advanceToHeadCPointDistanceNoIntersectionTest [inner4a, inner4b] outer4
  )



createCpointByDistanceTest2 = TestCase $ assertEqual
  "look at various values"
  (Right False)
  (let
      getAdvancingCpoint (AdvancingCPoint advancingCpoint') = advancingCpoint'

      
      advancingCpoint = AdvancingCPoint $ BottomRightLine {b4=Point 0 (-7) 0, f4=Point 0 (-19) 0}
      innerPerimeters = InnerPerimeters [inner3a, inner3b]
      outerPerimeter = OuterPerimeter outer3
      
      advancingInnerCpointE  =
        advancingCpointFromHeadOfInnerPerims (Just $ innerPerimeters) (advancingCpoint)
      advancingInnerCpointEAsCpoint =
                          case advancingInnerCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
      distanceToInnerE :: Either String DistanceA
      distanceToInnerE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpoint) <$> advancingInnerCpointEAsCpoint

      advancingOuterCpointE =
        advancingCpointFromHeadOfOuterPerims (Just outerPerimeter) (advancingCpoint)
      advancingOuterCpointEAsCpoint =
                          case advancingOuterCpointE of
                            Left e -> Left e
                            Right (AdvancingCPoint cpoint) -> Right cpoint
      distanceToOuterE :: Either String DistanceA
      distanceToOuterE = extractE $ calculateDistanceA (getAdvancingCpoint advancingCpoint) <$> advancingOuterCpointEAsCpoint
      isOuterDistanceLTInnerDistance' :: DistanceA -> DistanceA -> Bool
      isOuterDistanceLTInnerDistance'  (DistanceA innerDistance) (DistanceA outerDistance ) =
        outerDistance < innerDistance

      isOuterDistanceLTInnerDistance :: Either String Bool
      isOuterDistanceLTInnerDistance = isOuterDistanceLTInnerDistance' <$> distanceToInnerE <*> distanceToOuterE
                        
   in
     isOuterDistanceLTInnerDistance
     
  )
