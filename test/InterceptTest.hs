module InterceptTest(interceptTestDo) where

import Test.HUnit
--
import Geometry.Intercept(getChangeInX, getChangeInY, yIntercept, topCoderAreTheParallel, rayIntersection, lineIntersection,
                          onTheLine, legalIntersection, segmentIntersection,
                          perimetersContainIllegalIntersection, perimetersContainLegalIntersections,
                          segmentIntersectionT, runSegmentIntersectionT, closestPointOnLineParamGloss)
import Geometry.Angle(Angle(..))

import CornerPoints.CornerPoints(CornerPoints(..), cpointType, (+++))
import CornerPoints.Points (Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
                                  extractFrontLeftLine, extractFrontRightLine, extractBackBottomLine, extractBackFace)

import Math.Distance(DistanceA(..))

import Primitives.Cylindrical.Walled(cylinder)

interceptTestDo = do

  putStrLn "\n\n\n\n"
  putStrLn "intercept TestDo"

  {- run these to have a look at working values
  runTestTT seeInitialAdvancingCpoint
  runTestTT seeInnerCylinderPoints
  runTestTT seeOuterCylinderPoints
  runTestTT seeInnerCylinderBackLines
  runTestTT seeInitialAdvancingCpoint-}
  
  runTestTT checkChangeInXYTest1
  runTestTT checkChangeInXYTest2
  runTestTT checkChangeInXYTest3
  runTestTT checkChangeInXYTest4

  --runTestTT getYInterceptTest
  --delete if get rid of yIntercept

  runTestTT topCoderAreParallelTest
  runTestTT topCoderAreParallelTest2

  runTestTT rosettaInterceptTest2
  runTestTT segmentInterceptTest2
  runTestTT rosettaInterceptTest5
  runTestTT segmentInterceptTest5
  runTestTT segmentInterceptTest6

  runTestTT legalInterceptTest
  runTestTT legalInterceptTest2
  runTestTT legalInterceptTest3
  runTestTT legalInterceptTest4
  runTestTT legalInterceptTest4swapped
  runTestTT legalInterceptTest5
  runTestTT legalInterceptTest6
  runTestTT legalInterceptTest7
  runTestTT legalInterceptTest8
  runTestTT legalInterceptTest9
  runTestTT legalInterceptTest10
  runTestTT legalInterceptTest11
  runTestTT legalInterceptTest12

  runTestTT onTheLineTest
  runTestTT onTheLineTest2
  runTestTT onTheLineTest3
  runTestTT onTheLineTest4

  runTestTT perimetersContainIllegalIntersectionTest
  runTestTT perimetersContainIllegalIntersectionTest2
  runTestTT perimetersContainIllegalIntersectionTest3
  runTestTT perimetersContainIllegalIntersectionTest4
  runTestTT perimetersContainIllegalIntersectionTest5
  runTestTT perimetersContainIllegalIntersectionTest6
  runTestTT perimetersContainIllegalIntersectionTest7
  runTestTT perimetersContainIllegalIntersectionTest8
  runTestTT perimetersContainIllegalIntersectionTest9
  runTestTT perimetersContainIllegalIntersectionTest10
  
  runTestTT perimetersContainLegalIntersectionTest
  runTestTT perimetersContainLegalIntersectionTesta
  runTestTT perimetersContainLegalIntersectionTest2

  runTestTT lineIntersectionTest
  runTestTT lineIntersectionTest2
  runTestTT lineIntersectionTest3

  runTestTT segmentInterceptTest3
  runTestTT lineIntersectionPerpendicularTest
  runTestTT closestPointOnLineParamGlossTest
  runTestTT segmentInterceptTest3AlmostPerpendicular

  runTestTT legalInnerPerimTest1
  runTestTT legalInnerPerimTest2

-- ====================================================== delaunay viewer builder test =============================================
{-
Cx the shapes in delaunay viewr as they are failing somewhere in : bottomPointsBuilder with both inner and outer cpoint intersections were illegal
See delaunayTests for other tests
-}

--create the inner/outer cylinders
innerCylinder = cylinder [Radius 10 | r <- [1..]] [Radius 10 | r <- [1..]] ([Angle a | a <- [0,5..360]] ) (Point 0 0 0) 10
innerCylinderPoints = (extractB4 $ head innerCylinder) : (map (extractB1) innerCylinder)

innerCylinderLines =
  map (extractBackBottomLine) innerCylinder
  
outerCylinder =  cylinder [Radius 20 | r <- [1..]] [Radius 20 | r <- [1..]] ([Angle a | a <- [0,10..360]]) (Point 0 0 0) 10
outerCylinderPoints = (extractF4 $ head outerCylinder) : (map (extractF1)  outerCylinder)

seeInnerCylinderPoints = TestCase $ assertEqual
  "look at the inner cylinder points to plot them"
  ([CornerPointsError "filler"])
  (innerCylinderPoints)

seeOuterCylinderPoints = TestCase $ assertEqual
  "look at the outer cylinder points to plot them"
  ([CornerPointsError "filler"])
  (outerCylinderPoints)

seeInnerCylinderBackLines = TestCase $ assertEqual
  "look at the inner cylinder back lines to plot them"
  ([CornerPointsError "filler"])
  (innerCylinderLines)


seeInitialAdvancingCpoint = TestCase $ assertEqual
  "the first advancing point, hand made"
  (BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0}})
  ((head outerCylinderPoints) +++ (head innerCylinderPoints)
  )
-- ====================================================== end: delaunay viewer builder test =============================================

checkChangeInXYTest1 = TestCase $ assertEqual
  "x in pos direction"
   (Right (DistanceA {distance = 1.0}))
   (getChangeInX
     (BottomLeftLine {b1=(Point 0 0 0), f1=(Point 1 11 111)})
   )

checkChangeInXYTest2 = TestCase $ assertEqual
  "x in neg direction"
   (Right (DistanceA {distance = (-1.0)}))
   (getChangeInX
     (BottomLeftLine {f1=(Point 0 0 0), b1=(Point 1 11 111)})
   )


checkChangeInXYTest3 = TestCase $ assertEqual
  "y in pos direction"
   (Right (DistanceA {distance = 11.0}))
   (getChangeInY
     (BottomLeftLine {b1=(Point 0 0 0), f1=(Point 1 11 111)})
   )

checkChangeInXYTest4 = TestCase $ assertEqual
  "y in neg direction"
   (Right (DistanceA {distance = (-11.0)}))
   (getChangeInY
     (BottomLeftLine {f1=(Point 0 0 0), b1=(Point 1 11 111)})
   )
{- can delete if get rid of yIntercept
getYInterceptTest = TestCase $ assertEqual
  "y intercept test"
  (Left "filler")
  (let
      --bll = BottomLeftLine {b1=Point 3 (-3) 0, f1=Point 12 12 0}
      bll = BottomLeftLine {b1=Point 2 (-5) 0, f1=Point 13 15 0}
      --bbl = BackBottomLine {b1=Point 2 (-5) 0, b4=Point 13 15 0}
      bbl = BackBottomLine {b1=Point 3 (-3) 0, b4=Point 12 12 0}
   in yIntercept bll bbl
  )
-}
----------------------------------------------------------- top coder ----------------------------------------------------
topCoderAreParallelTest = TestCase $ assertEqual
  "bll and bbl are the same values starting from b1"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point 2 (-5) 0, f1=Point 13 15 0}
     bbl = BackBottomLine {b1=Point 2 (-5) 0, b4=Point 13 15 0}
   in
     topCoderAreTheParallel bll bbl
  )


topCoderAreParallelTest2 = TestCase $ assertEqual
  "bll and bbl parallel in opposite direcetion but parallel"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point 2 (-5) 0, f1=Point 13 15 0}
     bbl = BackBottomLine {b1=Point 13 15 0 , b4=Point 2 (-5) 0}
   in
     topCoderAreTheParallel bll bbl
  )


-- =================================================================================================
-- https://rosettacode.org/wiki/Find_the_intersection_of_two_lines#Haskell



rosettaInterceptTest2 = TestCase $ assertEqual
  "bll and bbl won't intercept at 7 7 0 as bll is too short"
  (Right Nothing)
  --(SegmentIntersectionType(Left "jkfdls"))
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 3 3 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     --segmentIntersection bll bbl
     runSegmentIntersectionT $ segmentIntersectionT bll bbl
  )

segmentInterceptTest2 = TestCase $ assertEqual
  "segmentIntersection: bll and bbl won't intercept at 7 7 0 as bll is too short"
  (Right Nothing)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 3 3 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     --segmentIntersection bll bbl
     segmentIntersection bll bbl
  )



--passed when using segmentIntersection 
rosettaInterceptTest5 = TestCase $ assertEqual
  "segmentIntersectionT: bll and bbl should intercept at 7 7 0 as it is just right"
  --(Right True)
  (Right $Just $Point {x_axis = 7, y_axis = 7, z_axis = 0})
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     --segmentIntersection bll bbl
     runSegmentIntersectionT $ segmentIntersectionT bll bbl
  )

segmentInterceptTest5 = TestCase $ assertEqual
  "segmentIntersection: bll and bbl should intercept at 7 7 0 as it is just right"
  --(Right True)
  (Right $Just $Point {x_axis = 7, y_axis = 7, z_axis = 0})
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     segmentIntersection bll bbl
     
  )


segmentInterceptTest6 = TestCase $ assertEqual
  "segmentIntersection: bll and bbl won't intercept at 7 7 0 because bbl is too short"
  (Right Nothing)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 5 4 0}
      
   in
     segmentIntersection bll bbl
     
  )

{--------------------------------------------------------------innerperims intercept-----------------------------------------
Have a segment intercept a square on a vertice. The segment is not long enough to intercept on the far side of the square, so it is legal.
This corresponds to an advCPnt cxing for legality from Joiners.AdvanceComposbable

                       --------------- btlNotIntersected
                       |             |
                       |             |
                       ------.-------- btlIntersected<Left/Right> 
                             |  tll
                             |  . is a legal intersection.
                             |    Does not extend up to and intersect top line
                               
-}
--has missing pattern match. Leave that way till see if later fx's show that, when cx'g for perimetersContainLegalIntersections
legalInnerPerimTest1 = TestCase $ assertEqual
  "segmentIntersection: tll and btlIntersectedLeft will intercept at  0 0 0 "
  (Right Nothing)
  (let
     tll = TopLeftLine {b2=Point (-5) 0 0, f2=Point 12 12 0}
     btlIntersectedLeft = BackTopLine {b2=Point 0 (-5) 0, b3=Point 0 0 0}
      
   in
     segmentIntersection  btlIntersectedLeft tll
     
  )

--should be legal, but gives false. Does not show the missing pattern match from test1
legalInnerPerimTest2 = TestCase $ assertEqual
  "segmentIntersection: tll and btlIntersectedLeft will intercept at  0 0 0 so it will be legal intersections"
  (Right True)
  (let
     tll = TopLeftLine {b2=Point (-5) 0 0, f2=Point 12 12 0}
     btlIntersectedLeft = BackTopLine {b2=Point 0 (-5) 0, b3=Point 0 0 0}
      
   in
     perimetersContainLegalIntersections  [[btlIntersectedLeft]] tll
     
  )

-- ========================================= legal intercept ==========================================
legalInterceptTest = TestCase $ assertEqual
  "should be legal bll and bbl as they intersect in the non-vertice perimeter segment"
  (Right False)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest2 = TestCase $ assertEqual
  "is legal as bll and bbl won't intercept at bll is too short"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 3 3 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest3 = TestCase $ assertEqual
  "is legal as bll and bbl won't intercept becuase bll is too short"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 3 3 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest4 = TestCase $ assertEqual
  "legal as bll and bbl should intercept on a vertice"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 7 7 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest4swapped = TestCase $ assertEqual
  "legal as bll and bbl should intercept on a vertice"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}
     bbl = BackBottomLine {b1=Point 7 7 0 , b4=Point (-2) (-5) 0}
      
   in
     legalIntersection bll bbl
  )


legalInterceptTest5 = TestCase $ assertEqual
  "not legal as bll and bbl should intercept at but not on a vertice"
  (Right False)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest6 = TestCase $ assertEqual
  "is legal bll and bbl won't intercept because bll is too short"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 5 4 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest7 = TestCase $ assertEqual
  "should intercept and be legal"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (1) (-10) 0, f1=Point 0 (-20) 0}
     bbl = BackBottomLine {b1=Point (1) (-10) 0, b4=Point 0 (-10) 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest8 = TestCase $ assertEqual
  "should intercept and be legal"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (1) (-10) 0, f1=Point 3.5 (-20) 0}
     bbl = BackBottomLine {b1=Point (1) (-10) 0, b4=Point 0 (-10) 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest9 = TestCase $ assertEqual
  "should intercept and be legal"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (0) (-10) 0, f1=Point 3.5 (-20) 0}
     bbl = BackBottomLine {b1=Point (1) (-10) 0, b4=Point 0 (-10) 0}
      
   in
     legalIntersection bll bbl
  )

legalInterceptTest10 = TestCase $ assertEqual
  "should intercept and be legal"
  (Right True)
  (let
     bll = BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0}}
     bbl = BackBottomLine
            {b1 = Point {x_axis = 0.8715574274765816, y_axis = -9.961946980917455, z_axis = 0.0},
             b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}}
      
   in
     legalIntersection bll bbl
  )

--segmentInterceptTest3 shows intersection at Nothing
legalInterceptTest11 = TestCase $ assertEqual
  "legalIntersection: they don't intersect"
  (Right True)
  (let
     bll = (BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0})
     bbl = BackBottomLine {b1=Point 5 9 0 , b4=Point 1 9 0}
      
   in
     legalIntersection bll bbl
  )

-- ========================================================================================================================================
--see why german hiker does not use the innerP's beyond the initial advCPt
legalInterceptTest12 = TestCase $ assertEqual
  "legalInterceptTest12: should have a legal interception"
  (Right True)
  (let
      tll = (TopRightLine (Point 1.7 (-1.9) 10) (Point 0 (-11.3) 18.75) )
      btl = BackTopLine (Point 1.7 (-1.9) 10) (Point 0 (-2) 10)
   in
   legalIntersection  tll btl 
  )

--http://bit-player.org/wp-content/extras/bph-publications/BeautifulCode-2007-Hayes.pdf
onTheLineTest = TestCase $ assertEqual
  "onTheLineTest  unsure if on line"
  (0)
  (onTheLine (Point 13 3 0) (Point 13 10 10) (Point 13 19 0))


onTheLineTest2 = TestCase $ assertEqual
  "onTheLineTest unsure if on line 1"
  (0)
  (onTheLine  (Point 13 10 10) (Point 13 19 0) (Point 13 3 0))


onTheLineTest3 = TestCase $ assertEqual
  "onTheLineTest unsure if on line 2"
  (0)
  (onTheLine   (Point 13 19 0) (Point 13 3 0) (Point 13 10 10))

onTheLineTest4 = TestCase $ assertEqual
  "should not be on the line"
  (-40)
  (onTheLine   (Point (-8) 3 0) (Point (-8) 13 0) (Point (-4) 7 10))

-- =========================================================== perimetersContainIllegalIntersection ===========================================

perimetersContainIllegalIntersectionTest = TestCase $ assertEqual
  "empty list has no errors"
  (Right False)
  (perimetersContainIllegalIntersection [] (BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}))


perimetersContainIllegalIntersectionTest2 = TestCase $ assertEqual
  "missing pattern match throws error"
  (Left "perimetersContainIllegalIntersection error: Geometry.Intercept.legalIntersectionGloss has missing or illegal pattern match for advancingCpoint: BackBottomLine and  perimeter: F1")
  (perimetersContainIllegalIntersection [[F1 $ Point 0 0 0]] (BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}))

---------------------------------------------------------------- change this for testig generic fx for legalIntersectionGloss.---------------------------------------------------------
perimetersContainIllegalIntersectionTest3 = TestCase $ assertEqual
  "perimetersContainIllegalIntersection: has legal intersection from shared endpoint"
  (Right False)
  (perimetersContainIllegalIntersection [[BackBottomLine {b1=Point 7 7 0 , b4=Point (-2) (-5) 0}]] (BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}))

perimetersContainIllegalIntersectionTest4 = TestCase $ assertEqual
  "perimetersContainIllegalIntersection: contains illegal intersection in the 2nd position of perimeter list"
  (Right True)
  (perimetersContainIllegalIntersection
   [[CornerPointsNothing,
     BackBottomLine {b1=Point 7 9 0 , b4=Point (-2) (-5) 0}]
                                        ]
   (BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}))

perimetersContainIllegalIntersectionTest5 = TestCase $ assertEqual
  "contains a single illegal intersection in the 2nd position of 2nd list"
  (Right True)
  (perimetersContainIllegalIntersection
   [ [CornerPointsNothing],
     [CornerPointsNothing,
     BackBottomLine {b1=Point 2 5 0 , b4=Point (-2) (-5) 0}]
                                        ]
   (BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0})
  )

perimetersContainIllegalIntersectionTest6 = TestCase $ assertEqual
  "perimetersContainIllegalIntersection: contains a single illegal intersection in the 2nd position of 2nd list"
  (Right True)
  (perimetersContainIllegalIntersection
   [ [CornerPointsNothing],
     [CornerPointsNothing,
     BackBottomLine {b1=Point 5 9 0 , b4=Point 1 9 0}]
                                        ]
   (BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0}))
------------------------- test BackBottomLine BottomRightLine
perimetersContainIllegalIntersectionTest7 = TestCase $ assertEqual
  "perimetersContainIllegalIntersection: has legal intersection from shared endpoint"
  (Right False)
  (perimetersContainIllegalIntersection [[BackBottomLine {b1=Point 7 7 0 , b4=Point (-2) (-5) 0}]] (BottomRightLine {b4=Point (-3) (-3) 0, f4=Point 7 7 0}))

perimetersContainIllegalIntersectionTest8 = TestCase $ assertEqual
  "contains an illegal intersection"
  (Right True)
  (perimetersContainIllegalIntersection
   [ [BackBottomLine {b1=Point 2 5 0 , b4=Point (-2) (-5) 0}]]
   (BottomRightLine {b4=Point (-3) (-3) 0, f4=Point 7 7 0})
  )
-- test (TopLeftLine b2 f2) (BackTopLine b2' b3')
perimetersContainIllegalIntersectionTest9 = TestCase $ assertEqual
  "contains an illegal intersection"
  (Right True)
  (perimetersContainIllegalIntersection
   [ [(BackTopLine {b2=Point (-3) (-3) 0, b3=Point 7 7 0})]]
   
   (TopLeftLine {b2=Point 2 5 0 , f2=Point (-2) (-5) 0})
  )

perimetersContainIllegalIntersectionTest10 = TestCase $ assertEqual
  "perimetersContainIllegalIntersection: has legal intersection from shared endpoint"
  (Right False)
  (perimetersContainIllegalIntersection [[BackTopLine {b2=Point 7 7 0 , b3=Point (-2) (-5) 0}]] (TopLeftLine {b2=Point (-3) (-3) 0, f2=Point 7 7 0}))

-- =========================================================== perimetersContainLegalIntersections ===========================================
perimetersContainLegalIntersectionTest = TestCase $ assertEqual
  "perimetersContainLegalIntersections: contains a single illegal intersection in the 2nd position of 2nd list"
  (Right False)
  (perimetersContainLegalIntersections
   [ [CornerPointsNothing],
     [CornerPointsNothing,
     BackBottomLine {b1=Point 5 9 0 , b4=Point 1 9 0}]
                                        ]
   (BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0}))

perimetersContainLegalIntersectionTesta = TestCase $ assertEqual
  "swap point in BLL of perimetersContainLegalIntersectionTest"
  (Right False)
  (perimetersContainLegalIntersections
   [ [CornerPointsNothing],
     [CornerPointsNothing,
     BackBottomLine {b1=Point 18 19 0 , b4=Point 9 19 0}]
                                        ]
   (BottomLeftLine {f1=Point (13) (3) 0, b1=Point 13 19 0}))


perimetersContainLegalIntersectionTest2 = TestCase $ assertEqual
  "contains a single legal intersection in the 2nd position of 2nd list"
  (Right True)
  (perimetersContainLegalIntersections
   [ [CornerPointsNothing],
     [CornerPointsNothing,
     BackBottomLine
     {b1 = Point {x_axis = 0.8715574274765816, y_axis = -9.961946980917455, z_axis = 0.0}, b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}}
     ]
   ]                                   
   (BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0}})
  )
  
-- --------------------------------------------------------- line intersections --------------------------------------------------------
--
lineIntersectionTest = TestCase $ assertEqual
  "lineIntersection: should intercept at 7 7 0 but it doesn't"
  (Right $ Just $ Point 5 5 0)
  (let
      bll = BottomLeftLine {b1=Point 4 0 0, f1=Point 6 10 0}
      bbl = BackBottomLine {b1=Point 0 3 0, b4=Point 10 7 0}
   in
    lineIntersection bll bbl
  )

lineIntersectionTest2 = TestCase $ assertEqual
  "lineIntersection: bll and bbl should intercept at 7 7 0"
  (Right (Just (Point {x_axis = 7.0, y_axis = 7.0, z_axis = 0.0})))
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     lineIntersection bll bbl
  )

lineIntersectionTest3 = TestCase $ assertEqual
  "lineIntersection: bll and bbl should intercept"
  (Right (Just (Point {x_axis = 3.0, y_axis = 9.0, z_axis = 0.0})))
  (let
     bll = BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0}
     bbl = BackBottomLine {b1=Point 5 9 0 , b4=Point 1 9 0}
      
   in
     lineIntersection bll bbl
  )

-- ============================================================= why doesn't segmentInterceptTest3 work-----------------------------------------------------------------------------
--test out almost perpendicular in next test: segmentInterceptTest3AlmostPerpendicular
segmentInterceptTest3 = TestCase $ assertEqual
  "segmentIntersection: they intercept but am getting a nothing. Is is because they are perpendicular"
  (Right $ Just $ Point 3 9 0)
  (let
     bll = (BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0})
     bbl = BackBottomLine {b1=Point 5 9 0 , b4=Point 1 9 0}
      
   in
     --segmentIntersection bll bbl
     segmentIntersection bll bbl
  )

lineIntersectionPerpendicularTest = TestCase $ assertEqual
  "lineIntersectionGloss: intersection for perpendicualar points"
  (Right (Just (Point {x_axis = 3.0, y_axis = 9.0, z_axis = 0.0})))
  (let
     bll = (BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0})
     bbl = BackBottomLine {b1=Point 5 9 0 , b4=Point 1 9 0}
      
   in
    lineIntersection bll bbl
  )

closestPointOnLineParamGlossTest = TestCase $ assertEqual
  "closestPointOnLineParamGloss"
  (0)
  (let
     bll = (BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0})
     bbl = BackBottomLine {b1=Point 5 9.1 0 , b4=Point 1 9 0}
     (Right (Just p0)) = lineIntersection bll bbl
     
   in
    
    closestPointOnLineParamGloss (b1 bll) (f1 bll) p0 )

segmentInterceptTest3AlmostPerpendicular = TestCase $ assertEqual
  "segmentIntersection: they intercept slightly off perpendicular"
  (Right $ Just $ Point {x_axis = 3.0, y_axis = 9.049999999999999, z_axis = 0.0})
  (let
     bll = (BottomLeftLine {b1=Point 3 3 0, f1=Point 3 10 0})
     bbl = BackBottomLine {b1=Point 5 9.1 0 , b4=Point 1 9 0}
      
   in
     --segmentIntersection bll bbl
     segmentIntersection bll bbl
  )

