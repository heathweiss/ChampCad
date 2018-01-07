module InterceptTest(interceptTestDo) where

import Test.HUnit

import Geometry.Intercept(getChangeInX, getChangeInY, yIntercept, topCoderAreTheParallel, topCoderXRayIntercept, lineIntersection,
                          onTheLine, segmentIntersection, segmentIntersectionBreakDown, legalIntersection, 
                          perimetersContainIllegalIntersection, perimetersContainLegalIntersections,
                          segmentIntersectionT, runSegmentIntersectionT)
import Geometry.Angle(Angle(..))

import CornerPoints.CornerPoints(CornerPoints(..), cpointType, (+++))
import CornerPoints.Points (Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction(extractB1, extractB4, extractF1, extractF4, extractBackLeftLine, extractBackRightLine,
                                  extractFrontLeftLine, extractFrontRightLine, extractBackBottomLine, extractBackFace)

import Math.Distance(DistanceA(..))

import Primitives.Cylindrical.Walled(cylinder)

interceptTestDo = do

  putStrLn ""
  putStrLn "intercept TestDo"

  runTestTT seeInitialAdvancingCpoint
  runTestTT seeInnerCylinderPoints
  runTestTT seeOuterCylinderPoints
  runTestTT seeInnerCylinderBackLines
  runTestTT seeInitialAdvancingCpoint
  
  runTestTT checkChangeInXYTest1
  runTestTT checkChangeInXYTest2
  runTestTT checkChangeInXYTest3
  runTestTT checkChangeInXYTest4

  runTestTT getYInterceptTest

  runTestTT topCoderAreParallelTest
  runTestTT topCoderAreParallelTest2

  runTestTT topCoderXRayInterceptTest

  runTestTT rosettaInterceptTest
  runTestTT rosettaInterceptTest2
  runTestTT rosettaInterceptTest3
  runTestTT rosettaInterceptTest5
  runTestTT rosettaInterceptTest6

  runTestTT rosettaInterceptTest
  runTestTT rosettaInterceptTest2
  

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

  runTestTT onTheLineTest
  runTestTT onTheLineTest2
  runTestTT onTheLineTest3

  runTestTT perimetersContainIllegalIntersectionTest
  runTestTT perimetersContainIllegalIntersectionTest2
  runTestTT perimetersContainIllegalIntersectionTest3
  runTestTT perimetersContainIllegalIntersectionTest4
  runTestTT perimetersContainIllegalIntersectionTest4

  runTestTT perimetersContainLegalIntersectionTest
  runTestTT perimetersContainLegalIntersectionTesta
  runTestTT perimetersContainLegalIntersectionTest2

  runTestTT lineIntersectionTest


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


topCoderXRayInterceptTest = TestCase $ assertEqual
  "bll and bbl should intercept at"
  (Left "filler")
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     topCoderXRayIntercept bll bbl
  )


-- =================================================================================================
-- https://rosettacode.org/wiki/Find_the_intersection_of_two_lines#Haskell

rosettaInterceptTest = TestCase $ assertEqual
  "bll and bbl should intercept at 7 7 0"
  (Right (Just (Point {x_axis = 7.0, y_axis = 7.0, z_axis = 0.0})))
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     lineIntersection bll bbl
  )


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
--segmentIntersectionT
rosettaInterceptTest3 = TestCase $ assertEqual
  "bll and bbl won't intercept at 7 7 0 as bll is too short"
  --(Right False)
  (Right Nothing)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 3 3 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     --segmentIntersection bll bbl
     runSegmentIntersectionT $ segmentIntersectionT bll bbl
  )


--passed when using segmentIntersection 
rosettaInterceptTest5 = TestCase $ assertEqual
  "bll and bbl should intercept at 7 7 0 as it is just right"
  --(Right True)
  (Right $Just $Point {x_axis = 7, y_axis = 7, z_axis = 0})
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     --segmentIntersection bll bbl
     runSegmentIntersectionT $ segmentIntersectionT bll bbl
  )


rosettaInterceptTest6 = TestCase $ assertEqual
  "bll and bbl won't intercept at 7 7 0 because bbl is too short"
  --(Right False)
  (Right Nothing)
  --but got: Right (Just (Point {x_axis = 8.5, y_axis = 8.5, z_axis = 0.0}))
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 5 4 0}
      
   in
     --segmentIntersection bll bbl
     runSegmentIntersectionT $ segmentIntersectionT bll bbl
  )
--


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
-- =====================================================================================================
--http://bit-player.org/wp-content/extras/bph-publications/BeautifulCode-2007-Hayes.pdf
onTheLineTest = TestCase $ assertEqual
  "whatever"
  (0)
  (onTheLine (Point 13 3 0) (Point 13 10 10) (Point 13 19 0))


onTheLineTest2 = TestCase $ assertEqual
  "whatever"
  (0)
  (onTheLine  (Point 13 10 10) (Point 13 19 0) (Point 13 3 0))


onTheLineTest3 = TestCase $ assertEqual
  "whatever"
  (0)
  (onTheLine   (Point 13 19 0) (Point 13 3 0) (Point 13 10 10))

-- =========================================================== perimetersContainIllegalIntersection ===========================================

perimetersContainIllegalIntersectionTest = TestCase $ assertEqual
  "empty list has no errors"
  (Right False)
  (perimetersContainIllegalIntersection [] (BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}))


perimetersContainIllegalIntersectionTest2 = TestCase $ assertEqual
  "missing pattern match throws error"
  (Left "Geometry.Intercept.legalIntersection has missing or illegal pattern match for advancingCpoint: BackBottomLine and  perimeter: F1")
  (perimetersContainIllegalIntersection [[F1 $ Point 0 0 0]] (BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}))


perimetersContainIllegalIntersectionTest3 = TestCase $ assertEqual
  "contains a single illegal intersection"
  (Right True)
  (perimetersContainIllegalIntersection [[BackBottomLine {b1=Point 7 7 0 , b4=Point (-2) (-5) 0}]] (BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}))

perimetersContainIllegalIntersectionTest4 = TestCase $ assertEqual
  "contains a single illegal intersection in the 2nd position of single list"
  (Right True)
  (perimetersContainIllegalIntersection
   [[CornerPointsNothing,
     BackBottomLine {b1=Point 7 7 0 , b4=Point (-2) (-5) 0}]
                                        ]
   (BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}))

perimetersContainIllegalIntersectionTest5 = TestCase $ assertEqual
  "contains a single illegal intersection in the 2nd position of 2nd list"
  (Right True)
  (perimetersContainIllegalIntersection
   [ [CornerPointsNothing],
     [CornerPointsNothing,
     BackBottomLine {b1=Point 7 7 0 , b4=Point (-2) (-5) 0}]
                                        ]
   (BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}))

-- =========================================================== perimetersContainLegalIntersections ===========================================
perimetersContainLegalIntersectionTest = TestCase $ assertEqual
  "contains a single illegal intersection in the 2nd position of 2nd list"
  (Right False)
  (perimetersContainLegalIntersections
   [ [CornerPointsNothing],
     [CornerPointsNothing,
     BackBottomLine {b1=Point 18 19 0 , b4=Point 9 19 0}]
                                        ]
   (BottomLeftLine {b1=Point (13) (3) 0, f1=Point 13 19 0}))

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
  "should intercept at 7 7 0 but it doesn't"
  (Right $ Just $ Point 7 7 0)
  (let
     --bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
      bll = BottomLeftLine {b1=Point 4 0 0, f1=Point 6 10 0}
     --bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 5 4 0}
      bbl = BackBottomLine {b1=Point 0 3 0, b4=Point 10 7 0}
   in
    lineIntersection bll bbl
  )
