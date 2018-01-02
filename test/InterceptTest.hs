module InterceptTest(interceptTestDo) where

import Test.HUnit

import Geometry.Intercept(getChangeInX, getChangeInY, yIntercept, topCoderAreTheParallel, topCoderXRayIntercept, lineIntersection,
                          onTheLine, segmentIntersection, segmentIntersectionBreakDown, legalIntersection, perimetersContainIllegalIntersection)

import CornerPoints.CornerPoints(CornerPoints(..), cpointType)
import CornerPoints.Points (Point(..))

import Math.Distance(DistanceA(..))

interceptTestDo = do

  putStrLn ""
  putStrLn "intercept TestDo"
  
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

  runTestTT onTheLineTest
  runTestTT onTheLineTest2
  runTestTT onTheLineTest3

  runTestTT perimetersContainIllegalIntersectionTest
  runTestTT perimetersContainIllegalIntersectionTest2
  runTestTT perimetersContainIllegalIntersectionTest3
  runTestTT perimetersContainIllegalIntersectionTest4
  runTestTT perimetersContainIllegalIntersectionTest4

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
  (Right False)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 3 3 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     segmentIntersection bll bbl
  )

rosettaInterceptTest3 = TestCase $ assertEqual
  "bll and bbl won't intercept at 7 7 0 as bll is too short"
  --(Right(Just False))
  (Right False)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 3 3 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     segmentIntersection bll bbl
  )



rosettaInterceptTest5 = TestCase $ assertEqual
  "bll and bbl should intercept at 7 7 0 as it is just right"
  (Right True)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 7 7 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 13 15 0}
      
   in
     segmentIntersection bll bbl
  )


rosettaInterceptTest6 = TestCase $ assertEqual
  "bll and bbl won't intercept at 7 7 0 because bbl is too short"
  (Right False)
  (let
     bll = BottomLeftLine {b1=Point (-3) (-3) 0, f1=Point 12 12 0}
     bbl = BackBottomLine {b1=Point (-2) (-5) 0, b4=Point 5 4 0}
      
   in
     segmentIntersection bll bbl
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
