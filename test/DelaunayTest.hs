module DelaunayTest(delaunayTestDo) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@),
                                cornerPointsError, isCubePoints, isCubePointsList)
import CornerPoints.Points (Point(..), )

import Joiners.Delaunay(delaunayB, orderByDistance,
                        removeAdvCPointFromIOPerims, extractI, Perimeters(..), AdvancingCPoint(..))

import Math.Distance(center ,(<-|->), centerA)

import Helpers.List(removeEmpty, safeHead )
import Helpers.Applicative(extractE)

delaunayTestDo = do

  putStrLn ""
  putStrLn "delaunayTestDo"
  
  runTestTT extractITest1
  runTestTT extractITest2
  runTestTT extractITest3
  runTestTT extractITest4
  runTestTT extractITest5
  
  runTestTT orderByDistanceTest1
  runTestTT orderByDistanceTest2
  runTestTT orderByDistanceTest3
  runTestTT orderByDistanceTest4
  runTestTT orderByDistanceTest5
  runTestTT orderedInnerPerimsTest1
  runTestTT advancingCpointTest1
  runTestTT removeAdvancingCpointFromPerimsTest1
  runTestTT removeAdvancingCpointFromPerimsTest2
  runTestTT removeAdvancingCpointFromPerimsTest3
  runTestTT removeAdvancingCpointFromPerimsTest4
  runTestTT advancingCpointTest2

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


-- ======================================== extractI tests =======================================

extractITest1 = TestCase $ assertEqual
  "extractITest1"
  (Right $ CornerPointsError "should be good")
   (extractI   [[CornerPointsError "should be good"]]) 

extractITest2 = TestCase $ assertEqual
  "extractITest2"
  (Left "Joiners.Delaunay.extractI: attempt to get i from [[]]")
   (extractI   [[]])

extractITest3 = TestCase $ assertEqual
  "extractITest3"
  (Left "Joiners.Delaunay.extractI: attempt to get i from []")
   (extractI   [])

extractITest4 = TestCase $ assertEqual
  "extractITest4"
  (Left "Joiners.Delaunay.extractI: attempt to get i from [[]:xs]")
   (extractI   [[],[F1 $ Point 0 0 0]])

extractITest5 = TestCase $ assertEqual
  "extractITest5"
  (Left "Joiners.Delaunay.extractI: attempt to get i from [[]:xs]")
   (extractI   [[],[]]) 
  
-- ---------------------------------------- orderByDistance tests ========================================


orderByDistanceTest1 = TestCase $ assertEqual
  "orderByDistanceTest1"
   (Right $ Just $ InnerPerimeters  [[B1 $ Point 0 0 0], [B1 $ Point 0 0 10]])
   (orderByDistance (Just $ InnerPerimeters [[B1 $ Point 0 0 10], [B1 $ Point 0 0 0]]) (AdvancingCPoint (F1 (Point 10 0 0))) )

orderByDistanceTest2 = TestCase $ assertEqual
  "orderByDistanceTest2"
   (Right $ Just $ InnerPerimeters [[(B1 $ Point 0 0 0), (B1 $ Point 0 0 110)], [(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)]])
   (orderByDistance  (Just $ InnerPerimeters [[(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)], [(B1 $ Point 0 0 0), (B1 $ Point 0 0 110)]]) (AdvancingCPoint (F1 (Point 10 0 0))) )

orderByDistanceTest3 = TestCase $ assertEqual
  "orderByDistanceTest3"
   (Right Nothing)
   (orderByDistance  (Just $ InnerPerimeters []) (AdvancingCPoint (F1 (Point 10 0 0))) )

orderByDistanceTest4 = TestCase $ assertEqual
  "orderByDistanceTest4"
   (Right Nothing)
   (orderByDistance  (Just $ InnerPerimeters [[]]) (AdvancingCPoint (F1 (Point 10 0 0))) )

orderByDistanceTest5 = TestCase $ assertEqual
  "orderByDistanceTest5"
   (Right (Just (InnerPerimeters {_innerPerimeters = [[B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}},B1 {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 110.0}}]]})))
   (orderByDistance  (Just $ InnerPerimeters [[],[(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)]]) (AdvancingCPoint (F1 (Point 10 0 0))) )

------------------- set points for delaunayB (and support fx's testing --------------------------------


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



------------------- delaunayB: from set points --------------------------------
orderedInnerPerimsTest1 = TestCase $ assertEqual
  "orderedInnerPerimsTest1"
   (Right $ Just $ InnerPerimeters [inner1a, inner1b])
   (orderByDistance  (Just $ InnerPerimeters [inner1a, inner1b]) (AdvancingCPoint (head outer1)) )


--build the initial advancingCpoint in delaunayB from:
  --head outer1Perims
  --head orderdInnerPerims
advancingCpointTest1 = TestCase $ assertEqual
  "advancingCpointTest1"
   (Right $ BottomRightLine {f4=(Point 0 (-19) 0), b4=(Point 0 (-7) 0)})
   (let
       orderedInnerPerims = orderByDistance (Just $ InnerPerimeters [inner1a, inner1b]) (AdvancingCPoint $ head outer1 )
       o = F4 $ Point 0 (-19) 0
    in
      ((\(Just (InnerPerimeters innerPerimeters)) -> o +++ (head $ head innerPerimeters))) <$> orderedInnerPerims
   )

--build the initial advancingCpoint in delaunayB from:
  --head outer1Perims
  --head orderdInnerPerims
--now remove the used cpoints from inner/outer lists
  --because this is initial call, there should be on removed from inner and outer perims
removeAdvancingCpointFromPerimsTest1 = TestCase $ assertEqual
  "can't remove advancingCpoint which does not exist in Perimeters"
  (Left "Joiners.Delaunay.removeAdvCPointFromIOPerims head $ InnerPerimeters = [B4 {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}},B1 {b1 = Point {x_axis = 2.0, y_axis = -5.0, z_axis = 0.0}}]:is did not remove the advancingCpoint because : Joiners.Delaunay.removeAdvCPointFromIOPerims InnerPerimeter = B4 {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}}:is, OuterPerimeter = F4 {f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}}:os did not remove the advancingCpoint: F1 {f1 = Point {x_axis = 11.0, y_axis = 22.0, z_axis = 33.0}}because advancingCpoint because it was not contained in either one.")

  
  (let
     advancingCpoint = AdvancingCPoint $ F1 $ Point 11 22 33 --a point that is not part of Perimeters
      
     orderedInnerPerims :: Either String (Maybe Perimeters)
     orderedInnerPerims = Right (Just $ (InnerPerimeters [inner1a, inner1b]))
        --distance from o, as that is the initial advancing line.
      
   in
      extractE (removeAdvCPointFromIOPerims <$> orderedInnerPerims <*>  (Right (Just (OuterPerimeter outer1))) <*> Right advancingCpoint)
  )

--do an orderByDistance on (Just $ InnerPerimeters [])
--now try to remove the advancingCpoint from outer list, where it does not exist.
removeAdvancingCpointFromPerimsTest2 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest2"
  (Left "Joiners.Delaunay.removeAdvCPointFromIOPerims: InnerPerimter = Nothing, OuterPermeter = F1 {f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}} :os did not remove advancingCpoint: F4 {f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}} because it was not contained in o ")
  (let
      orderedInnerPerims = orderByDistance (Just $ InnerPerimeters []) advancingCpoint
      advancingCpoint = AdvancingCPoint $ head outer1
   in
      extractE (removeAdvCPointFromIOPerims  <$> orderedInnerPerims <*> Right(Just(OuterPerimeter $ tail outer1))  <*> Right advancingCpoint)
  )

--give an advancingCpoint that is in outerPerimemeter, while inner perims is []
--now remove the used cpoints from outerPerimeter
removeAdvancingCpointFromPerimsTest3 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest3"
  (Right (Nothing,Just (OuterPerimeter {_outerPerimeter = [F1 {f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}}]})))
  (let
      advancingCpoint = AdvancingCPoint $ head outer1
        --1st advancing line so we know it is head outer perimeters
      orderedInnerPerims = orderByDistance (Just $ InnerPerimeters []) advancingCpoint
      
   in
     --because it is used applicatively, it will return a Right Right ...
     extractE (removeAdvCPointFromIOPerims  <$> orderedInnerPerims <*> Right (Just $ OuterPerimeter outer1) <*> Right advancingCpoint) 
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
      orderedInnerPerims = orderByDistance (Just $ InnerPerimeters [inner1a, inner1b]) o
      --B4 {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}}
      advancingCpoint = AdvancingCPoint $ BottomRightLine {f4=Point 0 (-19) 0, b4=Point 0 (-7) 0} 
        --should contain o  
      
   in
     extractE (removeAdvCPointFromIOPerims <$> orderedInnerPerims <*> Right Nothing <*> Right advancingCpoint)
  )

outer2 = outer1 ++ [F1 $ Point 10 (-11) 0]

--build the intial BtmRightLine from head <outer1/inner1a>
--remove the used cpoints
--build the next advancing cpoint from <outer1/inner1a>
  --this is where i need Deluanay to simplify
advancingCpointTest2 = TestCase $ assertEqual
  "advancingCpointTest2"
  --([CornerPointsError "just a filler"])
  (
   [BottomRightLine {
     b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0},
     f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}},
    
    BottomLeftLine {
     b1 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0},
     f1 = Point {x_axis = 6.0, y_axis = -17.0, z_axis = 0.0}},
    
    BottomLeftLine {
     b1 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0},
     f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}},
    
    BottomLeftLine {
     b1 = Point {x_axis = 2.0, y_axis = -5.0, z_axis = 0.0},
     f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}},
    
    BottomLeftLine {
     b1 = Point {x_axis = 0.0, y_axis = 4.0, z_axis = 1.0},
     f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}},
    
    BottomLeftLine {
     b1 = Point {x_axis = 3.0, y_axis = 6.0, z_axis = 1.0},
     f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}}]
  )
  (delaunayB outer2  [inner1a, inner1b] )

{-
but got: [CornerPointsError {errMessage = "Joiners.DeluanayB(Righ(Left e)): Joiners.Deluanay.newAdvancingCpointE had Nothing passed in for all parameters except:

AdvancingCPoint {_advancingCpoint =
BottomLeftLine {
b1 = Point {x_axis = 3.0, y_axis = 6.0, z_axis = 1.0},
f1 = Point {x_axis = 10.0, y_axis = -11.0, z_axis = 0.0}}}"}]


-}
