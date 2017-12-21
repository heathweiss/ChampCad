module DelaunayTest(delaunayTestDo) where

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), CornerPointsBuilder(..), (&+++#@), (|@+++#@|), (@+++#@),
                                cornerPointsError, isCubePoints, isCubePointsList)
import CornerPoints.Points (Point(..), )

import Joiners.Delaunay(delaunay, delaunayB, removeIfUsed, ensureGoodHeadDistance, orderByDistance, removeOneOrBothIfUsed, extractI)

import Math.Distance(center ,(<-|->), centerA)

import Helpers.List(removeEmpty, safeHead )

delaunayTestDo = do

  putStrLn ""
  putStrLn "delaunayTestDo"
  runTestTT removeIfUsedTest1
  runTestTT ensureGoodHeadDistanceTest1
  runTestTT ensureGoodHeadDistanceTest2

  runTestTT extractITest1
  runTestTT extractITest2
  runTestTT extractITest3
  runTestTT extractITest4
  runTestTT extractITest5
  
  runTestTT orderByDistanceTest1
  runTestTT orderByDistanceTest2
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
-- ================================================= removeIfUsed ========================================
removeIfUsedTest1 = TestCase $ assertEqual
  "removeIfUsedTest1"
   (Right [])
   (let
         f1 = Point 0 1 0
         f2 = Point 0 1 1
         b1 = Point 0 0 0
         b2 = Point 0 0 1
         frontLeftLine  = FrontLeftLine f1 f2
         leftFace = LeftFace   b1 b2 f1 f2
         
    in
    
     removeIfUsed
      [frontLeftLine]
      leftFace
   )

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
  
-- =====================================ensureGoodHeadDistance tests ===============================
ensureGoodHeadDistanceTest1 = TestCase $ assertEqual
  "ensureGoodHeadDistanceTest1"
  (Left "FaceConversions.raisedTo: illegal or missing pattern match for F1 and FrontLeftLine" )
   (ensureGoodHeadDistance  [[F1 $ Point 0 0 0]] [] (F2 $ Point 0 0 0))

-- ()

ensureGoodHeadDistanceTest2 = TestCase $ assertEqual
  "ensureGoodHeadDistanceTest2"
   (Right [[B1 $ Point 0 0 0]])
   (ensureGoodHeadDistance  [[B1 $ Point 0 0 0]]  [] (F1 (Point 0 0 0) ) )

-- ---------------------------------------- orderByDistance tests ========================================


orderByDistanceTest1 = TestCase $ assertEqual
  "orderByDistanceTest1"
   (Right [[B1 $ Point 0 0 0], [B1 $ Point 0 0 10]])
   (orderByDistance  [[B1 $ Point 0 0 10], [B1 $ Point 0 0 0]] (F1 (Point 10 0 0) ) )

orderByDistanceTest2 = TestCase $ assertEqual
  "orderByDistanceTest2"
   (Right [[(B1 $ Point 0 0 0), (B1 $ Point 0 0 110)], [(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)]])
   (orderByDistance  [[(B1 $ Point 0 0 10), (B1 $ Point 0 0 110)], [(B1 $ Point 0 0 0), (B1 $ Point 0 0 110)]] (F1 (Point 10 0 0) ) )



------------------- set points for delaunayB (and support fx's testing --------------------------------
inner2 =
  [B4 $ Point 0 4 1,
   B1 $ Point 3 6 1
  ]

inner1 =
  [B4 $ Point 0 (-7) 0,
   B1 $ Point 2 (-5) 0
  ]

outer1 =
  [F4 $ Point 0 (-19) 0,
   F1 $ Point 6 (-17) 0
  ]



------------------- delaunayB: from set points --------------------------------
orderedInnerPerimsTest1 = TestCase $ assertEqual
  "orderedInnerPerimsTest1"
   (Right [inner1, inner2])
   (orderByDistance  [inner1, inner2] (head outer1 ) )


--build the initial advancingCpoint in delaunayB from:
  --head outer1Perims
  --head orderdInnerPerims
advancingCpointTest1 = TestCase $ assertEqual
  "advancingCpointTest1"
   (Right $ BottomRightLine {f4=(Point 0 (-19) 0), b4=(Point 0 (-7) 0)})
   (let
       orderedInnerPerims = orderByDistance  [inner1, inner2] (head outer1 )
       o = F4 $ Point 0 (-19) 0
    in
      ((\x -> o +++ (head $ head x))) <$> orderedInnerPerims
   )





--build the initial advancingCpoint in delaunayB from:
  --head outer1Perims
  --head orderdInnerPerims
--now remove the used cpoints from inner/outer lists
  --because this is initial call, there should be on removed from inner and outer perims
removeAdvancingCpointFromPerimsTest1 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest1"
  (Right
   (
    (tail outer1),
    (tail inner1)
   )
  )
  (let
      o = head outer1
        --1st advancing line so we know it is head outer perimeters
      orderedInnerPerims = orderByDistance (removeEmpty  [[], inner1, inner2]) o
        --distance from o, as that is the initial advancing line.
      advancingCpoint = ((\x -> o +++ (head $ head x))) <$> orderedInnerPerims
        --the actual advancingCpoints calc'd by adding o to the nearest inner perim
      
   in
     --because it is used applicatively, it will return a Right Right ...
     case removeOneOrBothIfUsed outer1 <$> (fmap (head) orderedInnerPerims) <*> advancingCpoint of
       Left e -> Left e
       Right (Right val) -> Right val
       Right (Left e) -> Left e
       --Right val -> Right val
     
  )

--give an advancingCpoint that should be in outer perims, while inner perims is []
--now remove the used cpoints from outer list
removeAdvancingCpointFromPerimsTest2 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest2"
  (Right
   (
    (tail outer1),
    ([])
   )
  )
  (let
      o = head outer1
        --1st advancing line so we know it is head outer perimeters
      orderedInnerPerims = orderByDistance ([]) o
      advancingCpoint = Right $ BottomRightLine {f4=Point 0 (-19) 0, b4=Point 0 0 0} 
        --should contain o  
      
   in
     --because it is used applicatively, it will return a Right Right ...
     case removeOneOrBothIfUsed outer1 <$> (fmap (safeHead) orderedInnerPerims) <*> advancingCpoint of
       Left e -> Left e
       Right (Right val) -> Right val
       Right (Left e) -> Left e
  )

--give an advancingCpoint that is not in outer perims, while inner perims is []
--now attemp to remove the used cpoints from outer list
removeAdvancingCpointFromPerimsTest3 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest3"
  (Left "Joiners.Delaunay.removeIfUsed: did not find a used cpoint in outer perimeters for: BottomRightLine {b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -19999.0, z_axis = 0.0}} containing F4 {f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}}"
  )
  (let
      o = head outer1
        --1st advancing line so we know it is head outer perimeters
      orderedInnerPerims = orderByDistance ([]) o
        --distance from o, as that is the initial advancing line.
      --advancingCpoint = ((\x -> o +++ (head $ head x))) <$> orderedInnerPerims
      --F4 {f4 = Point {x_axis = 0.0, y_axis = -19.0, z_axis = 0.0}}
      advancingCpoint = Right $ BottomRightLine {f4=Point 0 (-19999) 0, b4=Point 0 0 0} 
        --just a filler, as there will be no innerPerims to remove it from.
      
        --the actual advancingCpoints calc'd by adding o to the nearest inner perim
      
   in
     --because it is used applicatively, it will return a Right Right ...
     case removeOneOrBothIfUsed outer1 <$> (fmap (safeHead) orderedInnerPerims) <*> advancingCpoint of
       Left e -> Left e
       Right (Right val) -> Right val
       Right (Left e) -> Left e
       --Right val -> Right val
     
  )

--give an advancingCpoint that should be in inner perims, while outer perims is []
--now remove the used cpoints from outer list
removeAdvancingCpointFromPerimsTest4 = TestCase $ assertEqual
  "removeAdvancingCpointFromPerimsTest4"
  (Right
   (
    [],tail inner1
   )
  )
  (let
      o = head outer1
        --1st advancing line so we know it is head outer perimeters
      orderedInnerPerims = orderByDistance ([inner1, inner2]) o
      --B4 {b4 = Point {x_axis = 0.0, y_axis = -7.0, z_axis = 0.0}}
      advancingCpoint = Right $ BottomRightLine {f4=Point 0 (-19) 0, b4=Point 0 (-7) 0} 
        --should contain o  
      
   in
     --because it is used applicatively, it will return a Right Right ...
     case removeOneOrBothIfUsed [] <$> (fmap (safeHead) orderedInnerPerims) <*> advancingCpoint of
       Left e -> Left e
       Right (Right val) -> Right val
       Right (Left e) -> Left e
  )

outer2 = outer1 ++ [F1 $ Point 10 (-11) 0]

--build the intial BtmRightLine from head <outer1/inner1>
--remove the used cpoints
--build the next advancing cpoint from <outer1/inner1>
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
  (delaunayB outer2 [inner1, inner2])

{-
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

-}
