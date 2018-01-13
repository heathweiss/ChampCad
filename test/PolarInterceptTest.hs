module PolarInterceptTest(polarInterceptTestDo) where
import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points (Point(..))

import Geometry.PolarIntercept(lineIntersectionXY, runPolarIntersect, lineIntersectionXYT, lineIntersectionViaSlopeLineXYT)

import Geometry.Angle(Angle(..), rotateAngle, getQuadrantAngle, RotateFactor(..), getXYAngle, getQuadrant, Quadrant(..))
import Geometry.Rotation(rotatePointAroundZAxis, rotateCornerPointAroundZAxis)

polarInterceptTestDo = do
  putStrLn "\n\n"
  putStrLn "polar intercept Test"

  runTestTT intersectTTest
  runTestTT intersectTTest2
  runTestTT intersectTTest3
  runTestTT intersectTTest4
  runTestTT intersectTTest5
  runTestTT intersectTTest6
  runTestTT intersectTTest7

  runTestTT point2aQuadrantInRelationToPoint1aTest
  runTestTT point2bQuadrantInRelationToPoint1aTest
  runTestTT backBtmLineQuad1EndPointAnglesTest
  runTestTT backBtmLineQuad1EndPointsSwappedAnglesTest
  runTestTT intersectTest
  runTestTT intersectQuad2Test
  runTestTT intersectQuad2EndsSwappedTest
  runTestTT intersectTest2
  runTestTT intersectTest3
  runTestTT intersectTest4
  runTestTT intersectQuad3Test
  runTestTT intersectQuad3Test2
  runTestTT intersectQuad4Test
  runTestTT intersectQuad4Test2

--line to compare others to.
--In quad 2
quad2BtmRightLine = BottomRightLine {b4=Point 2 4 0, f4=Point 8 13 0}
quad2BtmRightLineEndsSwapped = BottomRightLine {f4=Point 2 4 0, b4=Point 8 13 0}
quad2IntersectingLines = (quad2BtmRightLine, BackBottomLine {b1=Point 9 5 0, b4=Point 3 7 0})
quad2IntersectingLinesEndsSwapped = (quad2BtmRightLineEndsSwapped, BackBottomLine {b1=Point 9 5 0, b4=Point 3 7 0})
quad2IntersectPassEndPoint = (quad2BtmRightLine, BackBottomLine {b1=Point 13 11 0, b4=Point 7 19 0})
quad2DoNotIntersect = (quad2BtmRightLine,BackBottomLine {b1=Point 1 6 0, b4=Point 3 7 0})
quad2ShareAnEndPoint = (quad2BtmRightLine, BackBottomLine {b1=Point 2 4 0, b4=Point 3 7 0})
quad2ShareAnEndPoint2 = (quad2BtmRightLine, BackBottomLine {b1=Point 1 6 0, b4=Point 8 13 0})
--line to compare others to.
--In quad 3
quad3BtmRightLine = BottomRightLine {b4=Point (-12) 4 0, f4=Point (-5) 14 0}
quad3IntersectingLines = (quad3BtmRightLine, BackBottomLine {b1=Point (-7) 5 0, b4=Point (-11) 12 0})
quad3DoNotIntersect = (quad3BtmRightLine, BackBottomLine {b1=Point (-5) 3 0, b4=Point (-4) 10 0})
--line to compare others to.
--In quad 4
qua4BtmRightLine = BottomRightLine {b4=Point (-6) (-17) 0, f4=Point (-9) (-7) 0}
--but they don't. Is it because it intersects off the end of qua4BtmRightLine
quad4IntersectingLines = (qua4BtmRightLine, BackBottomLine {b1=Point (-7) (-24) 0, b4=Point (-1) (-14) 0})
quad4IntersectingLines2 = (qua4BtmRightLine, BackBottomLine {b1=Point (-10) (-16) 0, b4=Point (-5) (-9) 0})

----------------------------------------------------- currLineIntersectionXYT --------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------
currLineIntersectionXYT = lineIntersectionViaSlopeLineXYT
intersectTTest = TestCase $ assertEqual
  "lines should intercept Transformer test"
   (Right $ Just $ Point 0 0 0) 
   (runPolarIntersect currLineIntersectionXYT  (fst quad2IntersectingLines) (snd quad2IntersectingLines))


intersectTTest2 = TestCase $ assertEqual
  "lines should intercept with base line ends swapped Transformer test"
   (Right $ Just $ Point 0 0 0) 
   (runPolarIntersect currLineIntersectionXYT  (fst quad2IntersectingLinesEndsSwapped) (snd quad2IntersectingLinesEndsSwapped))

intersectTTest3 = TestCase $ assertEqual
  "lines should intercept past the endpoint of quad2IntersectingLines T test"
   (Right $ Just $ Point 0 0 0) 
   (runPolarIntersect currLineIntersectionXYT  (fst quad2IntersectPassEndPoint) (snd quad2IntersectPassEndPoint) )

intersectTTest4 = TestCase $ assertEqual
  "lines should not intercept T"
   (Right Nothing) 
   (runPolarIntersect currLineIntersectionXYT  (fst quad2DoNotIntersect) (snd quad2DoNotIntersect) )

intersectTTest5 = TestCase $ assertEqual
  "lines should intercept as they share endpoint 1 T"
   (Right $ Just $ Point 2 4 0) 
   (runPolarIntersect currLineIntersectionXYT  (fst quad2ShareAnEndPoint) (snd quad2ShareAnEndPoint) )

intersectTTest6 = TestCase $ assertEqual
  "lines should intercept as they share endpoint 2 T"
   (Right $ Just $ Point 8 13 0) 
   (runPolarIntersect currLineIntersectionXYT  (fst quad2ShareAnEndPoint2) (snd quad2ShareAnEndPoint2) )

intersectTTest7 = TestCase $ assertEqual
  "lines should intercept as they share endpoint 2 T"
   (Right $ Just $ Point 0 0 0) 
   (runPolarIntersect currLineIntersectionXYT  (fst quad3IntersectingLines) (snd quad3IntersectingLines) )
   {-
intersectQuad3Test = TestCase $ assertEqual
  "lines should intercept quad 3"
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad3IntersectingLines) (snd quad3IntersectingLines) )
-}
----------------------------------------------------- end:  currLineIntersectionXYT --------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------

point2aQuadrantInRelationToPoint1aTest = TestCase $ assertEqual
  "point2aQuadrantInRelationToPoint1a"
  (Quadrant2)
  (let

      point2aAdjustedForAngleOfLine1 =
        let line1QAngle = getQuadrantAngle $ getXYAngle (f4 quad2BtmRightLine) (b4 quad2BtmRightLine) 
            
        --in rotatePointAroundZAxis (angle line1QAngle) (b4 brl) (b1 bbl1)
        in rotatePointAroundZAxis (angle line1QAngle) (b4 (fst quad2IntersectingLines)) (b1 (snd quad2IntersectingLines)) 
      
      point2aXYAngleInRelationToPoint1a = getXYAngle (b4 quad2BtmRightLine)  point2aAdjustedForAngleOfLine1
      point2aQuadrantInRelationToPoint1a = getQuadrant point2aXYAngleInRelationToPoint1a
   in
     point2aQuadrantInRelationToPoint1a
  )

point2bQuadrantInRelationToPoint1aTest = TestCase $ assertEqual
  "point2bQuadrantInRelationToPoint1aTest"
  (Quadrant3)
  (let
     point2bAdjustedForAngleOfLine1 =
        let line1QAngle = getQuadrantAngle $ getXYAngle (f4 quad2BtmRightLine) (b4 quad2BtmRightLine) 
            
        --in rotatePointAroundZAxis (angle line1QAngle) (b4 brl) (b4 bbl1)
        in rotatePointAroundZAxis (angle line1QAngle) (b4 (fst quad2IntersectingLines)) (b4 (snd quad2IntersectingLines)) 
      
     point2bXYAngleInRelationToPoint1a = getXYAngle (b4 quad2BtmRightLine)  point2bAdjustedForAngleOfLine1
     point2bQuadrantInRelationToPoint1a = getQuadrant point2bXYAngleInRelationToPoint1a
   in
     point2bQuadrantInRelationToPoint1a
  )

backBtmLineQuad1EndPointAnglesTest = TestCase $ assertEqual
  "have a look at the angle between the endpoints of the leading line"
  (Quadrant2Angle {angle = 33.69006752597977})
  (getQuadrantAngle $ getXYAngle (b4 quad2BtmRightLine) (f4 quad2BtmRightLine))

--notice: Swapping ends of line from prev test changes from Quad2 to Quad4 but with exact same angle
backBtmLineQuad1EndPointsSwappedAnglesTest = TestCase $ assertEqual
  "have a look at the angle between the endpoints of the leading line with ends swapped"
  (Quadrant4Angle {angle = 33.69006752597977})
  (getQuadrantAngle $ getXYAngle (b4 quad2BtmRightLineEndsSwapped) (f4 quad2BtmRightLineEndsSwapped))

intersectTest = TestCase $ assertEqual
  "lines should intercept"
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad2IntersectingLines) (snd quad2IntersectingLines) )

intersectQuad2Test = TestCase $ assertEqual
  "lines should intercept past the endpoint of quad2IntersectingLines "
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad2IntersectPassEndPoint) (snd quad2IntersectPassEndPoint) )

--quad2IntersectingLinesEndsSwapped
--same as prev. test, except the BtmRightLn has it's ends swapped.
intersectQuad2EndsSwappedTest = TestCase $ assertEqual
  "lines should intercept past the endpoint of quad2IntersectingLinesEndsSwapped "
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad2IntersectingLinesEndsSwapped) (snd quad2IntersectingLinesEndsSwapped) )


intersectTest2 = TestCase $ assertEqual
  "lines should not intercept"
   (Right $ Nothing) 
   (lineIntersectionXY (fst quad2DoNotIntersect) (snd quad2DoNotIntersect) )

intersectTest3 = TestCase $ assertEqual
  "lines should intercept as they start at same point "
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad2ShareAnEndPoint) (snd quad2ShareAnEndPoint) )

intersectTest4 = TestCase $ assertEqual
  "lines should intercept as they end at same point "
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY  (fst quad2ShareAnEndPoint2) (snd quad2ShareAnEndPoint2) )

intersectQuad3Test = TestCase $ assertEqual
  "lines should intercept quad 3"
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad3IntersectingLines) (snd quad3IntersectingLines) )

intersectQuad3Test2 = TestCase $ assertEqual
  "lines should not intercept quad 3"
   (Right $ Nothing) 
   (lineIntersectionXY (fst quad3DoNotIntersect) (snd quad3DoNotIntersect) )

intersectQuad4Test = TestCase $ assertEqual
  "lines should intercept but they don't"
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad4IntersectingLines) (snd quad4IntersectingLines) )

intersectQuad4Test2 = TestCase $ assertEqual
  "lines should intercept"
   (Right $ Just $ Point 0 0 0) 
   (lineIntersectionXY (fst quad4IntersectingLines2) (snd quad4IntersectingLines2) )
