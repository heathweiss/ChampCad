module ComposableTest(composableTestDo) where

import Test.HUnit hiding (State)

import CornerPoints.Composable (createCornerPoint, Origin(..), Composable(..),
                                composableDefault, runComposer, createCornerPointComposable, createBottomFacesComposable, createTopFacesComposable,
                                createCornerPointComposableSloped, createComposable)
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.Create(Angle(..), Slope(..), flatXSlope, flatYSlope)

composableTestDo = do
  runTestTT createDefaultComposableWithCreateCornerPointComposable
  runTestTT createCornerPointComposableSlopedTest1
  runTestTT createComposableTest
  runTestTT createCornerPointTest

createDefaultComposableWithCreateCornerPointComposable = TestCase $ assertEqual
  "createDefaultComposableWithCreateCornerPointComposable"
  (composableDefault)
  (createCornerPointComposable (F1) (Point 0 0 0) (Radius 0) (Angle 0) )


createCornerPointComposableSlopedTest1 = TestCase $ assertEqual
  "createCornerPointComposableSlopedTest1"
  --set expected to what is the result of createCornerPointComposableSloped.
  (composableDefault {_cpoint = F1 (Point 1.73560 (-9.84355) (-0.302887)), _xyRadius = Radius 10, _xyAngle = Angle 10})
  (createCornerPointComposableSloped (NegXSlope 10) flatYSlope $ createCornerPointComposable (F1) (Point 0 0 0) (Radius 10) (Angle 10) )

createComposableTest = TestCase $ assertEqual
  "createComposableTest"
  (composableDefault {_cpoint = F1 (Point 1 2 3), _xyRadius = Radius 10, _xyAngle = Angle 10})
  (createComposable (F1(Point 1 2 3)) (Point 0 0 0) (Radius 10) (Angle 10))

createCornerPointTest = TestCase $ assertEqual
  "createCornerPointTest"
  ([F1 (Point 0 (-10) 0),
    F1 (Point 0 (10) 0)
   ])
  ([createCornerPoint (F1) (Point 0 0 0) (Radius 10) (Angle 0),
    createCornerPoint (F1) (Point 0 0 0) (Radius 10) (Angle 180)
   ]
  )
