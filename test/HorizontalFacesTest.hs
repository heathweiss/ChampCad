{-# LANGUAGE ParallelListComp #-}
module HorizontalFacesTest (horizontalFacesTestDo) where
import Test.HUnit
import CornerPoints.HorizontalFaces(createBottomFaces, createBottomFacesWithVariableSlope, createTopFaces,)
import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Create(flatXSlope,flatYSlope, Slope(..), Angle(..))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))

horizontalFacesTestDo = do
    --------------------bottom/top faces------------------
  putStrLn "\n\n" 
  putStrLn "createBottom/TopFaces tests"
  runTestTT createBottomFacesTest
  runTestTT createBottomFacesTest'
  runTestTT createBottomFacesWithVariableSlopeTest
  runTestTT createTopFacesTest


createTopFacesTest = TestCase $ assertEqual
  "createTopFaces"
  ([TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f2 = Point {x_axis = 0.34904812874567026, y_axis = -19.996953903127825, z_axis = 0.0}, b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f3 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}},TopFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f2 = Point {x_axis = 1.0469849010750292, y_axis = -29.981724810572874, z_axis = 0.0}, b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f3 = Point {x_axis = 0.34904812874567026, y_axis = -19.996953903127825, z_axis = 0.0}}])
  (createTopFaces (Point 0 0 0) (map (Radius) [10, 20, 30]) (map (Angle)[0, 1, 2])   {-flatXSlope flatYSlope-})

-- ==================================================createBottomFaces======================================


createBottomFacesTest = TestCase $ assertEqual 
  "createBottomFaces"
  ([BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f1 = Point {x_axis = 0.34904812874567026, y_axis = -19.996953903127825, z_axis = 0.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}},BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f1 = Point {x_axis = 1.0469849010750292, y_axis = -29.981724810572874, z_axis = 0.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f4 = Point {x_axis = 0.34904812874567026, y_axis = -19.996953903127825, z_axis = 0.0}}])
  (createBottomFaces (Point 0 0 0) (map (Radius) [10, 20, 30]) (map (Angle)[0, 1, 2])   {-flatXSlope flatYSlope-})

createBottomFacesTest' = TestCase $ assertEqual 
  "createBottomFaces'"
  ([BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f1 = Point {x_axis = 0.34904812874567026, y_axis = -19.996953903127825, z_axis = 0.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}},BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f1 = Point {x_axis = 1.0469849010750292, y_axis = -29.981724810572874, z_axis = 0.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, f4 = Point {x_axis = 0.34904812874567026, y_axis = -19.996953903127825, z_axis = 0.0}}])
  (createBottomFaces (Point 0 0 0) (map (Radius) [10, 20, 30]) (map (Angle)[0, 1, 2])   )

-- ===================================== createBottomFacesWithVariableSlope===========================================


createBottomFacesWithVariableSlopeTest = TestCase $ assertEqual
  "createBottomFacesWithVariableSlopeSimplified test"
  ( [BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                 f1 = Point {x_axis = 0.34884291803342393, y_axis = -19.985197389297014,
                             z_axis = -0.6857068829855988},
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis =0.0},
                 f4 = Point {x_axis = 0.0, y_axis = -9.998476951563912,
                             z_axis = -0.17452406437283513}},
     BottomFace {b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                 f1 = Point {x_axis = 1.045650113906792,
                             y_axis = -29.943501507144447, z_axis = -1.5143755580849436},
                 b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                 f4 = Point {x_axis = 0.34884291803342393,
                             y_axis = -19.985197389297014,
                             z_axis = -0.6857068829855988}}])

  (createBottomFacesWithVariableSlope
    (Point 0 0 0) (map (Radius) [10, 20, 30]) (map (Angle)[0, 1, 2])
    [(PosXSlope 1),(PosXSlope 2),(PosXSlope 3)]
    [(PosYSlope 1),(PosYSlope 2),(PosYSlope 3)]     )

