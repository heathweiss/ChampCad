module LineScannerTest(lineScannerTestDo) where

import Test.HUnit

import Scan.LineScanner(LineScan(..), Measurement(..), uniqueScanName, getMinHeight, adjustHeight,
                        adjustMeasurementHeightsToStartAtZero, measurementsToFrontLines, adjustRadius)

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..))

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

lineScannerTestDo = do
  putStrLn "\n\nlineScannerTestDo"
  runTestTT tryToMakeAMeasurement
  runTestTT getMinHeightTest
  runTestTT adjustHeightTest
  runTestTT adjustMeasurementHeightsToStartAtZeroTest
  runTestTT measurementsToFrontLinesTestBack
  runTestTT adjustMeasurementHeightsToStartAtZeroTest2
  runTestTT measurementsToFrontLinesTest2Back
  runTestTT adjustMeasurementHeightsToStartAtZeroTest3
  runTestTT measurementsToFrontLinesTest
  runTestTT measurementsToFrontLinesTest2
  runTestTT adjustRadiusTest
  runTestTT makeFirstTwoBottomFrontLines
  runTestTT makeFirstTwoBackBottomLines

makeFirstTwoBackBottomLines  = TestCase $ assertEqual
  "make the 1st 2 BackBottomLines of geox flex shoe"
  [BackBottomLine {b1 = Point {x_axis = 4.349071563108143, y_axis = -49.7101154347781, z_axis = 1.7999999999999972},
                   b4 = Point {x_axis = 0.0, y_axis = -50.3, z_axis = 1.7999999999999972}},
   BackBottomLine {b1 = Point {x_axis = 8.439301434612814, y_axis = -47.86165679639331, z_axis = 1.7999999999999972},
                   b4 = Point {x_axis = 4.349071563108143, y_axis = -49.7101154347781, z_axis = 1.7999999999999972}},
   BackBottomLine {b1 = Point {x_axis = 12.708015114533769, y_axis = -47.426958070793255, z_axis = 0.0},
                   b4 = Point {x_axis = 8.439301434612814, y_axis = -47.86165679639331, z_axis = 1.7999999999999972}}]



  (measurementsToFrontLines (B4) (B1) [(Measurement (toSqlKey 1) 39.5 0 50.3), (Measurement (toSqlKey 2) 39.5 5 49.9),
                                         (Measurement (toSqlKey 3) 39.5 10 48.6), (Measurement (toSqlKey 4) 37.7 15 49.1)]
   --id height degree radius
  )


makeFirstTwoBottomFrontLines  = TestCase $ assertEqual
  "make the 1st 2 BottomFrontLines of geox flex shoe"
  [BottomFrontLine {f1 = Point {x_axis = 8.70685870049105, y_axis = -99.51985033936539, z_axis = 1.7999999999999972},
                    f4 = Point {x_axis = 0.0, y_axis = -100.3, z_axis = 1.7999999999999972}},
   BottomFrontLine {f1 = Point {x_axis = 17.12171031795933, y_axis = -97.1020444470037, z_axis = 1.7999999999999972},
                    f4 = Point {x_axis = 8.70685870049105, y_axis = -99.51985033936539, z_axis = 1.7999999999999972}},
   BottomFrontLine {f1 = Point {x_axis = 25.648967369659804, y_axis = -95.72324938524666, z_axis = 0.0},
                    f4 = Point {x_axis = 17.12171031795933, y_axis = -97.1020444470037, z_axis = 1.7999999999999972}}]

  (measurementsToFrontLines (F4) (F1) [(Measurement (toSqlKey 1) 39.5 0 100.3), (Measurement (toSqlKey 2) 39.5 5 99.9),
                                         (Measurement (toSqlKey 3) 39.5 10 98.6), (Measurement (toSqlKey 4) 37.7 15 99.1)]
   --id height degree radius
  )

  
  
tryToMakeAMeasurement = TestCase $ assertEqual
  "can I make a measurement by hand"
  (Measurement (toSqlKey 1) 1 1 1)
  (Measurement (toSqlKey 1) 1 1 1)

getMinHeightTest  = TestCase $ assertEqual
  "get min height of [Measurement]"
  (1)
  (let list = [(Measurement (toSqlKey 1) 1 1 1), (Measurement (toSqlKey 2) 2 1 1)]
   in
   getMinHeight list
  )

adjustHeightTest  = TestCase $ assertEqual
  "adjust the height of Measurement"
  (Measurement (toSqlKey 1) 6 1 1)
  (adjustHeight 5 $ Measurement (toSqlKey 1) 1 1 1)

adjustMeasurementHeightsToStartAtZeroTest = TestCase $ assertEqual
  "adjust the height of [Measurement] to start at 0"
  [(Measurement (toSqlKey 1) 0 1 1), (Measurement (toSqlKey 2) 1 1 1)]
  (adjustMeasurementHeightsToStartAtZero [(Measurement (toSqlKey 1) 1 1 1), (Measurement (toSqlKey 2) 2 1 1)])

adjustMeasurementHeightsToStartAtZeroTest2 = TestCase $ assertEqual
  "adjust the height of [Measurement] to start at 0"
  [(Measurement (toSqlKey 1) 0 1 1), (Measurement (toSqlKey 2) 1 1 1)]
  (adjustMeasurementHeightsToStartAtZero [(Measurement (toSqlKey 1) 0 1 1), (Measurement (toSqlKey 2) 1 1 1)])

adjustMeasurementHeightsToStartAtZeroTest3 = TestCase $ assertEqual
  "adjust the height of [Measurement] to start at 0"
  [(Measurement (toSqlKey 1) 0 1 1), (Measurement (toSqlKey 2) 1 1 1)]
  (adjustMeasurementHeightsToStartAtZero [(Measurement (toSqlKey 1) (-1) 1 1), (Measurement (toSqlKey 2) 0 1 1)])

measurementsToFrontLinesTest = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [F3 $ Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}]
  (measurementsToFrontLines (F3) (F2)  [(Measurement (toSqlKey 1) 1 1 1)]
  )

measurementsToFrontLinesTestBack = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [B4 $ Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}]
  (measurementsToFrontLines (B4) (B1) [(Measurement (toSqlKey 1) 1 1 1)]
  )


measurementsToFrontLinesTest2 = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [FrontTopLine {f2 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 1.0},
                 f3 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}}]
  (measurementsToFrontLines (F3) (F2) [(Measurement (toSqlKey 1) 1 1 1), (Measurement (toSqlKey 2) 2 1 1)]
  )

measurementsToFrontLinesTest2Back = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [BackBottomLine {b1 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 1.0},
                 b4 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}}]
  (measurementsToFrontLines (B4) (B1) [(Measurement (toSqlKey 1) 1 1 1), (Measurement (toSqlKey 2) 2 1 1)]
  )

adjustRadiusTest = TestCase $ assertEqual
  "adust radius of Measurement"
  (Measurement (toSqlKey 1) 0 1 6)
  (adjustRadius 5 $ Measurement (toSqlKey 1) 0 1 1)
