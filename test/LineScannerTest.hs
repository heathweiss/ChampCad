module LineScannerTest(lineScannerTestDo) where

import Test.HUnit

import Scan.LineScanner(LineScan(..), Measurement(..), uniqueScanName, getMinHeight, adjustHeight,
                        adjustMeasurementHeightsToStartAtZero, measurementsToLines, adjustRadius,
                        measurementToLinesWithRadiusAdj, linearBackToFrontTopFaces, linearLeftToRightBottomFaces,
                        linearBackToFrontBottomFaces, findIndiceOfMeasurementDegree, splitAndReverseBackMeasurementsAtDegree,
                        linearLeftToRightTopFaces)

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
  runTestTT measurementsToLinesTestBack
  runTestTT adjustMeasurementHeightsToStartAtZeroTest2
  runTestTT measurementsToLinesTest2Back
  runTestTT adjustMeasurementHeightsToStartAtZeroTest3
  runTestTT measurementsToLinesTest
  runTestTT measurementsToLinesTest2
  runTestTT adjustRadiusTest
  runTestTT makeFirstTwoBottomFrontLines
  runTestTT makeFirstTwoBackBottomLines
  runTestTT adjustSingleRadiusTest
  runTestTT adjustSingleRadiusTest2
  runTestTT adjustSingleRadiusTest3
  runTestTT adjustSingleRadiusBackTest
  runTestTT adjustSingleRadiusBackTest3
  runTestTT makeFirstTwoBackBottomLinesBack
  -- ========= radial system =============
  runTestTT buildFirstLineFrontToBackBottomFaces
  runTestTT buildFirstLineFrontToBackBottomFrontFaces
  runTestTT findIndexOfMeasurement
  runTestTT splitAndReverseBackMeasurementsAtDegreeTest
  runTestTT buildFirstLineLeftToRightBottomFaces
  runTestTT buildFirst2CubesLeftToRightBottomFaces
  runTestTT buildFirst2CubesLeftToRightTopFaces
-- ========================================================================================================================================================================
-- ========================================================================================================================================================================
-- ==================================================================radial system ========================================================================================
buildFirst2CubesLeftToRightTopFaces = TestCase $ assertEqual
  "build the first 2 TopFaces of left to right"
  --will need to be confirmed by viewing geoxFlex
  [TopFace {b2 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0},
            f2 = Point {x_axis = 30.64177772475912, y_axis = -25.711504387461574, z_axis = 0.0},
            b3 = Point {x_axis = 6.945927106677213, y_axis = -39.39231012048832, z_axis = 0.0},
            f3 = Point {x_axis = 12.855752193730785, y_axis = -15.32088886237956, z_axis = 0.0}},
   TopFace {b2 = Point {x_axis = 6.945927106677213, y_axis = -39.39231012048832, z_axis = 0.0},
            f2 = Point {x_axis = 12.855752193730785, y_axis = -15.32088886237956, z_axis = 0.0},
            b3 = Point {x_axis = 6.840402866513374, y_axis = -18.79385241571817, z_axis = 0.0},
            f3 = Point {x_axis = 19.999999999999996, y_axis = -34.64101615137755, z_axis = 0.0}}]

  
  (linearLeftToRightTopFaces 30 [Measurement (toSqlKey 3) 10 0 20, Measurement (toSqlKey 3) 10 10 40,
                                             Measurement (toSqlKey 3) 10 20 20, Measurement (toSqlKey 3) 10 30 40,
                                             Measurement (toSqlKey 3) 10 40 20, Measurement (toSqlKey 3) 10 50 40
                                            ])

buildFirst2CubesLeftToRightBottomFaces = TestCase $ assertEqual
  "build the first 2 BottomFacesof left to right"
  --will need to be confirmed by viewing geoxFlex
  
  [BottomFace {b1 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0},
               f1 = Point {x_axis = 30.64177772475912, y_axis = -25.711504387461574, z_axis = 0.0},
               b4 = Point {x_axis = 6.945927106677213, y_axis = -39.39231012048832, z_axis = 0.0},
               f4 = Point {x_axis = 12.855752193730785, y_axis = -15.32088886237956, z_axis = 0.0}},
   BottomFace {b1 = Point {x_axis = 6.945927106677213, y_axis = -39.39231012048832, z_axis = 0.0},
               f1 = Point {x_axis = 12.855752193730785, y_axis = -15.32088886237956, z_axis = 0.0},
               b4 = Point {x_axis = 6.840402866513374, y_axis = -18.79385241571817, z_axis = 0.0},
               f4 = Point {x_axis = 19.999999999999996, y_axis = -34.64101615137755, z_axis = 0.0}}
  ]
  
  (linearLeftToRightBottomFaces 30 [Measurement (toSqlKey 3) 10 0 20, Measurement (toSqlKey 3) 10 10 40,
                                             Measurement (toSqlKey 3) 10 20 20, Measurement (toSqlKey 3) 10 30 40,
                                             Measurement (toSqlKey 3) 10 40 20, Measurement (toSqlKey 3) 10 50 40
                                            ])

buildFirstLineLeftToRightBottomFaces = TestCase $ assertEqual
  "build the first BottomFace of left to right"
  --will need to be confirmed by viewing geoxFlex
  [BottomFace {b1 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0},
               f1 = Point {x_axis = 19.999999999999996, y_axis = -34.64101615137755, z_axis = 0.0},
               b4 = Point {x_axis = 6.945927106677213, y_axis = -39.39231012048832, z_axis = 0.0},
               f4 = Point {x_axis = 6.840402866513374, y_axis = -18.79385241571817, z_axis = 0.0}}]
  (linearLeftToRightBottomFaces 20 [Measurement (toSqlKey 3) 10 0 20, Measurement (toSqlKey 3) 10 10 40,
                                     Measurement (toSqlKey 3) 10 20 20, Measurement (toSqlKey 3) 10 30 40
                                    ]
  )

buildFirstLineFrontToBackBottomFaces = TestCase $ assertEqual
  "build the first BottomFace of front to back"
  
  [BottomFace {b1 = Point {x_axis = 19.999999999999996, y_axis = -34.64101615137755, z_axis = 0.0},
               f1 = Point {x_axis = 6.840402866513374, y_axis = -18.79385241571817, z_axis = 0.0},
               b4 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0},
               f4 = Point {x_axis = 6.945927106677213, y_axis = -39.39231012048832, z_axis = 0.0}}]
  

  
  (linearBackToFrontBottomFaces 20 [Measurement (toSqlKey 3) 10 0 20, Measurement (toSqlKey 3) 10 10 40,
                                             Measurement (toSqlKey 3) 10 20 20, Measurement (toSqlKey 3) 10 30 40
                                            ])

buildFirstLineFrontToBackBottomFrontFaces = TestCase $ assertEqual
  "build the first BottomFace of front to back"
  [TopFace {b2 = Point {x_axis = 19.999999999999996, y_axis = -34.64101615137755, z_axis = 0.0},
            f2 = Point {x_axis = 6.840402866513374, y_axis = -18.79385241571817, z_axis = 0.0},
            b3 = Point {x_axis = 0.0, y_axis = -20.0, z_axis = 0.0},
            f3 = Point {x_axis = 6.945927106677213, y_axis = -39.39231012048832, z_axis = 0.0}}]
  
  (linearBackToFrontTopFaces 20 [Measurement (toSqlKey 3) 10 0 20, Measurement (toSqlKey 3) 10 10 40,
                                             Measurement (toSqlKey 3) 10 20 20, Measurement (toSqlKey 3) 10 30 40
                                            ])

findIndexOfMeasurement = TestCase $ assertEqual
  "find the index of Measurement in [Measurement] by degree"
  (2)
  (findIndiceOfMeasurementDegree 10 [Measurement (toSqlKey 1) 10 0 20, Measurement (toSqlKey 2) 20 5 20, Measurement (toSqlKey 3) 30 10 20, Measurement (toSqlKey 3) 30 20 20])

splitAndReverseBackMeasurementsAtDegreeTest = TestCase $ assertEqual
  "split [Measurement] by degree"
  ([Measurement (toSqlKey 1) 10 0 20, Measurement (toSqlKey 2) 20 5 20],
   [Measurement (toSqlKey 3) 30 20 20, Measurement (toSqlKey 3) 30 10 20])

  (splitAndReverseBackMeasurementsAtDegree
     10
     [Measurement (toSqlKey 1) 10 0 20, Measurement (toSqlKey 2) 20 5 20, Measurement (toSqlKey 3) 30 10 20, Measurement (toSqlKey 3) 30 20 20]
  )
-- ========================================================================================================================================================================
-- ========================================================================================================================================================================
-- ==================================================================radial system ========================================================================================


adjustSingleRadiusTest = TestCase $ assertEqual
  "adjust the Radius of a single Measurement and create cPoint"
  ([F4 {f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}}])
  (measurementToLinesWithRadiusAdj 0.5 (F4) (F1) [Measurement (toSqlKey 3) 10 0 20])

adjustSingleRadiusBackTest = TestCase $ assertEqual
  "adjust the Radius of a single Measurement and create cPoint"
  [B4 {b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}}]
  (measurementToLinesWithRadiusAdj 0.5 (B4) (B1) [Measurement (toSqlKey 3) 10 0 20])

adjustSingleRadiusTest2 = TestCase $ assertEqual
  "adjust the Radius of 2 Measurement and create cPoints"
  [BottomFrontLine {f1 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 5.0}, f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}}]
  (measurementToLinesWithRadiusAdj 0.5 (F4) (F1) [Measurement (toSqlKey 3) 10 0 20, Measurement (toSqlKey 3) 20 0 20])

adjustSingleRadiusTest3 = TestCase $ assertEqual
  "adjust the Radius of 3 Measurement and create cPoints"
  [BottomFrontLine {f1 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 5.0},
                    f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}},
   BottomFrontLine {f1 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 9.999999999999998},
                    f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 5.0}}]
  (measurementToLinesWithRadiusAdj 0.5 (F4) (F1) [Measurement (toSqlKey 1) 10 0 20, Measurement (toSqlKey 2) 20 0 20, Measurement (toSqlKey 3) 30 0 20])

adjustSingleRadiusBackTest3 = TestCase $ assertEqual
  "adjust the Radius of 3 Measurement and create cPoints"
  [BackBottomLine {b1 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 5.0},
                   b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}},
   BackBottomLine {b1 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 9.999999999999998},
                   b4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 5.0}}]

  (measurementToLinesWithRadiusAdj 0.5 (B4) (B1) [Measurement (toSqlKey 1) 10 0 20, Measurement (toSqlKey 2) 20 0 20, Measurement (toSqlKey 3) 30 0 20])

makeFirstTwoBackBottomLines  = TestCase $ assertEqual
  "make the 1st 2 BackBottomLines of geox flex shoe"
  [BackBottomLine {b1 = Point {x_axis = 4.349071563108143, y_axis = -49.7101154347781, z_axis = 1.7999999999999972},
                   b4 = Point {x_axis = 0.0, y_axis = -50.3, z_axis = 1.7999999999999972}},
   BackBottomLine {b1 = Point {x_axis = 8.439301434612814, y_axis = -47.86165679639331, z_axis = 1.7999999999999972},
                   b4 = Point {x_axis = 4.349071563108143, y_axis = -49.7101154347781, z_axis = 1.7999999999999972}},
   BackBottomLine {b1 = Point {x_axis = 12.708015114533769, y_axis = -47.426958070793255, z_axis = 0.0},
                   b4 = Point {x_axis = 8.439301434612814, y_axis = -47.86165679639331, z_axis = 1.7999999999999972}}]



  (measurementsToLines (B4) (B1) [(Measurement (toSqlKey 1) 39.5 0 50.3), (Measurement (toSqlKey 2) 39.5 5 49.9),
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

  (measurementsToLines (F4) (F1) [(Measurement (toSqlKey 1) 39.5 0 100.3), (Measurement (toSqlKey 2) 39.5 5 99.9),
                                         (Measurement (toSqlKey 3) 39.5 10 98.6), (Measurement (toSqlKey 4) 37.7 15 99.1)]
   --id height degree radius
  )

makeFirstTwoBackBottomLinesBack  = TestCase $ assertEqual
  "make the 1st 2 BackBottomLines of geox flex shoe using measurementToLinesWithRadiusAdj"
  
  [BackBottomLine {b1 = Point {x_axis = 4.353429350245525, y_axis = -49.759925169682695, z_axis = 0.8999999999999988},
                   b4 = Point {x_axis = 0.0, y_axis = -50.15, z_axis = 0.89985510587711}},
   BackBottomLine {b1 = Point {x_axis = 8.560855158979665, y_axis = -48.55102222350185, z_axis = 0.8999999999999986},
                   b4 = Point {x_axis = 4.353429350245525, y_axis = -49.759925169682695, z_axis = 0.8999999999999988}},
   BackBottomLine {b1 = Point {x_axis = 12.824483684829902, y_axis = -47.86162469262333, z_axis = 0.0},
                   b4 = Point {x_axis = 8.560855158979665, y_axis = -48.55102222350185, z_axis = 0.8999999999999986}}
  ]

  (measurementToLinesWithRadiusAdj 0.5 (B4) (B1) [(Measurement (toSqlKey 1) 39.5 0 100.3), (Measurement (toSqlKey 2) 39.5 5 99.9),
                                         (Measurement (toSqlKey 3) 39.5 10 98.6), (Measurement (toSqlKey 4) 37.7 15 99.1)]
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

measurementsToLinesTest = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [F3 $ Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}]
  (measurementsToLines (F3) (F2)  [(Measurement (toSqlKey 1) 1 1 1)]
  )

measurementsToLinesTestBack = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [B4 $ Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}]
  (measurementsToLines (B4) (B1) [(Measurement (toSqlKey 1) 1 1 1)]
  )


measurementsToLinesTest2 = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [FrontTopLine {f2 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 1.0},
                 f3 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}}]
  (measurementsToLines (F3) (F2) [(Measurement (toSqlKey 1) 1 1 1), (Measurement (toSqlKey 2) 2 1 1)]
  )

measurementsToLinesTest2Back = TestCase $ assertEqual
  "convert [Measurement] to [CornerPoints]"
  [BackBottomLine {b1 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 1.0},
                 b4 = Point {x_axis = 1.745240643728351e-2, y_axis = -0.9998476951563913, z_axis = 0.0}}]
  (measurementsToLines (B4) (B1) [(Measurement (toSqlKey 1) 1 1 1), (Measurement (toSqlKey 2) 2 1 1)]
  )

adjustRadiusTest = TestCase $ assertEqual
  "adust radius of Measurement"
  (Measurement (toSqlKey 1) 0 1 6)
  (adjustRadius 5 $ Measurement (toSqlKey 1) 0 1 1)
