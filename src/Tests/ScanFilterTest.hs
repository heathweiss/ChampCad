module Tests.ScanFilterTest (scanFilterTestDo) where
import Test.HUnit
import Scan.Filter(runningAverage, averageValueOf)
import CornerPoints.Radius(Radius(..))

scanFilterTestDo = do
  runTestTT filter2Test
  runTestTT filter3Test

  runTestTT averageValueOfTest
  
  


filter2Test = TestCase $ assertEqual
  ("filter at at least 2 Test")
  (map (Radius)[1,1.5,2.5])
  (runningAverage 2 [(Radius 1),(Radius 2),(Radius 3)])

filter3Test = TestCase $ assertEqual
  ("filter at at least 3 Test")
  (map (Radius)[1,1.33,2,5,11,20])
  (runningAverage 3 [(Radius 1),(Radius 2),(Radius 3), (Radius 10),(Radius 20),(Radius 30)])

averageValueOfTest = TestCase $ assertEqual
  ("averageValueOf test")
  (Radius 4)
  (averageValueOf $ map (Radius) [2,4,6])


