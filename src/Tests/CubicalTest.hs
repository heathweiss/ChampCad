module Tests.CubicalTest(cubicalTestDo) where

import Test.HUnit

import Cubical.Cubical(CubicalInput(..), createXaxisLine, zDownSlope, adjustWidth)

import TypeClasses.Transposable( transposeX, transposeY, transposeZ)

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..))

cubicalTestDo = do
  --------------------------------------------- transpose -----------------------------------------------
  let transposeYPositiveTest = TestCase $ assertEqual
        "transposeYPositiveTest"
        (CubeIn (B2(Point 0 10 0)) 1)
        (transposeY (+10) (CubeIn (B2(Point 0 0 0)) 1)
        )

  runTestTT transposeYPositiveTest

  --note the need for a lamda as ((-)10) gives the wrong result.
  let transposeYNegativeTest = TestCase $ assertEqual
        "transposeYNegativeTest"
        (CubeIn (B2(Point 0 (-10) 0)) 1)
        (transposeY (\y -> y - 10) (CubeIn (B2(Point 0 0 0)) 1)
        )

  runTestTT transposeYNegativeTest
