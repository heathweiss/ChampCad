module VertexTest(vertexTestDo) where

import Test.HUnit

import Geometry.Vertex(getXWithQuadrant, getYWithQuadrant, Vertex(..), adjustPointAxis)
import Geometry.Angle(Angle(..))

import Data.Typeable
import Data.Data(toConstr)

import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))

vertexTestDo = do
  putStrLn "\n\n" 
  putStrLn "vertexTestDo tests"
  runTestTT quad1XFromAngleTest
  runTestTT quad1XFromQuadAngleTest
  runTestTT quad2XFromAngleTest
  runTestTT quad2XFromQuadAngleTest
  runTestTT quad3XFromAngleTest
  runTestTT quad3XFromQuadAngleTest
  runTestTT quad4XFromAngleTest
  runTestTT quad4XFromQuadAngleTest
  -- ============================
  runTestTT quad1YFromAngleTest
  runTestTT quad1YFromQuadAngleTest
  runTestTT quad2YFromAngleTest
  runTestTT quad2YFromQuadAngleTest
  runTestTT quad3YFromAngleTest
  runTestTT quad3YFromQuadAngleTest
  runTestTT quad4YFromAngleTest
  runTestTT quad4YFromQuadAngleTest
  -- ===============================
  runTestTT adjustPointQuad1XAxisTest
  runTestTT adjustPointQuad2XAxisTest
  runTestTT adjustPointQuad3XAxisTest
  runTestTT adjustPointQuad4XAxisTest
  runTestTT adjustPointQuad1YAxisTest
  runTestTT adjustPointQuad2YAxisTest
  runTestTT adjustPointQuad3YAxisTest
  runTestTT adjustPointQuad4YAxisTest

quad1XFromAngleTest = TestCase $ assertEqual
  "quad1XFromAngleTest"
  (Quad1X 0.7071067811865475)
  (getXWithQuadrant (Angle 45) (Radius 1)  )

quad1XFromQuadAngleTest = TestCase $ assertEqual
  "quad1XFromQuadAngleTest"
  (Quad1X 0.7071067811865475)
  (getXWithQuadrant (Quadrant1Angle 45) (Radius 1)  )


quad2XFromAngleTest = TestCase $ assertEqual
  "quad2XFromAngleTest"
  (Quad2X 0.984807753012208)
  (getXWithQuadrant (Angle 100) (Radius 1)  )

quad2XFromQuadAngleTest = TestCase $ assertEqual
  "quad2XFromQuadAngleTest"
  (Quad2X 0.984807753012208)
  (getXWithQuadrant (Quadrant2Angle 80) (Radius 1)  )

quad3XFromAngleTest = TestCase $ assertEqual
  "quad3XFromAngleTest"
  (Quad3X 0.984807753012208)
  (getXWithQuadrant (Angle 260) (Radius 1)  )

quad3XFromQuadAngleTest = TestCase $ assertEqual
  "quad3XFromQuadAngleTest"
  (Quad3X 0.984807753012208)
  (getXWithQuadrant (Quadrant3Angle 80) (Radius 1)  )

quad4XFromAngleTest = TestCase $ assertEqual
  "quad4XFromAngleTest"
  (Quad4X 0.984807753012208)
  (getXWithQuadrant (Angle 280) (Radius 1)  )

quad4XFromQuadAngleTest = TestCase $ assertEqual
  "quad4XFromQuadAngleTest"
  (Quad4X 0.984807753012208)
  (getXWithQuadrant (Quadrant4Angle 80) (Radius 1)  )



quad1YFromAngleTest = TestCase $ assertEqual
  "quad1YFromAngleTest"
  (Quad1Y 0.17364817766693041)
  (getYWithQuadrant (Angle 80) (Radius 1)  )

quad1YFromQuadAngleTest = TestCase $ assertEqual
  "quad1YFromQuadAngleTest"
  (Quad1Y (0.17364817766693041))
  (getYWithQuadrant (Quadrant1Angle 80) (Radius 1)  )


quad2YFromAngleTest = TestCase $ assertEqual
  "quad2YFromAngleTest"
  (Quad2Y 0.17364817766693041)
  (getYWithQuadrant (Angle 100) (Radius 1)  )

quad2YFromQuadAngleTest = TestCase $ assertEqual
  "quad2YFromQuadAngleTest"
  (Quad2Y 0.17364817766693041)
  (getYWithQuadrant (Quadrant2Angle 80) (Radius 1)  )

quad3YFromAngleTest = TestCase $ assertEqual
  "quad3YFromAngleTest"
  (Quad3Y 0.17364817766693041)
  (getYWithQuadrant (Angle 260) (Radius 1)  )

quad3YFromQuadAngleTest = TestCase $ assertEqual
  "quad3YFromQuadAngleTest"
  (Quad3Y 0.17364817766693041)
  (getYWithQuadrant (Quadrant3Angle 80) (Radius 1)  )

quad4YFromAngleTest = TestCase $ assertEqual
  "quad4YFromAngleTest"
  (Quad4Y 0.17364817766693041)
  (getYWithQuadrant (Angle 280) (Radius 1)  )

quad4YFromQuadAngleTest = TestCase $ assertEqual
  "quad4YFromQuadAngleTest"
  (Quad4Y 0.17364817766693041)
  (getYWithQuadrant (Quadrant4Angle 80) (Radius 1)  )


adjustPointQuad1XAxisTest = TestCase $ assertEqual
  "adjustPointQuad1XAxisTest"
  (Point 1 0 0)
  (adjustPointAxis (Quad1X 1) (Point 0 0 0))

adjustPointQuad2XAxisTest = TestCase $ assertEqual
  "adjustPointQuad2XAxisTest"
  (Point 1 0 0)
  (adjustPointAxis (Quad2X 1) (Point 0 0 0))

adjustPointQuad3XAxisTest = TestCase $ assertEqual
  "adjustPointQuad3XAxisTest"
  (Point (-1) 0 0)
  (adjustPointAxis (Quad3X 1) (Point 0 0 0))

adjustPointQuad4XAxisTest = TestCase $ assertEqual
  "adjustPointQuad4XAxisTest"
  (Point (-1) 0 0)
  (adjustPointAxis (Quad4X 1) (Point 0 0 0))

adjustPointQuad1YAxisTest = TestCase $ assertEqual
  "adjustPointQuad1YAxisTest"
  (Point 0 (-1) 0)
  (adjustPointAxis (Quad1Y 1) (Point 0 0 0))

adjustPointQuad2YAxisTest = TestCase $ assertEqual
  "adjustPointQuad2YAxisTest"
  (Point 0 1 0)
  (adjustPointAxis (Quad2Y 1) (Point 0 0 0))

adjustPointQuad3YAxisTest = TestCase $ assertEqual
  "adjustPointQuad3YAxisTest"
  (Point  0 1 0)
  (adjustPointAxis (Quad3Y 1) (Point 0 0 0))

adjustPointQuad4YAxisTest = TestCase $ assertEqual
  "adjustPointQuad4YAxisTest"
  (Point 0 (-1) 0)
  (adjustPointAxis (Quad4Y 1) (Point 0 0 0))
