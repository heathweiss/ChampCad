
module Tests.BuilderTest(builderTestDo ) where
import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++), (+++>), (|+++|), (|@+++#@|), (@+++#@) )
import CornerPoints.Points (Point(..))
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..))

import Stl.StlCornerPoints(Faces(..))
import Stl.StlCornerPointsWithDegrees( (|@~?+++^|), FacesWithRange(..))

import Builder.Builder(CornerPointsBuilder(..), (&+++#@),   )


builderTestDo = do
  -- CornerPointsBuilder tests----
  
  
  let cubePoints = (BottomFace
              {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}})
              @+++#@
              ((transposeZ(+1)) . upperFaceFromLowerFace)

      cubePointsWithDegrees = CubesWithStartEndDegrees cubePoints (DegreeRange 0.0 10.0)
             
  {-
  let getFacesWithRange = TestCase $ assertEqual
        "Get the CubesWithStartEndDegrees that are within range"
        3
        ( let cubes =
               [ CubesWithStartEndDegrees cubePoints (DegreeRange 0.0 10.0),
                 CubesWithStartEndDegrees cubePoints (DegreeRange 10.0 20.0),
                 CubesWithStartEndDegrees cubePoints (DegreeRange 20.0 30.0)
               ]
          in 
        )
 -}
  
  --a very straight forward application
  let cornerPointsBldrTest1 = TestCase $ assertEqual
        "cornerPointsBldrTest1"
        (CornerPointsBuilder [[CubePoints {f1 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}, f2 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 4.0},
                                     f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 3.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                                     b1 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}, b2 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 4.0},
                                     b3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 3.0}, b4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}],
                        [BottomFace {b1 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0}, f1 = Point {x_axis = 2.0, y_axis = 2.0, z_axis = 2.0},
                                     b4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}]])
        (
          (CornerPointsBuilder [[(BottomFace (Point 2 2 2) (Point 2 2 2) (Point 1 1 1) (Point 1 1 1))]])
          &+++#@ (|@+++#@| ((transposeZ(+2)) . upperFaceFromLowerFace) )
        )
  runTestTT cornerPointsBldrTest1

  --extract the faces from the previous [CornerPoints] and add them to another set of faces
  let cornerPointsBldrTest2 = TestCase $ assertEqual
        "cornerPointsBldrTest2"
        (CornerPointsBuilder [[(CubePoints { f1 = Point {x_axis = 0, y_axis = 1, z_axis = 1.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 2.0},
                                     f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 2.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                                     b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 2.0},
                                     b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 2.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}})],
                        
                        [cubePoints]
                       ]
        )
        (CornerPointsBuilder [[cubePoints]] &+++#@
          (|+++| [(TopFace (Point 0 0 2) (Point 0 1 2) (Point 1 0 2) (Point 1 1 2))]) 
        )
  runTestTT cornerPointsBldrTest2


{------------------------------------------ no need to export @~+++^, but leave this test here for future use-----------------------------------------}
{-
  --make stl triangles from a CornerPointsWithDegrees
  let trianglesFromASingleCornerPointsWithDegrees = TestCase $ assertEqual
        "trianglesFromASingleCornerPointsWithDegrees"
        ( "[Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0}}},Triangle {v1 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0}}, v2 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}}, v3 = Vertex {vertexName = \"vertex \", point = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}}}]")
        (show $ FacesAll @~+++^  cubePointsWithDegrees)
  runTestTT trianglesFromASingleCornerPointsWithDegrees
-}
