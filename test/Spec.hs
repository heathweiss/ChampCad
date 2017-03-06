import BuilderMonadTest(builderMonadTest)
import SlicerTest (splicerTestDo)
import CornerPointsCreateTest(cornerPointsCreateTestDo )
import CornerPointsDebugTest(cornerPointsDebugTestDo)
import CornerPointsTest(cornerPointsTestDo )
import CornerPointsWithDegreesTest(cornerPointsWithDegreesTest)
import CubicalTest(cubicalTestDo)
import FaceExtractAndConvertTest(faceExtractAndConvertTestDo)
import HorizontalFacesTest (horizontalFacesTestDo)
import JsonTest(jsonTestDo)
import ListHelpersTest(listHelperTestDo)
import MathPolarTest (mathPolarTestDo)
import MeshGenerationTest(meshGenerationTest)
--does this get put back in when radial scanner is made again.
--import ParseAttoTest(parseAttoTestDo)
import ParseJuicyTest (parseJuicyTestDo)
import RadiusTest(radisuTestDo)
import RotationsTest(rotationsTestDo)
import ScanFilterTest (scanFilterTestDo)
import VerticalFacesTest(verticalFacesTestDo)
import SequenceTest(sequenceTestDo)
import GeometryRadiusTest(geometryRadiusTestDo)
import AngleTest(angleTestDo)
import VertexTest(vertexTestDo)

--main :: IO ()
main = do
  splicerTestDo
  builderMonadTest
  cornerPointsCreateTestDo
  cornerPointsDebugTestDo
  cornerPointsTestDo
  cornerPointsWithDegreesTest
  cubicalTestDo
  faceExtractAndConvertTestDo  
  horizontalFacesTestDo
  jsonTestDo
  listHelperTestDo
  mathPolarTestDo
  meshGenerationTest
  parseJuicyTestDo
  radisuTestDo
  rotationsTestDo
  scanFilterTestDo
  verticalFacesTestDo
  sequenceTestDo
  geometryRadiusTestDo
  angleTestDo
  vertexTestDo
