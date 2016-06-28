import BuilderMonadTest(builderMonadTest)
import SlicerTest (splicerTestDo)

--main :: IO ()
main = do
  --splicerTestDo
  --putStrLn "Test suite not yet implemented"
  splicerTestDo
  builderMonadTest
