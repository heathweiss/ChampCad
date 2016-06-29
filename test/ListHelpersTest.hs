module ListHelpersTest(listHelperTestDo) where
import Test.HUnit
import Helpers.List((++:))

listHelperTestDo = do
  runTestTT addTest

addTest = TestCase $ assertEqual
  ("hello")
  ([[1,2,3],[4,5,6],[7,8,9]])
  ([[1,2,3], [4,5,6]] ++: [7,8,9])
