module ListHelpersTest(listHelperTestDo) where
import Test.HUnit
import Helpers.List((++:), SplitAtT(..), splitAndReverseBackHalf )

listHelperTestDo = do
  putStrLn "\n\nListHelpersTest"
  runTestTT addTest
  runTestTT splitTest

addTest = TestCase $ assertEqual
  ("hello")
  ([[1,2,3],[4,5,6],[7,8,9]])
  ([[1,2,3], [4,5,6]] ++: [7,8,9])

splitTest  = TestCase $ assertEqual
  "split a valid list and reverse the 2nd half"
  ([1,2,3],[6,5,4])
  (splitAndReverseBackHalf 3 [1,2,3,4,5,6])
