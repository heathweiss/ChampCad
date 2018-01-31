module ListHelpersTest(listHelperTestDo) where
import Test.HUnit
import Helpers.List((++:), SplitAtT(..), splitAndReverseBackHalf, removeEmpty )

listHelperTestDo = do
  putStrLn "\n\nListHelpersTest"
  runTestTT addTest
  runTestTT splitTest

  runTestTT removeEmptyListsTest1
  runTestTT removeEmptyListsTest2
  runTestTT removeEmptyListsTest3
  runTestTT removeEmptyListsTest4

addTest = TestCase $ assertEqual
  ("hello")
  ([[1,2,3],[4,5,6],[7,8,9]])
  ([[1,2,3], [4,5,6]] ++: [7,8,9])

splitTest  = TestCase $ assertEqual
  "split a valid list and reverse the 2nd half"
  ([1,2,3],[6,5,4])
  (splitAndReverseBackHalf 3 [1,2,3,4,5,6])


-- ======================================== remove empty lists =======================================================
--these need to be moved to list helper tests, once removeEmpty is moved there.
removeEmptyListsTest1 = TestCase $ assertEqual
  "removeEmptyListsTest1"
   ([["not an empty list"]])
   (removeEmpty [["not an empty list"]]
   )

removeEmptyListsTest2 = TestCase $ assertEqual
  "removeEmptyListsTest2"
   ([["not an empty list"]])
   (removeEmpty [[],["not an empty list"]]
   )

removeEmptyListsTest3 = TestCase $ assertEqual
  "removeEmptyListsTest3"
   ([["not an empty list"]])
   (removeEmpty [[],["not an empty list"],[]]
   )

removeEmptyListsTest4 = TestCase $ assertEqual
  "removeEmpty: [[], []]"
  (0)
  (length $ removeEmpty [[],[]])
