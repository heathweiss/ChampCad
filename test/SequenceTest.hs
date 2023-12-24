{-# LANGUAGE ViewPatterns #-}
{- |
Used for building symmetrical sequences from one half of the symmetrical shape.
Eg: An oval can be described by giving the radius of the first 180 degrees, then reversing these first 180 degrees, and adding them on to the tail.
Note though, that the 180th degree would be there twice, which has to be handled. 
-}
module SequenceTest(sequenceTestDo) where
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Test.HUnit
import qualified Helpers.Symmetrical.Sequence as SS (mirror, mirrorPlusMidPoint, mirrorMinusMidPoint)
import qualified Helpers.Symmetrical.List as SL (mirror, mirrorPlusMidPoint, mirrorMinusMidPoint)
                                                     
sequenceTestDo = do
  putStrLn "" 
  putStrLn "SequenceTest"
  -- ================================================= Sequence =======================================
  let mirrorTestSeq = TestCase $ assertEqual
        "mirrorTestSeq"
        [1,2,3,3,2,1]
        ( 
          F.toList $ SS.mirror $ S.fromList [1,2,3]
        )
  runTestTT mirrorTestSeq

  let mirrorPlusMidPointSeqTest = TestCase $ assertEqual
        "mirrorPlusMidPointSeqTest"
        [1,2,3,2,1]
        ( 
          F.toList $ SS.mirrorPlusMidPoint (S.fromList [1,2]) 3
        )
  runTestTT mirrorPlusMidPointSeqTest

  let mirrorMinusMidPointSeqTest = TestCase $ assertEqual
        "mirrorMinusMidPointSeqTest"
        [1,2,3,2,1]
        ( 
          F.toList $ SS.mirrorMinusMidPoint (S.fromList [1,2,3])
        )
  runTestTT mirrorMinusMidPointSeqTest

  -- ================================================== List ===========================================
  let mirrorTestList = TestCase $ assertEqual
        "mirrorTestList"
        [1,2,3,3,2,1]
        ( 
           SL.mirror [1,2,3]
        )
  runTestTT mirrorTestList

  let mirrorPlusMidPointListTest = TestCase $ assertEqual
        "mirrorPlusMidPointListTest"
        [1,2,3,2,1]
        ( 
          SL.mirrorPlusMidPoint [1,2] 3
        )
  runTestTT mirrorPlusMidPointListTest

  let mirrorMinusMidPointListTest = TestCase $ assertEqual
        "mirrorMinusMidPointListTest"
        [1,2,3,2,1]
        ( 
           SL.mirrorMinusMidPoint [1,2,3]
        )
  runTestTT mirrorMinusMidPointListTest
