{-# LANGUAGE ParallelListComp #-}
module RadiusTest(radisuTestDo) where
import Test.HUnit
import CornerPoints.Radius(Radius(..), SingleDegreeRadii(..), Degree(..), MultiDegreeRadii(..), resetMultiDegreeRadiiIfNull,
                          extractSingle, extractList, rotateMDR, setRadiusIfNull,  resetSingleDegreeRadiiIfNull,
                          setRadiusWithPrecedingValueIfNull, resetMultiDegreeRadiiIfNullWithPreviousValue, transposeSDRList,
                          transposeMDRList, extractSDRWithinRange, transformSDRWithList, extractMaybeRadii, extractMaybeSDR,
                          singleDegreeRadiiListToMap)

import TypeClasses.Transposable(transpose, transposeWithList )
import Scan.ParseJuicy(averageValueOf)

import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Foldable as F

import qualified Flow as Flw

import Control.Lens


radisuTestDo = do
 runTestTT transposeListOfRadiusWithFlowTest
 runTestTT transposeSDRWithFlowTest
 runTestTT transposeMDRWithFlowTest
 runTestTT createMapOfSingleDegreeRadiiTest
 runTestTT singleDegreeRadiiListToMapTest
 runTestTT extractSDRWithRangeTest
 runTestTT transposeSDRListInRangeTest
  
 runTestTT extractRadiusFromMultiDegreeRadiiTest
 runTestTT extractRadiusFromMultiDegreeRadiiTest2
 runTestTT extractRadiusFromSinleDegreeRadiiTest
 runTestTT extractRadiusFromSinleDegreeRadiiTest2

 runTestTT transposeRadiusTest
 runTestTT transposeSDRTest
 runTestTT transposeSDRTest2
 runTestTT transposeListOfRadiusWithListOfFxTest
 runTestTT transposeSDRWithListTest
 runTestTT transposeMDRWithListTest

 runTestTT rotateMultiDegreeRadiiTest

 runTestTT setRadiusIfNullTest
 runTestTT setRadiusIfNotNullTest
 runTestTT setRadiusWithPrecedingRadiusIfNullTest
 runTestTT setRadiusWithPrecedingRadiusIfNullTest2
 runTestTT setRadiusWithPrecedingRadiusIfNullTest3
 

 runTestTT resetSingleDegreeRadiiNaNTest
 runTestTT resetMultiDegreeRadiiNaNTest
 runTestTT resetMultiDegreeRadiiNullWithPreviousValueTest

-- =============================== work on transposing an mdr using >><<===================
{-
(>><<) :: a -> (a -> b) -> b
a >><< f = f a
-}
transposeListOfRadiusWithFlowTest = TestCase $ assertEqual 
  "transposeListOfRadiusTest"
  (map Radius [1.0, 4.0, 9.0])
  (transposeWithList
     --[(*2), (*2),(*2)]
     (S.fromList [(*1)]
     Flw.|> (\seq -> seq S.>< (S.fromList [(*x)| x <-[2,3]]) )
     Flw.|> (\seq -> F.toList seq)
     )
     (map Radius [1.0, 2.0, 3.0])
  )

transposeSDRWithFlowTest = TestCase $ assertEqual 
  "transposeSDRWithFlowTest"
  (SingleDegreeRadii 0.0 (map Radius [1.0, 4.0, 9.0]))
  (
    let sdr = SingleDegreeRadii 0.0 (map Radius [1.0, 2.0, 3.0])
    in
        S.fromList [(*1)]
        Flw.|> (\seq -> seq S.>< (S.fromList [(*x)| x <-[2,3]]) )
        Flw.|> (\seq -> F.toList seq)
        Flw.|>(\list -> transformSDRWithList list sdr  )
  )


{-Create an MultiDegreeRadii.
  Transpose each SingleDegreeRadii, using a separate transpose function list for each SDR.
-}
transposeMDRWithFlowTest = TestCase $ assertEqual 
  "transposeMDRWithFlowTest"
  (MultiDegreeRadii "myMDR" [SingleDegreeRadii 0.0 (map Radius [1.0, 4.0, 9.0]),
                             SingleDegreeRadii 1.0 (map Radius [40.0, 100.0, 180.0])
                            ])
  {--}
  (let mdr = MultiDegreeRadii "myMDR" [
                                       SingleDegreeRadii 0.0 (map Radius [1.0, 2.0, 3.0]),
                                       SingleDegreeRadii 1.0 (map Radius [4.0, 5.0, 6.0])
                                      ]
       sdrMap = singleDegreeRadiiListToMap $ degrees mdr
   in
       --sdr0 transpose 
       (([(*1)] ++ [(*x)| x <-[2,3]]))
       Flw.|> (\listFx -> S.fromList $ [transformSDRWithList listFx (extractMaybeSDR (sdrMap^.at 0.0)) ])
       --sdr1 transpose
       Flw.|> (\seqSDR -> (seqSDR,(([(*10)] ++ [(*x)| x <-[20,30]]))))
       Flw.|> (\(seqSDR, listFx) -> (seqSDR, transformSDRWithList listFx (extractMaybeSDR (sdrMap^.at 1.0)) ) )
       Flw.|> (\(seqSDR, sdrAtDegree1) -> F.toList (seqSDR S.|> sdrAtDegree1) )
       --rebuild the mdr
       Flw.|> (\sdr -> mdr {degrees = sdr})
  )


extractSDRWithRangeTest = TestCase $ assertEqual
  "extractSDRWithRange"
  (
   [(SingleDegreeRadii 0.0 [Radius 0]),
    (SingleDegreeRadii 1.0 [Radius 1])
   ]
  )
  (let sdrList = [(SingleDegreeRadii 0.0 [Radius 0]),
                  (SingleDegreeRadii 1.0 [Radius 1]),
                  (SingleDegreeRadii 2.0 [Radius 2])
                 ]
       range = [0.0,1.0]
   in
       extractSDRWithinRange range sdrList
  )

{-
given a range of degrees [Double],and a list of SDR  [SingleDegreeRadii], and a list of transpose functions  [(Double -> Double)]
extract the SDR that are in range
do the transormation on SDR in range
return the transformed SDR
-}
transposeSDRListInRangeTest = TestCase $ assertEqual
  "transposeSDRListInRangeTest"
  ([(SingleDegreeRadii 0.0 [Radius 0]),
    (SingleDegreeRadii 1.0 [Radius 10])
    
   ])
  (let sdrList = [(SingleDegreeRadii 0.0 [Radius 0]),
                  (SingleDegreeRadii 1.0 [Radius 1]),
                  (SingleDegreeRadii 2.0 [Radius 2])
                 ]
       range = [0.0,1.0]
       transformFx = [(*10) | x <- [1..]]
   in
       transposeSDRList [transformFx | x <- [1..]]  (extractSDRWithinRange range sdrList)
       
  )

createMapOfSingleDegreeRadiiTest = TestCase $ assertEqual 
  "createMapOfSingleDegreeRadiiTest"
  --(SingleDegreeRadii 0.0 [Radius 0])
  (SingleDegreeRadii 0.0 [Radius 0])
  
  (let sdrList = [(SingleDegreeRadii 0.0 [Radius 0]),
                  (SingleDegreeRadii 1.0 [Radius 1])
                 ]
       sdrMap = singleDegreeRadiiListToMap sdrList
   in  extractMaybeSDR(sdrMap^.at 0.0) 
  )







singleDegreeRadiiListToMapTest = TestCase $ assertEqual 
  "singleDegreeRadiiListToMapTest"
  (SingleDegreeRadii 0.0 (map Radius [1.0, 2.0, 3.0]))
  (let sdrList = [
                  SingleDegreeRadii 0.0 (map Radius [1.0, 2.0, 3.0]),
                  SingleDegreeRadii 2.0 (map Radius [4.0, 5.0, 6.0])
                 ]
       sdrMap = singleDegreeRadiiListToMap sdrList
   in extractMaybeSDR $ sdrMap^.at (0.0)
  )
-- =============================== end: work on transposing an mdr using Flow ===================
resetMultiDegreeRadiiNaNTest  = TestCase $ assertEqual 
  "resetMultiDegreeRadiiNaNTest"
  (MultiDegreeRadii
   "myName"
   [(SingleDegreeRadii 0 (map (Radius) [1,2])),
    (SingleDegreeRadii 0 (map (Radius) [3,44]))
   ]
  )
  (resetMultiDegreeRadiiIfNull 44
   
    ( MultiDegreeRadii
      "myName"
      [(SingleDegreeRadii 0 [Radius 1, Radius 2]),
       (SingleDegreeRadii 0 [Radius 3, Radius (averageValueOf [])])
      ]
    )
  )

resetMultiDegreeRadiiNullWithPreviousValueTest  = TestCase $ assertEqual 
  "resetMultiDegreeRadiiNullWithPreviousValueTest"
  (MultiDegreeRadii
   "myName"
   [(SingleDegreeRadii 0 (map (Radius) [1,2])),
    (SingleDegreeRadii 0 (map (Radius) [3,3]))
   ]
  )
  (resetMultiDegreeRadiiIfNullWithPreviousValue 44
   
    ( MultiDegreeRadii
      "myName"
      [(SingleDegreeRadii 0 [Radius 1, Radius 2]),
       (SingleDegreeRadii 0 [Radius 3, Radius (averageValueOf [])])
      ]
    )
  )

resetSingleDegreeRadiiNaNTest  = TestCase $ assertEqual 
  "resetSingleDegreeRadiiNaNTest"
  (SingleDegreeRadii 0 (map (Radius) [1,2]))
  (resetSingleDegreeRadiiIfNull 3 (SingleDegreeRadii 0 [Radius 1, Radius 2]))


 
setRadiusIfNullTest = TestCase $ assertEqual 
  "setRadiusIfNullTest"
  (Radius 0)
  (setRadiusIfNull 0 $ Radius $ averageValueOf [])

setRadiusWithPrecedingRadiusIfNullTest = TestCase $ assertEqual 
  "setRadiusWithPrecedingRadiusIfNullTest"
  ([Radius 0, Radius 1])
  (setRadiusWithPrecedingValueIfNull 0  [Radius $ averageValueOf [], Radius 1])

setRadiusWithPrecedingRadiusIfNullTest2 = TestCase $ assertEqual 
  "setRadiusWithPrecedingRadiusIfNullTest2"
  ([Radius 1, Radius 1])
  (setRadiusWithPrecedingValueIfNull 2  [ Radius 1, Radius $ averageValueOf []])

setRadiusWithPrecedingRadiusIfNullTest3 = TestCase $ assertEqual 
  "setRadiusWithPrecedingRadiusIfNullTest3"
  ([Radius 1, Radius 2])
  (setRadiusWithPrecedingValueIfNull 3  [ Radius 1, Radius 2])

setRadiusIfNotNullTest = TestCase $ assertEqual 
  "setRadiusIfNotNullTest"
  (Radius 2)
  (setRadiusIfNull 1 $ Radius 2)

{-The first and last [Radius] must always match, which is why a [Radius0] was eliminated, and an extra [Radius 20] was created.-}
rotateMultiDegreeRadiiTest = TestCase $ assertEqual 
  "rotateMultiDegreeRadiiTest"
  (MultiDegreeRadii "name" [SingleDegreeRadii 0 [Radius 20], SingleDegreeRadii 10 [Radius 0], SingleDegreeRadii 20 [Radius 10],SingleDegreeRadii 30 [Radius 20]])
  (rotateMDR  (MultiDegreeRadii "name" [SingleDegreeRadii 0 [Radius 0], SingleDegreeRadii 10 [Radius 10], SingleDegreeRadii 20 [Radius 20],SingleDegreeRadii 30 [Radius 0]]))
  

transposeRadiusTest = TestCase $ assertEqual
  "transposeRadiusTest"
  (Radius 4)
  (transpose (+3) (Radius 1))

transposeListOfRadiusWithListOfFxTest = TestCase $ assertEqual
  "transposeListOfRadiusWithListOfFxTest"
  ([Radius 2, Radius 4])
  (transposeWithList [(+1),(+2)] [Radius 1, Radius 2])

transposeSDRTest = TestCase $ assertEqual
  "transposeSDRTest"
  (SingleDegreeRadii 1 [ Radius 5])
  (transpose (+3) (SingleDegreeRadii 1 [Radius 2]))
  
transposeSDRTest2 = TestCase $ assertEqual
  "transposeSDRTest2"
  (SingleDegreeRadii 1 [Radius 4, Radius 5])
  (transpose (+2) (SingleDegreeRadii 1 [Radius 2, Radius 3])  )

transposeSDRWithListTest = TestCase $ assertEqual
  "transposeSDRWithListTest"
  ([SingleDegreeRadii 1 [Radius 2, Radius 4]])
  (transposeSDRList [[(+1), (+2)]] [(SingleDegreeRadii 1 [Radius 1, Radius 2])]  )

transposeMDRWithListTest = TestCase $ assertEqual
  "transposeMDRWithListTest"
  (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 2, Radius 4]])
  (transposeMDRList [[(+1), (+2)]] (MultiDegreeRadii "name" [(SingleDegreeRadii 1 [Radius 1, Radius 2])])  ) 

extractRadiusFromMultiDegreeRadiiTest = TestCase $ assertEqual
  "extractRadiusFromMultiDegreeRadiiTest"
  (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 1]])
  (extractSingle head (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 1]]))

extractRadiusFromMultiDegreeRadiiTest2 = TestCase $ assertEqual
  "extractRadiusFromMultiDegreeRadiiTest2"
  (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 2, Radius 3]])
  (extractList tail (MultiDegreeRadii "name" [SingleDegreeRadii 1 [Radius 1, Radius 2, Radius 3]]))
  
extractRadiusFromSinleDegreeRadiiTest = TestCase $ assertEqual
  "extractRadiusFromSinleDegreeRadiiTest"
  (SingleDegreeRadii 1 [Radius 1])
  (extractSingle head (SingleDegreeRadii 1 [Radius 1]))

extractRadiusFromSinleDegreeRadiiTest2 = TestCase $ assertEqual
  "extractRadiusFromSinleDegreeRadiiTest2"
  (SingleDegreeRadii 1 [Radius 2, Radius 3])
  (extractList tail (SingleDegreeRadii 1 [Radius 1, Radius 2, Radius 3]))


