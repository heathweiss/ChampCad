
module Tests.RadiusTest(radisuTestDo) where
import Test.HUnit
import CornerPoints.Radius(Radius(..), SingleDegreeRadii(..), Degree(..), MultiDegreeRadii(..), resetMultiDegreeRadiiIfNull,
                          extractSingle, extractList, rotateMDR, setRadiusIfNull,  resetSingleDegreeRadiiIfNull,
                          setRadiusWithPrecedingValueIfNull, resetMultiDegreeRadiiIfNullWithPreviousValue, transposeSDRList,
                          transposeMDRList)
import CornerPoints.Transposable(transpose, transposeWithList)
import Scan.ParseJuicy(averageValueOf)


radisuTestDo = do
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


