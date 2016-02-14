module Tests.MathTest(mathTestDo) where
import Math.Trigonometry
import Test.HUnit


mathTestDo = do

  putStrLn "cosDegrees" 
  runTestTT cosDegreesTest
  runTestTT aproxEqTest1
  runTestTT aproxEqTest1a
  runTestTT aproxEqTest2
  runTestTT aproxEqTest3
  runTestTT aproxEqTest4
  
--note how it does not come out to 0. This is due to rounding errors and/or irrational # errors of pi.
cosDegreesTest = TestCase $ assertEqual 
  "cosDegrees" (1.102182119232618e-15) ((cosDegrees 90) * 18  )

myEqual a b
  | (abs (a - b)) <= 0.011 = True
  | otherwise     = False

aproxEqTest1 = TestCase $ assertBool
  "simple integer" (myEqual 1 1.01)

aproxEqTest1a = TestCase $ assertBool
  "now reverse them" (myEqual 1.01 1.0)
  
aproxEqTest2 = TestCase $ assertBool
  "should not be equal" ((myEqual 1 1.1) == False)

aproxEqTest3 = TestCase $ assertBool
  "this was giving an error even though they are the same." ((myEqual (-6.123233995736766e-17) (-6.123233995736766e-17)))

aproxEqTest4 = TestCase $ assertBool
  "these are very close, but with diff sign, typical of what I was seening" ((myEqual (6.123233995736766e-17) (-6.123233995736766e-17)))
