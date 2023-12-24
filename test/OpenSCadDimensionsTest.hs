module OpenSCadDimensionsTest (openSCadDimensionsTest) where 

import Test.HUnit

import OpenSCad.Dimensions(XLength(..), newXLength, YLength(..), newYLength, ZHeight(..), newZHeight) 
import OpenSCad.Exceptions(ScriptingError(ZeroXLen,ZeroYLen,ZeroZHght))

openSCadDimensionsTest :: IO ()
openSCadDimensionsTest = do
 putStrLn "\nopenSCadDimensionsTest"
 let 
  lookAtAXLenthErrorScript = TestCase 
   (do
     assertEqual "invalid XLength show an error msg as a script output"
      (Left ZeroXLen) 
      (newXLength 0.0 )
   )
 _ <- runTestTT lookAtAXLenthErrorScript

 let 
  lookAtAYLenthErrorScript = TestCase 
   (do
     assertEqual "invalid YLength show an error msg as a script output"
      (Left ZeroYLen) 
      (newYLength 0.0)
   )
 _ <- runTestTT lookAtAYLenthErrorScript

 let 
  lookAtAZHeightErrorScript = TestCase 
   (do
     assertEqual "invalid ZHeight show an error msg as a script output"
      (Left ZeroZHght) 
      (newZHeight 0.0)
   )
 _ <- runTestTT lookAtAZHeightErrorScript



 return ()
