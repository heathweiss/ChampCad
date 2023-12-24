{-# LANGUAGE OverloadedStrings #-}

module OpenSCadCubesTest(openSCadCubesTest) where

import OpenSCad.Cubes(Cube(..))
import Test.HUnit

import OpenSCad.Dimensions(XLength(..), YLength(..), ZHeight(..), newXLength, newYLength, newZHeight)
import OpenSCad.Exceptions(ScriptingError(..))
import OpenSCad.Cubes(Cube(..))


openSCadCubesTest :: IO ()
openSCadCubesTest = do
 putStrLn "\nOpenSCadCubesTest"
 let 
  createInvlalidXLength = TestCase 
   (do
     assertEqual "invalid XLength returns Left ZeroXLen"
      (Left ZeroXLen) 
      (Cube <$> newXLength 0.0 <*> newYLength 1.0 <*> newZHeight 2.0)
    
   )
 _ <- runTestTT createInvlalidXLength

 let 
  createValidCube = TestCase 
   (do
     assertEqual "valid cube"
      (Right $ Cube (XLen 10) (YLen 1) (ZHght 2)) 
      (Cube <$> newXLength 10.0 <*> newYLength 1.0 <*> newZHeight 2.0)
    
   )
 _ <- runTestTT createValidCube

 return ()

