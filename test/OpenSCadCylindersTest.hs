{-# LANGUAGE OverloadedStrings #-}

module OpenSCadCylindersTest(openSCadCylindersTest) where

import Test.HUnit

import OpenSCad.Dimensions(ZHeight(..), newZHeight)
import OpenSCad.Exceptions(ScriptingError(..)) 
import OpenSCad.Cylinders(Cylinder(..), newSides, newRadius, Sides(..))

import CornerPoints.Radius(Radius(..))

openSCadCylindersTest :: IO ()
openSCadCylindersTest = do
 putStrLn "\nOpenSCadCylindersTest"
 let 
  lookAtInvalidXLenthError = TestCase 
   (do
     assertEqual "invalid XLength show an error msg as a script output"
      (Left LessThanOrEqTo0Radius) 
      (Cylinder <$> (newZHeight 10) <*> (newRadius 0.0) <*> (newSides 4) )
   )
 _ <- runTestTT lookAtInvalidXLenthError

 let 
  createValidCylinder = TestCase 
   (do
     assertEqual "invalid XLength show an error msg as a script output"
      (Right $ Cylinder (ZHght 10) (Radius 6) (Sides 4)) 
      (Cylinder <$> (newZHeight 10) <*> (newRadius 6.0) <*> (newSides 4) )
   )
 _ <- runTestTT createValidCylinder


 return ()