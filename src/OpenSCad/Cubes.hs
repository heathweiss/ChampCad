{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Create an OpenSCad cube.
-}
module OpenSCad.Cubes(Cube(..)) where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P
import Test.HUnit

import OpenSCad.Dimensions(XLength(..), YLength(..), ZHeight(..), newXLength, newYLength, newZHeight)
import OpenSCad.Exceptions(ScriptingError(..))

{-|
For creating an OpenSCad cube from dimensions.

Note that the dimensions all have smart constructors of 'Either' 'OpenSCad.Exceptions.ScriptingError' 
which can be used directly with this contructor, to return an 'Either' 'OpenSCad.Exceptions.ScriptingError' 'Cube'.

-}
data Cube = Cube { xLen :: XLength, yLen :: YLength, zHght :: ZHeight}
 deriving (Eq,Show)

--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------- local testing ------------------------------------------------------
cubeTest :: IO ()
cubeTest = do
 P.putStrLn "================================= OpenSCad Cube ===================================================="
 let 
  createInvlalidXLength = TestCase 
   (do
     assertEqual "invalid XLength returns Either, using data constructor"
      (Left ZeroXLen) 
      (Cube <$> newXLength 0.0 <*> newYLength 1.0 <*> newZHeight 2.0)
    
   )
 _ <- runTestTT createInvlalidXLength


 return ()