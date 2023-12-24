{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{- | 
All things dimension related such as length, width, height. 
 
-}
module OpenSCad.Dimensions(XLength(..), newXLength, YLength(..), newYLength, ZHeight(..), newZHeight) where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P
import Test.HUnit

import OpenSCad.Exceptions(ScriptingError(ZeroXLen,ZeroYLen,ZeroZHght))

-- | Length on the x axis.
--
--known uses: Input to: 'OpenSCad.Cubes.Cube', 'OpenSCad.Translations.TranslationArray'
newtype XLength = XLen { xLen_ :: Double }
 deriving (Eq,Show)

-- | Create an 'XLength' that is > 0
newXLength :: Double -> Either ScriptingError XLength 
newXLength dbl = case dbl > 0 of
  True -> 
    Right $ XLen dbl
  _ -> Left ZeroXLen

-- | Length on the y axis.
--
--known uses: Input to: 'OpenSCad.Cubes.Cube', 'OpenSCad.Translations.TranslationArray'
newtype YLength = YLen { yLen_ :: Double }
 deriving (Eq,Show)

-- | Ensure that the value is > 0
newYLength :: Double -> Either ScriptingError YLength 
newYLength dbl = case dbl > 0 of
  True -> Right $ YLen dbl
  _ -> Left ZeroYLen

-- | Height on the z axis.
--
--known uses: Input to: 'OpenSCad.Cubes.Cube', 'OpenSCad.Translations.TranslationArray'
newtype ZHeight = ZHght { zHght_ :: Double}
 deriving (Eq,Show)

-- | Ensure that the value is > 0
newZHeight :: Double -> Either ScriptingError ZHeight 
newZHeight dbl = case dbl > 0 of
  True -> Right $ ZHght dbl
  _ -> Left ZeroZHght



--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------- local testing -------------------------------------------------------------------------
dimensionTests :: IO ()
dimensionTests = do
 P.putStrLn "================================= OpenSCad.Dimensions ===================================================="
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
