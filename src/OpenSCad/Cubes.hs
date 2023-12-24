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

