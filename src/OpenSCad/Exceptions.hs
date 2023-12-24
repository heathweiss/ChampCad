{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Corresponds to errors during OpenSCad scripting. Are handled through Either.
-}
module OpenSCad.Exceptions(ScriptingError(..)) where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P

import Test.HUnit

-- | For OpenSCad script generation errors.
data ScriptingError = ZeroXLen
                    | ZeroYLen
                    | ZeroZHght
                    | LessThan3Sides
                    | LessThanOrEqTo0Radius
 deriving (Eq,Show)
