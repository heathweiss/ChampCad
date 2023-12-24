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
data ScriptingError = ZeroXLen -- ^ Any shape with no y length would be a line. Could be possible, but maybe not allowed in certain situations.
                    | ZeroYLen  -- ^ Any shape with no y length would be a line. Could be possible, but maybe not allowed in certain situations.
                    | ZeroZHght -- ^ Any shape with <= 0 height will have no volume.
                    | LessThan3Sides -- ^ Any shape witnh < 3 sides will have no volume.
                    | LessThanOrEqTo0Radius
 deriving (Eq,Show)
