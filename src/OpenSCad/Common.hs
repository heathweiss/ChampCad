{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Hold various basic types that are needed in multiple modules, in order to avoid cyclic redundancy errors.
-}
module OpenSCad.Common(Name(..)) where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P

-- | Attach a name to a OpenSCad script object, such as a cylinder or polyhedron.
newtype Name = Name {name :: Utf8Builder}