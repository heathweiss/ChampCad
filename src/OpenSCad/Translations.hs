
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{- | OpenSCad translations -}
module OpenSCad.Translations(TranslationArray(..)) where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P
import Test.HUnit

import OpenSCad.Dimensions(XLength(..), YLength(..), ZHeight)

-- | Bring together the x, y, & z lengths needed to do a translation.
-- All 3 dimensions have a smart constructor so 'TransArray' can be used with 'Either' 'ScriptingError'.
data TranslationArray = TransArray {transX :: XLength, transY :: YLength, transZ :: ZHeight}

