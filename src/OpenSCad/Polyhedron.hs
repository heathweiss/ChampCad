{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{- |
Create the OpenSCad polyhedron.

So far, only have an 8 point polyhedron based on a 'CornerPoints.CornerPoints.CornerPoints'.
The only instance is for the CornerPoints constructor.
There is no actual Polyhedron ADT, just a 'OpenSCad.ScriptBase.Script' constructor,
which in turn has pattern match for 'OpenSCad.ScriptBase.ToOpenScript'

Todo:

Need to make a general polyhedron that can take a list of points and faces.
Will use existing ChampCad types as much as possible.
See the exiting CornerPoints instance for an example.
Maybe the CornerPoints -> polyhedron should be in a separate module from this work.

-}
module OpenSCad.Polyhedron(Polyhedron(..)) where

import RIO
import qualified RIO.Text as T
import qualified Prelude as P

import Test.HUnit

import CornerPoints.CornerPoints(CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)
import CornerPoints.FaceConversions(toBackFace, toFrontTopLine)

import OpenSCad.Common(Name(..))


{- |
An OpenSCad polyhedron.
-}
data Polyhedron = PolyCPoints {polyCPointsName :: Name, polyCPoints :: CornerPoints} -- * From a 'CornerPoints.CornerPoints'


