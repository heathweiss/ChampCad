{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{- |
Create the OpenSCad polyhedron. See 'OpenSCad.PolyhedronExamples' for examples of building polyhedrons.
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

Todo: Add more constructors, such as a ['CornerPoints.Points.Point'], where they are processed in groups of 4.
-}
data Polyhedron = PolyCPoints {polyCPointsName :: Name, polyCPoints :: CornerPoints} -- * From a 'CornerPoints.CornerPoints'


