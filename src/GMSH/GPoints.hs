{-# LANGUAGE TemplateHaskell #-}

module GMSH.GPoints() where
{-
All functionality for the GMSH.State.GPointId other than:
It's use in the Builder monad transformer stack, which is in GMSH.Builder.GPoints.
and
It's constructor which is hidden in GMSH.State.

A GPointId is a generated sequential Int used as an identifier by gmsh.
Gmsh associates it with a Point(x,y,z), and uses them to build lines.
-}
import qualified CornerPoints.Points as Pts
import qualified GMSH.State as GST

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H

import qualified Data.Text as T
import qualified System.IO as SIO

import qualified Helpers.FileWriter as FW

import Control.Lens

makeLenses ''GST.GPointId
makeLenses ''GST.BuilderStateData




retrieve ::  GST.BuilderStateData -> Pts.Point -> Maybe GST.GPointId
retrieve  builderStateData point =
  HM.lookup (H.hash point) (builderStateData ^. pointsMap)


