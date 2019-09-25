{-# LANGUAGE TemplateHaskell #-}
module GmshPointsTest(gmshPointsMasterTestDo) where
{-
Test the GMSH.Points module.
-}

import CornerPoints.Points(Point(..))

import Test.HUnit

--import qualified GMSH.Points as GP
--import qualified GMSH.Lines as GL 
--import qualified GMSH.Common as GC
import qualified GMSH.State as GST
--import qualified GMSH.Builder as GB
import qualified GMSH.Writer as GW
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)
import Control.Lens

--makeLenses ''GC.PointsBuilderData
makeLenses ''GST.BuilderStateData

gmshPointsMasterTestDo = do
  putStrLn("\n\nGmshPointsTest:")
  GST.runStateTests
  

