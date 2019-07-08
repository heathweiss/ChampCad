{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.Base(errorHandler, {-GC.newBuilderData, GC.BuilderStateData(..),-}ExceptStackCornerPointsBuilder{-, GC.BuilderMonadData,-}) where
{- |
Build up a shape from [CornerPoints] and 
save to file as gmsh points, lines, etc.

Tests and example are in Tests.GmshTest
-}

import qualified GMSH.Common as GC

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer (Writer, tell, execWriter)
import qualified System.IO as SIO

import Control.Lens

makeLenses ''GC.BuilderStateData



-- | The ExceptT State Builder for building shapes, and convertering to gmsh <Points/Lines...>.
type ExceptStackCornerPointsBuilder =  ExceptT String (StateT GC.BuilderStateData (IO)) GC.BuilderMonadData


{- |
Handles a CornerPoints error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
Suggest using it in case error handling changes.
-}
errorHandler :: String -> ExceptStackCornerPointsBuilder 
errorHandler error = do
  TE.throwE error


