{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.Base(errorHandler, errorHandler_h, ExceptStackCornerPointsBuilder) where
{- |
Build up a shape from [CornerPoints] and 
save to file as gmsh points, lines, etc.

Tests and example are in Tests.GmshTest
-}

import qualified GMSH.Common as GC
import qualified GMSH.Writer as GW
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer (Writer, tell, execWriter)
import qualified System.IO as SIO

import Control.Lens

makeLenses ''GC.BuilderStateData



-- | The ExceptT State Builder for building shapes, and convertering to gmsh <Points/Lines...>.
type ExceptStackCornerPointsBuilder t =  ExceptT String (StateT GC.BuilderStateData (IO)) (GC.BuilderMonadData t)


{- |
Handles a CornerPoints error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
Suggest using it in case error handling changes.
-}
errorHandler :: String -> ExceptStackCornerPointsBuilder t
errorHandler error = do
  TE.throwE error

errorHandler_h :: SIO.Handle -> String -> ExceptStackCornerPointsBuilder t
errorHandler_h h error = do
  (liftIO $ GW.writeComment h $ error )
  TE.throwE error

