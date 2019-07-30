{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.Base(errorHandler, errorHandler_h, ExceptStackCornerPointsBuilder) where
{- |
Build up a shape from [CornerPoints] from within a ExceptStackCornerPointsBuilder.
May be depracated in the future, as CornerPoints were designed to generate the mesh, which gmsh is now doing.
Should be able to do everything with Points(vertices) so simplify.

Tests and example are in Tests.GmshTest
-}

import qualified GMSH.Common as GC
import qualified GMSH.Writer as GW
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO


-- | The ExceptT State Builder for building shapes, and convertering to gmsh <Points/Lines...>.
type ExceptStackCornerPointsBuilder t =  E.ExceptT String (SL.StateT GC.BuilderStateData (IO)) (GC.BuilderMonadData t)


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
  E.liftIO $ GW.writeComment h error 
  TE.throwE error

