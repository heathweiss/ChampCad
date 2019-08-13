module GMSH.Builder.Base(errorHandler, errorHandler_h, ExceptStackCornerPointsBuilder) where
{- |
Build up a shape from [Points] from within a ExceptStackCornerPointsBuilder.
New version of GMSH.Builder.Base which does not use GADT.
It also is polymorphic on the Right value for the ExceptStackCornerPointsBuilder

Tests and example are in Tests.GmshTest
-}
--import qualified GMSH.Common as GC
import qualified GMSH.State as GST
import qualified GMSH.Writer as GW
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

-- | The ExceptT State Builder for building shapes, and convertering to gmsh <Points/Lines...>.
-- The polymorphic t is so that any type within the transformer stack can be returned.
type ExceptStackCornerPointsBuilder t =  E.ExceptT String (SL.StateT GST.BuilderStateData (IO)) t


{- |
Handles an error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
Snoyman suggested using it in case error handling changes, instead of not using the `catchErorr` at all.
Could be used as a default when errorHandler_h is not used.
Or is it just unneccesary noise?
-}
errorHandler :: String -> ExceptStackCornerPointsBuilder t
errorHandler error = do
  TE.throwE error

errorHandler_h :: SIO.Handle -> String -> ExceptStackCornerPointsBuilder t
errorHandler_h h error = do
  E.liftIO $ GW.writeComment h error 
  TE.throwE error
