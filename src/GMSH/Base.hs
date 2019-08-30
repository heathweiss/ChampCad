module GMSH.Base(NonOverLappedClosed(..),errorHandler, errorHandler_h, ExceptStackCornerPointsBuilder) where

import qualified GMSH.State as GST
--import qualified GMSH.Writer.Base as GWB
import qualified GMSH.Writer as GWB
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

-- | The ExceptT State Builder for building shapes, and convertering to gmsh <Points/Lines...>.
-- The polymorphic t is so that any type within the transformer stack can be returned by the Builder function instance.
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
  E.liftIO $ GWB.writeComment h error 
  TE.throwE error


{- |
A [] in which the contents have no overlap in that no 2 consecutive items are ==. This represents the NonOverLapped state.
So to make [GMESH.Line], will need to reuse each point(except head and last) for making 2 lines.
The last Point will be the same as head, which gives the closed state.

This state cannot be enforced, as the constructor is exported. It is up to the user to treat it like a law.
Always ensure the contained list is in the required state.

Known uses:
[CornerPoints.Points] must be in this state to make a [CurvePoints].
The resulting [CurvePoints] will also be returned in this state, which is required to make a [Curves].
-}
newtype NonOverLappedClosed a  = NonOverLappedClosed a
