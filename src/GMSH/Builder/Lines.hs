{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.Lines(buildLines) where
{- |
Supply the Builder functionality to go along with the GMSH.Lines.
Lines are the ChampCad version of gmsh curves.line.
-}
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified Control.Monad.Trans.Except as TE
import qualified System.IO as SIO

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.State as GST
import qualified GMSH.GPoints as GP
import qualified GMSH.Lines as GL
import qualified GMSH.Writer.Lines as GWL
import qualified GMSH.Writer.GPoints as GWGPts



import Control.Lens

makeLenses ''GST.BuilderStateData

buildLines :: SIO.Handle -> String -> [GST.GPointId]  -> GBB.ExceptStackCornerPointsBuilder [GL.Line]
buildLines h errMsg gpoints = buildLines' h errMsg gpoints

buildLines' :: SIO.Handle -> String -> [GST.GPointId] ->  GBB.ExceptStackCornerPointsBuilder [GL.Line]
buildLines' h errMsg [] = do --should throw an error here instead.
  {-
  let
    builder = \state' -> (reverse workingList, state')
  E.lift $ SL.state $ builder -}
  (TE.throwE $ errMsg ++ "Builder.Lines.buildPointsListOrFail: empty [GPointId] passed in.")
buildLines' h errMsg gpoints = do
  state' <- SL.get
  let
    (lines, builderStateData) = GL.gPointIdsToLines gpoints state'
    
   
  --write lines to file here
  E.liftIO $ GWL.writeGScriptsToFile h lines
  
  E.lift $ SL.state $ \_ -> (lines, builderStateData)
  
