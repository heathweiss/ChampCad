{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.Lines( buildGPointLines) where
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
import qualified GMSH.GPoints as GGPts
import qualified GMSH.Builder.GPoints as GBGPts
import qualified GMSH.Lines as GL
import qualified GMSH.Writer.Lines as GWL
import qualified GMSH.Writer.GPoints as GWGPts



import Control.Lens

makeLenses ''GST.BuilderStateData

buildGPointLines :: SIO.Handle -> String -> GBGPts.NonOverLappedClosedGPoints  -> GBB.ExceptStackCornerPointsBuilder [GL.Line]
buildGPointLines h errMsg (GBGPts.NonOverLappedClosedGPoints' []) = do
  TE.throwE $ errMsg ++ " GMSH.Builder.Lines.buildGPointLines: empty [NonOverLappedClosedGPoints] passed in."
buildGPointLines h errMsg (GBGPts.NonOverLappedClosedGPoints' gpoints) = do 
  state' <- SL.get
  let maybeLines = GL.gPointsToLines errMsg gpoints state'
  case maybeLines of
    Right (lines, builderStateData) -> do
      E.liftIO $ GWL.writeGScriptsToFile h lines
      E.lift $ SL.state $ \_ -> (lines, builderStateData)
    Left e -> TE.throwE e
  
  


