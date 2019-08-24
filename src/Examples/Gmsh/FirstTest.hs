module Examples.Gmsh.FirstTest() where
import qualified GMSH.Points as GP
import qualified GMSH.Lines as GL
import qualified GMSH.Builder.Lines as GBL
import qualified GMSH.Writer.Base as GWB
import qualified GMSH.Builder.Base as GB
import qualified GMSH.Builder.CornerPoints as GBC
import qualified GMSH.Builder.Points as GBP
import qualified GMSH.Builder.GPoints as GBGPts
import qualified GMSH.State as GST

import qualified CornerPoints.Points as Pts
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E


import qualified System.IO as SIO

generateFrontFace :: GB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
generateFrontFace = do
  h <- E.liftIO $ GWB.openFile "firstTest"
  let
    errorHandler = GB.errorHandler_h h
                    
  frontFace <- (GBC.buildCubePointsListSingle "frontFace <-"
                 [CPts.FrontFace (Pts.Point 0 0 1) (Pts.Point 0 1 1) (Pts.Point 1 1 1) (Pts.Point 1 0 1)]
               ) `E.catchError` errorHandler

  points <- (GBP.buildPointsList "points <- " frontFace) `E.catchError`  errorHandler
  closedNonOverlappedPoints <- (GBP.toNonOverlappingClosedPointsOrFail "closedNonOverlappedPoints <- " points) `E.catchError`  errorHandler

  E.liftIO $ GWB.writeComment h "frontFace points"  
  gpoints <-
    (GBGPts.buildGPointsList h "gpoints" closedNonOverlappedPoints) `E.catchError` errorHandler

  lines <- GBL.buildLines h "lines" gpoints
  
  E.liftIO $  SIO.hClose h
  return frontFace


runGenerateFrontFace :: IO()
runGenerateFrontFace = do
  --((SL.execStateT $ E.runExceptT generateFrontFace ) GB.newBuilderData)
  io <- ((SL.execStateT $ E.runExceptT generateFrontFace ) GST.newBuilderData)
  --print $ show $ io
  
  putStrLn "done"
