module Examples.Gmsh.FirstTestNGADT() where
import qualified GMSH.PointsNGADT as GP
import qualified GMSH.Lines as GL 
import qualified GMSH.Writer as GW
import qualified GMSH.Builder.BaseNGADT as GB
import qualified GMSH.Builder.CornerPointsNADT as GBC
import qualified GMSH.Builder.PointsNGADT as GBP
import qualified GMSH.Builder.GPointsNGADT as GBGPts
import qualified GMSH.State as GST

import qualified CornerPoints.Points as Pts
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E


import qualified System.IO as SIO

generateFrontFace :: GB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
generateFrontFace = do
  h <- E.liftIO $ GW.openFile "firstTest"
  let
    errorHandler = GB.errorHandler_h h
                    
  frontFace <- (GBC.buildCubePointsListSingle "frontFace <-"
                 [CPts.FrontFace (Pts.Point 1 1 1) (Pts.Point 2 2 2) (Pts.Point 3 3 3) (Pts.Point 4 4 4)]
               ) `E.catchError` errorHandler

  points <- (GBP.buildPointsList "points <- " frontFace) `E.catchError`  errorHandler
  closedNonOverlappedPoints <- (GBP.toNonOverlappingClosedPointsOrFail "closedNonOverlappedPoints <- " points) `E.catchError`  errorHandler

  E.liftIO $ GW.writeComment h "frontFace points"   ssdd
  gpoints <-
    (GBGPts.buildGPointsList_h h "gpoints" closedNonOverlappedPoints) `E.catchError` errorHandler
  
  E.liftIO $  SIO.hClose h
  return frontFace


runGenerateFrontFace :: IO()
runGenerateFrontFace = do
  --((SL.execStateT $ E.runExceptT generateFrontFace ) GB.newBuilderData)
  io <- ((SL.execStateT $ E.runExceptT generateFrontFace ) GST.newBuilderData)
  --print $ show $ io
  
  putStrLn "done"
