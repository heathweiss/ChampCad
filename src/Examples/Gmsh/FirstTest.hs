module Examples.Gmsh.FirstTest() where
import qualified GMSH.Writer.Base as GWB
import qualified GMSH.Builder.Base as GB
import qualified GMSH.Builder.CornerPoints as GBC
import qualified GMSH.Builder.Points as GBP
import qualified GMSH.State as GST
import qualified GMSH.Curve as Curve
import qualified GMSH.CurvePoints as CurvePoints

import qualified CornerPoints.Points as Pts
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E


import qualified System.IO as SIO

generateFrontFace :: GB.ExceptStackCornerPointsBuilder () -- [CPts.CornerPoints]
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
  
  let
    failingConstructors = (CurvePoints.EndPoint : CurvePoints.CircleArcPoint : [CurvePoints.EndPoint | a <- [1..]])
    passingConstructors = [CurvePoints.EndPoint | a <- [1..]]
  curves <- CurvePoints.buildCurveList h "curves" closedNonOverlappedPoints passingConstructors   `E.catchError` errorHandler

  lines <- Curve.buildCurves h "lines" curves  `E.catchError` errorHandler
  
  E.liftIO $  SIO.hClose h
  return ()


runGenerateFrontFace :: IO()
runGenerateFrontFace = do
  io <- ((SL.execStateT $ E.runExceptT generateFrontFace ) GST.newBuilderData)
  
  putStrLn "done"
