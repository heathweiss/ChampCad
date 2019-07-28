{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Examples.Gmsh.FirstTest() where
{-
Testing of the GMSH modules.
-}

import qualified GMSH.Points as GP
import qualified GMSH.Lines as GL 
import qualified GMSH.Common as GC
import qualified GMSH.Writer as GW
import qualified GMSH.Builder.Base as GB
import qualified GMSH.Builder.CornerPoints as GBC
import qualified GMSH.Builder.Points as GBP
import qualified GMSH.Builder.GPoints as GBGPts

import qualified CornerPoints.Points as Pts
import qualified CornerPoints.CornerPoints as CPts

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E


import Control.Lens

import qualified System.IO as SIO

makeLenses ''GC.BuilderMonadData

{-
Create a FrontFace using the Gmsh Builder.
Used by runGenerateFrontFace.
-}
generateFrontFace :: GB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
generateFrontFace = do
  --h <- E.liftIO $ SIO.openFile  "src/Data/gmshScripts/test.geo" SIO.WriteMode
  h <- E.liftIO $ GW.openFile "firstTest"
  let
    cPtsErrorHandler = GB.errorHandler_h h
    pointsErrorHandler = GB.errorHandler_h h
    nonOverLappedClosedPoints_errorHandler = GB.errorHandler_h h
    gPtsErrorHandler = GB.errorHandler_h h
  {-
  badDonkey <- (GBC.buildCubePointsListSingle  "badDonkey"
                 [CPts.CornerPointsError "bad donkey error"
                 ]
               ) `E.catchError` errorHandler_h'-}
                    
  frontFace <- (GBC.buildCubePointsListSingle "frontFace <-"
                 [CPts.FrontFace (Pts.Point 1 1 1) (Pts.Point 2 2 2) (Pts.Point 3 3 3) (Pts.Point 4 4 4)]
               ) `E.catchError` cPtsErrorHandler
  
  topFrontLine <- (GBC.buildCubePointsListSingle "topFrontLine: " [CPts.B1 $ Pts.Point 1 2 3, CPts.B1 $ Pts.Point 11 12 13]) `E.catchError`  cPtsErrorHandler
  
  
  E.liftIO $ GW.writeComment h "frontFace points" 
  points <- (GBP.buildPointsList "points <- " frontFace) `E.catchError`  pointsErrorHandler
  
  
  nonoverlapped_closed_points <-
    (GBP.toNonOverlappingClosedPointsOrFail "nonoverlapped_closed_points <- " points)
    `E.catchError`
    nonOverLappedClosedPoints_errorHandler
  --need to create a insertNoOvrLap_GADT for gpoints, ensures points is a [Points].
  --gpoints <- GBP.insertNoOvrLap h $ GC.eval points -- (points ^. bmdPts)
  gpoints <-
    (GBGPts.buildGPointsList_h h "gpoints" nonoverlapped_closed_points)
    `E.catchError`
    gPtsErrorHandler
  E.liftIO $ GW.writeSeparator4 h
  E.liftIO $  SIO.hClose h
  return frontFace



{-
 generateFrontFace using SL.execState $ E.runExceptT and an empty GB.BuilderData
-}

runGenerateFrontFace :: IO()
runGenerateFrontFace = do
  --((SL.execStateT $ E.runExceptT generateFrontFace ) GB.newBuilderData)
  io <- ((SL.execStateT $ E.runExceptT generateFrontFace ) GC.newBuilderData)
  --print $ show $ io
  
  putStrLn "done"


