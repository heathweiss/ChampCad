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
  frontFace <- GBC.buildCubePointsListSingle "FrontFace"
                 [CPts.FrontFace (Pts.Point 1 1 1) (Pts.Point 2 2 2) (Pts.Point 3 3 3) (Pts.Point 4 4 4),
                  CPts.FrontFace (Pts.Point 11 11 11) (Pts.Point 12 12 12) (Pts.Point 13 13 13) (Pts.Point 14 14 14)
                 ]
                 
  E.liftIO $ GW.writeSeparator0 h
  E.liftIO $ GW.writeComment h "All points from the [FrontFace]"
  
  points <- GBP.buildPointsList "FrontFace to Points" frontFace
    

  --need to create a insertNoOvrLap_GADT for gpoints, ensures points is a [Points].
  gpoints <- GBP.insertNoOvrLap h $ GC.eval points -- (points ^. bmdPts)

  E.liftIO $  SIO.hClose h
  return frontFace



{-
 generateFrontFace using SL.execState $ E.runExceptT and an empty GB.BuilderData
-}

runGenerateFrontFace :: IO()
runGenerateFrontFace = do
  --((SL.execStateT $ E.runExceptT generateFrontFace ) GB.newBuilderData)
  io <- ((SL.execStateT $ E.runExceptT generateFrontFace ) GC.newBuilderData)

  --Look at the 'return frontFace'
  --print $ show $ io
  
  putStrLn "done"


