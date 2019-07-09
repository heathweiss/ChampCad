{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
generateFrontFace :: GB.ExceptStackCornerPointsBuilder
generateFrontFace = do
  --h <- E.liftIO $ SIO.openFile  "src/Data/gmshScripts/test.geo" SIO.WriteMode
  h <- E.liftIO $ GW.openFile "test.geo" 
  frontFace <- GBC.buildCubePointsListSingle "FrontFace"
                 [CPts.FrontFace (Pts.Point 1 1 1) (Pts.Point 2 2 2) (Pts.Point 3 3 3) (Pts.Point 4 4 4),
                  CPts.FrontFace (Pts.Point 11 11 11) (Pts.Point 12 12 12) (Pts.Point 13 13 13) (Pts.Point 14 14 14)
                 ]
                 
  E.liftIO $ GW.writeSeparator0 h
  E.liftIO $ GW.writeComment h "All points from the [FrontFace]"
  points <-
    --This can be wrapped up in a fx, so as not to have to do the case statement manually.
    --Or is there a better way of dereferencing frontFace?
    case frontFace of
      GC.BuilderMonadData_CPoints(cpts) ->
        GBP.buildPointsList "FrontFace to Points" cpts
      _ -> GBP.buildPointsList "FrontFace to Points" [CPts.CornerPointsError "no front face"]


  --this was used to print/look_at the frontFaces
  {-
  case frontFace of
    GC.BuilderMonadData_CPoints(cpts) ->
      E.liftIO $ writeFileUtf8_str h $ show cpts -- frontFace
  -}

  --print/look_at the points as extracted from frontFace
  {-
  case points of
    GC.BuilderMonadData_Points(pts) ->
      E.liftIO $ writeFileUtf8_str h $ show pts
  
-}
{-
  gpoints <-
    --let
    --  state' = SL.get
    --GP.insert points [] (SL.get)
    GB.buildGPointsList "do the gpoints" (points ^. bmdPts) h
  testIO <- GB.writeGPnts "test msg" -}
  gpoints <- GBP.insertNoOvrLap h (points ^. bmdPts)
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


