{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module GMSH.Builder.GPointsNGADT(buildGPointsList_h) where

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

import qualified GMSH.Builder.BaseNGADT as GBB
import qualified GMSH.State as GST
import qualified GMSH.PointsNGADT as GP
--import qualified GMSH.GPointsNGADT as GGPts

import qualified CornerPoints.Points as Pts

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

makeLenses ''GST.BuilderStateData

buildGPointsList_h :: SIO.Handle -> String -> GP.NonOverLappedClosedPoints  -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
--buildPointsList_GADT extraMsg cPoints = buildPointsList extraMsg $ GC.eval cPoints
buildGPointsList_h h extraMsg points = 
  buildGPointsListOrFail_h h extraMsg points --`catchError` GBB.errorHandler

buildGPointsListOrFail_h :: SIO.Handle -> String -> GP.NonOverLappedClosedPoints  -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointsListOrFail_h _ _ (GP.NonOverLappedClosedPoints' []) = do
  E.lift $ SL.state $ \state' -> ([], state')

buildGPointsListOrFail_h h extraMsg (GP.NonOverLappedClosedPoints' points) = 
  buildGPointsListOrFail_h' h points []

buildGPointsListOrFail_h' :: SIO.Handle -> [Pts.Point] -> [GST.GPointId] -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointsListOrFail_h' h [] workingList = do
  let
    builder = \state' -> (reverse workingList, state')
  E.lift $ SL.state $ builder
  

buildGPointsListOrFail_h' h (point:points) workingList = do
  state' <- SL.get
  let
    --get the Maybe GPointId from the BuilderStateData.pointsMap
    maybe_gpoint = GST.retrieve state' point
  
  case maybe_gpoint of
    Just gpoint -> do
      --pass the GPointId to the overlapper fx to be added to the current State value of [GPointId] as per rules of the overlapping fx.
      buildGPointsListOrFail_h' h points (gpoint : workingList) 
    Nothing -> do
      --GPoint doesn't yet exsist so:
      --Create and append the new GPointId to the current State value of [GPointId], and add to the BuilderStateData.pointsMap.
      --Write the gpoint to the gmsh script file.
      
      let
        gpoint = head $ state' ^. pointsIdSupply
        
      E.liftIO $ GP.writeGScriptToFile h gpoint point
      --reset the state with the new GPointId. The following recursive call to insertBase' will use it.
      --lift $ state $ builder
      E.lift $ SL.state $
        \state'' ->
          (gpoint: workingList,
           --state''{GST._pointsMap = (HM.insert (H.hash point)  gpoint) (state'' ^. pointsMap),
           --        GST._pointsIdSupply = tail (state'' ^. pointsIdSupply) }
           GST.insertGPointId state'' point gpoint
                  
          )
      buildGPointsListOrFail_h' h points (gpoint : workingList)


--writeGScriptToFile
