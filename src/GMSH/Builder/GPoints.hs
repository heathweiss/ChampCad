{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module GMSH.Builder.GPoints(buildGPointsList_h) where

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.State as GST
import qualified GMSH.Points as GP
import qualified GMSH.Writer.GPoints as GWGPts

import qualified CornerPoints.Points as Pts

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

makeLenses ''GST.BuilderStateData

buildGPointsList_h :: SIO.Handle -> String -> [GP.NonOverLappedClosedPoints]  -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
--buildPointsList_GADT extraMsg cPoints = buildPointsList extraMsg $ GC.eval cPoints
buildGPointsList_h h extraMsg points = 
  buildGPointsListOrFail_h h extraMsg points --`catchError` GBB.errorHandler

buildGPointsListOrFail_h :: SIO.Handle -> String -> [GP.NonOverLappedClosedPoints]  -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointsListOrFail_h _ _ [] = do
  E.lift $ SL.state $ \state' -> ([], state')

buildGPointsListOrFail_h h extraMsg noc_points = 
  buildGPointsListOrFail_h' h noc_points []

buildGPointsListOrFail_h' :: SIO.Handle -> [GP.NonOverLappedClosedPoints] -> [GST.GPointId] -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointsListOrFail_h' h [] workingList = do
  let
    builder = \state' -> (reverse workingList, state')
  E.lift $ SL.state $ builder
  

buildGPointsListOrFail_h' h ((GP.NonOverLappedClosedPoints' noc_point):noc_points) workingList = do
  state' <- SL.get
  let
    --get the Maybe GPointId from the BuilderStateData.pointsMap
    maybe_gpoint = GST.lookupGPointId state' noc_point
  
  case maybe_gpoint of
    Just gpoint -> do
      --pass the GPointId to the overlapper fx to be added to the current State value of [GPointId] as per rules of the overlapping fx.
      buildGPointsListOrFail_h' h noc_points (gpoint : workingList) 
    Nothing -> do
      --GPoint doesn't yet exsist so:
      --Extract the new GPointId from the State pointsIdSupply, and add to the BuilderStateData.pointsMap, along with the vertices.
      --Write the gpoint to the gmsh script file.
      
      let
        --gpoint = head $ state' ^. pointsIdSupply
        gpoint = GST.newGPointId state' 
        
      E.liftIO $ GWGPts.writeGScriptToFile h gpoint noc_point
      --Add the new GPointId to the working list, and reset the state with the new GPointId.
      E.lift $ SL.state $
        \state'' ->
          (gpoint: workingList,
           GST.insertGPointId state'' noc_point gpoint 
          )
      buildGPointsListOrFail_h' h noc_points (gpoint : workingList)



{-
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
    maybe_gpoint = GST.lookupGPointId state' point
  
  case maybe_gpoint of
    Just gpoint -> do
      --pass the GPointId to the overlapper fx to be added to the current State value of [GPointId] as per rules of the overlapping fx.
      buildGPointsListOrFail_h' h points (gpoint : workingList) 
    Nothing -> do
      --GPoint doesn't yet exsist so:
      --Extract the new GPointId from the State pointsIdSupply, and add to the BuilderStateData.pointsMap, along with the vertices.
      --Write the gpoint to the gmsh script file.
      
      let
        --gpoint = head $ state' ^. pointsIdSupply
        gpoint = GST.newGPointId state' 
        
      E.liftIO $ GWGPts.writeGScriptToFile h gpoint point
      --Add the new GPointId to the working list, and reset the state with the new GPointId.
      E.lift $ SL.state $
        \state'' ->
          (gpoint: workingList,
           GST.insertGPointId state'' point gpoint 
          )
      buildGPointsListOrFail_h' h points (gpoint : workingList)


-}
