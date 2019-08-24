{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.GPoints(buildGPointsList) where

import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.State as GST
import qualified GMSH.Points as GP
import qualified GMSH.Writer.GPoints as GWGPts
import qualified CornerPoints.Points as Pts


import Control.Lens

makeLenses ''GST.BuilderStateData

{- |
-----Given-----
Handle: file handle to the .geo file.

String: An error string, though it doesn't get used here as there are no errors.

[NonOverLappedClosedPoints]: Points that have been run through GMSH.Points.toNonOverlappingClosedPoints to ensure
  they are in a proper state. This is where possible errors would have occurred, such as [] of NonOverLappedClosedPoints.

-----Task-----
Convert the [NonOverLappedClosedPoints] to a [GPointId] using GMSH.GPoints.
Write the GPoints to the .geo file.

-----Return-----
Left error if any errors occcur, thought there are no apperent errors.
([GPointId],BuilderStateData) where:
[GPointId] have all been created and inserted into the BuilderStateData, unless a GPoint already existed, in which case it is only added to the  [GPointId].
BuilderStateData: All new GPointIds added to the GPointId map.

-----SideEffects-----
All new GPointId's written to the .geo file.

-}
buildGPointsList :: SIO.Handle -> String -> GP.NonOverLappedClosedPoints -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointsList h extraMsg (GP.NonOverLappedClosedPoints' points) = 
  buildGPointsList' h points []

--Add the empty workingList of GPointId's, and recursively process the [GP.NonOverLappedClosedPoints] to create and write to file, the GPointId's.
buildGPointsList' :: SIO.Handle -> [Pts.Point] -> [GST.GPointId] -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointsList' h [] workingList = do
  let
    builder = \state' -> (reverse workingList, state')
  E.lift $ SL.state $ builder
  

buildGPointsList' h (noc_point:noc_points) workingList = do
  state' <- SL.get
  let
    --get the Maybe GPointId from the BuilderStateData.pointsMap
    maybe_gpoint = GST.lookupGPointId state' noc_point
  
  case maybe_gpoint of
    Just gpoint -> do
      --pass the GPointId to the overlapper fx to be added to the current State value of [GPointId] as per rules of the overlapping fx.
      buildGPointsList' h noc_points (gpoint : workingList) 
    Nothing -> do
      --GPoint doesn't yet exsist so:
      --Extract the new GPointId from the State pointsIdSupply, and add to the BuilderStateData.pointsMap, along with the vertices.
      --Write the gpoint to the gmsh script file.
      
      let
        newGPoint = GST.getId state'
        newWorkingList = newGPoint : workingList
        
      E.liftIO $ GWGPts.writeGScriptToFile h newGPoint noc_point
      --Add the new GPointId to the working list, and reset the state with the new GPointId.
      E.lift $ SL.state $
        \state'' ->
          (newWorkingList,
           GST.insertGPointId state'' noc_point  
          )
      buildGPointsList' h noc_points newWorkingList



