{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module GMSH.Builder.GPoints(buildGPointIdsList, buildCurveList, NonOverLappedClosedGPoints(), pattern NonOverLappedClosedGPoints') where

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.State as GST
import qualified GMSH.Points as GP
import qualified GMSH.GPoints as GGPts
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
buildGPointIdsList :: SIO.Handle -> String -> GP.NonOverLappedClosedPoints -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointIdsList h extraMsg (GP.NonOverLappedClosedPoints' points) = 
  buildGPointIdsList' h points []

--Add the empty workingList of GPointId's, and recursively process the [GP.NonOverLappedClosedPoints] to create and write to file, the GPointId's.
buildGPointIdsList' :: SIO.Handle -> [Pts.Point] -> [GST.GPointId] -> GBB.ExceptStackCornerPointsBuilder [GST.GPointId]
buildGPointIdsList' h [] workingList = do
  let
    builder = \state' -> (reverse workingList, state')
  E.lift $ SL.state $ builder
  

buildGPointIdsList' h (noc_point:noc_points) workingList = do
  state' <- SL.get
  let
    --get the Maybe GPointId from the BuilderStateData.pointsMap
    maybe_gpoint = GST.lookupGPointId state' noc_point
  
  case maybe_gpoint of
    Just gpoint -> do
      --pass the GPointId to the overlapper fx to be added to the current State value of [GPointId] as per rules of the overlapping fx.
      buildGPointIdsList' h noc_points (gpoint : workingList) 
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
      buildGPointIdsList' h noc_points newWorkingList


-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-------------------------- now build a version that returns [GMSH.GPoints.Curve] instead of [GST.GPointId]-------------------
newtype NonOverLappedClosedGPoints  = NonOverLappedClosedGPoints [GGPts.GPoints]

pattern NonOverLappedClosedGPoints' a = NonOverLappedClosedGPoints a


buildCurveList :: SIO.Handle
               -> String
               -> GP.NonOverLappedClosedPoints
               -> [(GST.GPointId -> Pts.Point -> GGPts.GPoints)]
               -> GBB.ExceptStackCornerPointsBuilder NonOverLappedClosedGPoints
buildCurveList _ errMsg (GP.NonOverLappedClosedPoints' []) _ =
  TE.throwE $ errMsg ++ " GMSH.Builder.GPoints.buildCurveList: empty NonOverLappedClosedPoints [] passed in."
buildCurveList _ errMsg _ [] =
  TE.throwE $ errMsg ++ " GMSH.Builder.GPoints.buildCurveList: empty  [Curve constructor] passed in."
buildCurveList h errMsg (GP.NonOverLappedClosedPoints' points) constructors =
  buildCurveList' h errMsg points constructors []

--leftOff 
--implement buildCurveList'
buildCurveList' :: SIO.Handle
               -> String -> [Pts.Point]
               -> [(GST.GPointId -> Pts.Point -> GGPts.GPoints)]
               -> [GGPts.GPoints]
               -> GBB.ExceptStackCornerPointsBuilder NonOverLappedClosedGPoints

buildCurveList' _ _ [] _ workingList = do
  let
    builder = \state' -> (NonOverLappedClosedGPoints $ reverse workingList, state')
  E.lift $ SL.state $ builder

buildCurveList' _ errMsg _ [] _ = do
  TE.throwE $ errMsg ++ " GMSH.Builder.GPoints.buildCurveList': empty  [Curve constructor] passed in."

buildCurveList' h errMsg (p:points) (c:constructors) workingList = do
  state' <- SL.get
  let
    --get the Maybe GPointId from the BuilderStateData.pointsMap
    maybe_gpoint = GST.lookupGPointId state' p
  case maybe_gpoint of
    Just gpoint -> do
      --pass the GPointId to the overlapper fx to be added to the current State value of [GPointId] as per rules of the overlapping fx.
      buildCurveList' h errMsg points constructors ((c gpoint p) : workingList) 
    
    Nothing -> do
      --GPoint doesn't yet exsist so:
      --Extract the new GPointId from the State pointsIdSupply, and add to the BuilderStateData.pointsMap, along with the vertices.
      --Write the gpoint to the gmsh script file.
      
      let
        newGPoint = GST.getId state'
        newWorkingList = (c newGPoint p) : workingList

      E.liftIO $ GWGPts.writeGScriptToFile h newGPoint p
      --Add the new GPointId to the working list, and reset the state with the new GPointId.
      E.lift $ SL.state $
        \state'' ->
          (newWorkingList,
           GST.insertGPointId state'' p 
          )
      buildCurveList' h errMsg points constructors newWorkingList
