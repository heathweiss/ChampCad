{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.Points(buildPointsList, insertWithOvrLap, insertNoOvrLap) where
{- |
All GMSH Builder functions dealing with [CornerPoints.Points].
-}

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.Common as GC
import qualified GMSH.Points as GP

import qualified CornerPoints.CornerPoints as CPts  --((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>), cornerPointsError, findCornerPointsError, isCubePointsList)
import CornerPoints.CornerPoints((|+++|), (+++), (+++>))
import qualified CornerPoints.Points as Pts

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer (Writer, tell, execWriter)
import qualified System.IO as SIO

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

makeLenses ''GC.BuilderStateData

{- |
Given:
[CPts.CornerPoints]
The [Points] will be extracted from this list.
-}
buildPointsList :: String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder 
buildPointsList extraMsg cPoints = 
  (buildPointsListOrFail  extraMsg cPoints) `catchError` GBB.errorHandler

{- |
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
-}
buildPointsListOrFail :: String -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder
--if an [] is passed in, nothing to do.
buildPointsListOrFail _ [] =  lift $ state $ \builderData -> (GC.BuilderMonadData_Points([]), builderData)

buildPointsListOrFail extraMsg cPoints = do
  state' <- get
  let
    points =  buildPointsListOrFail' cPoints (CPts.toPointsFromList)
  case points of
    Right points' ->
      let
        builder = \builderMonadData -> (GC.BuilderMonadData_Points(points'), state')
      in
        lift $ state $ builder
    Left e -> TE.throwE $ extraMsg ++ ": " ++ e
  
    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
buildPointsListOrFail' :: [CPts.CornerPoints] -> ([CPts.CornerPoints] -> Either String [Pts.Point]) -> Either String [Pts.Point]
--end of the list. Return whatever has been built up in the BuilderData.
buildPointsListOrFail' [] _ = Right []
buildPointsListOrFail' cubeList toPoints =
  let
    --points = CPts.toPointsFromList (cube:cubeList)
    points = toPoints cubeList
    
  in
  case points of
    Right points' ->
      Right $ points' 
    Left e -> Left e


-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--create new insert version that returns ExceptStackCornerPointsBuilder so I can do IO
{- |
Given:
h: a handle to the gmsh script file.
points: [CornerPoints.Points.Point] to be converted to GPointsId and written to the script file.

Task:
Convert each Point into a GPointId if it does not already exist in state.
If already in state:
append to the current state value of [GPointId].
If not already in state:
Add it to state, append to the current state value of [GPointId], and print to file.
If there are overlapping points, such as a common point shared between 2 adjacent lines, that point will
added to the current State value of [GPointId]. That in turn, will change the way that gmsh lines are created from the [GPointId].

Return:
All new GPointId added to GMSH.Common.BuilderStateData.pointsMap.
All generated GPointId, including overlapping points, added to the current StateT value of [GPointId].
Side effect:
All new GPointId written to the gmsh script file.

Still need to handle the Closed/Open idea, where the first point may or may not have been duplicated at the end of the [GpointId] for creating a closed loop from the lines.
-}
insertWithOvrLap ::  SIO.Handle -> [Pts.Point] -> GBB.ExceptStackCornerPointsBuilder
insertWithOvrLap h points = insertBase h (:) points 

{- |
Same as insertWithOvrLap, but without overlapping points added to the current State value of [GPointId].
This will only have an effect if the [Pts.Point] has overlapping Points.
This will affect how gmsh lines are created from the [GPointId].

Know uses:
The [GPointId] is turned into gmsh lines with a fx (still to be written) that know that all but the 1st and last GpointId
are shared by adjacent lines.

Still need to handle the Closed/Open idea, where the first point may or may not have been duplicated at the end of the [GpointId] for creating a closed loop from the lines.
-}
insertNoOvrLap ::  SIO.Handle -> [Pts.Point] -> GBB.ExceptStackCornerPointsBuilder
insertNoOvrLap h points  =
  let
    overlapper :: GC.GPointId -> [GC.GPointId] -> [GC.GPointId]
    overlapper gpoint [] = [gpoint]
    overlapper gpoint (g:gpoints) =
      case gpoint == g of
        True -> g:gpoints
        False -> gpoint:g:gpoints
  in
  insertBase h overlapper points 

{-
Implements <insertWithOvrLap/insertNoOvrLap> by calling insertBase', supplying the empty [GC.GPointId] working list.
Also handles empty [Pts.Point]
-}
insertBase :: SIO.Handle -> (GC.GPointId -> [GC.GPointId] -> [GC.GPointId]) -> [Pts.Point] -> GBB.ExceptStackCornerPointsBuilder

insertBase h _ [] = do
  state' <- get
  let
    builder = \builderMonadData -> (GC.BuilderMonadData_Points([]), state')
  lift $ state $ builder

  
insertBase h overlapper points = insertBase' h overlapper points [] 

{-
Given:
Same as insertBase, but additionaly with the workingList of [GC.GPointId] as they are created, and optionaly inserted, printed.
Task:
Implement insertBase, with the extra workingList param.
Return:
Same as insertBase.
-}
insertBase' :: SIO.Handle -> (GC.GPointId -> [GC.GPointId] -> [GC.GPointId]) -> [Pts.Point] -> [GC.GPointId] -> GBB.ExceptStackCornerPointsBuilder
insertBase' h _ [] workingList  = do
  state' <- get
  let
    builder = \builderMonadData -> (GC.BuilderMonadData_GPointIds(reverse workingList), state')
  lift $ state $ builder


insertBase' h overlapper (point:points) workingList = do
  state' <- get
  let
    --get the Maybe GPointId from the BuilderStateData.pointsMap
    maybe_gpoint = GP.retrieve state' point
  
  case maybe_gpoint of
    Just gpoint -> do
      --pass the GPointId to the overlapper fx to be added to the current State value of [GPointId] as per rules of the overlapping fx.
      insertBase' h overlapper points (overlapper gpoint workingList) 
    Nothing -> do
      --Create and append the new GPointId to the current State value of [GPointId], and add to the BuilderStateData.pointsMap.
        --As it is a new GPointId, it can't be overlapping, so no need to use the overlap fx.
      --Write the gpoint to the gmsh script file.
      let
        gpoint = head $ state' ^. pointsIdSupply
        mapWithCurrentPointInserted = (HM.insert (H.hash point)  gpoint) (state' ^. pointsMap)
        builder = \builderMonadData ->
          (GC.BuilderMonadData_GPointIds(gpoint: workingList),
           state'{GC._pointsMap = mapWithCurrentPointInserted,
                  GC._pointsIdSupply = tail (state' ^. pointsIdSupply)
                 }
          )
      liftIO $ GP.writeGScriptToFile h gpoint point
      --reset the state with the new GPointId. The following recursive call to insertBase' will use it.
      lift $ state $ builder
      insertBase'
             
             h overlapper points
             (overlapper gpoint workingList)
      

