{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder(buildCubePointsList, buildCubePointsListSingle, buildPointsList, {-buildGPointsList, -}
                    GC.newBuilderData, GC.BuilderStateData(..),ExceptStackCornerPointsBuilder, GC.BuilderMonadData,
                    insertWithOvrLap, insertNoOvrLap) where
{- |
Build up a shape from [CornerPoints]. But instead of saving the CornerPoints,
save the gmsh points, lines, etc along with an ID, within hash maps.
0

Tests and example are in Tests.GmshTest
-}

import qualified CornerPoints.CornerPoints as CPts  --((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>), cornerPointsError, findCornerPointsError, isCubePointsList)
import CornerPoints.CornerPoints((|+++|), (+++), (+++>))
import qualified CornerPoints.Points as Pts


import qualified GMSH.Lines as GL
import qualified GMSH.Common as GC
import qualified GMSH.Points as GP
import qualified GMSH.Writer as GW

import qualified Helpers.FileWriter as FW

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer (Writer, tell, execWriter)
import qualified System.IO as SIO

import Control.Lens

makeLenses ''GC.BuilderStateData



-- | The ExceptT State Builder for building up shapes, and convertering to gmsh Lines and points.
--the original before using IO or writer
--type ExceptStackCornerPointsBuilder =  ExceptT String (State GC.BuilderData ) [CPts.CornerPoints]
--Including IO makes it hard to test, but that is the way it is.
--This is before replacing [CPts.CornerPoints]
--type ExceptStackCornerPointsBuilder =  ExceptT String (StateT GC.BuilderData (IO)) [CPts.CornerPoints]
--type ExceptStackCornerPointsBuilder =  ExceptT String (StateT GC.BuilderStateData (IO)) [CPts.CornerPoints]
type ExceptStackCornerPointsBuilder =  ExceptT String (StateT GC.BuilderStateData (IO)) GC.BuilderMonadData


{- |
Handles a CornerPoints error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
Suggest using it in case error handling changes.
-}
errorHandler :: String -> ExceptStackCornerPointsBuilder 
errorHandler error = do
  TE.throwE error



{- |
Task:
Build a [CPts] by adding the 2 given [CPts].
Return:
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
If no errors, return the list as current value of Builder, with the state unchanged.
-}

buildCubePointsList :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       ExceptStackCornerPointsBuilder 
buildCubePointsList extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail  extraMsg cPoints cPoints') `catchError` errorHandler

-- | Runs buildCubePointsList when a single [CPts] is given.
buildCubePointsListSingle :: String -> [CPts.CornerPoints] -> ExceptStackCornerPointsBuilder
                       
buildCubePointsListSingle extraMsg cPoints =
  buildCubePointsList extraMsg [CPts.CornerPointsId | x <- [1..]] cPoints

-- | Executes buildCubePointsList as per standard ExceptT procedure in the MTL package.
buildCubePointsListOrFail :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             ExceptStackCornerPointsBuilder
--if an [] is passed in, nothing to do.
--buildCubePointsListOrFail _ [] _ =  lift $ state $ \builderData -> ([], builderData)
buildCubePointsListOrFail _ [] _ =  lift $ state $ \builderData -> (GC.BuilderMonadData_CPoints([]), builderData)
--buildCubePointsListOrFail _ _ [] =  lift $ state $ \builderData -> ([], builderData)
buildCubePointsListOrFail _ _ [] =  lift $ state $ \builderData -> (GC.BuilderMonadData_CPoints([]), builderData)

buildCubePointsListOrFail extraMsg cPoints cPoints' = do
  state' <- get
  
  
  let
    cubeList = cPoints |+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        Nothing -> --has no CornerPointsError
          let
            builder = \builderData -> (GC.BuilderMonadData_CPoints(cubeList), state')
          in
            lift $ state $ builder
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          TE.throwE $ extraMsg ++ ": " ++ (err)

    

-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------ Points version of buildCubePointsList --------------------------------------------------------
{- |
Given:
[CPts.CornerPoints]
The [Points] will be extracted from this list.
-}
buildPointsList :: String -> [CPts.CornerPoints] -> ExceptStackCornerPointsBuilder 
buildPointsList extraMsg cPoints = 
  (buildPointsListOrFail  extraMsg cPoints) `catchError` errorHandler

{- |
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
-}
buildPointsListOrFail :: String -> [CPts.CornerPoints] ->
                             ExceptStackCornerPointsBuilder
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
insertWithOvrLap ::  SIO.Handle -> [Pts.Point] -> ExceptStackCornerPointsBuilder
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
insertNoOvrLap ::  SIO.Handle -> [Pts.Point] -> ExceptStackCornerPointsBuilder
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
insertBase :: SIO.Handle -> (GC.GPointId -> [GC.GPointId] -> [GC.GPointId]) -> [Pts.Point] -> ExceptStackCornerPointsBuilder

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
insertBase' :: SIO.Handle -> (GC.GPointId -> [GC.GPointId] -> [GC.GPointId]) -> [Pts.Point] -> [GC.GPointId] -> ExceptStackCornerPointsBuilder
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
      
      

-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------ GPoints version of buildCubePointsList --------------------------------------------------------
{- |
Given:
extraMsg: error messsage for exceptions.
points: from which the GPoints will be generated.
handle: IO file handle for writing the gmsh GPoints script

Task:
Generate the GPoints from the Points.
If no error, write the gmsh GPoints script to file pointed to by handle.



buildGPointsList :: String -> [Pts.Point] -> SIO.Handle ->  ExceptStackCornerPointsBuilder 
buildGPointsList extraMsg points handle = 
  (buildGPointsListOrFail  extraMsg points handle) `catchError` errorHandler

{- |
Task:
Fulfill the Task of buildGPointsList with standard ExceptT error handling.
-}
buildGPointsListOrFail :: String -> [Pts.Point] -> SIO.Handle ->
                             ExceptStackCornerPointsBuilder
--if an [] is passed in, nothing to do.
buildGPointsListOrFail _ [] _ =  lift $ state $ \builderData -> (GC.BuilderMonadData_GPointIds([]), builderData)

buildGPointsListOrFail extraMsg points handle = do
  state' <- get
  let
    --gpoints =  buildGPointsListOrFail' state' points
    --it is in insert, that the gmsh GPoint script needs to be written.
    gpoints =  Right $ GP.insertWithOvrLap handle points state'
  case gpoints of
    Right (state'',  gpoints') ->
      let
        builder = \builderMonadData -> (GC.BuilderMonadData_GPointIds gpoints', state'')
      in
        do
          -- leave the writing of the GPoints script to GP.insert as it knows whether the gpoint already exists.
          --liftIO $ GW.writeFileUtf8_str handle $ show gpoints'
          lift $ state $ builder
    Left e -> TE.throwE $ extraMsg ++ ": " ++ e
  
{-Don't need this as GP.insert does the traversal of the [Pts.Point]    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
--buildGPointsListOrFail' :: GC.BuilderStateData -> [Pts.Point] -> Either String (GC.BuilderStateData, [GC.PointsBuilderData])
buildGPointsListOrFail' :: GC.BuilderStateData -> [Pts.Point] -> Either String (GC.BuilderStateData, [GC.GPointId])
--end of the list. Return whatever has been built up in the BuilderData.

buildGPointsListOrFail' state' points =
  Right $ GP.insert points [] state'
-}





{-
buildGPointsList :: String -> [Pts.Point] -> ExceptStackCornerPointsBuilder 
buildGPointsList extraMsg points = 
  (buildGPointsListOrFail  extraMsg points) `catchError` errorHandler

{- |
Task:

-}
buildGPointsListOrFail :: String -> [Pts.Point] ->
                             ExceptStackCornerPointsBuilder
--if an [] is passed in, nothing to do.
buildGPointsListOrFail _ [] =  lift $ state $ \builderData -> (GC.BuilderMonadData_GPointIds([]), builderData)

buildGPointsListOrFail extraMsg points = do
  state' <- get
  let
    gpoints =  buildGPointsListOrFail' state' points 
  case gpoints of
    Right (state'',  gpoints') ->
      let
        --builder = \builderMonadData -> (GC.BuilderMonadData_GPointIds gpoints', state'')
        builder = \builderMonadData -> (GC.BuilderMonadData_GPointIds gpoints', state'')
      in
        lift $ state $ builder
    Left e -> TE.throwE $ extraMsg ++ ": " ++ e
  
    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
--buildGPointsListOrFail' :: GC.BuilderStateData -> [Pts.Point] -> Either String (GC.BuilderStateData, [GC.PointsBuilderData])
buildGPointsListOrFail' :: GC.BuilderStateData -> [Pts.Point] -> Either String (GC.BuilderStateData, [GC.GPointId])
--end of the list. Return whatever has been built up in the BuilderData.

buildGPointsListOrFail' state' points =
  Right $ GP.insert points [] state'

-}  
-}


