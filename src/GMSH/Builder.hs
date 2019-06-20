{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder(buildCubePointsList, buildCubePointsListSingle, buildPointsList, buildGPointsList, writeGPnts,
                    GC.newBuilderData, GC.BuilderStateData(..),ExceptStackCornerPointsBuilder, GC.BuilderMonadData) where
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


-}
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
    --it is in insert2, that the gmsh GPoint script needs to be written.
    gpoints =  Right $ GP.insertWithOvrLap handle points state'
  case gpoints of
    Right (state'',  gpoints') ->
      let
        builder = \builderMonadData -> (GC.BuilderMonadData_GPointIds gpoints', state'')
      in
        do
          -- leave the writing of the GPoints script to GP.insert2 as it knows whether the gpoint already exists.
          --liftIO $ GW.writeFileUtf8_str handle $ show gpoints'
          lift $ state $ builder
    Left e -> TE.throwE $ extraMsg ++ ": " ++ e
  
{-Don't need this as GP.insert2 does the traversal of the [Pts.Point]    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
--buildGPointsListOrFail' :: GC.BuilderStateData -> [Pts.Point] -> Either String (GC.BuilderStateData, [GC.PointsBuilderData])
buildGPointsListOrFail' :: GC.BuilderStateData -> [Pts.Point] -> Either String (GC.BuilderStateData, [GC.GPointId])
--end of the list. Return whatever has been built up in the BuilderData.

buildGPointsListOrFail' state' points =
  Right $ GP.insert2 points [] state'
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
  Right $ GP.insert2 points [] state'

-}  
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
--write the gpoints test

writeGPnts :: String -> ExceptStackCornerPointsBuilder
writeGPnts extraMsg = (writeGPntsOrFail "writeGPnts msg") `catchError` writeGPntsOrFail

writeGPntsOrFail :: String -> ExceptStackCornerPointsBuilder
writeGPntsOrFail extraMsg = do
                              state' <- get
                              let
                                builder = \builderMonadData -> (GC.BuilderMonadData_CPoints [CPts.B1 $ Pts.Point 1 2 3], state')
                              liftIO $ putStrLn "test of writeGPnts"
                              lift $ state $ \builderData -> (GC.BuilderMonadData_GPointIds([]), builderData) 
{-
writeGPnts :: String -> ExceptStackCornerPointsBuilder
writeGPnts extraMsg = (writeGPntsOrFail "writeGPnts msg") `catchError` writeGPntsOrFail

writeGPntsOrFail :: String -> ExceptStackCornerPointsBuilder
writeGPntsOrFail extraMsg = do
                              state' <- get
                              let
                                builder = \builderMonadData -> (GC.BuilderMonadData_CPoints [CPts.B1 $ Pts.Point 1 2 3], state')
                              liftIO $ putStrLn "test of writeGPnts"
                              lift $ state $ \builderData -> (GC.BuilderMonadData_GPointIds([]), builderData) 
-}
