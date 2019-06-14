{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder(buildCubePointsList, buildCubePointsListSingle, buildPointsList,
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


import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer (Writer, tell, execWriter)

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

    

{-
Building up the [CPts] no longer affects the state, as that is done using Points, or perhaps GPoints,
so can be reduced. Only need to check for errors.

buildCubePointsListOrFail extraMsg cPoints cPoints' = do
  state' <- get
  
  
  let
    cubeList = cPoints |+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        Nothing -> --has no CornerPointsError
          let
            builderData =  buildCubePointsListOrFail' cubeList state'
          in
          case builderData of
            Right builderData' ->
              let
                --builder = \builderData -> (cubeList, builderData')
                builder = \builderData -> (GC.BuilderMonadData_CPoints(cubeList), builderData')
              in
              lift $ state $ builder
              --lift $ return cubeList
            Left e -> TE.throwE $ extraMsg ++ ": " ++ e
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          TE.throwE $ extraMsg ++ ": " ++ (err)

    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
buildCubePointsListOrFail' :: [CPts.CornerPoints] -> GC.BuilderStateData -> Either String GC.BuilderStateData
--end of the list. Return whatever has been built up in the BuilderData.
buildCubePointsListOrFail' [] builderData = Right builderData
buildCubePointsListOrFail' (cube:cubeList) builderData =
  let
    --newLinesHashmap = GL.insert cube (builderData ^. linesId) ( builderData ^. pointsId) ( builderData ^. linesMap)
    --newLinesHashmap = GL.insert cube builderData
    points = CPts.toPointsFromList (cube:cubeList)
  in
  case points of
    Right points' ->
      Right $ GP.insert points' builderData
    Left e -> Left e
-}          
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
