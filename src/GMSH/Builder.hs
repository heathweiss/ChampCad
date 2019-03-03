{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder(buildCubePointsList, GC.BuilderData(..),ExceptStackCornerPointsBuilder, buildCubePointsListSingle, GC.newBuilderData) where
{- |
Build up a shape from [CornerPoints]. But instead of saving the CornerPoints,
save the gmsh points, lines, etc along with an ID, within hash maps.
0

Tests and example are in Tests.GmshTest
-}

import qualified CornerPoints.CornerPoints as CPts  --((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>), cornerPointsError, findCornerPointsError, isCubePointsList)
import CornerPoints.CornerPoints((|+++|), (+++), (+++>))

import qualified GMSH.Lines as GL
import qualified GMSH.Common as GC


import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer (Writer, tell, execWriter)

import Control.Lens

makeLenses ''GC.BuilderData



-- | The ExceptT State Builder for building up shapes, and convertering to gmsh Lines and points.
--the original before using IO or writer
type ExceptStackCornerPointsBuilder =  ExceptT String (State GC.BuilderData ) [CPts.CornerPoints]
--Including IO makes it hard to test.
--type ExceptStackCornerPointsBuilder =  ExceptT String (StateT GC.BuilderData (IO)) [CPts.CornerPoints]
--try it with Writer monad at bottom. Causes several errors in this module. Not worth figuring out the solution.
--type ExceptStackCornerPointsBuilder =  ExceptT String (StateT GC.BuilderData (Writer String)) [CPts.CornerPoints]

{- |
Handles a CornerPoints error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
Suggest using it in case error handling changes.
-}
errorHandler :: String -> ExceptStackCornerPointsBuilder 
errorHandler error = do
  TE.throwE error





buildCubePointsList :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       ExceptStackCornerPointsBuilder 
buildCubePointsList extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail  extraMsg cPoints cPoints') `catchError` errorHandler
{-
buildCubePointsList :: ([CPts.CornerPoints] -> BuilderData -> BuilderData) -> String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       ExceptStackCornerPointsBuilder 
buildCubePointsList pushToStack extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail pushToStack extraMsg cPoints cPoints') `catchError` errorHandler
-}

buildCubePointsListSingle :: String -> [CPts.CornerPoints] -> ExceptStackCornerPointsBuilder
                       
buildCubePointsListSingle extraMsg cPoints =
  buildCubePointsList extraMsg [CPts.CornerPointsId | x <- [1..]] cPoints

{- |
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
-}
buildCubePointsListOrFail :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             ExceptStackCornerPointsBuilder
--if an [] is passed in, nothing to do.
buildCubePointsListOrFail _ [] _ =  lift $ state $ \builderData -> ([], builderData)
buildCubePointsListOrFail _ _ [] =  lift $ state $ \builderData -> ([], builderData)

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
                builder = \builderData -> (cubeList, builderData')
              in
              lift $ state $ builder
              --lift $ return cubeList
            Left e -> TE.throwE $ extraMsg ++ ": " ++ e
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          TE.throwE $ extraMsg ++ ": " ++ (err)

    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
buildCubePointsListOrFail' :: [CPts.CornerPoints] -> GC.BuilderData -> Either String GC.BuilderData
--end of the list. Return whatever has been built up in the BuilderData.
buildCubePointsListOrFail' [] builderData = Right builderData
buildCubePointsListOrFail' (cube:cubeList) builderData =
  let
    --newLinesHashmap = GL.insert cube (builderData ^. linesId) ( builderData ^. pointsId) ( builderData ^. linesMap)
    newLinesHashmap = GL.insert cube builderData 
  in
  case newLinesHashmap of
    Right builderData' ->
      buildCubePointsListOrFail' cubeList $ builderData' 
    Left e -> Left e
          

