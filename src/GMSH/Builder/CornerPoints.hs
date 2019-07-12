{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.CornerPoints(buildCubePointsList, buildCubePointsListSingle) where
{- |
Supply the Builder functions that deal with [CornerPoints.CornerPoints]
-}
import qualified CornerPoints.CornerPoints as CPts  --((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>), cornerPointsError, findCornerPointsError, isCubePointsList)
import CornerPoints.CornerPoints((|+++|), (+++), (+++>))

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.Common as GC

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Writer (Writer, tell, execWriter)
import qualified System.IO as SIO

{- |
Task:
Build a [CPts] by adding the 2 given [CPts].
Return:
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
If no errors, return the list as current value of Builder, with the state unchanged.
-}

buildCubePointsList :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
buildCubePointsList extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail  extraMsg cPoints cPoints') `catchError` GBB.errorHandler

-- | Runs buildCubePointsList when a single [CPts] is given.
buildCubePointsListSingle :: String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
                       
buildCubePointsListSingle extraMsg cPoints =
  buildCubePointsList extraMsg [CPts.CornerPointsId | x <- [1..]] cPoints

-- | Executes buildCubePointsList as per standard ExceptT procedure in the MTL package.
buildCubePointsListOrFail :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
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

