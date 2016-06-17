{-# LANGUAGE TemplateHaskell #-}
{-ToDo:
Get rid of the custom state stuff.

Do a module for extensible-effects
-https://hackage.haskell.org/package/extensible-effects

Do a module for layers
-https://hackage.haskell.org/package/layers
-}
 

module Builder.Monad (BuilderError(..), cornerPointsErrorHandler, buildCubePointsList,
                      CpointsStack, CpointsList) where
{- |
Build up a shape from [CornerPoints].
Do it using the State monad inside of the ExceptT monad transformer.

Tests and example are in Tests.BuilderMonadTest
-}

{-
ToDo:
Add an IO layer in the bottom to have something like Persist read in values for building up shapes.
This would allow shapes to be tweaked live, by editing a file or database, and not have to recompile
in order to change simple values.
-}

import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>),
                                 cornerPointsError, findCornerPointsError, isCubePointsList)

import Stl.StlBase(Triangle(..))

import Control.Monad.Trans.Except
import Control.Monad.Except

import Control.Monad.State.Lazy

-- | type to clarify code.
type CpointsStack = [CornerPoints]
-- | type to clarify code.
type CpointsList = [CornerPoints] 
  
-- | data type for an exception as required for the Except monad.
data BuilderError  = BuilderError {errMsg :: String }

-- | common pattern to show the exception
instance Show BuilderError where
  show (BuilderError errMsg') = show errMsg' 

{- |
Handles a CornerPoints error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
Suggest using it in case error handling changes.
-}
cornerPointsErrorHandler :: BuilderError -> ExceptT BuilderError (State CpointsStack ) CpointsList
cornerPointsErrorHandler error = do
  throwE error

buildCubePointsList :: (CpointsList -> CpointsStack -> CpointsStack) -> String -> CpointsList -> CpointsList ->
                       ExceptT BuilderError (State CpointsStack ) CpointsList
buildCubePointsList pushToStack extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail pushToStack extraMsg cPoints cPoints') `catchError` cornerPointsErrorHandler
{- |
Same as buildCornerPointsListOrFail, but only pushes list onto the stack if all the elements are CubePoints.
If any of the [CornerPoints] that are not CubePoints, they are still returned as the current value, so they can
be used in the next computation.

If the work is done right, all elements will be CubePoints or none of them will be. If it is a mixture, then
that will not work with the whole monad builder system.

pushToStack:
The function to push onto the stack.
For now ++ is the only option. Once stl autogenerate module is done, then that system can be used.
-}
buildCubePointsListOrFail :: (CpointsList -> CpointsStack -> CpointsStack) -> String -> CpointsList -> CpointsList ->
                             ExceptT BuilderError (State CpointsStack ) CpointsList
buildCubePointsListOrFail pushToStack  extraMsg cPoints cPoints' =
  let  cubeList = cPoints |+++| cPoints'
  in   case findCornerPointsError cubeList of
        Nothing ->
          if isCubePointsList cubeList 
             then lift $ state $ \cubeStack -> (cubeList, cubeList `pushToStack` cubeStack)
             else lift $ state $ \cubeStack -> (cubeList, cubeStack)
        Just err -> throwE (BuilderError (extraMsg ++ " " ++ (errMessage err)) {-cube-})



