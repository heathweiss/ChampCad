{-# LANGUAGE TemplateHaskell #-}
{-ToDo:
Get rid of the custom state stuff.

Do a module for extensible-effects
-https://hackage.haskell.org/package/extensible-effects

Do a module for layers
-https://hackage.haskell.org/package/layers
-}
 

module Builder.Monad (BuilderError(..), cornerPointsErrorHandler, buildCornerPointsList, buildCubePointsList,
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

--common pattern to show the exception
instance Show BuilderError where
  show (BuilderError errMsg') = show errMsg' 

{- |
Handles a CornerPoints error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
-}
cornerPointsErrorHandler :: BuilderError -> ExceptT BuilderError (State CpointsStack ) CpointsList
cornerPointsErrorHandler error = do
  throwE error


{- |
A wrapper around buildCornerPointsListOrFail, using the ExceptT `catchError` error handling system. 
-}
buildCornerPointsList :: String -> CpointsList -> CpointsList -> ExceptT BuilderError (State CpointsStack ) CpointsList
buildCornerPointsList extraMsg cPoints cPoints' = 
  (buildCornerPointsListOrFail extraMsg cPoints cPoints') `catchError` cornerPointsErrorHandler


{- |
Check for error in the [CornerPoints].

If no error, push it onto the stack. Return it as the current state value, in case it is needed for the next compution.

Else throw an error with error message from the |+++| computation, with the extraMsg prepended to it.
Typical extraMsg would be the location in the Do notation, where the error occurred.

See buildCornerPointsList, which is a wrapper around this with catchError

-}
{-
ToDo:
Should it only push onto the stack if it is a CubePoints, which is a fully formed cube.

If partial cubes allowed:
The stl shape will possibly not be closed.
The partial cube would still be set as the current value, so it could be used in the next computation.
It would be up to the user to ensure all shapes get closed off.

Or:

Only CubePoints allowed (fully formed cubes).
Would (help) ensure that stl shape is closed.
Throw an error if it is not a CubePoints, and force the user to add only complete cubes.
Will have to build up a complete cube in each line of Do notaion.
Seems like the safest option.
-}
buildCornerPointsListOrFail :: String -> CpointsList -> CpointsList -> ExceptT BuilderError (State CpointsStack ) CpointsList
buildCornerPointsListOrFail extraMsg cPoints cPoints' =
  let cubeList = cPoints |+++| cPoints'
  in  case findCornerPointsError cubeList of
        Nothing -> lift $ state $ \cubeStack -> (cubeList, cubeList ++ cubeStack)
        Just err -> throwE (BuilderError (extraMsg ++ " " ++ (errMessage err)) {-cube-})

buildCubePointsList :: String -> CpointsList -> CpointsList -> ExceptT BuilderError (State CpointsStack ) CpointsList
buildCubePointsList extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail extraMsg cPoints cPoints') `catchError` cornerPointsErrorHandler
{- |
Same as buildCornerPointsListOrFail, but only pushes list onto the stack if all the elements are CubePoints.
If any of the [CornerPoints] that are not CubePoints, they are still returned as the current value, so they can
be used in the next computation.

If the work is done right, all elements will be CubePoints or none of them will be. If it is a mixture, then
that will not work with the whole monad builder system.
-}
buildCubePointsListOrFail :: String -> CpointsList -> CpointsList -> ExceptT BuilderError (State CpointsStack ) CpointsList
buildCubePointsListOrFail  extraMsg cPoints cPoints' =
  let  cubeList = cPoints |+++| cPoints'
  in   case findCornerPointsError cubeList of
        Nothing ->
          if isCubePointsList cubeList 
             then lift $ state $ \cubeStack -> (cubeList, cubeList ++ cubeStack)
             else lift $ state $ \cubeStack -> (cubeList, cubeStack)
        Just err -> throwE (BuilderError (extraMsg ++ " " ++ (errMessage err)) {-cube-})
