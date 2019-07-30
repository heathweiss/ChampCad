{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.CornerPoints({-buildCubePointsList_h, buildCubePointsListSingle_h,-} buildCubePointsList, buildCubePointsListSingle) where
{- |
Supply the Builder functions that deal with [CornerPoints.CornerPoints]
-}
import qualified CornerPoints.CornerPoints as CPts  

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.Common as GC
import qualified GMSH.Writer as GW

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO


{- |
Given
errMsg: Error message information that is passed in by previous functions that have called this fx.
Typically used to build up a chain of fx calls, to identify the chain of events that resulted in the error.
Any prior errors, will have thrown an exception before reaching here.

cPoints<'>: The 2 [CPts.CornerPoints] to be added together.

Task
Add the 2 given [CPts], and check for errors from the addition operation.

Return
If error during [CPts] |+++| [CPts]
An error is thrown so the ExceptT short circuits.
Error msg includes information passed in via errMsg, plus this function name/location, and the err msg.
The error msg gets passed up and handled at a higher level, such as writing it to the .geo file.

If no errors
Return the newly added [CornerPoints] as current value of BuilderMonadData(a), with the BuilderStateData(s) unchanged.
That is (a,s) or (BuilderMonadData::[CornerPoints], BuilderStateData)

Known uses
Build up shapes using CornerPoints, from within a ExceptStackCornerPointsBuilder.
Perhaps this should be removed, and only use Points in the future, as GMSH takes care of the meshing,
and the whole concept of CornerPoints (vs Points) was to generate the stl mesh.
-}
buildCubePointsList :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
--if an [] is passed in, nothing to do.
buildCubePointsList _ [] _ =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
buildCubePointsList _ _ [] =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
--has 2 valid [CornerPoints], so process them.
buildCubePointsList errMsg cPoints cPoints' = do
  let
    cubeList = cPoints CPts.|+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        --has no CornerPointsError
        Nothing -> 
          E.lift $ SL.state (\state' -> (GC.BuilderMonadData_CPoints(cubeList), state'))
        --has a CornerPointsError
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          (TE.throwE $ errMsg ++ ": GMSH.Builder.CornerPoints.buildCubePointsList: " ++ (err))

-- | Runs buildCubePointsList when a single [CPts] is given.
buildCubePointsListSingle :: String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
buildCubePointsListSingle errMsg cPoints =
  buildCubePointsList (errMsg ++ "GMSH.Builder.CornerPoints.buildCubePointsListSingle: ") [CPts.CornerPointsId | x <- [1..]] cPoints

