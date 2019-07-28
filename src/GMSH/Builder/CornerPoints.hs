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
Task:
Build a [CPts] by adding the 2 given [CPts].
Return:
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
If no errors, return the list as current value of Builder, with the state unchanged.
-}
{-
buildCubePointsList_h :: SIO.Handle ->  String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
buildCubePointsList_h h extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail_h h  (extraMsg ++ "GMSH.Builder.CornerPoints.buildCubePointsList_h: ") cPoints cPoints')  --`E.catchError` GBB.errorHandler
  

-- | Runs buildCubePointsList_h when a single [CPts] is given.
buildCubePointsListSingle_h :: SIO.Handle -> String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
                       
buildCubePointsListSingle_h h extraMsg cPoints =
  buildCubePointsList_h h (extraMsg ++ "GMSH.Builder.CornerPoints.buildCubePointsListSingle_h: ") [CPts.CornerPointsId | x <- [1..]] cPoints

-- | Executes buildCubePointsList_h as per standard ExceptT procedure in the MTL package.
buildCubePointsListOrFail_h :: SIO.Handle -> String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
--if an [] is passed in, nothing to do.
buildCubePointsListOrFail_h _ _ [] _ =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
buildCubePointsListOrFail_h _ _ _ [] =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
--has 2 valid [CornerPoints], so process them.
buildCubePointsListOrFail_h h extraMsg cPoints cPoints' = do
  let
    cubeList = cPoints CPts.|+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        --has no CornerPointsError
        Nothing -> 
          E.lift $ SL.state (\state' -> (GC.BuilderMonadData_CPoints(cubeList), state'))
        --has a CornerPointsError
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          (E.liftIO $ GW.writeComment h $ extraMsg ++ ": buildCubePointsListOrFail_h: " ++ err)
          >> (TE.throwE $ extraMsg ++ ": " ++ (err))

-}
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------

buildCubePointsList :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
buildCubePointsList extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail (extraMsg ++ "GMSH.Builder.CornerPoints.buildCubePointsList: ") cPoints cPoints')  --`E.catchError` GBB.errorHandler
  

-- | Runs buildCubePointsList when a single [CPts] is given.
buildCubePointsListSingle :: String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
                       
buildCubePointsListSingle extraMsg cPoints =
  buildCubePointsList (extraMsg ++ "GMSH.Builder.CornerPoints.buildCubePointsListSingle: ") [CPts.CornerPointsId | x <- [1..]] cPoints

-- | Executes buildCubePointsList as per standard ExceptT procedure in the MTL package.
buildCubePointsListOrFail :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
--if an [] is passed in, nothing to do.
buildCubePointsListOrFail _ [] _ =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
buildCubePointsListOrFail _ _ [] =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
--has 2 valid [CornerPoints], so process them.
buildCubePointsListOrFail extraMsg cPoints cPoints' = do
  let
    cubeList = cPoints CPts.|+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        --has no CornerPointsError
        Nothing -> 
          E.lift $ SL.state (\state' -> (GC.BuilderMonadData_CPoints(cubeList), state'))
        --has a CornerPointsError
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          (TE.throwE $ extraMsg ++ ": buildCubePointsListOrFail: " ++ (err))


{-
buildCubePointsList :: SIO.Handle ->  String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
buildCubePointsList h extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail h  (extraMsg ++ "GMSH.Builder.CornerPoints.buildCubePointsList: ") cPoints cPoints')  --`E.catchError` GBB.errorHandler
  

-- | Runs buildCubePointsList when a single [CPts] is given.
buildCubePointsListSingle :: SIO.Handle -> String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
                       
buildCubePointsListSingle h extraMsg cPoints =
  buildCubePointsList h (extraMsg ++ "GMSH.Builder.CornerPoints.buildCubePointsListSingle: ") [CPts.CornerPointsId | x <- [1..]] cPoints

-- | Executes buildCubePointsList as per standard ExceptT procedure in the MTL package.
buildCubePointsListOrFail :: SIO.Handle -> String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
--if an [] is passed in, nothing to do.
buildCubePointsListOrFail _ _ [] _ =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
buildCubePointsListOrFail _ _ _ [] =  E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_CPoints([]), state')
--has 2 valid [CornerPoints], so process them.
buildCubePointsListOrFail h extraMsg cPoints cPoints' = do
  let
    cubeList = cPoints CPts.|+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        --has no CornerPointsError
        Nothing -> 
          E.lift $ SL.state (\state' -> (GC.BuilderMonadData_CPoints(cubeList), state'))
        --has a CornerPointsError
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          (E.liftIO $ GW.writeComment h $ extraMsg ++ ": buildCubePointsListOrFail: " ++ err)
          >> (TE.throwE $ extraMsg ++ ": " ++ (err))



-}
