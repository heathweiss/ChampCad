{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder.Points(buildPointsList, toNonOverlappingClosedPointsOrFail) where
{- |
All GMSH Builder functions dealing with [CornerPoints.Points].
-}

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.Common as GC
import qualified GMSH.Points as GP
import qualified GMSH.Writer as GW

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
Given
GC.BuilderMonadData [CPts.CornerPoints]:
 The [Points] will be extracted from the [CPts.CornerPoints].

h: a handle to the .geo file, that t

Task
Extract the [Point] from [CornerPoints]

Return
[Pts.Point].
Known uses:
Used by toNonOverlappingClosedPointsOrFail to ensure points are not overlapped and the [point] is closed.


-}
buildPointsList :: String -> GC.BuilderMonadData [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [Pts.Point]
--buildPointsList_GADT extraMsg cPoints = buildPointsList extraMsg $ GC.eval cPoints
buildPointsList extraMsg cPoints =
  (buildPointsListOrFail extraMsg (GC.eval cPoints)) --  `catchError` GBB.errorHandler 

{-
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
-}
buildPointsListOrFail :: String -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [Pts.Point]
--if an [] is passed in, just return an [].
buildPointsListOrFail  _ [] =  lift $ state $ \builderData -> (GC.BuilderMonadData_Points([]), builderData)

buildPointsListOrFail extraMsg cPoints = do
  --state' <- get
  let
    points =  buildPointsListOrFail' cPoints (CPts.toPointsFromList)
  case points of
    Right points' ->
      lift $ state $ \state' -> (GC.BuilderMonadData_Points(points'), state')
    Left e ->
      {-
      (liftIO $ GW.writeComment h $ extraMsg ++ ": " ++ e)
      >> (TE.throwE $ extraMsg ++ ": buildPointsListOrFail: " ++ e) -}
      (TE.throwE $ extraMsg ++ ": buildPointsListOrFail: " ++ e)
    
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

{-
buildPointsList :: SIO.Handle -> String -> GC.BuilderMonadData [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [Pts.Point]
--buildPointsList_GADT extraMsg cPoints = buildPointsList extraMsg $ GC.eval cPoints
buildPointsList h extraMsg cPoints =
  (buildPointsListOrFail h extraMsg (GC.eval cPoints)) --  `catchError` GBB.errorHandler 

{-
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
-}
buildPointsListOrFail :: SIO.Handle -> String -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [Pts.Point]
--if an [] is passed in, just return an [].
buildPointsListOrFail _ _ [] =  lift $ state $ \builderData -> (GC.BuilderMonadData_Points([]), builderData)

buildPointsListOrFail h extraMsg cPoints = do
  --state' <- get
  let
    points =  buildPointsListOrFail' h cPoints (CPts.toPointsFromList)
  case points of
    Right points' ->
      lift $ state $ \state' -> (GC.BuilderMonadData_Points(points'), state')
    Left e -> 
      (liftIO $ GW.writeComment h $ extraMsg ++ ": " ++ e)
      >> (TE.throwE $ extraMsg ++ ": buildPointsListOrFail: " ++ e)
  
    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
buildPointsListOrFail' :: SIO.Handle -> [CPts.CornerPoints] -> ([CPts.CornerPoints] -> Either String [Pts.Point]) -> Either String [Pts.Point]
--end of the list. Return whatever has been built up in the BuilderData.
buildPointsListOrFail' _ [] _ = Right []
buildPointsListOrFail' h cubeList toPoints =
  let
    --points = CPts.toPointsFromList (cube:cubeList)
    points = toPoints cubeList
    
  in
  case points of
    Right points' ->
      Right $ points' 
    Left e -> Left e

-}
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--create new insert version that returns ExceptStackCornerPointsBuilder so I can do IO

{- |
Given:::
extaMsg: error msg info passed in such as the name of the do notation variable name.

points:
[Points] which is to be converted into a NonOverLappedClosedPoints

return:::
If the [Points] can be turned into a NonOverLappedClosedPoints: GBB.ExceptStackCornerPointsBuilder GC.NonOverLappedClosedPoints
If not: Left.
-}
toNonOverlappingClosedPointsOrFail :: String ->  GC.BuilderMonadData [Pts.Point] -> GBB.ExceptStackCornerPointsBuilder GC.NonOverLappedClosedPoints
toNonOverlappingClosedPointsOrFail extraMsg points = do 
  state' <- get
  
  
  let
    nonOverLappedClosedPoints = GC.toNonOverlappingClosedPoints $ GC.eval points
    
  case nonOverLappedClosedPoints of
        Right (GC.NonOverLappedClosedPoints' nonOverLappedClosedPoints') -> do
          let
            bd = \builderMonadData -> (GC.BuilderMonadData_NonOverLappedClosedPoints(nonOverLappedClosedPoints'), state')
          
          lift $ state $ bd
          
        Left e -> TE.throwE $ extraMsg ++ e
  
  

