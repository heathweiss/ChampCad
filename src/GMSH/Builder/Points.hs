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
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H

{- |
Given
GC.BuilderMonadData [CPts.CornerPoints]:
 The [Points] will be extracted from the [CPts.CornerPoints].

h: a handle to the .geo file, that t

Task
Extract the Either String [Point] from [CornerPoints]

Return
If Right [Point]
GBB.ExceptStackCornerPointsBuilder [Pts.Point]
otherwise throw the error msg.

Known uses:
The resulting [Point] can be manipulated by fx's such as traspose<X/Y/Z> for creating shapes.
In order to be converted into GPoints, must use "toNonOverlappingClosedPointsOrFail" to ensure points are not overlapped and the [point] is closed.
Once converted to GPoints, they do not get used for creating gmsh script, only GPoints are used.


-}
buildPointsList :: String -> GC.BuilderMonadData [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [Pts.Point]
buildPointsList extraMsg cPoints =
  (buildPointsListOrFail ( extraMsg ++ ": GMSH.Builder.Points.buildPointsList") (GC.eval cPoints)) 

{-
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
-}
buildPointsListOrFail :: String -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [Pts.Point]
--If an [] is passed in, just return an [], and the fact that it is empty will be handled by toNonOverlappingClosedPointsOrFail,
--at the point when they are to be converted into [GPoint]
buildPointsListOrFail  _ [] =  E.lift $ SL.state $ \builderData -> (GC.BuilderMonadData_Points([]), builderData)

buildPointsListOrFail extraMsg cPoints = do
  let
    points = CPts.toPointsFromList cPoints
  case points of
    Right points' ->
      E.lift $ SL.state $ \state' -> (GC.BuilderMonadData_Points(points'), state')
    Left e ->
      (TE.throwE $ extraMsg ++ ".buildPointsListOrFail: " ++ e)
    

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
  state' <- SL.get
  
  
  let
    nonOverLappedClosedPoints = GC.toNonOverlappingClosedPoints $ GC.eval points
    
  case nonOverLappedClosedPoints of
        Right (GC.NonOverLappedClosedPoints' nonOverLappedClosedPoints') -> do
          let
            bd = \builderMonadData -> (GC.BuilderMonadData_NonOverLappedClosedPoints(nonOverLappedClosedPoints'), state')
          
          E.lift $ SL.state $ bd
          
        Left e -> TE.throwE $ extraMsg ++ e
  
  

