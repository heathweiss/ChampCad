module GMSH.Builder.PointsNGADT(buildPointsList, toNonOverlappingClosedPointsOrFail) where

import qualified CornerPoints.CornerPoints as CPts
import qualified CornerPoints.Points as Pts

import qualified GMSH.Builder.BaseNGADT as GBB
import qualified GMSH.PointsNGADT as GPts

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO

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

buildPointsList :: String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [Pts.Point]
buildPointsList extraMsg cPoints = 
  (buildPointsListOrFail ( extraMsg ++ ": GMSH.Builder.Points.buildPointsList") cPoints )

{-
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.
-}
buildPointsListOrFail :: String -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [Pts.Point]
--If an [] is passed in, just return an [], and the fact that it is empty will be handled by toNonOverlappingClosedPointsOrFail,
--at the point when they are to be converted into [GPoint]
buildPointsListOrFail  _ [] =  E.lift $ SL.state $ \builderData -> ([], builderData)

buildPointsListOrFail extraMsg cPoints = do
  let
    points = CPts.toPointsFromList cPoints
  case points of
    Right points' ->
      E.lift $ SL.state $ \state' -> (points', state')
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

toNonOverlappingClosedPointsOrFail :: String ->  [Pts.Point] -> GBB.ExceptStackCornerPointsBuilder GPts.NonOverLappedClosedPoints
toNonOverlappingClosedPointsOrFail extraMsg points = do 
  state' <- SL.get
  
  
  let
    nonOverLappedClosedPoints = GPts.toNonOverlappingClosedPoints points
    
  case nonOverLappedClosedPoints of
        Right (nonOverLappedClosedPoints') -> do
          let
            bd = \builderMonadData -> (nonOverLappedClosedPoints', state')
          
          E.lift $ SL.state $ bd
          
        Left e -> TE.throwE $ extraMsg ++ e
  
