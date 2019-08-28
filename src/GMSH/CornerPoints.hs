{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PatternSynonyms #-}
module GMSH.CornerPoints(buildCornerPointsSingle, buildCornerPoints, buildPoints, buildNoOverlapClosedPoints, NonOverLappedClosedPoints(..)) where
{- |
Bring [CornerPoints.CornerPoints] into the ExceptStackCornerPointsBuilder transformer stack.

-}
import qualified CornerPoints.CornerPoints as CPts
import qualified CornerPoints.Points as Pts

import qualified GMSH.Builder.Base as GBB
import qualified GMSH.State as GS
import qualified GMSH.State as GST
import qualified GMSH.Base as GB

import qualified Helpers.FileWriter as FW

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import qualified System.IO as SIO
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Lens

--default (T.Text)
--Won't compile with this. It was used in GMSH.Points, but causes probs with fx from GMSH.Builder.CornerPoints

makeLenses ''GST.BuilderStateData
makeLenses ''GST.GPointId

type ID = Int
type NonOverLappedClosedPoints = GB.NonOverLappedClosed [Pts.Point]


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
buildCornerPoints :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
--if an [] is passed in, nothing to do.
buildCornerPoints _ [] _ =  E.lift $ SL.state $ \state' -> ([], state')
buildCornerPoints _ _ [] =  E.lift $ SL.state $ \state' -> ([], state')
--has 2 valid [CornerPoints], so process them.
buildCornerPoints errMsg cPoints cPoints' = do
  let
    cubeList = cPoints CPts.|+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        --has no CornerPointsError
        Nothing -> 
          E.lift $ SL.state (\state' -> (cubeList, state'))
        --has a CornerPointsError
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          (TE.throwE $ errMsg ++ ": GMSH.Builder.CornerPoints.buildCornerPoints: " ++ (err))

-- | Runs buildCornerPoints when a single [CPts] is given.
buildCornerPointsSingle :: String -> [CPts.CornerPoints] -> GBB.ExceptStackCornerPointsBuilder [CPts.CornerPoints]
buildCornerPointsSingle errMsg cPoints =
  buildCornerPoints (errMsg ++ "GMSH.Builder.CornerPoints.buildCornerPointsSingle: ") [CPts.CornerPointsId | x <- [1..]] cPoints



{- |
definitions
closed:
A [a] in which the last == head.
Lines created from a closed [Point] will form a well formed surface, as the last line will end at the start of the 1st line.

nonoverlapping:
The point which is the end of a line, is reused to form the start of the next line.
If they were intstead overlapping, each point would exist twice(once as 1st point, once as second point, except the 1st and last points.
If they are not overlappoing, each point would have to be resued to start the next line, except the 1st and last points.

Given
points: A [Pts.Point] which may or may not be closed and overlapping.
Must be at least length == 3, so 3 lines can be created, and a well formed surface can be created from the 3 lines.

Task
Modify the [Pts.Point] to be closed and nonoverlapping.
Ensure lenght >= 3.

Return
A closed and nonoverlapping [Pts.Point] with length >=3

Known uses
This is the only way to create a NonOverLappedClosedPoints, as the constructor is not exported.
This creates a State in the BuilderMonadData, where a [GPoints] can be generated from a NonOverLappedClosedPoints, which is a closed nonoverlapped [Points].
The NonOverLappedClosedPoints will be turned into a nonoverlapped closed [GPoints].
-}
toNonOverlappingClosedPoints :: [Pts.Point] -> Either String NonOverLappedClosedPoints 
toNonOverlappingClosedPoints [] = do
  Left "toNonOverlappingClosedPoints has [] passed in. Length must be at least 3 for a well formed surface"

toNonOverlappingClosedPoints (p:[]) = Left "toNonOverlappingClosedPoints has (p:[]) passed in. Length must be at least 3 for a well formed surface"
toNonOverlappingClosedPoints (p:p1:[]) = Left "toNonOverlappingClosedPoints has (p:p1:[]) passed in. Length must be at least 3 for a well formed surface"
toNonOverlappingClosedPoints (p:points) =
  let
    --nonOverlappingClosedPoints = toNonOverlappingClosedPoints' p p points (NonOverLappedClosedPoints [p])
    nonOverlappingClosedPoints = toNonOverlappingClosedPoints' p p points (GB.NonOverLappedClosed [p])
    --with p: set as head, set as previous point, add to working list.
    
    --ensure that the resulting [NonOverLappedClosedPoints] length >= 3,
    --as a surface needs at least 3 points to have a surface area, otherwise it is just a pair of lines.
    has3 :: NonOverLappedClosedPoints  -> Bool
    has3 (GB.NonOverLappedClosed []) = False
    has3 (GB.NonOverLappedClosed (a:[])) = False
    has3 (GB.NonOverLappedClosed (a:b:[])) = False
    has3 (GB.NonOverLappedClosed (a:b:c:[])) = False
    has3 _ = True
    
  in
  case has3 nonOverlappingClosedPoints of
    True -> Right nonOverlappingClosedPoints
    False -> Left $ "toNonOverlappingClosedPoints: length of resulting [Point] < 3" 

{-
Given
head': The head of original [Point]. When the end of points is reached, it is used to see if list is closed.

prevPoint: Compared to the current point to see if they are overlapped.

origPoints: The orignal [Point] that is being checked for overlap and closure.

workingPoints: The [Point] being created that is not overlapped and is closed.

Task
Work through the orignal [Point], ensuring no overlap, and that it is closed.

Return
Closed and nonoverlapped [Point]
-}
toNonOverlappingClosedPoints' :: Pts.Point -> Pts.Point -> [Pts.Point] -> NonOverLappedClosedPoints -> NonOverLappedClosedPoints 
--Have hit the end of list. Ensure closed, then reverse the build up working list.
toNonOverlappingClosedPoints' head' prevPoint [] (GB.NonOverLappedClosed workingPoints) =
  case head' == prevPoint of
    True ->  GB.NonOverLappedClosed $ reverse workingPoints --Is closed.
    False ->  GB.NonOverLappedClosed $ reverse $ ( head' : workingPoints) --Not closed, so head' to the end of working list.
    --False -> fmap (reverse) $ fmap (head' :)  (NonOverLappedClosedPoints workingPoints)
    --False -> {-fmap (reverse) $-} fmap (head' :)  (NonOverLappedClosedPoints workingPoints)
toNonOverlappingClosedPoints' head' prevPoint (p:origPoints) (GB.NonOverLappedClosed workingPoints) =
  case p == prevPoint of
    True -> toNonOverlappingClosedPoints' head' p origPoints $ GB.NonOverLappedClosed workingPoints
    --False -> toNonOverlappingClosedPoints' head' p origPoints (GB.NonOverLappedClosed (p:workingPoints))
    False -> toNonOverlappingClosedPoints' head' p origPoints  $
                (GB.NonOverLappedClosed (p:workingPoints) )


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
In order to be converted into GPoints, must use "buildNoOverlapClosedPoints" to ensure points are not overlapped and the [point] is closed.
Once converted to GPoints, they do not get used for creating gmsh script, only GPoints are used.
-}
buildPoints :: String -> [CPts.CornerPoints] ->
                             GBB.ExceptStackCornerPointsBuilder [Pts.Point]
--If an [] is passed in, just return an [], and the fact that it is empty will be handled by buildNoOverlapClosedPoints,
--at the point when they are to be converted into [GPoint]
buildPoints  _ [] =  E.lift $ SL.state $ \builderData -> ([], builderData)

buildPoints extraMsg cPoints = do
  let
    points = CPts.toPointsFromList cPoints
  case points of
    Right points' ->
      E.lift $ SL.state $ \state' -> (points', state')
    Left e ->
      (TE.throwE $ extraMsg ++ ".buildPoints: " ++ e)

{- |
Given:::
extaMsg: error msg info passed in such as the name of the do notation variable name.

points:
[Points] which is to be converted into a NonOverLappedClosedPoints

return:::
If the [Points] can be turned into a NonOverLappedClosedPoints: GBB.ExceptStackCornerPointsBuilder GC.NonOverLappedClosedPoints
If not: Left error message.
-}

buildNoOverlapClosedPoints :: String ->  [Pts.Point] -> GBB.ExceptStackCornerPointsBuilder NonOverLappedClosedPoints
buildNoOverlapClosedPoints extraMsg points = do 
  state' <- SL.get
  
  
  let
    nonOverLappedClosedPoints = toNonOverlappingClosedPoints points
    
  case nonOverLappedClosedPoints of
        Right (nonOverLappedClosedPoints') -> do
          let
            bd = \builderMonadData -> (nonOverLappedClosedPoints', state')
          
          E.lift $ SL.state $ bd
          
        Left e -> TE.throwE $ extraMsg ++ e
  

