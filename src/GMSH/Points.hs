{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PatternSynonyms #-}
module GMSH.Points(NonOverLappedClosedPoints(), pattern NonOverLappedClosedPoints', toNonOverlappingClosedPoints) where
{- |

GMSH functionality for CornerPoints.Points:

Hash CornerPoints.Point so they can be stored in a hash map.

Insert Points into a GC.BuilderStateData, for use with the GMSH.Builder Builder monad stack.
All that gets inserted, is the ID.

Note that all Data types for Points, are kept in GMSH.Common.
-}

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified System.IO as SIO
import qualified Control.Monad.Except as E

import Control.Lens

import qualified CornerPoints.Points as Pts
import CornerPoints.CornerPoints(CornerPoints(..))
import qualified CornerPoints.CornerPoints as CPts
import qualified GMSH.State as GST
import qualified GMSH.GPoints as GGPts
import qualified Helpers.FileWriter as FW

import qualified Helpers.FileWriter as FW

default (T.Text)

makeLenses ''GST.BuilderStateData
makeLenses ''GST.GPointId

type ID = Int



 

  

-- | A [Pts.Point] in which the points have no overlap, so to make line, will need to reuse each point(except head and last) for making 2 lines.
-- | The last Point will be the same as head.
-- | Does not have the constructor exported, as the the fx: toNonOverlappingClosedPoints is the only way to get to this state.
--newtype NonOverLappedClosedPoints = NonOverLappedClosedPoints [Pts.Point]
--newtype NonOverLappedClosedPoints = NonOverLappedClosedPoints Pts.Point
--newtype NonOverLappedClosedPoints = NonOverLappedClosedPoints [Pts.Point]
newtype NonOverLappedClosedPoints  = NonOverLappedClosedPoints [Pts.Point]

pattern NonOverLappedClosedPoints' a <- NonOverLappedClosedPoints a

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
    nonOverlappingClosedPoints = toNonOverlappingClosedPoints' p p points (NonOverLappedClosedPoints [p])
    --with p: set as head, set as previous point, add to working list.
    
    --ensure that the resulting [NonOverLappedClosedPoints] length >= 3,
    --as a surface needs at least 3 points to have a surface area, otherwise it is just a pair of lines.
    has3 :: NonOverLappedClosedPoints  -> Bool
    has3 (NonOverLappedClosedPoints []) = False
    has3 (NonOverLappedClosedPoints (a:[])) = False
    has3 (NonOverLappedClosedPoints (a:b:[])) = False
    has3 (NonOverLappedClosedPoints (a:b:c:[])) = False
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
toNonOverlappingClosedPoints' head' prevPoint [] (NonOverLappedClosedPoints workingPoints) =
  case head' == prevPoint of
    True ->  NonOverLappedClosedPoints $ reverse workingPoints --Is closed.
    False ->  NonOverLappedClosedPoints $ reverse $ ( head' : workingPoints) --Not closed, so head' to the end of working list.
    --False -> fmap (reverse) $ fmap (head' :)  (NonOverLappedClosedPoints workingPoints)
    --False -> {-fmap (reverse) $-} fmap (head' :)  (NonOverLappedClosedPoints workingPoints)
toNonOverlappingClosedPoints' head' prevPoint (p:origPoints) (NonOverLappedClosedPoints workingPoints) =
  case p == prevPoint of
    True -> toNonOverlappingClosedPoints' head' p origPoints $ NonOverLappedClosedPoints workingPoints
    --False -> toNonOverlappingClosedPoints' head' p origPoints (NonOverLappedClosedPoints (p:workingPoints))
    False -> toNonOverlappingClosedPoints' head' p origPoints  $
                (NonOverLappedClosedPoints (p:workingPoints) )

