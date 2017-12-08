module Joiners.Delaunay() where

{- |
Join together 2,3,or 4 [CornerPoints].
Eg: cutting a cylinder out of a scanned tread.
-}

import CornerPoints.CornerPoints(CornerPoints(..),(+++))

data Terminator =
 Early -- ^ Terminate as soon as 1 list has ended
 |
 Late  -- ^ Keep running till all lists have ended.

-- | Join together 2 [CornerPoints]
delaunay2 :: Terminator -> [CornerPoints] -> [CornerPoints] -> [CornerPoints]
delaunay2 Late (x:xs) (y:ys) =
  delaunay2Late (xs) (ys) (x +++ y) ((x +++ y):[])

delaunay2 _ [] [] = [CornerPointsError "empty xs/ys passed into delaunay2"]
delaunay2 _ [] _ = [CornerPointsError "empty xs passed into delaunay2"]
delaunay2 _ _ []= [CornerPointsError "empty ys passed into delaunay2"]

--Run 2 lists to the end of both lists.
--
delaunay2Late :: [CornerPoints] -> [CornerPoints] ->  CornerPoints -> [CornerPoints] -> [CornerPoints] 
delaunay2Late    (x:xs)            (y:ys)             cpointCurr      cpointsJoined =
  --temp to compile
  cpointsJoined
  --let
    --get center of cpointCurr
      --get distance from center to endpoint
    --if distance is < both dist's to xLead/xFollow
      --move it ahead
    --if distance is >= xLead and < xFollow
      --add tocpointsCurr to xLead and continue
    --if distance is >= xFollow and < xLead
      --add cpointsCurr to xFollow and continue
    --if distance == xLead and xFollow
      --add cPointsCurr to $ xLead +++ xFollow and continue
    --if distance > xLead and xFollow
      --move center ahead 1 mm and continue

delaunay2Late  [] [] _ cpointsJoined =  reverse cpointsJoined
