module GMSH.Lines(toLines, toPoints, insert) where
{- |

-}

import CornerPoints.CornerPoints(CornerPoints(..), cpointType)
import qualified CornerPoints.FaceExtraction as FE
import CornerPoints.Points (Point(..))
import qualified GMSH.Hashable.Points as GP
import qualified GMSH.Common as GC

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

{-
Given:
  CornerPoints constructor: CornerPoints from which will extract [<BackTop/FrontTop/...>Line]

Task:
  Extract the [<BackTop/FrontTop/...>Line] from the CornPoints if it is a valid CornPoints.
  Invalid CornPoints are: B1 B2 B3 B4 F1 F2 F3 F4 as lines can't be extracted.

Return:
  Left msg: if CornerPoints can't be converted into a [GPoint]
  Right [Point]: if CornerPoints can be converted into a [GPoint]

Known uses:
Break CornerPoints Cubes and Faces into lines, that will be converted into gmsh lines.

-}
--only Lines, Faces, and Cubes can be converted into Lines.
toLines :: CornerPoints -> Either String [CornerPoints]
toLines (B1 _) = Left "B1 can't be converted into a line."
toLines (B2 _) = Left "B2 can't be converted into a line."
toLines (B3 _) = Left "B3 can't be converted into a line."
toLines (B4 _) = Left "B4 can't be converted into a line."
toLines (F1 _) = Left "F1 can't be converted into a line."
toLines (F2 _) = Left "F2 can't be converted into a line."
toLines (F3 _) = Left "F3 can't be converted into a line."
toLines (F4 _) = Left "F4 can't be converted into a line."

toLines (BackFace b1 b2 b3 b4) =
  let
    backFace = (BackFace b1 b2 b3 b4)
  in
  Right [FE.extractBackLeftLine backFace, FE.extractBackTopLine backFace, FE.extractBackRightLine backFace, FE.extractBackBottomLine backFace]

toLines (FrontFace f1 f2 f3 f4) =
  let
    frontFace = (FrontFace f1 f2 f3 f4)
  in
  Right [FE.extractFrontLeftLine frontFace, FE.extractFrontTopLine frontFace, FE.extractFrontRightLine frontFace, FE.extractBottomFrontLine frontFace]

toLines unhandled = Left $ "GMSH.Lines.toLines: missing pattern match: " ++ (cpointType unhandled)


{- |
Task:
Extract the [Point] that make up a CornerPoints

Known uses:
Extract the Points, so they can be converted into gmsh points.
-}
toPoints :: CornerPoints -> Either String [Point]
toPoints (BackFace b1 b2 b3 b4) = Right [b1, b2, b3, b4]


---------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------hashable-----------------------------------------------------------------
{- |
Task:
Make CornerPoint an instance of Hashable so it can be  used with Data.HashMap.Strict

Task:
Need to ensure it is a Line, and not a Point(B1,F1...),Face, or Cube. But how?


Known uses:
-- Associate CornerPoints Lines with an Id::Int for generating GMSH script lines.
-}
instance H.Hashable CornerPoints where
    hashWithSalt s (BackTopLine b2 b3) =
        s `H.hashWithSalt`
        b2 `H.hashWithSalt` b3
        
    hash (BackTopLine b2 b3) =
      1 `H.hashWithSalt` (BackTopLine b2 b3)


{- |
Task:

Return:
Is Line and not already inserted: Hash and insert it and return map as Right Changed
Is Line and already inserted: return map unchanged as  Right UnChanged
Is anything but a line: return Left e
-}
insert ::  CornerPoints -> Int -> HM.HashMap Int Int -> Either String (GC.Changes (HM.HashMap Int Int ))
insert (BackTopLine b2 b3) value map =
  let hashedCPoint = H.hash (BackTopLine b2 b3)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ GC.UnChanged map
    False -> Right $ GC.Changed $ HM.insert hashedCPoint value map

next
{-
insert should return Either String (HM.HashMap Int Int, [Int])
so that FrontFace can be broken down into 2 lines and both inserted.
This requires that 2 id's be taken from the [Int] and thus using GC.Changes does not work,
as that only allows for a single id to be taken off at the calling fx.
Thus
todo:
Get rid of the GC.UnChanged system.
Change insert to: CornerPoints -> [Int] -> HM.HashMap Int Int -> Either String (HM.HashMap Int Int, [Int])
-}
insert (FrontFace _ _ _ _) _ _ =
  Left "GMSH.Lines.insert: FrontFace cannot be inserted."

insert (B1 b1) _ map = Right $ GC.UnChanged map

--Catch unhandled pattern matches.
insert unhandled _ map =
  --GC.UnChanged map
  Left $ "GMSH.Lines.insert: missing pattern match for " ++ (cpointType unhandled)

{-
insert ::  CornerPoints -> Int -> HM.HashMap Int Int -> GC.Changes (HM.HashMap Int Int )
insert (BackTopLine b2 b3) value map =
  let hashedCPoint = H.hash (BackTopLine b2 b3)
  in
  case HM.member hashedCPoint map of
    True ->  GC.UnChanged map
    False -> GC.Changed $ HM.insert hashedCPoint value map

--Catch unhandled pattern matches.
--Should be a Either(or another Changes constructor) as unhandled should be an error.
--
insert unhandled _ map =
  GC.UnChanged map

-}  
