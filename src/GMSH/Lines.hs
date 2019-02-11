module GMSH.Lines(toLines, toPoints, insert) where
{- |

-}

import CornerPoints.CornerPoints(CornerPoints(..), cpointType)
import qualified CornerPoints.FaceExtraction as FE
import CornerPoints.Points (Point(..))
import qualified GMSH.Points as GP
import qualified GMSH.Common as GC

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

-- | Id's for gmsh script objects. eg: Points, Lines
type LineID = Int
type PointID = Int

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
    hashWithSalt s (BottomFrontLine f1 f4) =
        s `H.hashWithSalt`
        f1 `H.hashWithSalt` f4
    hashWithSalt s (FrontLeftLine f1 f2) =
        s `H.hashWithSalt`
        f1 `H.hashWithSalt` f2
    hashWithSalt s (FrontTopLine f2 f3) =
        s `H.hashWithSalt`
        f2 `H.hashWithSalt` f3
    hashWithSalt s (FrontRightLine f3 f4) =
        s `H.hashWithSalt`
        f3 `H.hashWithSalt` f4
        
    hash (BackTopLine b2 b3) =
      1 `H.hashWithSalt` (BackTopLine b2 b3)
    hash (BottomFrontLine f1 f4) =
      1 `H.hashWithSalt` (BottomFrontLine f1 f4)
    hash (FrontLeftLine f1 f2) =
      1 `H.hashWithSalt` (FrontLeftLine f1 f2)
    hash (FrontTopLine f2 f3) =
      1 `H.hashWithSalt` (FrontTopLine f2 f3)
    hash (FrontRightLine f3 f4) =
      1 `H.hashWithSalt` (FrontRightLine f3 f4)


{- |
Task:

Return:
Is Line and not already inserted: Hash and insert it and return map as Right Changed
Is Line and already inserted: return map unchanged as  Right UnChanged
Is anything but a line: return Left e
-}
insert ::  CornerPoints -> [LineID] -> [PointID] -> HM.HashMap Int Int ->  Either String (HM.HashMap Int Int,[LineID], [PointID])
--insert ::  CornerPoints -> [LineID] -> [PointID] -> HM.HashMap Int Int ->  Either String (HM.HashMap Int Int,[LineID], [PointID])
leftOff
--replace all but CornerPoints params with BuilderData.

insert (CornerPointsError _) lineIds pointsIds hashmap  = Right (hashmap, lineIds, pointsIds)

insert (BottomFrontLine f1 f4) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (BottomFrontLine f1 f4)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds), pointsIds)
    False -> Right $ ((HM.insert hashedCPoint lineId map), lineIds, pointsIds)


insert (FrontLeftLine f1 f2) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (FrontLeftLine f1 f2)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds),pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)

insert (FrontRightLine f3 f4) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (FrontRightLine f3 f4)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds),pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)


insert (FrontTopLine f2 f3) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (FrontTopLine f2 f3)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds),pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)


insert (BackTopLine b2 b3) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (BackTopLine b2 b3)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds), pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)


insert (FrontFace f1 f2 f3 f4) lineIds pointsIds hashmap =
  --break into lines
  let
    frontFace = FrontFace f1 f2 f3 f4
    frontLines = toLines frontFace
    {-maybe better to do with recursion
    frontLeftLine   = FE.extractFrontLeftLine frontFace
    frontTopLine    = FE.extractFrontTopLine frontFace
    frontRightLine  = FE.extractFrontRightLine frontFace
    bottomFrontLine = FE.extractBottomFrontLine frontFace
-}
  in
  --Left "GMSH.Lines.insert: FrontFace missing pattern match."
  case frontLines of
    Right frontLines' -> insert' frontLines' lineIds pointsIds hashmap
    Left e -> Left e

  
{-
insert (FrontFace _ _ _ _) _ _ =
  Left "GMSH.Lines.insert: FrontFace missing pattern match."
-}
insert (B1 b1) lineIds pointsIds map = Right $ (map, lineIds, pointsIds)

--Catch unhandled pattern matches.
insert unhandled _ _ map =
  --GC.UnChanged map
  Left $ "GMSH.Lines.insert: missing pattern match for " ++ (cpointType unhandled)


insert' :: [CornerPoints] -> [LineID] -> [PointID] -> HM.HashMap Int Int ->  Either String (HM.HashMap Int Int, [LineID], [PointID])
insert' [] lineIds pointsIds hashmap = Right (hashmap, lineIds, pointsIds)
insert' (cpt:cpts) lineIds pointsIds hashmap =
  let
    inserted = insert cpt lineIds pointsIds hashmap
  in
  case inserted of
    Right (hashmap', lineIds', pointsIds') -> insert' cpts lineIds' pointsIds' hashmap'
    Left e -> Left e
{-
insert ::  CornerPoints -> [LineID] -> [PointID] -> HM.HashMap Int Int ->  Either String (HM.HashMap Int Int,[LineID], [PointID])

insert (CornerPointsError _) lineIds pointsIds hashmap  = Right (hashmap, lineIds, pointsIds)

insert (BottomFrontLine f1 f4) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (BottomFrontLine f1 f4)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds), pointsIds)
    False -> Right $ ((HM.insert hashedCPoint lineId map), lineIds, pointsIds)


insert (FrontLeftLine f1 f2) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (FrontLeftLine f1 f2)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds),pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)

insert (FrontRightLine f3 f4) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (FrontRightLine f3 f4)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds),pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)


insert (FrontTopLine f2 f3) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (FrontTopLine f2 f3)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds),pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)


insert (BackTopLine b2 b3) (lineId:lineIds) pointsIds map =
  let hashedCPoint = H.hash (BackTopLine b2 b3)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ (map,(lineId:lineIds), pointsIds)
    False -> Right $ (HM.insert hashedCPoint lineId map, lineIds, pointsIds)


insert (FrontFace f1 f2 f3 f4) lineIds pointsIds hashmap =
  --break into lines
  let
    frontFace = FrontFace f1 f2 f3 f4
    frontLines = toLines frontFace
    {-maybe better to do with recursion
    frontLeftLine   = FE.extractFrontLeftLine frontFace
    frontTopLine    = FE.extractFrontTopLine frontFace
    frontRightLine  = FE.extractFrontRightLine frontFace
    bottomFrontLine = FE.extractBottomFrontLine frontFace
-}
  in
  --Left "GMSH.Lines.insert: FrontFace missing pattern match."
  case frontLines of
    Right frontLines' -> insert' frontLines' lineIds pointsIds hashmap
    Left e -> Left e

  
{-
insert (FrontFace _ _ _ _) _ _ =
  Left "GMSH.Lines.insert: FrontFace missing pattern match."
-}
insert (B1 b1) lineIds pointsIds map = Right $ (map, lineIds, pointsIds)

--Catch unhandled pattern matches.
insert unhandled _ _ map =
  --GC.UnChanged map
  Left $ "GMSH.Lines.insert: missing pattern match for " ++ (cpointType unhandled)


insert' :: [CornerPoints] -> [LineID] -> [PointID] -> HM.HashMap Int Int ->  Either String (HM.HashMap Int Int, [LineID], [PointID])
insert' [] lineIds pointsIds hashmap = Right (hashmap, lineIds, pointsIds)
insert' (cpt:cpts) lineIds pointsIds hashmap =
  let
    inserted = insert cpt lineIds pointsIds hashmap
  in
  case inserted of
    Right (hashmap', lineIds', pointsIds') -> insert' cpts lineIds' pointsIds' hashmap'
    Left e -> Left e


--------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------

insert ::  CornerPoints -> Int -> HM.HashMap Int Int -> Either String (GC.Changes (HM.HashMap Int Int ))
insert (BackTopLine b2 b3) value map =
  let hashedCPoint = H.hash (BackTopLine b2 b3)
  in
  case HM.member hashedCPoint map of
    True ->  Right $ GC.UnChanged map
    False -> Right $ GC.Changed $ HM.insert hashedCPoint value map

insert (FrontFace _ _ _ _) _ _ =
  Left "GMSH.Lines.insert: FrontFace cannot be inserted."

insert (B1 b1) _ map = Right $ GC.UnChanged map

--Catch unhandled pattern matches.
insert unhandled _ map =
  --GC.UnChanged map
  Left $ "GMSH.Lines.insert: missing pattern match for " ++ (cpointType unhandled)


-}  
