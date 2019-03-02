{-# LANGUAGE TemplateHaskell #-}
module GMSH.Lines(toLines, toPoints, insert) where
{- |
Hash and insert Lines into the GMSH.Common.BuilderData datatype.
Gets a unique ID for each line inserted.

When writing out the script, will it get the Point Id's from the point hashmap or should it store them itself.
Will have to either store them, or store the Points so they can be hashed to retrieve the Point Id.

Lines are only inserted once. How do I ensure this?
I hash the 2 Points and check the hashmap. But what about swapping the order of the points?
And will it matter if they are put in with opposite Point ordering?

CornerPoints such as B1/F4 don't get inserted. Must be at least a line such as FrontTopLine.
CubePoints and Faces get broken down into their constituent Lines.



Tests: GmshLinesTest
-}

import CornerPoints.CornerPoints(CornerPoints(..), cpointType)
import qualified CornerPoints.FaceExtraction as FE
import CornerPoints.Points (Point(..))
import qualified GMSH.Points as GP
import qualified GMSH.Common as GC

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens
makeLenses ''GC.BuilderData

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
Given:
CornerPoints: A CornerPoints to be inserted into a GMSH.Common.BuilderData using GMSH.Builder.ExceptStackCornerPointsBuilder monad stack.
GC.BuilderData: The BuilderData into which the CornerPoints gets hashed/inserted.


Task:
If the CornerPoints is line, face or cube: use pattern matching on the [CPts] param.
If is a face or cube: break it down into lines and insert each one.
Hash the line and see if it already exists in the BuilderData lines hashmap.
If not already inserted:
 Insert the hashed line into BuilderData lines hashmap.
  Use the current Id form the BuilderData linesIds and adjust the BuilderData linesIds by removing the head id.
 Insert each of the Points it contains, into the BuilderData points hashmap. See GMSH.Points.insert about that.


Return:
Is a Line and not already inserted: Hash and insert it and return map as Right Changed
 Right BuilderData if no errors
 Left e if inserting line gave errors.
Is a Line and already inserted: return Right $ BuilderData unchanged 
Is a face or cube already inserted:
 Right $ BuilderData (with changes)
Is a face or cube previously inserted:
 Right $ BuilderData (with changes applied)
Is a B1/B2... : Right $ BuilderData unchanged

If an error occurs, such as unhandled pattern match: Left e
-}

insert ::  CornerPoints -> GC.BuilderData ->  Either String GC.BuilderData

insert (CornerPointsError _) builderData  = Right builderData

--now inserts the points. Passes tests.
insert (BackTopLine b2 b3) builderData =
  let
    hashedCPoint = H.hash (BackTopLine b2 b3)
  in
  case HM.member hashedCPoint (builderData ^. linesMap) of --is CPnt already in the lines map.
    True ->  Right builderData --yes it is so do nothing
    False -> --not it is not so insert the CPt.
      let
        builderDataWithPointsInserted = GP.insert [b2,b3] builderData 
      in
      Right $ builderDataWithPointsInserted
                {GC._linesMap = HM.insert hashedCPoint (head $ builderData ^. linesId) (builderData ^. linesMap),
                 GC._linesId = tail $ builderData ^. linesId
                }

--next
--adjust the rest of the insert fx's and make sure they are tested.
--Make sure all CornerPoints constructors have been handled.

--inserts points and passes a test
insert (BottomFrontLine f1 f4) builderData =
  let hashedCPoint = H.hash (BottomFrontLine f1 f4)
  in
  case HM.member hashedCPoint (builderData ^. linesMap) of
    True ->  Right $ builderData 
    False ->
      let
        builderDataWithPointsInserted = GP.insert [f1,f4] builderData 
      in
      Right $ builderDataWithPointsInserted
                       {GC._linesMap = HM.insert hashedCPoint (head $ builderData ^. linesId) (builderData ^. linesMap),
                        GC._linesId = tail $ builderData ^. linesId
                       }

insert (FrontLeftLine f1 f2) builderData =
  let hashedCPoint = H.hash (FrontLeftLine f1 f2)
  in
  case HM.member hashedCPoint (builderData ^.linesMap ) of
    True ->  Right builderData
    False ->
      let
        builderDataWithPointsInserted = GP.insert [f1,f2] builderData 
      in
      Right $ builderDataWithPointsInserted
        {GC._linesMap = HM.insert hashedCPoint (head $ builderData ^. linesId) (builderData ^. linesMap),
         GC._linesId = tail $ builderData ^. linesId
        }


insert (FrontRightLine f3 f4) builderData =
  let hashedCPoint = H.hash (FrontRightLine f3 f4)
  in
  case HM.member hashedCPoint (builderData ^.linesMap ) of
    True ->  Right builderData
    False ->
      let
        builderDataWithPointsInserted = GP.insert [f3,f4] builderData 
      in
      Right $ builderDataWithPointsInserted {GC._linesMap = HM.insert hashedCPoint (head $ builderData ^. linesId) (builderData ^. linesMap),
                                  GC._linesId = tail $ builderData ^. linesId
                                 }

insert (FrontTopLine f2 f3) builderData =
  let hashedCPoint = H.hash (FrontTopLine f2 f3)
  in
  case HM.member hashedCPoint (builderData ^.linesMap ) of
    True ->  Right builderData
    False ->
      let
        builderDataWithPointsInserted = GP.insert [f2,f3] builderData 
      in
      Right $ builderDataWithPointsInserted {GC._linesMap = HM.insert hashedCPoint (head $ builderData ^. linesId) (builderData ^. linesMap),
                                  GC._linesId = tail $ builderData ^. linesId
                                 }


insert (FrontFace f1 f2 f3 f4) builderData =
  --break into lines
  let
    frontFace = FrontFace f1 f2 f3 f4
    frontLines = toLines frontFace
  in
  case frontLines of
    Right frontLines' -> insert' frontLines' builderData
    Left e -> Left e

insert (B1 b1) builderData = Right builderData
--insert (B1 b1) lineIds pointsIds map = Right $ (map, lineIds, pointsIds)

insert unhandled _  =
  Left $ "GMSH.Lines.insert: missing pattern match for " ++ (cpointType unhandled)

insert' :: [CornerPoints] -> GC.BuilderData ->  Either String GC.BuilderData
insert' [] builderData = Right builderData
insert' (cpt:cpts) builderData =
  case insert cpt builderData of
    Right builderData' -> insert' cpts builderData'
    Left e -> Left e





