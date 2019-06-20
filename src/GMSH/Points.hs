{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module GMSH.Points({- H.hash, H.hashWithSalt, insert,-} insertWithOvrLap, insertNoOvrLap, retrieve) where

{- |
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
import qualified GMSH.Common as GC
import qualified Helpers.FileWriter as FW

import qualified Helpers.FileWriter as FW

default (T.Text)

makeLenses ''GC.BuilderStateData
makeLenses ''GC.GPointsStateData

type ID = Int


-- | Make CornerPoints.Point an instance of Hashable so it can be  used with Data.HashMap.Strict
-- Known uses:
-- Associate CornerPoints.Point with an Id::Int for generating GMSH script.
-- Only 'hash' is being used so far.
instance H.Hashable Pts.Point where
    hashWithSalt s (Pts.Point x y z) =
        s `H.hashWithSalt`
        x `H.hashWithSalt`
        y `H.hashWithSalt` z
    hash point =
      1 `H.hashWithSalt` point



{- |
Given:
 map :: HM.HashMap Int Int
 An existing HashMap to insert the point into.

 value :: Int
 The value (gmsh point id) to store in the map.
 Does the Point have to be stored, or just the ID of the gmsh point?
 -Leave for now, but look at deleting it later, if not used.
 Should the GPoint Id have it's own type instead of being just an Int?

Task:
 Hash the point and see if it already exists in the map.
 If not already in map insert it.
 

Return:
 If point did not already exist in map:
 The map with the hashed point as key,  and id and point inserted as a GMSH.Common.GPointsStateData

 If point already exists in map:
 The original map, unchanged.
-}
--insert ::  [Pts.Point] -> [ID] -> HM.HashMap Int Int -> (HM.HashMap Int Int,[ID])
{- Can I delete this instead of fixing for use of GC.GPointsId
insert ::  [Pts.Point] -> GC.BuilderStateData -> GC.BuilderStateData
insert [] builderData = builderData
insert  (point:points) builderData   = 
  let
    hashedPoint = H.hash point
  in
  case HM.member hashedPoint (builderData ^. pointsMap) of
    True ->  
      insert points builderData
    False ->
      let
        mapWithCurrentPointInserted = (HM.insert hashedPoint  (GC.PointsBuilderData (head $ builderData ^. pointsIdSupply) point) (builderData ^. pointsMap))
      in
      insert points (builderData
                     {GC._pointsIdSupply = (tail $ (builderData ^. pointsIdSupply)),
                      GC._pointsMap = mapWithCurrentPointInserted
                     }
                    ) 
-}

retrieve ::  GC.BuilderStateData -> Pts.Point -> Maybe GC.GPointsStateData
retrieve  builderStateData point =
  HM.lookup (H.hash point) (builderStateData ^. pointsMap)
  
{- |
Given:
points: [CornerPoints.Points.Point] that are to be converted into [GMSH.Points.GPoint].
They may or may not contain overlapping points, such as a common point shared between 2 adjacent lines.
Task:
Convert the [CornerPoints.Points.Point] to a [GC.GPointsId].
Return:
(Curent state, [BuilderMonadData_GPointIds] ) where:
The modified state, with any new GPoints that were added.
The GC.BuilderMonadData as: [GC.BuilderMonadData_GPointIds] that were created, including any overlapping GPoints, such as those where lines meet.
This will only be applicable if the [Pts.Point] has overlaping Points.
-}  

insertWithOvrLap ::  SIO.Handle -> [Pts.Point] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.GPointId])
insertWithOvrLap h points builderStateData = insertBase h (:) points builderStateData

-- | Same as insertWithOvrLap, but without overlapping points.
-- | This will only be applicable if the [Pts.Point] has overlaping Points.
insertNoOvrLap ::  SIO.Handle -> [Pts.Point] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.GPointId])
insertNoOvrLap h points builderStateData = insertBase h (\gPoint gPoints -> gPoints) points builderStateData

{-
Implemets <insertWithOvrLap/insertNoOvrLap> by calling insertBase with the applicable fx for overlapper paramenter,
and supplies the empty working list of [GC.GPointId].
-}
insertBase :: SIO.Handle -> (GC.GPointId -> [GC.GPointId] -> [GC.GPointId]) -> [Pts.Point] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.GPointId])
insertBase h _ [] builderStateData = (builderStateData,[])
insertBase h overlapper points builderStateData = insertBase' h overlapper points [] builderStateData
{-
Given:
Same as insertBase, but additionaly with the workingList of [GC.GPointId] as they are created, and optionaly inserted, printed.
Task:
Implement insertBase, with the extra workingList param.
Return:
Same as insertBase.
-}

insertBase' :: SIO.Handle -> (GC.GPointId -> [GC.GPointId] -> [GC.GPointId]) -> [Pts.Point] -> [GC.GPointId] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.GPointId])
insertBase' h _ [] workingList builderStateData = (builderStateData,reverse workingList)
insertBase' h overlapper (point:points) workingList builderStateData =
  let
    --get the GPointsStateData if it exsits in state.
    gpoint_InState = retrieve builderStateData point
  in
  --case HM.member hashedPoint (builderStateData ^. pointsMap) of
  case gpoint_InState of
    Just gpoint -> do
      --extract the GPointId from the GPointsStateData, and add to working list if overlapping is used.
      -- write the gpoint to file using FW.writeFileUtf8_str is overlapping is used. Still need to add a handle to insertBase, and insertBase'.
      leftOff
      --how do I perform IO here? Do I need to return a ExceptStackCornerPointsBuilder and change the way
      --that GB.buildGPointsListOrFail calls this by have this do the: lift $ state $ builder
      E.liftIO $ writeGScriptToFile h gpoint
      --insertBase' points ((gpoint ^. pointsId):workingList) builderStateData
      insertBase' h overlapper points (overlapper (gpoint ^. pointsId) workingList) builderStateData
    --False ->
    Nothing -> 
      let
        gpoint = (GC.GPointsStateData (head $ builderStateData ^. pointsIdSupply) point)
        --mapWithCurrentPointInserted = (HM.insert hashedPoint  gpoint) (builderStateData ^. pointsMap)
        mapWithCurrentPointInserted = (HM.insert (H.hash point)  gpoint) (builderStateData ^. pointsMap)
      in
      --leftOff
      -- write the gpoint here using FW.writeFileUtf8_str, and a fx still to be written for generating gmsh script from a GPointId.
      insertBase' h overlapper points
             --add the new GPointsId to the workingList.
             --This should be a passed in fx, so adding overlapping gpnts can be optional.
             --((gpoint ^. pointsId):workingList)
             (overlapper (gpoint ^. pointsId) workingList)
             (builderStateData
               {GC._pointsIdSupply = (tail $ (builderStateData ^. pointsIdSupply)),
                GC._pointsMap = mapWithCurrentPointInserted
               }
             ) 
{-
insertBase' :: [Pts.Point] -> [GC.GPointId] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.GPointId])
insertBase' [] workingList builderStateData = (builderStateData,reverse workingList)
insertBase' (point:points) workingList builderStateData =
  let
    --get the GPointsStateData if it exsits in state.
    gpoint_InState = retrieve builderStateData point
  in
  --case HM.member hashedPoint (builderStateData ^. pointsMap) of
  case gpoint_InState of
    Just gpoint -> 
      --extract the GPointId from the GPointsStateData, and add to working list if overlapping is used.
      -- write the gpoint to file using FW.writeFileUtf8_str is overlapping is used. Still need to add a handle to insertBase, and insertBase'.
      insertBase' points ((gpoint ^. pointsId):workingList) builderStateData
    --False ->
    Nothing -> 
      let
        gpoint = (GC.GPointsStateData (head $ builderStateData ^. pointsIdSupply) point)
        --mapWithCurrentPointInserted = (HM.insert hashedPoint  gpoint) (builderStateData ^. pointsMap)
        mapWithCurrentPointInserted = (HM.insert (H.hash point)  gpoint) (builderStateData ^. pointsMap)
      in
      --leftOff
      -- write the gpoint here using FW.writeFileUtf8_str, and a fx still to be written for generating gmsh script from a GPointId.
      insertBase' points
             --add the new GPointsId to the workingList.
             --This should be a passed in fx, so adding overlapping gpnts can be optional.
             ((gpoint ^. pointsId):workingList)
             (builderStateData
               {GC._pointsIdSupply = (tail $ (builderStateData ^. pointsIdSupply)),
                GC._pointsMap = mapWithCurrentPointInserted
               }
             ) 
-}

{-
https://stackoverflow.com/questions/26778415/using-overloaded-strings
-}
toGScript :: GC.GPointsStateData -> T.Text
toGScript (GC.GPointsStateData id (Pts.Point x y z)) =
  T.pack $
    "\nPoint(" ++
      (show (id)) ++ ") = {"  ++
      (show x) ++ "," ++
      (show y) ++ "," ++
      (show z) ++ "};"


writeGScriptToFile :: SIO.Handle -> GC.GPointsStateData -> IO ()
writeGScriptToFile h gPointsStateData = 
  FW.writeFileUtf8 h $ toGScript gPointsStateData
