{-# LANGUAGE TemplateHaskell #-}
module GMSH.Points({- H.hash, H.hashWithSalt, insert,-} insert2, retrieve) where

{- |
Hash CornerPoints.Point so they can be stored in a hash map.

Insert Points into a GC.BuilderStateData, for use with the GMSH.Builder Builder monad stack.
All that gets inserted, is the ID.

Note that all Data types for Points, are kept in GMSH.Common.
-}

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..))
import qualified CornerPoints.CornerPoints as CPts
import qualified GMSH.Common as GC
--import qualified GMSH.Builder as GB

makeLenses ''GC.BuilderStateData
makeLenses ''GC.GPointsStateData

type ID = Int


-- | Make CornerPoints.Point an instance of Hashable so it can be  used with Data.HashMap.Strict
-- Known uses:
-- Associate CornerPoints.Point with an Id::Int for generating GMSH script.
-- Only 'hash' is being used so far.
instance H.Hashable Point where
    hashWithSalt s (Point x y z) =
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
--insert ::  [Point] -> [ID] -> HM.HashMap Int Int -> (HM.HashMap Int Int,[ID])
{- Can I delete this instead of fixing for use of GC.GPointsId
insert ::  [Point] -> GC.BuilderStateData -> GC.BuilderStateData
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

retrieve ::  GC.BuilderStateData -> Point -> Maybe GC.GPointsStateData
retrieve  builderStateData point =
  HM.lookup (H.hash point) (builderStateData ^. pointsMap)
  
{- |
Task:
Convert a [Pts.Point] to [GC.GPointsStateData]. This will be changed to [GC.GPointsId]
Return:
(Curent state,BuilderMonadData_GPointIds )
-}  
--insert2 :: [Point] -> [GC.PointsBuilderData] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.PointsBuilderData])
--replace return type to use the new [GC.GPointsId]
--change working list to use the new [GC.GPointsId]
insert2 :: [Point] -> [GC.GPointId] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.GPointId])
insert2 [] workingList builderStateData = (builderStateData,reverse workingList)
insert2 (point:points) workingList builderStateData =
  let
    --replace with maybe_gpoint
    --hashedPoint = H.hash point
    --get the GPointsStateData if it exsits
    maybe_gpoint = retrieve builderStateData point
  in
  --case HM.member hashedPoint (builderStateData ^. pointsMap) of
  case maybe_gpoint of
    --True ->
    Just gpoint -> 
      --insert2 points workingList builderStateData
      --extract the GPointId from the GPointsStateData
      insert2 points ((gpoint ^. pointsId):workingList) builderStateData
    --False ->
    Nothing -> 
      let
        gpoint = (GC.GPointsStateData (head $ builderStateData ^. pointsIdSupply) point)
        --mapWithCurrentPointInserted = (HM.insert hashedPoint  gpoint) (builderStateData ^. pointsMap)
        mapWithCurrentPointInserted = (HM.insert (H.hash point)  gpoint) (builderStateData ^. pointsMap)
      in
      insert2 points
             --use nte new GPointsId
             ((gpoint ^. pointsId):workingList)
             (builderStateData
               {GC._pointsIdSupply = (tail $ (builderStateData ^. pointsIdSupply)),
                GC._pointsMap = mapWithCurrentPointInserted
               }
             ) 
{-Before changes to use GC.GPointsId instead of GC.PointsBuilderData
insert2 :: [Point] -> [GC.PointsBuilderData] -> GC.BuilderStateData -> (GC.BuilderStateData,[GC.PointsBuilderData])
insert2 [] workingList builderStateData = (builderStateData,reverse workingList)
insert2 (point:points) workingList builderStateData =
  let
    hashedPoint = H.hash point
  in
  case HM.member hashedPoint (builderStateData ^. pointsMap) of
    True ->  
      insert2 points workingList builderStateData
    False ->
      let
        gpoint = (GC.PointsBuilderData (head $ builderStateData ^. pointsIdSupply) point)
        mapWithCurrentPointInserted = (HM.insert hashedPoint  gpoint) (builderStateData ^. pointsMap)
      in
      insert2 points
             (gpoint:workingList)
             (builderStateData
               {GC._pointsIdSupply = (tail $ (builderStateData ^. pointsIdSupply)),
                GC._pointsMap = mapWithCurrentPointInserted
               }
             ) 

-}
