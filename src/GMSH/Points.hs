{-# LANGUAGE TemplateHaskell #-}
module GMSH.Points({- H.hash, H.hashWithSalt,-} insert) where

{- |
Hash CornerPoints.Point so they can be stored in a hash map.

Insert Points into a GC.BuilderData, for use with the GMSH.Builder Builder monad stack.
-}

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..))
import qualified GMSH.Common as GC
--import qualified GMSH.Builder as GB

makeLenses ''GC.BuilderData

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

Task:
 Hash the point and see if it already exists in the map.
 If not already in map insert it.
 

Return:
 If point did not already exist in map:
 The map with the hashed point as key,  and id and point inserted as a GMSH.Common.PointsBuilderData

 If point already exists in map:
 The original map, unchanged.
-}
--insert ::  [Point] -> [ID] -> HM.HashMap Int Int -> (HM.HashMap Int Int,[ID])
insert ::  [Point] -> GC.BuilderData -> GC.BuilderData
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


