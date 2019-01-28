module GMSH.Hashable.Points({- H.hash, H.hashWithSalt,-} insert) where

{- |
Hash CornerPoints.Point so they can be stored in a hash map.
-}

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..))
import qualified GMSH.Common as GC


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
 If not insert it, else return the map unchanged.
 Indicate if the point was inserted by wrapping it in the Changes datatype.

Return:
 If point did not already exist in map:
 The map with the hashed point and value inserted, wrapped in 'Changed' constructor.

 If point already exists in map:
 The original map, unchaged. Wrapped in 'UnChanged' constructor.
-}
insert ::  Point -> Int -> HM.HashMap Int Int -> GC.Changes (HM.HashMap Int Int )
insert  point value map   = 
  let
    hashedPoint = H.hash point
  in
  case HM.member hashedPoint map of
    True ->  GC.UnChanged map
    False -> GC.Changed $ HM.insert hashedPoint value map




