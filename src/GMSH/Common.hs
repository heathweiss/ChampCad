{-# LANGUAGE TemplateHaskell #-}
module GMSH.Common(BuilderStateData(..), BuilderMonadData(..), newBuilderData, {-GPointsStateData(..),-} GPointId(..)) where
{- |
Contains common datatypes, functions, etc. that are required by multiple modules, which otherwise would cause circular references.
-}

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

import CornerPoints.Points(Point(..))
import qualified CornerPoints.CornerPoints as CPts
import qualified CornerPoints.Points as Pts

newtype GPointId = GPointId {_gPointId :: Int}
 deriving (Show, Eq)

{- |
Supplies the state data for the GMSH.Builder.Base.ExceptStackCornerPointsBuilder.
Know uses:
Combines the gmsh id, and the x y z point info and keeps it in state in the BuilderStateData.
Used to make sure that there are no duplicate points in gmsh, when inserting a Pts.Point.
-}
data BuilderStateData = BuilderStateData
                     { -- | Should be able to delete this, as lines will be written to file as they are created, and kept as the current value of state.
                       _linesMap::HM.HashMap Int Int,
                       -- | All gmsh points(GPointId) are kept here, keyed by the hashed x,y,z values.
                       -- | Ensures there are not duplicates, by seeing if the hashed x,y,z value already exists.
                       -- | If so, then retrieve the GPointId and use instead of creating a new gmsh point.
                       _pointsMap::HM.HashMap Int GPointId,
                       -- | Will supply id's for the gmsh lines once they are implemented.
                       _linesId :: [Int],
                       -- | Supply id's for the new GPointId's
                       _pointsIdSupply :: [GPointId]
                       
                     }

makeLenses ''BuilderStateData

-- | Initialize the empty BuilderStateData for running the Builder monad stack.
newBuilderData :: BuilderStateData
newBuilderData = BuilderStateData (HM.fromList []) (HM.fromList []) [1..] (map GPointId [1..])

{- |
The datatype that the GB.ExceptStackCornerPointsBuilder monad stack returns.
Needs to track gmsh: points, lines, planes, etc, for printing.
-}

data BuilderMonadData = 
  -- | Get generated when a insertWithOvrLap/insertNoOvrLap is used on [CPts.Points] to generate and insert GPoints.
  BuilderMonadData_GPointIds 
    {_bmd_gmshPts :: [GPointId]}
  |
  -- | Uses to build up CPts like the original Builder monad.
  BuilderMonadData_CPoints
    {_bmdCPts :: [CPts.CornerPoints]}
  |
  -- | [CPts.Points] that can be extracted from [CPts], or created directly, and turned into gmsh points, which are written to file as new ones are created.
  BuilderMonadData_Points
    {_bmdPts :: [Pts.Point]}

--needed for testing
instance Show BuilderStateData where
  --show (BuilderData linesMap _) = show linesMap
  show builderData = show $ (builderData ^. linesMap,builderData ^. pointsMap)
  
--only needed for testing
instance Eq BuilderStateData where
  --(BuilderData linesMap _) == (BuilderData linesMap' _) = linesMap == linesMap'
  builderData == builderData' = ((builderData ^. linesMap) == (builderData' ^. linesMap))
                                &&
                                ((builderData ^. pointsMap) == (builderData' ^. pointsMap))





