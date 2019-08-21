{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module GMSH.State(BuilderStateData(), newBuilderData, GPointId(), pattern GPointId', lookupGPointId, insertGPointId,
                 newGPointId, LineId()) where
{- |
Supply State functionality for the GMSH transformer stack.
-}
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

import CornerPoints.Points(Point(..))
import qualified CornerPoints.CornerPoints as CPts
import qualified CornerPoints.Points as Pts

{- |
The ID that identifies each GPointId.
The constructor does not get exported, to ensure that the gPointId only gets generated through State module.
-}
newtype GPointId = GPointId {_gPointId :: Int}
 deriving (Show, Eq)


pattern GPointId' a <- GPointId a

newtype LineId = LineId {_lineId :: Int}
  deriving (Eq, Show)


{- |
Supplies the state data for the GMSH.Builder.Base.ExceptStackCornerPointsBuilder.
Know uses:
Combines the gmsh id, and the x y z point info and keeps it in state in the BuilderStateData.
Used to make sure that there are no duplicate points in gmsh, when inserting a Pts.Point.
-}
data BuilderStateData = BuilderStateData
                     { _pointsMap::HM.HashMap Int GPointId,
                       -- | Will supply id's for the gmsh lines once they are implemented.
                       _linesIdSupply :: [LineId],
                       -- | Supply id's for the new GPointId's
                       _pointsIdSupply :: [GPointId]
                       
                     }

makeLenses ''BuilderStateData

-- | Initialize the empty BuilderStateData for running the Builder monad stack.
newBuilderData :: BuilderStateData
newBuilderData = BuilderStateData (HM.fromList []) (map LineId [1..]) (map GPointId [1..])

--needed for testing
instance Show BuilderStateData where
  --show (BuilderData linesMap _) = show linesMap
  show builderData = show $ builderData ^. pointsMap
  
--only needed for testing
instance Eq BuilderStateData where
  --(BuilderData linesMap _) == (BuilderData linesMap' _) = linesMap == linesMap'
  builderData == builderData' = (builderData ^. pointsMap) == (builderData' ^. pointsMap)




-- | Extract the next available GPointId.
newGPointId :: BuilderStateData -> GPointId
newGPointId builderStateData = head $ builderStateData ^. pointsIdSupply


{- |
Given
builderStateData: A BuilderStateData which may contain the target GPointId in the pointsMap field.
point: a CornerPoints.Points associated with the target GPointId.

Task
Retrieve a GPointId from a BuilderStateData pointsMap.

Return
Nothing if not found, otherwise the Just GPointId
-}
lookupGPointId ::  BuilderStateData -> Pts.Point -> Maybe GPointId
lookupGPointId  builderStateData point =
  HM.lookup (H.hash point) (builderStateData ^. pointsMap)


{- |
Given
builderStateData: represents the current state of a builder stack.
point: The Point associated with the GPointId
gPoint: The gmsh point to be inserted into the state.

Task
Insert a GPointId into the BuilderStateData and adjust the pointsIdSupply.
Adjust the gpointId supply to remove the new Id.

Return
The builderStateData with the GPointId inserted into the map, and the supply with the new Id removed.
-}
insertGPointId :: BuilderStateData -> Pts.Point -> GPointId -> BuilderStateData
insertGPointId builderStateData point gPoint =
  builderStateData
    {_pointsMap = (HM.insert (H.hash point)  gPoint) (builderStateData ^. pointsMap),
     _pointsIdSupply = tail (builderStateData ^. pointsIdSupply)
    }
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
------------------------------------------ Line -----------------------------------------------





-- | Get the next available LineId from the BuilderStateData.
-- Remove the next available LineId from the BuilderStateData.
-- Returns: (next available LineId, BuilderStateData with the next available LineId removed)
newLineId :: BuilderStateData -> (LineId, BuilderStateData)
newLineId builderStateData =
  let
    getLineId :: BuilderStateData -> LineId
    getLineId builderStateData = head $ builderStateData ^. linesIdSupply

    removeLineId :: BuilderStateData -> BuilderStateData
    removeLineId builderStateData =
      builderStateData {_linesIdSupply = tail $ (builderStateData ^. linesIdSupply)}
    
  in
  (getLineId builderStateData, removeLineId builderStateData)
