{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module GMSH.State(BuilderStateData(), newBuilderData, GPointId(), pattern GPointId', lookupGPointId, insertGPointId,
                 getId, removeId, getRemoveId, CurveId(), pattern CurveId', Id) where
{- |
Supply State functionality for the GMSH transformer stack.
-}
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)
import qualified Control.Monad.State.Lazy as SL

import Control.Lens
import  Data.Data
import  Data.Typeable

import CornerPoints.Points(Point(..))
import qualified CornerPoints.CornerPoints as CPts
import qualified CornerPoints.Points as Pts



{- |
The ID that identifies each GPointId.
The constructor does not get exported, to ensure that the gPointId only gets generated through State module.
-}
newtype GPointId = GPointId {_gPointId :: Int}
 deriving (Show, Eq, Typeable, Data)


pattern GPointId' a <- GPointId a

newtype CurveId = CurveId {_lineId :: Int}
  deriving (Eq, Show)

pattern CurveId' a <- CurveId a


{- |
Supplies the state data for the GMSH.Builder.Base.ExceptStackCornerPointsBuilder.
Know uses:
Combines the gmsh id, and the x y z point info and keeps it in state in the BuilderStateData.
Used to make sure that there are no duplicate points in gmsh, when inserting a Pts.Point.
-}
data BuilderStateData = BuilderStateData
                     { _pointsMap::HM.HashMap Int GPointId,
                       -- | Will supply id's for the gmsh lines once they are implemented.
                       _curveIdSupply :: [CurveId],
                       -- | Supply id's for the new GPointId's
                       _pointsIdSupply :: [GPointId]
                       
                     }
{-
data BuilderStateData = BuilderStateData
                     { _pointsMap::HM.HashMap Int GPointId,
                       -- | Will supply id's for the gmsh lines once they are implemented.
                       _linesIdSupply :: [LineId],
                       -- | Supply id's for the new GPointId's
                       _pointsIdSupply :: [GPointId]
                       
                     }
-}
makeLenses ''BuilderStateData

-- | Initialize the empty BuilderStateData for running the Builder monad stack.
newBuilderData :: BuilderStateData
newBuilderData = BuilderStateData (HM.fromList []) (map CurveId [1..]) (map GPointId [1..])

--needed for testing
instance Show BuilderStateData where
  --show (BuilderData linesMap _) = show linesMap
  show builderData = show $ builderData ^. pointsMap
  
--only needed for testing
instance Eq BuilderStateData where
  --(BuilderData linesMap _) == (BuilderData linesMap' _) = linesMap == linesMap'
  builderData == builderData' = (builderData ^. pointsMap) == (builderData' ^. pointsMap)



{- |
For getting the next available Id from BuilderStateData.
-}
class Id a where
  getId :: BuilderStateData -> a
  -- | get the next available Id
  removeId :: a -> BuilderStateData -> BuilderStateData
  -- | remove the next available Id
  getRemoveId :: BuilderStateData -> (a, BuilderStateData)
  getRemoveId builderStateData =
    let
      a = getId builderStateData
      builderStateData' = removeId a builderStateData
    in
    (a, builderStateData')
  -- | Perform both getId and removeId in a single step. Requires getId and removeId to be implemented.
  -- Not to be used for GPointId, as GPointId's are kept in a map, along with a Point, to keep them unique.
  -- Instead use insertGPointId, which also takes a Point.

instance Id GPointId where
  getId builderStateData = head $ builderStateData ^. pointsIdSupply
  removeId id builderStateData = builderStateData {_pointsIdSupply = (tail $ builderStateData ^. pointsIdSupply)}
  
instance Id CurveId where
  getId builderStateData = head $ builderStateData ^. curveIdSupply
  removeId id builderStateData = builderStateData {_curveIdSupply = (tail $ builderStateData ^. curveIdSupply)}
{-
instance Id LineId where
  getId builderStateData = head $ builderStateData ^. linesIdSupply
  removeId id builderStateData = builderStateData {_linesIdSupply = (tail $ builderStateData ^. linesIdSupply)}
-}

-- | Extract the next available GPointId.
--newGPointId :: BuilderStateData -> GPointId
--newGPointId builderStateData = head $ builderStateData ^. pointsIdSupply
--replaced with getId from Id class


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
insertGPointId :: BuilderStateData -> Pts.Point -> BuilderStateData
insertGPointId builderStateData point =
  
  let
    gPoint = getId builderStateData
  in
  removeId gPoint $ 
  builderStateData
    {_pointsMap = (HM.insert (H.hash point) gPoint ) (builderStateData ^. pointsMap)
     
    }
  
