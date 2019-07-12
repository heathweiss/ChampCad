{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module GMSH.Common(BuilderStateData(..), BuilderMonadData(..), {-evalBuilderMonadData_GPointIds, evalBuilderMonadData_Points, evalBuilderMonadData_CPoints,-} newBuilderData,  GPointId(..), eval) where
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
{-
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
-}
{-
evalBuilderMonadData_GPointIds :: BuilderMonadData -> Either String [GPointId]
evalBuilderMonadData_GPointIds builderMonadData =
  case builderMonadData of
    BuilderMonadData_GPointIds gPointIds -> Right gPointIds
    _                                    -> Left "Required BuilderMonadData_GPointIds constructor not supplied."

evalBuilderMonadData_CPoints :: BuilderMonadData -> Either String [CPts.CornerPoints]
evalBuilderMonadData_CPoints builderMonadData =
  case builderMonadData of
    BuilderMonadData_CPoints cPoints -> Right cPoints
    _                                    -> Left "Required BuilderMonadData_CPoints constructor not supplied."

evalBuilderMonadData_Points :: BuilderMonadData -> Either String [Pts.Point]
evalBuilderMonadData_Points builderMonadData =
  case builderMonadData of
    BuilderMonadData_Points points -> Right points
    _                                    -> Left "Required BuilderMonadData_Points constructor not supplied."
-}
--try the GADT
{-
data BuilderMonadData t where  
  -- | Get generated when a insertWithOvrLap/insertNoOvrLap is used on [CPts.Points] to generate and insert GPoints.
  BuilderMonadData_GPointIds :: [GPointId] ->  BuilderMonadData [GPointId]
  BuilderMonadData_CPoints :: [CPts.CornerPoints] -> BuilderMonadData [CPts.CornerPoints]
  BuilderMonadData_Points :: [Pts.Point] -> BuilderMonadData [Pts.Point]
-}

data BuilderMonadData t where  
  -- | Get generated when a insertWithOvrLap/insertNoOvrLap is used on [CPts.Points] to generate and insert GPoints.
  BuilderMonadData_GPointIds :: [GPointId] ->  BuilderMonadData [GPointId]
  BuilderMonadData_CPoints :: [CPts.CornerPoints] -> BuilderMonadData [CPts.CornerPoints]
  BuilderMonadData_Points :: [Pts.Point] -> BuilderMonadData [Pts.Point]


eval :: BuilderMonadData t -> t 
eval (BuilderMonadData_GPointIds gPointIds) = gPointIds
eval (BuilderMonadData_Points points) = points
eval (BuilderMonadData_CPoints cpts) = cpts

{-
eval :: BuilderMonadData_GADT t -> t -> Int
eval (BuilderMonadData_GPointIds_GADT i) j = 3 -- length j

runEval :: Int
runEval =
  eval  (BuilderMonadData_GPointIds_GADT []) [GPointId 3, GPointId 3]

runEval2 :: Int
runEval2 =
  eval  (BuilderMonadData_CPoints_GADT []) [CPts.CornerPointsNothing]
-}
{-
eval :: (BuilderMonadData_GADT t) -> t 
eval (BuilderMonadData_CPoints_GADT cpts) = cpts
eval (BuilderMonadData_Points_GADT pts) = pts
-}
{-
--Compiles as it knows eval will return a [CPts.CornerPoints]
runEval :: [CPts.CornerPoints]
runEval = eval $ BuilderMonadData_CPoints_GADT  [CPts.CornerPointsNothing]

--won't compile as it knows eval will return a [Pts.Point] 
--runEval2 :: [CPts.CornerPoints]
--runEval2 = eval $ BuilderMonadData_Points_GADT  [Pts.Point 1 2 3]


runEval3 :: [CPts.CornerPoints] -> [CPts.CornerPoints]
runEval3 cpts = cpts

--Won't compile as it knows eval will return a [CPts.CornerPoints]
--runEval4 = runEval3 $ eval $ BuilderMonadData_Points_GADT  [Pts.Point 1 2 3]

-}

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





