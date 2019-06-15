{-# LANGUAGE TemplateHaskell #-}
module GMSH.Common(BuilderStateData(..), BuilderMonadData(..), newBuilderData, PointsBuilderData(..), GPointId(..)) where
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

data PointsBuilderData = PointsBuilderData
  { -- _pointsId :: Int,
   _pointsId :: GPointId,
   _point :: Point
  }
  deriving (Show, Eq)
{-need to change _pointsId to a newtype.
data PointsBuilderData = PointsBuilderData
  {_pointsId :: Int,
   _point :: Point
  }
  deriving (Show, Eq)

-}

data BuilderStateData = BuilderStateData
                     {
                       _linesMap::HM.HashMap Int Int,
                       _pointsMap::HM.HashMap Int PointsBuilderData,
                       _linesId :: [Int],
                     --  _pointsIdSupply :: [Int]
                       _pointsIdSupply :: [GPointId]
                     }

makeLenses ''BuilderStateData

-- | Initialize the empty BuilderStateData for running the Builder monad stack.
newBuilderData :: BuilderStateData
newBuilderData = BuilderStateData (HM.fromList []) (HM.fromList []) [1..] (map GPointId [1..])

{- |
The datatype that the GB.ExceptStackCornerPointsBuilder monad stack returns.
This will replace the [CPts] that is currently uses, as will need to track gmsh: points, lines, planes, etc, for printing.
All will be Maybe values, as all of them may not be needed for each step of the monad.
Will still need the [CPts], for using the existing systems that I have developed and used before going to gmsh.
-}
--leftOff
--should first create the datatype for the Gmsh Point Id's.
--figure out how I will build up the shape.
--should I have a single contstructor like this, that contains all possible values as Maybe
{-
data BuilderMonadData = BuilderMonadData
                         {_bmd_CPts :: Maybe [CPts.CornerPoints], 
                          _bmd_GPts :: Maybe [Int] --need to create a datatype for the Gmsh Point Id's.
                         }
-}
--or have a constructor for each type of data:
data BuilderMonadData = 
  -- | A list of gmsh points ID's.
  -- | Need to create a datatype for the Gmsh Point Id's, instead of just using an Int.
  BuilderMonadData_GPointIds --BuilderMonadData_gmshPoints
    --{_bmd_gmshPts :: [PointsBuilderData]}
    {_bmd_gmshPts :: [GPointId]}
  |
  -- | Uses to build up CPts like the original Builder monad.
  BuilderMonadData_CPoints
    {_bmdCPts :: [CPts.CornerPoints]}
  |
  -- | [CPts.Points] that can be extracted from [CPts], and turned into gmsh points(GPts).
  -- | Should this be the only pathway between CPts and gmsh points, or should CPts be converted directly into GPnts.
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





