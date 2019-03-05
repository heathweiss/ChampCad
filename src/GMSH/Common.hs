{-# LANGUAGE TemplateHaskell #-}
module GMSH.Common(BuilderData(..), newBuilderData, PointsBuilderData(..)) where
{- |
Contains common datatypes, functions, etc. that are required by multiple modules, which otherwise would cause circular references.
-}

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

import CornerPoints.Points(Point(..))


data PointsBuilderData = PointsBuilderData
  {_pointsId :: Int,
   _point :: Point
  }
  deriving (Show, Eq)

data BuilderData = BuilderData
                     {
                       _linesMap::HM.HashMap Int Int,
                       _pointsMap::HM.HashMap Int PointsBuilderData,
                       _linesId :: [Int],
                       _pointsIdSupply :: [Int]
                     }

makeLenses ''BuilderData

--needed for testing
instance Show BuilderData where
  --show (BuilderData linesMap _) = show linesMap
  show builderData = show $ (builderData ^. linesMap,builderData ^. pointsMap)
  
--only needed for testing
instance Eq BuilderData where
  --(BuilderData linesMap _) == (BuilderData linesMap' _) = linesMap == linesMap'
  builderData == builderData' = ((builderData ^. linesMap) == (builderData' ^. linesMap))
                                &&
                                ((builderData ^. pointsMap) == (builderData' ^. pointsMap))

newBuilderData :: BuilderData
newBuilderData = BuilderData (HM.fromList []) (HM.fromList []) [1..] [1..]



{- 
Wrap 'a' to show if it has been changed form its original state.
toDo:
Extract this to a higher level module, as it probably will be used by 'Lines' and other Gmsh modules.

Known uses:
No longer used. Get rid of it when gmsh modules are done, if never used.

data Changes a =
  Changed a
  |
  UnChanged a
  deriving (Eq, Show)
-}
