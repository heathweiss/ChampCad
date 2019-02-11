{-# LANGUAGE TemplateHaskell #-}
module GMSH.Common(BuilderData(..), newBuilderData) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import Control.Lens

{- |
Wrap 'a' to show if it has been changed form its original state.
toDo:
Extract this to a higher level module, as it probably will be used by 'Lines' and other Gmsh modules.

Known uses:
No longer used. Get rid of it when gmsh modules are done, if never used.
-}
data Changes a =
  Changed a
  |
  UnChanged a
  deriving (Eq, Show)


data BuilderData = BuilderData
                     {
                       _linesMap::HM.HashMap Int Int,
                       _pointsMap::HM.HashMap Int Int,
                       _linesId :: [Int],
                       _pointsId :: [Int]
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
