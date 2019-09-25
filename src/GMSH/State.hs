{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Supply State functionality for the GMSH transformer stack.
-}
module GMSH.State(BuilderStateData(), newBuilderData, CurvePointId(..), pattern CurvePointId', lookupCurvePointId, insertCurvePointId,
                 getId, removeId, getRemoveId, CurveId(), pattern CurveId', Id, runStateTests) where

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
import qualified TypeClasses.Showable as Showable

import Test.HUnit

{- |
The ID that identifies each CurvePointId.
The constructor should not get exported, to ensure that the gPointId only gets generated through State module.

-}
newtype CurvePointId = CurvePointId {_gPointId :: Int -- ^ The Id.
                                    }
 deriving (Show, Eq, Typeable, Data)

instance Showable.Showable CurvePointId  --where
  --showId (CurvePointId id) = show id

{-
newtype CurvePointId = CurvePointId {_gPointId :: Int}
 deriving (Show, Eq, Typeable, Data)

-}
pattern CurvePointId' a <- CurvePointId a

-- | 
newtype CurveId = CurveId {_lineId :: Int}
  deriving (Eq, Show, Typeable, Data)

pattern CurveId' a <- CurveId a


{- |
Supplies the state data for the GMSH.Builder.Base.ExceptStackCornerPointsBuilder.
Know uses:
Combines the gmsh id, and the x y z point info and keeps it in state in the BuilderStateData.
Used to make sure that there are no duplicate points in gmsh, when inserting a Pts.Point.
-}
data BuilderStateData = BuilderStateData
                     { _pointsMap :: HM.HashMap Int CurvePointId, -- ^ Hashmap used to ensure each CurvePointId has a unique Point.
                       
                       _curveIdSupply :: [CurveId], -- ^ Will supply id's for the gmsh lines once they are implemented.
                       
                       _curvePointIdSupply :: [CurvePointId] -- ^ Supply id's for the new CurvePointId's
                       
                     }

makeLenses ''BuilderStateData

--pattern BuilderStateData' a b c <- BuilderStateData a b c 

-- | Initialize the empty BuilderStateData for running the Builder monad stack.
newBuilderData :: BuilderStateData
newBuilderData = BuilderStateData (HM.fromList []) (map CurveId [1..]) (map CurvePointId [1..])

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
  -- Not to be used for CurvePointId, as CurvePointId's are kept in a map, along with a Point, to keep them unique.
  -- Instead use insertCurvePointId, which also takes a Point.

instance Id CurvePointId where
  getId builderStateData = head $ builderStateData ^. curvePointIdSupply
  removeId id builderStateData = builderStateData {_curvePointIdSupply = (tail $ builderStateData ^. curvePointIdSupply)}
  
instance Id CurveId where
  getId builderStateData = head $ builderStateData ^. curveIdSupply
  removeId id builderStateData = builderStateData {_curveIdSupply = (tail $ builderStateData ^. curveIdSupply)}
{-
instance Id LineId where
  getId builderStateData = head $ builderStateData ^. linesIdSupply
  removeId id builderStateData = builderStateData {_linesIdSupply = (tail $ builderStateData ^. linesIdSupply)}
-}

-- | Extract the next available CurvePointId.
--newCurvePointId :: BuilderStateData -> CurvePointId
--newCurvePointId builderStateData = head $ builderStateData ^. curvePointIdSupply
--replaced with getId from Id class


{- |
Given
builderStateData: A BuilderStateData which may contain the target CurvePointId in the pointsMap field.
point: a CornerPoints.Points associated with the target CurvePointId.

Task
Retrieve a CurvePointId from a BuilderStateData pointsMap.

Return
Nothing if not found, otherwise the Just CurvePointId
-}
lookupCurvePointId ::  BuilderStateData -> Pts.Point -> Maybe CurvePointId
lookupCurvePointId  builderStateData point =
  HM.lookup (H.hash point) (builderStateData ^. pointsMap)


{- |
Given
builderStateData: represents the current state of a builder stack.
point: The Point associated with the CurvePointId
gPoint: The gmsh point to be inserted into the state.

Task
Insert a CurvePointId into the BuilderStateData and adjust the curvePointIdSupply.
Adjust the gpointId supply to remove the new Id.

Return
The builderStateData with the CurvePointId inserted into the map, and the supply with the new Id removed.
-}
insertCurvePointId :: BuilderStateData -> Pts.Point -> BuilderStateData
insertCurvePointId builderStateData point =
  
  let
    gPoint = getId builderStateData
  in
  removeId gPoint $ 
  builderStateData
    {_pointsMap = (HM.insert (H.hash point) gPoint ) (builderStateData ^. pointsMap)
     
    }
  
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------- Testing of non-exported functions and ADT's --------------------------------------------------------------------
--export the tests
runStateTests = do
  runTestTT insertPointTest
  runTestTT insertPointTest2
  runTestTT insertPointTest3

  runTestTT hashWithSaltPointTest
  runTestTT   hashWithSaltPointTest2
  runTestTT hashPointTest
  

--Insert a Point into an empty BuilderStateData.
insertPointTest = TestCase $ assertEqual
  "insert a Point into an empty map"
  --(BuilderStateData ( HM.fromList [(2171024669747360587, CurvePointId 1)]) (map CurveId [1..]) (map CurvePointId [1..]))
  (newBuilderData{ _pointsMap = ( HM.fromList [(2171024669747360587, CurvePointId 1)])})
  (insertCurvePointId newBuilderData (Pts.Point 1 2 3))


--Insert a Point into a BuilderStateData that already contains the point.
--The BuilderStateData will remain unchanged.
insertPointTest2 = TestCase $ assertEqual
  "insert a Point into a map that already contains the point"
  (newBuilderData { _pointsMap = (HM.fromList [((H.hash $ Point 1 2 3), CurvePointId 1)])})
  (let
      builderDataWithPointAlreadyInserted = (newBuilderData { _pointsMap = HM.fromList [((H.hash $ Point 1 2 3), CurvePointId 1)]} )
      pointWhichIsAlreadyInserted = (Point 1 2 3)
   in 
    insertCurvePointId builderDataWithPointAlreadyInserted pointWhichIsAlreadyInserted
  )

--Insert a Point into a BuilderStateData that already contains a different point.
--The point will be inserted, as it does not already exist. It will have the next available CurvePointId.

insertPointTest3 = TestCase $ assertEqual
  "insert a Point into a map that already contains a diff. point"
  (newBuilderData  { _pointsMap = HM.fromList [(H.hash $ Point 1 2 3, CurvePointId 1), (H.hash $ Point 11 22 33, CurvePointId 2)]})
  ( let
      builderDataWithADiffPointAlreadyInserted = insertCurvePointId newBuilderData $ Point 1 2 3
    in
      insertCurvePointId builderDataWithADiffPointAlreadyInserted (Point 11 22 33)
      
  )

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
--look at how hash module works.
hashWithSaltPointTest = TestCase $ assertEqual
  "run hashWithSalt on a Point"
  (HM.fromList [(-3644729555267505095,1)])
  (HM.insert (H.hashWithSalt 11 $ Point 1 2 3) 1 HM.empty)

hashWithSaltPointTest2 = TestCase $ assertEqual
  "run hashWithSalt on a Point. Note that hash == hashWithSalt 1"
  (HM.fromList [(2171024669747360587,1)])
  (HM.insert (H.hashWithSalt 1 $ Point 1 2 3) 1 HM.empty)

hashPointTest = TestCase $ assertEqual
  "run hash on a Point"
  (HM.fromList [(2171024669747360587,1)])
  (HM.insert (H.hash $ Point 1 2 3) 1 HM.empty)
