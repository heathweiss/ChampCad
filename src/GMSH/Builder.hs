{-# LANGUAGE TemplateHaskell #-}
module GMSH.Builder(buildCubePointsList, GC.BuilderData(..),ExceptStackCornerPointsBuilder, buildCubePointsListSingle, GC.newBuilderData) where
{- |
Build up a shape from [CornerPoints]. But instead of saving the CornerPoints,
save the gmsh points, lines, etc along with an ID, within hash maps.


Tests and example are in Tests.GmshTest
-}

import qualified CornerPoints.CornerPoints as CPts  --((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>), cornerPointsError, findCornerPointsError, isCubePointsList)
import CornerPoints.CornerPoints((|+++|), (+++), (+++>))

import qualified GMSH.Lines as GL
import qualified GMSH.Common as GC


import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import GHC.Generics (Generic)

import qualified Control.Monad.Trans.Except as TE
import Control.Monad.State.Lazy
import Control.Monad.Except

import Control.Lens
{-
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
-}
makeLenses ''GC.BuilderData


{-
data BuilderData = BuilderData
                     {
                       _linesMap::HM.HashMap Int Int,
                       _linesId :: [Int],
                       _pointsId :: [Int]
                     }

makeLenses ''BuilderData

--needed for testing
instance Show BuilderData where
  --show (BuilderData linesMap _) = show linesMap
  show builderData = show $ builderData ^. linesMap
  
--only needed for testing
instance Eq BuilderData where
  --(BuilderData linesMap _) == (BuilderData linesMap' _) = linesMap == linesMap'
  builderData == builderData' = (builderData ^. linesMap) == (builderData' ^. linesMap)

newBuilderData :: BuilderData
newBuilderData = BuilderData (HM.fromList []) [1..] [1..]

-}

-- | The ExceptT State Builder for building up shapes, and convertering to gmsh Lines and points.
type ExceptStackCornerPointsBuilder =  ExceptT String (State GC.BuilderData ) [CPts.CornerPoints]

{- |
Handles a CornerPoints error in ExceptT catchError calls.
At this time, can be replaced with throwE in the code, as that is all it does.
Suggest using it in case error handling changes.
-}
errorHandler :: String -> ExceptStackCornerPointsBuilder 
errorHandler error = do
  TE.throwE error





buildCubePointsList :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       ExceptStackCornerPointsBuilder 
buildCubePointsList extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail  extraMsg cPoints cPoints') `catchError` errorHandler
{-
buildCubePointsList :: ([CPts.CornerPoints] -> BuilderData -> BuilderData) -> String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                       ExceptStackCornerPointsBuilder 
buildCubePointsList pushToStack extraMsg cPoints cPoints' = 
  (buildCubePointsListOrFail pushToStack extraMsg cPoints cPoints') `catchError` errorHandler
-}

buildCubePointsListSingle :: String -> [CPts.CornerPoints] -> ExceptStackCornerPointsBuilder
                       
buildCubePointsListSingle extraMsg cPoints =
  buildCubePointsList extraMsg [CPts.CornerPointsId | x <- [1..]] cPoints
{-
buildCubePointsListSingle extraMsg cPoints =
  buildCubePointsList (++) extraMsg [CPts.CornerPointsId | x <- [1..]] cPoints
-}
{- temp version of |+++|
buildLinesAndPushToStack :: [CPts.CornerPoints] -> BuilderData -> BuilderData
buildLinesAndPushToStack cpoints builderData =
  builderData
-}

{- |
Task:
Add CornerPoints Lines to the lines map if none of the elements are CornerPointsError.
If any of the [CornerPoints] that are CornerPointsError, then an error is thrown so the ExceptT short circuits.


-}

buildCubePointsListOrFail :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             ExceptStackCornerPointsBuilder
--if an [] is passed in, nothing to do.
buildCubePointsListOrFail _ [] _ =  lift $ state $ \builderData -> ([], builderData)
buildCubePointsListOrFail _ _ [] =  lift $ state $ \builderData -> ([], builderData)

buildCubePointsListOrFail extraMsg cPoints cPoints' = do
  state' <- get
  
  
  let
    cubeList = cPoints |+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        Nothing -> --has no CornerPointsError
          let
            builderData =  buildCubePointsListOrFail' cubeList state'
          in
          case builderData of
            Right builderData' ->
              let
                builder = \builderData -> (cubeList, builderData')
              in
              lift $ state $ builder
              --lift $ return cubeList
            Left e -> TE.throwE $ extraMsg ++ ": " ++ e
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          TE.throwE $ extraMsg ++ ": " ++ (err)

{-
buildCubePointsListOrFail extraMsg cPoints cPoints' = do
  state' <- get
  
  
  let
    cubeList = cPoints |+++| cPoints'
  case CPts.findCornerPointsError cubeList of
        Nothing -> --has no CornerPointsError
          let
            builderData =  buildCubePointsListOrFail' cubeList state'
          in
          case builderData of
            Right builderData' ->
              let
                builder = \builderData -> (cubeList, builderData')
              in
              lift $ state $ builder
              --lift $ return cubeList
            Left e -> TE.throwE $ extraMsg ++ ": " ++ e
        Just (CPts.CornerPointsError err) -> --has a CornerPointsError
          TE.throwE $ extraMsg ++ ": " ++ (err)
-}

    
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
buildCubePointsListOrFail' :: [CPts.CornerPoints] -> GC.BuilderData -> Either String GC.BuilderData
--end of the list. Return whatever has been built up in the BuilderData.
buildCubePointsListOrFail' [] builderData = Right builderData
buildCubePointsListOrFail' (cube:cubeList) builderData =
  let
    --newLinesHashmap = GL.insert cube (linesId builderData) $ linesMap builderData
    newLinesHashmap = GL.insert cube (builderData ^. linesId) ( builderData ^. pointsId) ( builderData ^. linesMap) 
  in
  case newLinesHashmap of
    Right builderData' ->
      buildCubePointsListOrFail' cubeList $ builderData 
    Left e -> Left e
          
  
{-
--The recursive handling of [CornerPoints] for buildCubePointsListOrFail.
buildCubePointsListOrFail' :: [CPts.CornerPoints] -> BuilderData -> Either String BuilderData
--end of the list. Return whatever has been built up in the BuilderData.
buildCubePointsListOrFail' [] builderData = Right builderData
buildCubePointsListOrFail' (cube:cubeList) builderData =
  let
    --newLinesHashmap = GL.insert cube (linesId builderData) $ linesMap builderData
    newLinesHashmap = GL.insert cube (builderData ^. linesId) ( builderData ^. pointsId) ( builderData ^. linesMap) 
  in
  case newLinesHashmap of
    (Right (changedMap, lineIds, pointIds)) ->
      buildCubePointsListOrFail' cubeList $ BuilderData { _linesId = lineIds, _pointsId = pointIds,  _linesMap = changedMap }
    Left e -> Left e

-}

