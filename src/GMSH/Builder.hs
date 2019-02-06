module GMSH.Builder(buildCubePointsList, BuilderData(..),ExceptStackCornerPointsBuilder, buildCubePointsListSingle) where
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

data BuilderData = BuilderData
                     {linesMap::HM.HashMap Int Int,
                     linesId :: [Int]}

--needed for testing
instance Show BuilderData where
  show (BuilderData linesMap _) = show linesMap

--only needed for testing
instance Eq BuilderData where
  (BuilderData linesMap _) == (BuilderData linesMap' _) = linesMap == linesMap'


extractor :: GC.Changes BuilderData -> BuilderData
extractor (GC.Changed builderData) = builderData
extractor (GC.UnChanged builderData) = builderData

-- | The ExceptT State Builder for building up shapes, and convertering to gmsh Lines and points.
type ExceptStackCornerPointsBuilder =  ExceptT String (State BuilderData ) [CPts.CornerPoints]

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
buildCubePointsListOrFail extraMsg cPoints cPoints' =
  let  cubeList = cPoints |+++| cPoints'
  
  in
    case CPts.findCornerPointsError cubeList of
      Nothing -> --has no CornerPointsError
        case (length cubeList) == 0 of
          True -> --cubeList is []
            lift $ state $ \builderData ->
              (cubeList, builderData)
          False -> --cubeList is not []
            lift $ state $ \builderData ->
              let
                --for now use head, but need to figure out how to use map
                --newLinesHashmap = GL.insert (head cubeList) (head $ linesId builderData) $ linesMap builderData
                builderData' = buildCubePointsListOrFail' cubeList builderData
                
                {-  case newLinesHashmap of
                    (Right (GC.Changed changedMap)) ->
                      builderData { linesId = tail $ linesId builderData, linesMap = changedMap }-}
              in
              (cubeList, builderData')
      Just (CPts.CornerPointsError err) -> --has a CornerPointsError
        TE.throwE $ extraMsg ++ ": " ++ (err)
    
{-
buildCubePointsListOrFail :: String -> [CPts.CornerPoints] -> [CPts.CornerPoints] ->
                             ExceptStackCornerPointsBuilder
buildCubePointsListOrFail extraMsg cPoints cPoints' =
  let  cubeList = cPoints |+++| cPoints'
  in
    case CPts.findCornerPointsError cubeList of
      Nothing -> lift $ state $ \builderData ->
        let
          --for now use head, but need to figure out how to use map
          newLinesHashmap = GL.insert (head cubeList) (head $ linesId builderData) $ linesMap builderData
          builderData' = 
            case newLinesHashmap of
              (Right (GC.Changed changedMap)) ->
                builderData { linesId = tail $ linesId builderData, linesMap = changedMap }
        in
        (cubeList, builderData')
      Just (CPts.CornerPointsError err) -> TE.throwE $ extraMsg ++ ": " ++ (err)
-}
buildCubePointsListOrFail' :: [CPts.CornerPoints] -> BuilderData -> BuilderData
buildCubePointsListOrFail' [] builderData = builderData
{-
buildCubePointsListOrFail' (c:cubeList) (GC.UnChanged builderData) =
  let
    --for now use head, but need to figure out how to use map
    newLinesHashmap = GL.insert c (head $ linesId builderData) $ linesMap builderData
    builderData' = 
      case newLinesHashmap of
        (Right (GC.Changed changedMap)) ->
          buildCubePointsListOrFail' cubeList $ GC.Changed $ builderData { linesId = tail $ linesId builderData, linesMap = changedMap }
  in
  (cubeList, builderData')
-}    
buildCubePointsListOrFail' (c:cubeList) builderData =
  let
    --for now use head, but need to figure out how to use map
    newLinesHashmap = GL.insert c (head $ linesId builderData) $ linesMap builderData
    builderData' = 
      case newLinesHashmap of
        (Right (GC.Changed changedMap)) ->
          buildCubePointsListOrFail' cubeList $ builderData { linesId = tail $ linesId builderData, linesMap = changedMap }
  in
  builderData'
