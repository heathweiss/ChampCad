--cx that all of these are required.
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Examples.Persist.MappedToCornerPoints where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Builder.Monad(BuilderError(..),
                     cornerPointsErrorHandler, buildCubePointsList,
                     buildCubePointsListWithIOCpointsListBase,
                     CpointsStack, CpointsList)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy hiding (get)
import qualified Control.Monad.State.Lazy as ST (get)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Surface
   NameUnique name
   name String
  
  
  deriving Show

BackBottomLineP
    b1x Double
    b1y Double
    b1z Double
    b4x Double
    b4y Double
    b4z Double
    surfaceId SurfaceId
    deriving Show

BackTopLineP
    b2x Double
    b2y Double
    b2z Double
    b3x Double
    b3y Double
    b3z Double
    surfaceId SurfaceId
    deriving Show

BottomFrontLineP
    f1x Double
    f1y Double
    f1z Double
    f4x Double
    f4y Double
    f4z Double
    surfaceId SurfaceId
    deriving Show

FrontTopLineP
    f2x Double
    f2y Double
    f2z Double
    f3x Double
    f3y Double
    f3z Double
    surfaceId SurfaceId
    deriving Show
|]

--connection string
databaseName = "MappedToCornerPoints.sql"

initializeDatabase :: IO ()
initializeDatabase = runSqlite databaseName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

initializeSurfaces :: IO ()
initializeSurfaces  = runSqlite databaseName $ do
  insert $ Surface "top"
  insert $ Surface "bottom"
  liftIO $ putStrLn "surfaces initialized"

insertTopLines :: IO ()
insertTopLines = runSqlite databaseName $ do
  topId <- getBy $ NameUnique "top"

  insert $ BackTopLineP  0 0 1 1 0 1 $ extractSurfaceId topId
  insert $ FrontTopLineP 0 1 1 1 1 1 $ extractSurfaceId topId

  liftIO $ putStrLn "top lines inserted"

insertBottomLines :: IO ()
insertBottomLines = runSqlite databaseName $ do
  bottomId <- getBy $ NameUnique "bottom"

  insert $ BackBottomLineP 0 0 0 1 0 0 $ extractSurfaceId bottomId
  insert $ BottomFrontLineP 0 1 0 1 1 0 $ extractSurfaceId bottomId

  liftIO $ putStrLn "bottom lines inserted"

createBottomFaceFromDB :: IO (CpointsList)
createBottomFaceFromDB  = runSqlite databaseName $ do
  btmId <- getBy $ NameUnique "bottom"
  listOfBackBtmPoints <- selectList [ BackBottomLinePSurfaceId ==. (extractSurfaceId btmId)] []
  listOfBtmFrontPoints <- selectList [ BottomFrontLinePSurfaceId ==. (extractSurfaceId btmId)] []
  let backBtmLine = getBackBottomLine (head listOfBackBtmPoints)
      btmFace = backBtmLine +++> (map getBottomFrontLine listOfBtmFrontPoints)
  return btmFace

createTopFaceFromDB :: IO (CpointsList)
createTopFaceFromDB  = runSqlite databaseName $ do
  topId <- getBy $ NameUnique "top"
  listOfBackTopPoints <- selectList [ BackTopLinePSurfaceId ==. (extractSurfaceId topId)] []
  listOfFrontTopPoints <- selectList [ FrontTopLinePSurfaceId ==. (extractSurfaceId topId)] []
  let backTopLine = getBackTopLine (head listOfBackTopPoints)
      topFace = backTopLine +++> (map getFrontTopLine listOfFrontTopPoints)
  return topFace

buildCubePointsListWithIOCpointsListBase' = buildCubePointsListWithIOCpointsListBase (++) 

createCubeWithBuilderMonadWithIOBase :: ExceptT BuilderError (StateT CpointsStack IO ) CpointsList
createCubeWithBuilderMonadWithIOBase = do
  btmFaceFromPersist <- liftIO $ createBottomFaceFromDB
  topFaceFromPersist <- liftIO $ createTopFaceFromDB
  btmFaces <- buildCubePointsListWithIOCpointsListBase'  "push bottom faces from persist" btmFaceFromPersist  [CornerPointsId | x <- [1..]]
  topFaces <- buildCubePointsListWithIOCpointsListBase'  "push top faces from persist" topFaceFromPersist  [CornerPointsId | x <- [1..]]
  cube <- buildCubePointsListWithIOCpointsListBase' "add top/bottom faces" btmFaces topFaces
  currSt <- ST.get
  liftIO $ writeStlToFile $ newStlShape "HeelSandalBtmSlice"   ([FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube []  currSt))
  return cube

runCreateCubeWithBuilderMonadWithIOBase = do
  (runStateT $  runExceptT  createCubeWithBuilderMonadWithIOBase )  []

extractSurfaceId :: Maybe (Entity Surface) -> Key Surface
extractSurfaceId (Just (Entity key val))  = key

getBackBottomLine :: (Entity BackBottomLineP) -> CornerPoints
getBackBottomLine (Entity _ (BackBottomLineP b1x b1y b1z b4x b4y b4z  _)) =
  BackBottomLine (Point b1x b1y b1z) (Point b4x b4y b4z)


getBackTopLine :: (Entity BackTopLineP) -> CornerPoints
getBackTopLine (Entity _ (BackTopLineP b2x b2y b2z b3x b3y b3z  _)) =
  BackTopLine (Point b2x b2y b2z) (Point b3x b3y b3z)

getBottomFrontLine :: (Entity BottomFrontLineP) -> CornerPoints
getBottomFrontLine (Entity _ (BottomFrontLineP f1x f1y f1z f4x f4y f4z  _)) =
  BottomFrontLine (Point f1x f1y f1z) (Point f4x f4y f4z)

getFrontTopLine :: (Entity FrontTopLineP) -> CornerPoints
getFrontTopLine (Entity _ (FrontTopLineP f2x f2y f2z f3x f3y f3z  _)) =
  FrontTopLine (Point f2x f2y f2z) (Point f3x f3y f3z)
