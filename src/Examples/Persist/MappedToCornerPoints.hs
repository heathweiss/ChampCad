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
import qualified Persistable.Base as PstB

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

import Persistable.Mapping (Surface(..), BackBottomLineP(..), BackTopLineP(..), BottomFrontLineP(..), FrontTopLineP(..),
                            nameUnique', backBottomLinePSurfaceId', bottomFrontLinePSurfaceId', backTopLinePSurfaceId', frontTopLinePSurfaceId',
                            extractSurfaceId, getBackBottomLine, getBackTopLine, getBottomFrontLine, getFrontTopLine) 


--connection string
databaseName = "MappedToCornerPoints.sql"


{-
initializeDatabase :: IO ()
initializeDatabase = runSqlite databaseName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"
-}
initializeSurfaces :: IO ()
initializeSurfaces  = runSqlite databaseName . PstB.asSqlBackendReader $ do
  insert $ Surface "top"
  insert $ Surface "bottom"
  liftIO $ putStrLn "surfaces initialized"

insertTopLines :: IO ()
insertTopLines = runSqlite databaseName . PstB.asSqlBackendReader $ do
  topId <- getBy $ nameUnique' "top"

  insert $ BackTopLineP  0 0 1 1 0 1 $ extractSurfaceId topId
  insert $ FrontTopLineP 0 1 1 1 1 1 $ extractSurfaceId topId

  liftIO $ putStrLn "top lines inserted"

insertBottomLines :: IO ()
insertBottomLines = runSqlite databaseName . PstB.asSqlBackendReader $ do
  bottomId <- getBy $ nameUnique' "bottom"

  insert $ BackBottomLineP 0 0 0 1 0 0 $ extractSurfaceId bottomId
  insert $ BottomFrontLineP 0 1 0 1 1 0 $ extractSurfaceId bottomId

  liftIO $ putStrLn "bottom lines inserted"

createBottomFaceFromDB :: IO (CpointsList)
createBottomFaceFromDB  = runSqlite databaseName . PstB.asSqlBackendReader $ do
  btmId <- getBy $ nameUnique' "bottom"
  listOfBackBtmPoints <- selectList [ backBottomLinePSurfaceId' ==. (extractSurfaceId btmId)] []
  listOfBtmFrontPoints <- selectList [ bottomFrontLinePSurfaceId' ==. (extractSurfaceId btmId)] []
  let backBtmLine = getBackBottomLine (head listOfBackBtmPoints)
      btmFace = backBtmLine +++> (map getBottomFrontLine listOfBtmFrontPoints)
  return btmFace

createTopFaceFromDB :: IO (CpointsList)
createTopFaceFromDB  = runSqlite databaseName . PstB.asSqlBackendReader $ do
  topId <- getBy $ nameUnique' "top"
  listOfBackTopPoints <- selectList [ backTopLinePSurfaceId' ==. (extractSurfaceId topId)] []
  listOfFrontTopPoints <- selectList [ frontTopLinePSurfaceId' ==. (extractSurfaceId topId)] []
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

