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

module Examples.Persist.SingleLineTable where

import CornerPoints.Points(Point(Point))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))
import CornerPoints.MeshGeneration(autoGenerateEachCube, autoGenerateEachFace)

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

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
{-------------------------------------------------------------------- overview--------------------------------------------------------
sqlite db is in heath/3D/sqlLiteFiles/PersistBasic.sql

First attempt at using persist to incorporate a database for storing CornerPoints.
Creates a simple cube.

Mapping to CornerPoints consisted of creating a single table which contains a Line.
This line in turn gets converted to various CornerPoints lines such as BackBottomLine or BackTopLine.
Problem with this, it is hard to go into the db, and edit values as it gets confusing.

Each Line entry referces the Surface table, which allows Lines to be grouped by a name, such as "BottomFace".
This table is not a mapping to any particular Data type.

Will create another version in which a table will be created for each type of CornerPoints Line. See MappedToCornerPoints.hs example.
-}



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Surface
   NameUnique name
   name String
  
  
  deriving Show

Line
    x1 Double
    y1 Double
    z1 Double
    x2 Double
    y2 Double
    z2 Double
    surfaceIddd SurfaceId
    deriving Show
|]

--connection string
persistBasicDB = "persistBasic.sql"

initializeDatabase :: IO ()
initializeDatabase = runSqlite persistBasicDB $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"


insertTopLines :: IO ()
insertTopLines = runSqlite persistBasicDB $ do
  surfaceid <- insert $ Surface "top"
  backTopLine <- insert $ Line 0 0 1 1 0 1 surfaceid
  frontTopLine <- insert $ Line 0 1 1 1 1 1 surfaceid
  liftIO $ putStrLn "top lines inserted"

insertBotomLines :: IO ()
insertBotomLines = runSqlite persistBasicDB $ do
  surfaceid <- insert $ Surface "bottom"
  backBtmLine <- insert $ Line  0 0 0 1 0 0 surfaceid
  btmFrontLine <- insert $ Line 0 1 1 1 1 0 surfaceid
  liftIO $ putStrLn "bottom lines inserted"  
  
createStlWithoutBuilder :: IO ()
createStlWithoutBuilder = runSqlite persistBasicDB $ do
  btmId <- getBy $ NameUnique "bottom"
  topId <- getBy $ NameUnique "top"
  --listOfPoints <- selectList [ LineX1 ==. 0] []
  case btmId of
    (Just (Entity key val)) -> do
      listOfBtmPoints <- selectList [ LineSurfaceIddd ==. (extractSurfaceId btmId)] []
      let backLine = getBackBottomLine (head listOfBtmPoints)
          backLines = backLine +++> (map getBottomFrontLine (tail listOfBtmPoints))
            
      case topId of
        (Just (Entity key val)) -> do
          listOfTopPoints <- selectList [ LineSurfaceIddd ==. (extractSurfaceId topId)] []
          let topLine = getBackTopLine (head listOfTopPoints)
              topLines = topLine +++> (map getFrontTopLine (tail listOfTopPoints))
          liftIO $ writeStlToFile $ newStlShape "a simple cube" ([FacesAll | x <- [1..]] |+++^| ( backLines |+++| topLines))      
          liftIO $ print $ show $ backLines |+++| topLines
        Nothing -> liftIO $ putStrLn "bad bottom key"
    Nothing -> liftIO $ putStrLn "bad bottom key"

            
  
                    
--curry in the stack pushing function
buildCubePointsList' = buildCubePointsList (++)

{-Shows that persist runs in the IO monad.
It does not have to be in IO ()-}
--lookAtBtmId :: ReaderT SqlBackend (Except String) (Maybe (Entity Surface))
lookAtBtmId :: IO (Maybe (Entity Surface))
lookAtBtmId =  runSqlite persistBasicDB $ do
  btmId <- getBy $ NameUnique "bottom"
  liftIO $ return btmId



{-
write a buildCubePointsList version that uses ReaderT SqlBackend
Problem:
How to go from what persist returns, and (ReaderT SqlBackend (State CpointsStack )).
Pershaps I should stay in the original  ExceptT BuilderError (State CpointsStack ) CpointsList and:
Create function that gets the lines as CornerPoints, and pass this info in.

createCubesWithExceptTPesistStack :: ExceptT BuilderError (ReaderT SqlBackend (State CpointsStack ))  CpointsList
createStlWithoutBuilder = runSqlite persistBasicDB $ do
-}


  


extractSurfaceId :: Maybe (Entity Surface) -> Key Surface
extractSurfaceId (Just (Entity key val))  = key

--testing...
lookAtSurfaceId :: IO ()
lookAtSurfaceId =  runSqlite persistBasicDB $ do
  btmId <- getBy $ NameUnique "bottom"
  case btmId of
   (Just (Entity key val)) -> liftIO $ putStrLn "good key"
   Nothing -> liftIO $ putStrLn "no good val"
  
  


----------- build lines from the db-----------

getBackBottomLine :: (Entity Line) -> CornerPoints
getBackBottomLine entity =
  getLineBase entity (B1) (B4)
  
getBottomFrontLine :: (Entity Line) -> CornerPoints
getBottomFrontLine entity =
  getLineBase entity (F1) (F4)

getBackTopLine :: (Entity Line) -> CornerPoints
getBackTopLine entity =
  getLineBase entity (B2) (B3)

getFrontTopLine :: (Entity Line) -> CornerPoints
getFrontTopLine entity =
  getLineBase entity (F2) (F3)

--support for the get...Line functions
getLineBase :: (Entity Line) -> (Point -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints
getLineBase entity const1 const2 =
  (getLeftPoint entity const1) +++ (getRightPoint entity const2)

--------extract left/right point from a db entry------
--support for getLineBase

getLeftPoint :: (Entity Line) -> (Point -> CornerPoints) -> CornerPoints
getLeftPoint (Entity _ (Line x1 y1 z1 _ _ _ _)) const =
  const $ Point x1 y1 z1

getRightPoint :: (Entity Line) -> (Point -> CornerPoints) -> CornerPoints
getRightPoint (Entity _ (Line _ _ _ x2 y2 z2 _)) const =
  const $ Point x2 y2 z2


{---------------------------------------Do everything in the IO monad where Persist works----------------------------------------------------

Work in the IO monad for the db, as that is all I can get it to work in so far.

Downside:
If I introduce errors that are caught by my Builder module, how do I look at the errors.
I have to rewrite/compile to show the state, instead of generating/writing stl.
-}
createStlWithBuilderInIOMOnad :: IO ()
createStlWithBuilderInIOMOnad = runSqlite persistBasicDB $ do
  btmId <- getBy $ NameUnique "bottom"
  topId <- getBy $ NameUnique "top"
  listOfBtmPoints <- selectList [ LineSurfaceIddd ==. (extractSurfaceId btmId)] []
  listOfTopPoints <- selectList [ LineSurfaceIddd ==. (extractSurfaceId topId)] []
  let btmFace = getBackBottomLine (head listOfBtmPoints)
      btmFaces = btmFace +++> (map getBottomFrontLine (tail listOfBtmPoints))
      topFace = getBackTopLine (head listOfTopPoints)
      --introduce an error
      --topFace = getBackBottomLine (head listOfTopPoints)
      topFaces = topFace +++> (map getFrontTopLine (tail listOfTopPoints))
      --introduce an error
      --topFaces = topFace +++> (map getBottomFrontLine (tail listOfTopPoints))

  let buildCornerPoints :: CpointsList -> CpointsList -> ExceptT BuilderError (State CpointsStack ) CpointsList
      buildCornerPoints btmFacesIn topFacesIn = do
        btmFaces <-  buildCubePointsList' "add bottom faces" btmFacesIn [CornerPointsId | x <- [1..]]
        topFaces <-  buildCubePointsList' "add top faces" topFacesIn [CornerPointsId | x <- [1..]]
        cube     <-  buildCubePointsList'  "combine faces" btmFaces topFaces
        return cube

  liftIO $ writeStlToFile $ newStlShape "the cube" $ [FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube [] ((execState $ runExceptT (buildCornerPoints btmFaces topFaces ) ) []))
  liftIO $ putStrLn "stl generated, maybe"
        
  
{-----------------------------------------Create a stack that has IO as the base, to pass in persist results-------------------------}

buildCubePointsListWithIOCpointsListBase' = buildCubePointsListWithIOCpointsListBase (++) 

{-Convert the persist results into CPointsList.-}
createBottomFacesFromDB :: IO (CpointsList)
createBottomFacesFromDB  = runSqlite persistBasicDB $ do
  btmId <- getBy $ NameUnique "bottom"
  listOfBtmPoints <- selectList [ LineSurfaceIddd ==. (extractSurfaceId btmId)] []
  let btmFace = getBackBottomLine (head listOfBtmPoints)
      btmFaces = btmFace +++> (map getBottomFrontLine (tail listOfBtmPoints))
  return btmFaces

{-Convert the persist results into CPointsList.-}
createTopFacesFromDB :: IO (CpointsList)
createTopFacesFromDB  = runSqlite persistBasicDB $ do
  topId <- getBy $ NameUnique "top"
  listOfTopPoints <- selectList [ LineSurfaceIddd ==. (extractSurfaceId topId)] []
  let topFace = getBackTopLine (head listOfTopPoints)
      topFaces = topFace +++> (map getFrontTopLine (tail listOfTopPoints))
  return topFaces

{-
Get the Surface/Line info from persist, and convert to CpointsList.
Build up the cube.
Generate the stl.
Run it with runCreateCubeWithBuilderMonadWithIOBase
-}
createCubeWithBuilderMonadWithIOBase :: ExceptT BuilderError (StateT CpointsStack IO ) CpointsList
createCubeWithBuilderMonadWithIOBase = do
  btmFacesFromPersist <- liftIO $ createBottomFacesFromDB
  topFacesFromPersist <- liftIO $ createTopFacesFromDB
  btmFaces <- buildCubePointsListWithIOCpointsListBase'  "push bottom faces from persist" btmFacesFromPersist  [CornerPointsId | x <- [1..]]
  topFaces <- buildCubePointsListWithIOCpointsListBase'  "push top faces from persist" topFacesFromPersist  [CornerPointsId | x <- [1..]]
  cube <- buildCubePointsListWithIOCpointsListBase' "add top/bottom faces" btmFaces topFaces
  currSt <- ST.get
  liftIO $ writeStlToFile $ newStlShape "HeelSandalBtmSlice"   ([FacesAll | x <- [1..]] |+++^| (autoGenerateEachCube []  currSt))
  return cube

--unwrap the transformers to have a look at the error or last CpointsList
runCreateCubeWithBuilderMonadWithIOBase = do
  (runStateT $  runExceptT  createCubeWithBuilderMonadWithIOBase )  []

