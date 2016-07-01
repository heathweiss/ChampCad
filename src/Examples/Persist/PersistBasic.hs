{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Examples.Persist.PersistBasic where

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)
{-------------------------------------------------------------------- overview--------------------------------------------------------
sqlite db is in heath/3D/sqlLiteFiles/PersistBasic.db
-}

{-
Create the th for 2 points, that would make up a line.

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
persistBasicDB = "persistBasic.db"

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
      listOfBtmPoints <- selectList [ LineSurfaceIddd ==. (getSurfaceId btmId)] []
      let backLine = getBackBottomLine (head listOfBtmPoints)
          backLines = backLine +++> (map getBottomFrontLine (tail listOfBtmPoints))
            
      case topId of
        (Just (Entity key val)) -> do
          listOfTopPoints <- selectList [ LineSurfaceIddd ==. (getSurfaceId topId)] []
          let topLine = getBackTopLine (head listOfTopPoints)
              topLines = topLine +++> (map getFrontTopLine (tail listOfTopPoints))
          liftIO $ writeStlToFile $ newStlShape "a simple cube" ([FacesAll | x <- [1..]] |+++^| ( backLines |+++| topLines))      
          liftIO $ print $ show $ backLines |+++| topLines
        Nothing -> liftIO $ putStrLn "bad bottom key"
    Nothing -> liftIO $ putStrLn "bad bottom key"
                    
{-
createStlWithoutBuilder :: IO ()
createStlWithoutBuilder = runSqlite persistBasicDB $ do
  btmId <- getBy $ NameUnique "bottom"
  topId <- getBy $ NameUnique "top"
  --listOfPoints <- selectList [ LineX1 ==. 0] []
  case btmId of
    (Just (Entity key val)) -> do
      listOfBtmPoints <- selectList [ LineSurfaceIddd ==. (getSurfaceId btmId)] []
      let backLine = getBackBottomLine (head listOfBtmPoints)
          backLines = backLine +++> (map getBottomFrontLine (tail listOfBtmPoints))
            
      case topId of
        (Just (Entity key val)) -> do
          listOfTopPoints <- selectList [ LineSurfaceIddd ==. (getSurfaceId topId)] []
          let frontLine = getBottomFrontLine (head listOfTopPoints)
                
          liftIO $ print $ show $ backLine +++ frontLine
        Nothing -> liftIO $ putStrLn "bad bottom key"
    Nothing -> liftIO $ putStrLn "bad bottom key"
                    
-}  

getSurfaceId :: Maybe (Entity Surface) -> Key Surface
getSurfaceId (Just (Entity key val))  = key
 
lookAtSurfaceId :: IO ()
lookAtSurfaceId =  runSqlite persistBasicDB $ do
  btmId <- getBy $ NameUnique "bottom"
  case btmId of
   (Just (Entity key val)) -> liftIO $ putStrLn "good key"
   Nothing -> liftIO $ putStrLn "no good val"
  
  
getLeftPoint :: (Entity Line) -> (Point -> CornerPoints) -> CornerPoints
getLeftPoint (Entity _ (Line x1 y1 z1 _ _ _ _)) const =
  const $ Point x1 y1 z1

getRightPoint :: (Entity Line) -> (Point -> CornerPoints) -> CornerPoints
getRightPoint (Entity _ (Line _ _ _ x2 y2 z2 _)) const =
  const $ Point x2 y2 z2

getLineBase :: (Entity Line) -> (Point -> CornerPoints) -> (Point -> CornerPoints) -> CornerPoints
getLineBase entity const1 const2 =
  (getLeftPoint entity const1) +++ (getRightPoint entity const2)

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
