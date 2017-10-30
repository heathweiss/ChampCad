{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Persistable.Mapping (Surface(..), BackBottomLineP(..), BackTopLineP(..), BottomFrontLineP(..), FrontTopLineP(..),
                            nameUnique', backBottomLinePSurfaceId', bottomFrontLinePSurfaceId', backTopLinePSurfaceId', frontTopLinePSurfaceId',
                            extractSurfaceId, getBackBottomLine, getBackTopLine, getBottomFrontLine, getFrontTopLine) where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++),(+++>),(|+++|))

import Data.Text

{- | -------------------------overview---------------------------------------------
Create persist Datatypes that map to CornerPoints lines. eg: BackTopLine.
This was used for scanning a shoe in a linear fashion with the wood scanner I built.
Is to be replaced with the new radial lineScanner.
After that is done, this should be deleted, along with any projects that used it.
-}

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

-- | Create the database.
initializeDatabase :: Text -> IO ()
initializeDatabase databaseName = runSqlite databaseName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

{-
Export these as functions, otherwise 'not in scope' error, when declaring them as exported functions.
Had to add the ' on the end, or there is > 1 declaration. Must be a field name of a Data type.
-}
nameUnique' = NameUnique
backBottomLinePSurfaceId' = BackBottomLinePSurfaceId
bottomFrontLinePSurfaceId' = BottomFrontLinePSurfaceId
backTopLinePSurfaceId' = BackTopLinePSurfaceId
frontTopLinePSurfaceId' = FrontTopLinePSurfaceId

-- | Get the key from a Surface selection.
extractSurfaceId :: Maybe (Entity Surface) -> Key Surface
extractSurfaceId (Just (Entity key val))  = key

{----------------------------------- Entity -> CornerPoints-------------------------------
Map database fields to CornerPoints.
Should I have used persistEntity?
-}

-- | Convert a BackBottomLineP Persist datatype to a CornerPoints 
getBackBottomLine :: (Entity BackBottomLineP) -> CornerPoints
getBackBottomLine (Entity _ (BackBottomLineP b1x b1y b1z b4x b4y b4z  _)) =
  BackBottomLine (Point b1x b1y b1z) (Point b4x b4y b4z)

-- | Convert a BackTopLineP Persist datatype to a CornerPoints
getBackTopLine :: (Entity BackTopLineP) -> CornerPoints
getBackTopLine (Entity _ (BackTopLineP b2x b2y b2z b3x b3y b3z  _)) =
  BackTopLine (Point b2x b2y b2z) (Point b3x b3y b3z)

-- | Convert a BottomFrontLineP Persist datatype to a CornerPoints
getBottomFrontLine :: (Entity BottomFrontLineP) -> CornerPoints
getBottomFrontLine (Entity _ (BottomFrontLineP f1x f1y f1z f4x f4y f4z  _)) =
  BottomFrontLine (Point f1x f1y f1z) (Point f4x f4y f4z)

-- | Convert a FrontTopLineP Persist datatype to a CornerPoints
getFrontTopLine :: (Entity FrontTopLineP) -> CornerPoints
getFrontTopLine (Entity _ (FrontTopLineP f2x f2y f2z f3x f3y f3z  _)) =
  FrontTopLine (Point f2x f2y f2z) (Point f3x f3y f3z)
