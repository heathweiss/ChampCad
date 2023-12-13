--for persist
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}


{- |
Common functions and database values used by any/all modules in OpenHand
-}

module Examples.OpenHand.Common(initializeDatabase, insertDimensions, Dimensions(..), commontDBName, uniqueDimensionName,
                               flexOuterTranspose, CommonFactors(..), setMountCommonFactors, setFlexiSocketCommonFactors,
                               setWristCommonFactors, seeCommonFactors) where

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Persistable.Base as PstB

import Data.Text

import Control.Lens

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

commontDBName = "src/Examples/OpenHand/Common.sql" :: Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dimensions
   name String
   UniqueDimensionName name
   desc String
   flexInnerTransposeFactor Double
   flexThickness Double
   mountThickness Double
   wristThickness Double
   wristDrop Int
   wristTake Int
   flexDrop Int
   flexTake Int
   mountDrop Int
   mountTake Int
  deriving Show



|]

uniqueDimensionName = UniqueDimensionName

-- | Initialize a new database with all tables. Will alter tables of existing db.
initializeDatabase :: IO ()
initializeDatabase = runSqlite commontDBName $ do
       
    runMigration migrateAll
    liftIO $ putStrLn "db initializes"

-- mess with ghc to resolve the types. Used as: runSqlite commontDBName . asSqlBackendReader  $ do
--asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
--asSqlBackendReader = id

-- | Insert a new Dimensions into the database. Sqlite browser will not do this.
insertDimensions :: IO ()
insertDimensions     = runSqlite commontDBName . PstB.asSqlBackendReader  $ do
  dimensionsId
            <- insert $ Dimensions
               "sharkfin" 
               "make it the same dimensions as the shark swim fin which fits him good in Mar/17"
               3 --flexInnerTransposeFactor Double
               2 --flexThickness Double
               3 --mountThickness Double
               3 --wristThicknes' Double
               1 --wristDrop Int
               1 --wristTake Int
               4 --flexDrop Int
               100 --flexTake Int
               1 --mountDrop Int
               100 --mountTake Int
  liftIO $ putStrLn "dimensions inserted"



flexOuterTranspose :: Double -> Double -> Double
flexOuterTranspose flexInnerTranspose flexThickness = flexInnerTranspose + flexThickness

-- | Datatype to hold all calculated transpose factors
data CommonFactors =
       CommonFactors
         {_innerTranspose :: Double,
          _outerTranspose :: Double,
          _drop :: Int,
          _take :: Int
         }
  deriving Show

-- | Set up the factors for the SocketMount
setMountCommonFactors :: Dimensions -> CommonFactors
setMountCommonFactors (Dimensions _ _ flexInner flexThickness mountThickness _ _ _ _ _ mountDrop mountTake) =
  let mountInner = flexInner + flexThickness
  in
    CommonFactors
     mountInner 
     ( mountInner + mountThickness)
     mountDrop
     mountTake


-- | Set up the factors for the SocketMount
setFlexiSocketCommonFactors :: Dimensions -> CommonFactors
setFlexiSocketCommonFactors (Dimensions _ _ flexInner flexThickness _ _ _ _ flexDrop flexTake _ _) =
  CommonFactors
    flexInner
    (flexInner + flexThickness)
    flexDrop
    flexTake

setWristCommonFactors :: Dimensions -> CommonFactors
setWristCommonFactors (Dimensions _ _ flexInner flexThickness mountThickness wristThickness wristDrop wristTake _ _ _ _) =
  let writsInner = flexInner + flexThickness + mountThickness
  in  CommonFactors
        writsInner
        (writsInner + wristThickness)
        wristDrop
        wristTake

-- testing: look at factors to see why flexi and mount overlap in xy plane
-- It was because I tranpose the innerMDR instead of the raw MDR. Had to change the way I used transpose factors.
seeCommonFactors :: IO ()
seeCommonFactors = runSqlite commontDBName . PstB.asSqlBackendReader $ do
  maybeFactors <- getBy $ UniqueDimensionName "dimensions 1"
  case maybeFactors of
   Nothing -> liftIO $ putStrLn "common factors not found"
   Just (Entity dimensionsId dimensions') -> do
     liftIO $ putStrLn $ show $ (setMountCommonFactors dimensions')
     liftIO $ putStrLn $ show $ (setFlexiSocketCommonFactors dimensions')
      
