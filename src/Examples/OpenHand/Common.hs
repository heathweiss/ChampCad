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

{- |
Common functions and database values used by any/all modules in OpenHand
-}

module Examples.OpenHand.Common(initializeDatabase, insertDimensions, insertDefaultDimensions, Dimensions(..), commontDBName, uniqueDimensionName,
                               flexOuterTranspose, CommonFactors(..), setMountCommonFactors, setFlexiSocketCommonFactors,
                               setWristCommonFactors, seeCommonFactors) where

-- for persist
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Control.Lens

commontDBName = "src/Examples/OpenHand/Common.sql"

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

-- | Insert a new Dimensions into the database. Sqlite browser will not do this.

insertDimensions :: IO ()
insertDimensions     = runSqlite commontDBName $ do
  dimensionsId
            <- insert $ Dimensions
               "no name dimensions" 
               "put description here"
               2 --flexInnerTransposeFactor Double
               2 --flexThickness Double
               3 --mountThickness Double
               3 --wristThicknes' Double
               1 --wristDrop Int
               1 --wristTake Int
               4 --flexDrop Int
               3 --flexTake Int
               1 --mountDrop Int
               100 --mountTake Int
  --insert $ CurrentDimensions dimensionsId
  liftIO $ putStrLn "dimensions inserted"


insertDefaultDimensions :: IO ()
insertDefaultDimensions     = runSqlite commontDBName $ do
  dimensionsId
            <- insert $ Dimensions
               "dimensions 1" 
               "Wrist goes over top of the mount. Flex socket goes inside the mount"
               2 --flexInnerTransposeFactor Double
               2 --flexThickness Double
               3 --mountThickness Double
               3 --wristThicknes' Double
               1 --wristDrop Int
               1 --wristTake Int
               4 --flexDrop Int
               3 --flexTake Int
               1 --mountDrop Int
               100 --mountTake Int
  --insert $ CurrentDimensions dimensionsId
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
seeCommonFactors = runSqlite commontDBName $ do
  maybeFactors <- getBy $ UniqueDimensionName "dimensions 1"
  case maybeFactors of
   Nothing -> liftIO $ putStrLn "common factors not found"
   Just (Entity dimensionsId dimensions') -> do
     liftIO $ putStrLn $ show $ (setMountCommonFactors dimensions')
     liftIO $ putStrLn $ show $ (setFlexiSocketCommonFactors dimensions')
      
