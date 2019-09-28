{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ParallelListComp #-}
{- |
Supply common values-types etc. to all Pillar modules.
-}
module Examples.ShoeLift.Pillars.Common(LayerName(..), databaseName) where


import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Persistable.Base as PstB

import Control.Monad.Trans.Reader

type LayerName = String

databaseName = "src/Examples/ShoeLift/Pillars/geoxPillarsWithAnkleBrace.db" 

-- mess with ghc to resolve the types. Used as: runSqlite commontDBName . asSqlBackendReader  $ do
--asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
--asSqlBackendReader = id

--Have to have this so that databaseName is in the proper format for runSqlite.
--If not, other modules using 'databaseName' don't compile.
--ToDo: Figure out the proper way to make it Data.Text.Internal.Text
test :: IO ()
test = runSqlite databaseName . PstB.asSqlBackendReader $ do
  liftIO $ putStrLn "hello"
