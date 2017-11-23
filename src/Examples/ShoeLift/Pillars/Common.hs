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


type LayerName = String

databaseName = "src/Examples/ShoeLift/Pillars/geoxPillarsWithAnkleBrace.db" -- :: T.Text

--Have to have this so that databaseName is in the proper format for runSqlite.
--If not, other modules using 'databaseName' don't compile.
--ToDo: Figure out the proper way to make it Data.Text.Internal.Text
test :: IO ()
test = runSqlite (databaseName) $ do
  liftIO $ putStrLn "hello"
