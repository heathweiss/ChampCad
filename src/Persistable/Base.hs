module Persistable.Base(asSqlBackendReader) where

import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import UnliftIO

asSqlBackendReader :: (MonadUnliftIO m) => ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id
