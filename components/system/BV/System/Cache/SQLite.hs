{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.Cache.SQLite
    ( withSQLiteCacheContext
    ) where

import BV.Logging
import BV.System.Core

import Database.SQLite.Simple (Only (..), execute, execute_, query, setTrace,
                               withConnection)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Data.Text as T

withSQLiteCacheContext :: (MonadUnliftIO m, MonadLoggerWithContext m) => String -> (CacheContext m -> m a) -> m a
withSQLiteCacheContext connString f =
    withRunInIO $ \run -> withConnection connString $ \conn -> run $ do
        liftIO $ do
            setTrace conn . Just $ \msg -> run $ do
                withPushLogContext "sqlite" $ do
                    logTrace $ T.unpack msg
            execute_ conn
                "CREATE TABLE IF NOT EXISTS checks (fingerprint BLOB UNIQUE, result INTEGER)"
        f . liftIOCacheContext $ CacheContext
            { queryCache = \fingerprint -> do
                resp <- query conn
                    "SELECT result FROM checks WHERE fingerprint = (?)" (Only fingerprint)
                return $ case resp of
                    [Only result] -> Just result
                    [] -> Nothing
                    _ -> error "!"
            , updateCache = \result fingerprint -> do
                execute conn
                    "INSERT OR IGNORE INTO checks (fingerprint, result) VALUES (?,?)" (fingerprint, result)
            }

instance FromField AcceptableSatResult where
  fromField field = toEnum <$> fromField field

instance ToField AcceptableSatResult where
  toField = toField . fromEnum

instance FromField CheckFingerprint where
  fromField field = CheckFingerprint <$> fromField field

instance ToField CheckFingerprint where
  toField = toField . (.unwrap)
