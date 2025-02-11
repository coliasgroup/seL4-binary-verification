{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BV.System.Cache.Persistent
    (
    ) where

import BV.Logging
import BV.System.Cache
import BV.System.Fingerprinting

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import qualified Data.ByteString as B
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)

share [mkPersist sqlSettings] [persistLowerCase|
CacheEntry
    checkFingerprint B.ByteString
    result Int
|]
