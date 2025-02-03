{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module BV.System.LocalCheckBackend.Cache
    ( AcceptableSatResult (..)
    , LocalCheckCacheContext (..)
    , LocalCheckCacheT
    , MonadLocalCheckCache (..)
    , liftIOLocalCheckCacheContext
    , runLocalCheckCacheT
    , trivialLocalCheckCacheContext
    ) where

import BV.Core.Types

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.Reader.Class (asks)
import GHC.Generics (Generic)

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadLocalCheckCache m where
    queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
    updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()

instance MonadLocalCheckCache m => MonadLocalCheckCache (ExceptT e m) where
    queryCache check = lift $ queryCache check
    updateCache check result = lift $ updateCache check result

instance MonadLocalCheckCache m => MonadLocalCheckCache (LoggingT m) where
    queryCache check = lift $ queryCache check
    updateCache check result = lift $ updateCache check result

--

newtype LocalCheckCacheT m a
  = LocalCheckCacheT { unwrap :: LocalCheckCacheTInner m a }
  deriving
    ( Applicative
    , Functor
    , Generic
    , Monad
    , MonadCatch
    , MonadError e
    , MonadFail
    , MonadIO
    , MonadLogger
    , MonadLoggerIO
    , MonadMask
    , MonadThrow
    , MonadUnliftIO
    )

type LocalCheckCacheTInner m = ReaderT (LocalCheckCacheContext m) m

runLocalCheckCacheT :: LocalCheckCacheT m a -> LocalCheckCacheTInner m a
runLocalCheckCacheT = (.unwrap)

data LocalCheckCacheContext m
  = LocalCheckCacheContext
      { queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
      , updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()
      }

instance Monad m => MonadLocalCheckCache (LocalCheckCacheT m) where
    queryCache check = LocalCheckCacheT $ do
        f <- asks (.queryCache)
        lift $ f check
    updateCache check result = LocalCheckCacheT $ do
        f <- asks (.updateCache)
        lift $ f check result

liftIOLocalCheckCacheContext :: MonadIO m => LocalCheckCacheContext IO -> LocalCheckCacheContext m
liftIOLocalCheckCacheContext ctx = LocalCheckCacheContext
    { queryCache = \check -> liftIO $ ctx.queryCache check
    , updateCache = \check result -> liftIO $ ctx.updateCache check result
    }

trivialLocalCheckCacheContext :: Monad m => LocalCheckCacheContext m
trivialLocalCheckCacheContext = LocalCheckCacheContext
    { queryCache = \_check -> return Nothing
    , updateCache = \_check _result -> return ()
    }
