{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module BV.System.Cache
    ( AcceptableSatResult (..)
    , CacheContext (..)
    , CacheT
    , MonadCache (..)
    , liftIOCacheContext
    , runCacheT
    , trivialCacheContext
    ) where

import BV.Core.Types
import BV.System.Utils.Logger (MonadLoggerAddContext)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.Reader.Class (asks)
import Data.Functor (void)
import GHC.Generics (Generic)

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadCache m where
    queryCache :: SMTProofCheck a -> m (Maybe AcceptableSatResult)
    updateCache :: SMTProofCheck a -> AcceptableSatResult -> m ()

instance MonadCache m => MonadCache (ExceptT e m) where
    queryCache check = lift $ queryCache check
    updateCache check result = lift $ updateCache check result

instance MonadCache m => MonadCache (LoggingT m) where
    queryCache check = lift $ queryCache check
    updateCache check result = lift $ updateCache check result

--

newtype CacheT m a
  = CacheT { unwrap :: CacheTInner m a }
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
    , MonadLoggerAddContext
    , MonadLoggerIO
    , MonadMask
    , MonadThrow
    , MonadUnliftIO
    )

type CacheTInner m = ReaderT (CacheContext m) m

runCacheT :: CacheT m a -> CacheContext m -> m a
runCacheT = runReaderT . (.unwrap)

data CacheContext m
  = CacheContext
      { queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
      , updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()
      }

instance Monad m => MonadCache (CacheT m) where
    queryCache check = CacheT $ do
        f <- asks (.queryCache)
        lift $ f (void check)
    updateCache check result = CacheT $ do
        f <- asks (.updateCache)
        lift $ f (void check) result

liftIOCacheContext :: MonadIO m => CacheContext IO -> CacheContext m
liftIOCacheContext ctx = CacheContext
    { queryCache = \check -> liftIO $ ctx.queryCache check
    , updateCache = \check result -> liftIO $ ctx.updateCache check result
    }

trivialCacheContext :: Monad m => CacheContext m
trivialCacheContext = CacheContext
    { queryCache = \_check -> return Nothing
    , updateCache = \_check _result -> return ()
    }
