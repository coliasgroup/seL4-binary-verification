{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module BV.System.Cache
    ( AcceptableSatResult (..)
    , CacheContext (..)
    , CacheT
    , MonadCache (..)
    , liftIOCacheContext
    , queryCache
    , runCacheT
    , trivialCacheContext
    , updateCache
    ) where

import BV.Logging (MonadLoggerWithContext)
import BV.System.Fingerprinting

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import GHC.Generics (Generic)

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadCache m where
    queryCacheUsingFingerprint :: SMTProofCheckFingerprint -> m (Maybe AcceptableSatResult)
    updateCacheUsingFingerprint :: AcceptableSatResult -> SMTProofCheckFingerprint -> m ()

queryCache :: (MonadCache m, HasEmbeddedFingerprint SMTProofCheckFingerprint a) => a -> m (Maybe AcceptableSatResult)
queryCache = queryCacheUsingFingerprint . embeddedFingerprint

updateCache :: (MonadCache m, HasEmbeddedFingerprint SMTProofCheckFingerprint a) => AcceptableSatResult -> a -> m ()
updateCache r = updateCacheUsingFingerprint r . embeddedFingerprint

instance MonadCache m => MonadCache (ExceptT e m) where
    queryCacheUsingFingerprint check = lift $ queryCache check
    updateCacheUsingFingerprint result check = lift $ updateCache result check

instance MonadCache m => MonadCache (LoggingT m) where
    queryCacheUsingFingerprint check = lift $ queryCache check
    updateCacheUsingFingerprint result check = lift $ updateCache result check

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
    , MonadLoggerIO
    , MonadLoggerWithContext
    , MonadMask
    , MonadThrow
    , MonadUnliftIO
    )

type CacheTInner m = ReaderT (CacheContext m) m

runCacheT :: CacheT m a -> CacheContext m -> m a
runCacheT = runReaderT . (.unwrap)

data CacheContext m
  = CacheContext
      { queryCacheUsingFingerprint :: SMTProofCheckFingerprint -> m (Maybe AcceptableSatResult)
      , updateCacheUsingFingerprint :: AcceptableSatResult -> SMTProofCheckFingerprint -> m ()
      }

instance Monad m => MonadCache (CacheT m) where
    queryCacheUsingFingerprint fingerprint = CacheT $ do
        f <- asks (.queryCacheUsingFingerprint)
        lift $ f fingerprint
    updateCacheUsingFingerprint result fingerprint = CacheT $ do
        f <- asks (.updateCacheUsingFingerprint)
        lift $ f result fingerprint

liftIOCacheContext :: MonadIO m => CacheContext IO -> CacheContext m
liftIOCacheContext ctx = CacheContext
    { queryCacheUsingFingerprint = \check -> liftIO $ ctx.queryCacheUsingFingerprint check
    , updateCacheUsingFingerprint = \result check -> liftIO $ ctx.updateCacheUsingFingerprint result check
    }

trivialCacheContext :: Monad m => CacheContext m
trivialCacheContext = CacheContext
    { queryCacheUsingFingerprint = \_check -> return Nothing
    , updateCacheUsingFingerprint = \_result _check -> return ()
    }
