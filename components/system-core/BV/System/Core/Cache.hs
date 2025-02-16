{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module BV.System.Core.Cache
    ( AcceptableSatResult (..)
    , CacheContext (..)
    , CacheT
    , MonadCache (..)
    , augmentCacheContextWithLogging
    , liftIOCacheContext
    , makeAugmentCacheContextWithMutualExclusion
    , runCacheT
    , trivialCacheContext
    , withTrivialCacheContext
    ) where

import BV.Logging
import BV.System.Core.Fingerprinting
import BV.System.Core.Utils.Logging

import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger (LoggingT, MonadLoggerIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import GHC.Generics (Generic)

-- TODO cache sat for subgroup with all scope?

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Enum, Eq, Generic, Ord, Show)

class Monad m => MonadCache m where
    queryCache :: CheckFingerprint -> m (Maybe AcceptableSatResult)
    updateCache :: AcceptableSatResult -> CheckFingerprint -> m ()

instance MonadCache m => MonadCache (ExceptT e m) where
    queryCache check = lift $ queryCache check
    updateCache result check = lift $ updateCache result check

instance MonadCache m => MonadCache (LoggingT m) where
    queryCache check = lift $ queryCache check
    updateCache result check = lift $ updateCache result check

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
      { queryCache :: CheckFingerprint -> m (Maybe AcceptableSatResult)
      , updateCache :: AcceptableSatResult -> CheckFingerprint -> m ()
      }

instance Monad m => MonadCache (CacheT m) where
    queryCache fingerprint = CacheT $ do
        f <- asks (.queryCache)
        lift $ f fingerprint
    updateCache result fingerprint = CacheT $ do
        f <- asks (.updateCache)
        lift $ f result fingerprint

liftIOCacheContext :: MonadIO m => CacheContext IO -> CacheContext m
liftIOCacheContext ctx = CacheContext
    { queryCache = \check -> liftIO $ ctx.queryCache check
    , updateCache = \result check -> liftIO $ ctx.updateCache result check
    }

trivialCacheContext :: Monad m => CacheContext m
trivialCacheContext = CacheContext
    { queryCache = \_check -> return Nothing
    , updateCache = \_result _check -> return ()
    }

withTrivialCacheContext :: Monad m => (CacheContext m -> m a) -> m a
withTrivialCacheContext f = f trivialCacheContext

augmentCacheContextWithLogging :: MonadLoggerWithContext m => CacheContext m -> CacheContext m
augmentCacheContextWithLogging ctx =
    CacheContext
        { queryCache = \check -> do
            withPushLogContext "cache" $ do
                withPushLogContext "query" $ do
                    withPushLogContextCheckFingerprint check $ do
                        logTrace "querying"
                        resp <- ctx.queryCache check
                        logTrace $ "got: " ++ show resp
                        return resp
        , updateCache = \result check -> do
            withPushLogContext "cache" $ do
                withPushLogContext "update" $ do
                    withPushLogContextCheckFingerprint check $ do
                        logTrace $ "sending: " ++ show result
                        ctx.updateCache result check
                        logTrace "done"
        }

makeAugmentCacheContextWithMutualExclusion :: (MonadUnliftIO m, MonadLoggerWithContext m) => IO (CacheContext m -> CacheContext m)
makeAugmentCacheContextWithMutualExclusion = do
    sem <- newQSem 1
    let withLock m = withRunInIO $ \run -> bracket_ (waitQSem sem) (signalQSem sem) (run m)
    return $ \ctx -> CacheContext
        { queryCache = \check -> withLock $ ctx.queryCache check
        , updateCache = \result check -> withLock $ ctx.updateCache result check
        }
