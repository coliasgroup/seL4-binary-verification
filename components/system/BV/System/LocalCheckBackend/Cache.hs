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

import Control.Monad.Free (liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Control.Monad.Logger (MonadLogger (monadLoggerLog), runStderrLoggingT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (..))
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Free.Church (FT, iterT)
import GHC.Generics (Generic)

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadLocalCheckCache m where
    queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
    updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()

--

newtype LocalCheckCacheT m a
  = LocalCheckCacheT { unwrap :: FT LocalCheckCacheDSL (LocalCheckCacheTInner m) a }
  deriving (Applicative, Functor, Generic, Monad, MonadFail, MonadIO)

data LocalCheckCacheDSL a
  = QueryCache (SMTProofCheck ()) (Maybe AcceptableSatResult -> a)
  | UpdateCache (SMTProofCheck ()) AcceptableSatResult a
  deriving (Functor, Generic)

type LocalCheckCacheTInner m = ReaderT (LocalCheckCacheContext m) m

data LocalCheckCacheContext m
  = LocalCheckCacheContext
      { queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
      , updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()
      }

runLocalCheckCacheT :: Monad m => LocalCheckCacheT m a -> LocalCheckCacheTInner m a
runLocalCheckCacheT (LocalCheckCacheT inner) = iterT f inner
  where
    f = \case
        QueryCache check cont -> do
            queryCache' <- asks (.queryCache)
            result <- lift $ queryCache' check
            cont result
        UpdateCache check result cont -> do
            updateCache' <- asks (.updateCache)
            lift $ updateCache' check result
            cont

instance Monad m => MonadLocalCheckCache (LocalCheckCacheT m) where
    queryCache check = LocalCheckCacheT . liftF $ QueryCache check id
    updateCache check result = LocalCheckCacheT . liftF $ UpdateCache check result ()

instance MonadUnliftIO m => MonadUnliftIO (LocalCheckCacheT m) where
    withRunInIO inner = LocalCheckCacheT . lift $ withRunInIO (\run -> inner (run . runLocalCheckCacheT))

instance MonadLogger m => MonadLogger (LocalCheckCacheT m) where
    monadLoggerLog loc source level msg = LocalCheckCacheT . lift $ monadLoggerLog loc source level msg

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
