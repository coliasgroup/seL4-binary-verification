{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module BV.System.LocalCheckBackend.Cache
    ( AcceptableSatResult (..)
    , LocalCheckCacheT
    , LocalCheckCacheTInner
    , LocalCheckCacheTInnerContext (..)
    , MonadLocalCheckCache (..)
    , liftIOLocalCheckCacheContext
    , runLocalCheckCacheT
    , trivialLocalCheckCacheContext
    ) where

import BV.Core.Types

import Control.Monad.Free (liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
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

data LocalCheckCacheDSL a
  = QueryCache (SMTProofCheck ()) (Maybe AcceptableSatResult -> a)
  | UpdateCache (SMTProofCheck ()) AcceptableSatResult a
  deriving (Functor, Generic)

instance Monad m => MonadLocalCheckCache (FT LocalCheckCacheDSL m) where
    queryCache check = liftF $ QueryCache check id
    updateCache check result = liftF $ UpdateCache check result ()

type LocalCheckCacheT m = FT LocalCheckCacheDSL (LocalCheckCacheTInner m)

type LocalCheckCacheTInner m = ReaderT (LocalCheckCacheTInnerContext m) m

data LocalCheckCacheTInnerContext m
  = LocalCheckCacheTInnerContext
      { queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
      , updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()
      }

liftIOLocalCheckCacheContext :: MonadIO m => LocalCheckCacheTInnerContext IO -> LocalCheckCacheTInnerContext m
liftIOLocalCheckCacheContext ctx = LocalCheckCacheTInnerContext
    { queryCache = \check -> liftIO $ ctx.queryCache check
    , updateCache = \check result -> liftIO $ ctx.updateCache check result
    }

runLocalCheckCacheT :: Monad m => LocalCheckCacheT m a -> LocalCheckCacheTInner m a
runLocalCheckCacheT = iterT $ \case
    QueryCache check cont -> do
        queryCache' <- asks (.queryCache)
        result <- lift $ queryCache' check
        cont result
    UpdateCache check result cont -> do
        updateCache' <- asks (.updateCache)
        lift $ updateCache' check result
        cont

trivialLocalCheckCacheContext :: Monad m => LocalCheckCacheTInnerContext m
trivialLocalCheckCacheContext = LocalCheckCacheTInnerContext
    { queryCache = \_check -> return Nothing
    , updateCache = \_check _result -> return ()
    }

--

-- TODO
instance MonadUnliftIO m => MonadUnliftIO (LocalCheckCacheT m) where
    withRunInIO inner = lift $ withRunInIO (\run -> inner (run . runLocalCheckCacheT))
