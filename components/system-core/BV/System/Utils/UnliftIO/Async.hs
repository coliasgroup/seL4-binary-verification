{-# LANGUAGE DerivingVia #-}
module BV.System.Utils.UnliftIO.Async
    ( ConcurrentlyUnliftIO
    , ConcurrentlyUnliftIOE
    , concurrentlyUnliftIO
    , concurrentlyUnliftIOE
    , concurrentlyUnliftIO_
    , makeConcurrentlyUnliftIO
    , makeConcurrentlyUnliftIOE
    , raceUnliftIO
    , raceUnliftIO_
    , runConcurrentlyUnliftIO
    , runConcurrentlyUnliftIOE
    ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Async (Concurrently (Concurrently),
                                 ConcurrentlyE (ConcurrentlyE), concurrently,
                                 concurrentlyE, concurrently_, race, race_,
                                 runConcurrently, runConcurrentlyE)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO, askUnliftIO, unliftIO,
                                withRunInIO)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), mapReaderT)
import Data.Bifunctor (Bifunctor (bimap))
import GHC.Generics (Generic)
import Optics ((%~))

newtype ConcurrentlyUnliftIO m a
  = ConcurrentlyUnliftIO { unwrap :: ReaderT (UnliftIO m) Concurrently a }
  deriving (Alternative, Applicative, Functor, Generic)

instance Semigroup a => Semigroup (ConcurrentlyUnliftIO m a) where
    (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (ConcurrentlyUnliftIO m a) where
    mempty = pure mempty
    mappend = (<>)

makeConcurrentlyUnliftIO :: MonadUnliftIO m => m a -> ConcurrentlyUnliftIO m a
makeConcurrentlyUnliftIO m = ConcurrentlyUnliftIO . ReaderT $ \unliftIO' -> Concurrently (unliftIO unliftIO' m)

runConcurrentlyUnliftIO :: MonadUnliftIO m => ConcurrentlyUnliftIO m a -> m a
runConcurrentlyUnliftIO m = askUnliftIO >>= liftIO . runConcurrently . runReaderT m.unwrap

newtype ConcurrentlyUnliftIOE m e a
  = ConcurrentlyUnliftIOE { unwrap :: ReaderT (UnliftIO m) (ConcurrentlyE e) a }
  deriving (Applicative, Functor, Generic)

instance Bifunctor (ConcurrentlyUnliftIOE m) where
    bimap f g = #unwrap %~ mapReaderT (bimap f g)

instance Semigroup a => Semigroup (ConcurrentlyUnliftIOE m e a) where
    (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (ConcurrentlyUnliftIOE m e a) where
    mempty = pure mempty
    mappend = (<>)

makeConcurrentlyUnliftIOE :: MonadUnliftIO m => m (Either e a) -> ConcurrentlyUnliftIOE m e a
makeConcurrentlyUnliftIOE m = ConcurrentlyUnliftIOE . ReaderT $ \unliftIO' -> ConcurrentlyE (unliftIO unliftIO' m)

runConcurrentlyUnliftIOE :: MonadUnliftIO m => ConcurrentlyUnliftIOE m e a -> m (Either e a)
runConcurrentlyUnliftIOE m = askUnliftIO >>= liftIO . runConcurrentlyE . runReaderT m.unwrap

raceUnliftIO :: MonadUnliftIO m => m a -> m b -> m (Either a b)
raceUnliftIO left right = withRunInIO $ \run -> race (run left) (run right)

raceUnliftIO_ :: MonadUnliftIO m => m a -> m b -> m ()
raceUnliftIO_ left right = withRunInIO $ \run -> race_ (run left) (run right)

concurrentlyUnliftIO :: MonadUnliftIO m => m a -> m b -> m (a, b)
concurrentlyUnliftIO left right = withRunInIO $ \run -> concurrently (run left) (run right)

concurrentlyUnliftIO_ :: MonadUnliftIO m => m a -> m b -> m ()
concurrentlyUnliftIO_ left right = withRunInIO $ \run -> concurrently_ (run left) (run right)

concurrentlyUnliftIOE :: MonadUnliftIO m => m (Either e a) -> m (Either e b) -> m (Either e (a, b))
concurrentlyUnliftIOE left right = withRunInIO $ \run -> concurrentlyE (run left) (run right)
