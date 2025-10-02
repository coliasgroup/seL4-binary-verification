{-# LANGUAGE UndecidableInstances #-}

module BV.Core.GraphSlice.New.Flatten.Tagged
    ( GraphSliceTaggedT
    , askTag
    , askWithTag
    , liftUntagged
    , mapGraphSliceTaggedT
    , runTagged
    , runWithTag
    ) where

import BV.Core.GraphSlice.New.Common (MonadInner (..))

import BV.Core.Types

import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)
import Optics

newtype GraphSliceTaggedT t m a
  = GraphSliceTaggedT { run :: ReaderT t m a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadInner n m => MonadInner n (GraphSliceTaggedT t m) where
    liftInner = GraphSliceTaggedT . lift . liftInner

mapGraphSliceTaggedT :: (m a -> n b) -> GraphSliceTaggedT t m a -> GraphSliceTaggedT t n b
mapGraphSliceTaggedT f = #run %~ mapReaderT f

liftUntagged :: Monad m => m a -> GraphSliceTaggedT t m a
liftUntagged = GraphSliceTaggedT . lift

askTag :: Monad m => GraphSliceTaggedT t m t
askTag = GraphSliceTaggedT ask

askWithTag :: Monad m => a -> GraphSliceTaggedT t m (WithTag t a)
askWithTag a = do
    tag <- askTag
    return $ WithTag tag a

runTagged :: Monad m => t -> GraphSliceTaggedT t m a -> m a
runTagged tag m = runReaderT m.run tag

runWithTag :: Monad m => (a -> GraphSliceTaggedT t m b) -> WithTag t a -> m b
runWithTag f (WithTag tag a) = runTagged tag $ f a
