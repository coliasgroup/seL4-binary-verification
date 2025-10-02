{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.GraphSlice.New.Flatten.Tagged
    ( GraphSliceTaggedT
    , askTag
    , askWithTag
    , liftUntagged
    , runTagged
    , runWithTag
    ) where

import BV.Core.GraphSlice.New.Common (MonadInner (..))

import BV.Core.Types

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import GHC.Generics (Generic)

newtype GraphSliceTaggedT t m a
  = GraphSliceTaggedT { run :: ReaderT t m a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadInner n m => MonadInner n (GraphSliceTaggedT t m) where
    liftInner = GraphSliceTaggedT . lift . liftInner

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

liftUntagged :: Monad m => m a -> GraphSliceTaggedT t m a
liftUntagged = GraphSliceTaggedT . lift
