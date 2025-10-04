{-# LANGUAGE UndecidableInstances #-}

module BV.Core.GraphSlice.New.Flatten.Tagged
    ( GraphSliceTaggedT
    , askTag
    , askWithTag
    , forTagged
    , liftUntagged
    , mapGraphSliceTaggedT
    , runTagged
    , runWithTag
    ) where

import BV.Core.Types

import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

-- TODO implement MonadTrans?

newtype GraphSliceTaggedT t m a
  = GraphSliceTaggedT { run :: ReaderT t m a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

liftUntagged :: Monad m => m a -> GraphSliceTaggedT t m a
liftUntagged = GraphSliceTaggedT . lift

mapGraphSliceTaggedT :: (m a -> n b) -> GraphSliceTaggedT t m a -> GraphSliceTaggedT t n b
mapGraphSliceTaggedT f = #run %~ mapReaderT f

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

forTagged :: (Tag t, Monad m) => ByTag t a -> (a -> GraphSliceTaggedT t m b) -> m (ByTag t b)
forTagged as f = for (withTags as) $ runWithTag f
