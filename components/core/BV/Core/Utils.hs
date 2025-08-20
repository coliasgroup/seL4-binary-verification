module BV.Core.Utils
    ( adjacently
    , whenJustThen
    , whenNothing
    , zipWithTraversable
    , zipWithTraversableM
    ) where

import Control.Monad.Identity (Identity (Identity, runIdentity))
import Control.Monad.State (evalStateT, state)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Optics

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing opt m = maybe m return opt

whenJustThen :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
whenJustThen opt f = runMaybeT $ hoistMaybe opt >>= MaybeT . f

zipWithTraversableM :: (Foldable t1, Traversable t2, Monad m) => (a -> b -> m c) -> t1 a -> t2 b -> m (t2 c)
zipWithTraversableM f t1 t2 = evalStateT (traverse m t2) (toList t1)
  where
    m b = do
        a <- state $ fromJust . uncons
        lift $ f a b

zipWithTraversable :: (Foldable t1, Traversable t2) => (a -> b -> c) -> t1 a -> t2 b -> t2 c
zipWithTraversable f t1 t2 = runIdentity $ zipWithTraversableM (\a b -> Identity (f a b)) t1 t2

liftIso :: Iso' c (a, b) -> Lens' s a -> Lens' s b -> Lens' s c
liftIso f l r =
    withIso f $ \deconstruct construct ->
    withLens l $ \getl setl ->
    withLens r $ \getr setr ->
        lens
            (\s -> construct (getl s, getr s))
            (\s c -> case deconstruct c of (b, b') -> setr (setl s b) b')

adjacently :: Lens' s a -> Lens' s b -> Lens' s (a, b)
adjacently = liftIso simple
