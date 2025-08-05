module BV.Core.Utils
    ( adjacently
    , optionals
    , whenJustThen
    , whenNothing
    , zipWithTraversable
    ) where

import Control.Monad.State (evalState, state)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import Data.Maybe (fromJust)
import Optics

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

optionals :: Monoid m => Bool -> m -> m
optionals p m = if p then m else mempty

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing opt m = maybe m return opt

whenJustThen :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
whenJustThen opt f = runMaybeT $ hoistMaybe opt >>= MaybeT . f

-- (!) :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
-- (!) = findWithCallstack

zipWithTraversable :: Traversable t => (a -> b -> c) -> [a] -> t b -> t c
zipWithTraversable f xs t = evalState (traverse m t) xs
  where
    m b = do
        a <- state $ fromJust . uncons
        return $ f a b
