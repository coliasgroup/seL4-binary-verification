{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module BV.Core.Utils
    ( compareLength
    , foldAssocBalanced
    , maybeFromSingletonList
    , whenJustThen
    , whenNothing
    , withMapSlotWith
    ) where

import Control.Monad.State (State)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import Data.Function (on)
import qualified Data.Map as M
import Optics (Lens', at, use, (%))
import Optics.State.Operators ((%=))

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing opt m = maybe m return opt

whenJustThen :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
whenJustThen opt f = runMaybeT $ hoistMaybe opt >>= MaybeT . f

withMapSlotWith :: (Monad m, Ord k) => (forall a. State s a -> m a) -> Lens' s (M.Map k v) -> k -> m v -> m v
withMapSlotWith liftState l k m = do
    opt <- liftState (use (l % at k))
    whenNothing opt $ do
        v <- m
        liftState $ l %= M.insertWith undefined k v
        return v

maybeFromSingletonList :: [a] -> Maybe a
maybeFromSingletonList = \case
    [] -> Nothing
    [a] -> Just a

compareLength :: Int -> [a] -> Ordering
compareLength = go
  where
    go n xs = case n `compare` 0 of
        LT -> GT
        EQ -> case xs of
            [] -> EQ
            _ -> GT
        GT -> case xs of
            [] -> LT
            (_:xs') -> go (n - 1) xs'

foldAssocBalanced :: (a -> a -> a) -> [a] -> a
foldAssocBalanced f = go
  where
    go [x] = x
    go xs = uncurry (f `on` go) (splitAt (length xs `div` 2) xs)
