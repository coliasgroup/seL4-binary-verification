module BV.Core.Utils
    ( adjacently
    , optionals
    , partially
    , partially_
    , tryLast
    , unwrap
    , whileM
    ) where

import Control.Monad (when)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Monoid (Last (Last, getLast))
import Optics.Core

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

tryLast :: [a] -> Maybe a
tryLast = getLast . foldMap (Last . Just)

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond body = go
  where
    go = do
        p <- cond
        when p $ do
            body
            go

unwrap :: Getter (Maybe a) a
unwrap = to fromJust

partially :: AffineTraversal s t a b -> Lens s t a b
partially optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update

partially_ :: AffineFold s a -> Getter s a
partially_ optic = to (fromJust . preview optic)
