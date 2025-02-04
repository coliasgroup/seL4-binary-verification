module BV.Core.Utils
    ( adjacently
    , ensure
    , ensureM
    , expecting
    , expectingIx
    , is
    , optionals
    , partially
    , partially_
    , tryLast
    , unwrapped
    , whileM
    ) where

import Control.Monad (when)
import Data.Either (fromRight)
import Data.Function (applyWhen)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Last (Last, getLast))
import GHC.Stack (HasCallStack)
import Optics

ensure :: HasCallStack => Bool -> a -> a
ensure p = applyWhen (not p) (error "ensure failed")

ensureM :: (Applicative f, HasCallStack) => Bool -> f ()
ensureM p = ensure p $ pure ()

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

unwrapped :: HasCallStack => Lens (Maybe a) (Maybe b) a b
unwrapped = expecting _Just

expecting :: HasCallStack => Prism s t a b -> Lens s t a b
expecting optic = partially (castOptic optic)

expectingIx :: HasCallStack => (Ixed m, IxKind m ~ An_AffineTraversal) => Index m -> Lens' m (IxValue m)
expectingIx i = partially (ix i)

partially :: HasCallStack => AffineTraversal s t a b -> Lens s t a b
partially optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update

partially_ :: HasCallStack => AffineFold s a -> Getter s a
partially_ optic = to (fromJust . preview optic)

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k s = isJust (preview k s)
