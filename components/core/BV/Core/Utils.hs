module BV.Core.Utils
    ( adjacently
    , ensure
    , ensureM
    , expecting
    , expectingIx
    , expecting_
    , findWithCallstack
    , is
    , optionals
    , tryLast
    , unwrapped
    , whileM
    , (!@)
    ) where

import Control.Monad (when)
import Data.Either (fromRight)
import Data.Function (applyWhen)
import qualified Data.Map as M
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

expectingIx :: HasCallStack => (Ixed m, IxKind m ~ An_AffineTraversal) => Index m -> Lens' m (IxValue m)
expectingIx i = expecting (ix i)

expecting :: (Is k An_AffineTraversal, HasCallStack) => Optic k is s t a b -> Lens s t a b
expecting optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update

expecting_ :: (Is k An_AffineFold, HasCallStack) => Optic' k is s a -> Getter s a
expecting_ optic = to (fromJust . preview optic)

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k s = isJust (preview k s)

findWithCallstack :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
findWithCallstack m k = if k `M.member` m then m M.! k else error ("not present: " ++ show k)

(!@) :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
(!@) = findWithCallstack

-- (!) :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
-- (!) = findWithCallstack
