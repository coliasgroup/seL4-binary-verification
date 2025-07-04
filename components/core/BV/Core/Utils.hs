{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Utils
    ( IncludeExcludeFilter (..)
    , adjacently
    , applyIncludeExcludeFilter
    , ensure
    , ensureM
    , expecting
    , expectingAt
    , expectingIx
    , expecting_
    , findWithCallstack
    , is
    , optionals
    , unwrapped
    , whenJustThen
    , whenJust_
    , whenNothing
    , whileM
    , (!@)
    ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import Data.Binary (Binary)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Function (applyWhen)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics

ensure :: HasCallStack => Bool -> a -> a
ensure p = applyWhen (not p) (error "ensure failed")

ensureM :: HasCallStack => Applicative f => Bool -> f ()
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

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond body = go
  where
    go = do
        p <- cond
        when p $ do
            body
            go

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing opt m = maybe m return opt

whenJust_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust_ = for_

whenJustThen :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
whenJustThen opt f = runMaybeT $ hoistMaybe opt >>= MaybeT . f

unwrapped :: HasCallStack => Lens (Maybe a) (Maybe b) a b
unwrapped = expecting _Just

expectingIx :: HasCallStack => (Ixed m, IxKind m ~ An_AffineTraversal) => Index m -> Lens' m (IxValue m)
expectingIx i = expecting (ix i)

expectingAt :: HasCallStack => At m => Index m -> Lens' m (IxValue m)
expectingAt i = at i % unwrapped

expecting :: HasCallStack => Is k An_AffineTraversal => Optic k is s t a b -> Lens s t a b
expecting optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update

expecting_ :: HasCallStack => Is k An_AffineFold => Optic' k is s a -> Getter s a
expecting_ optic = to (fromJust . preview optic)

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k s = isJust (preview k s)

findWithCallstack :: HasCallStack => (Show k, Ord k) => M.Map k a -> k -> a
findWithCallstack m k = if k `M.member` m then m M.! k else error ("not present: " ++ show k)

(!@) :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
(!@) = findWithCallstack

-- (!) :: (HasCallStack, Show k, Ord k) => M.Map k a -> k -> a
-- (!) = findWithCallstack

data IncludeExcludeFilter a
  = IncludeExcludeFilter
      { include :: Maybe (Set a)
      , exclude :: Set a
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary a => Binary (IncludeExcludeFilter a)

applyIncludeExcludeFilter :: Ord a => IncludeExcludeFilter a -> a -> Bool
applyIncludeExcludeFilter f a = maybe (const True) (flip S.member) f.include a && a `S.notMember` f.exclude
