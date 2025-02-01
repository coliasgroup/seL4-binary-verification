{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SMTProofChecks
    ( AtomOrPlaceholder
    , FlattenedSMTProofChecks (..)
    , SExprPlaceholder (..)
    , SExprWithPlaceholders
    , SMTProofCheck (..)
    , SMTProofCheckGroup (..)
    , SMTProofCheckImp (..)
    , SMTProofChecks (..)
    , flattenSMTProofChecks
    , readSExprWithPlaceholders
    , readSExprsWithPlaceholders
    , splitSMTProofCheckGroup
      -- , traverseAndFilterChecksOfSMTProofCheckGroup
      -- , traverseAndFilterMetaOfSMTProofCheckGroup
      -- , traverseChecksOfSMTProofCheckGroup
    , tryReadSExprWithPlaceholders
    , tryReadSExprsWithPlaceholders
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.ProofScript
import BV.Core.Types.SExprWithPlaceholders

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

splitSMTProofCheckGroup :: SMTProofCheckGroup a -> [SMTProofCheck a]
splitSMTProofCheckGroup group = group.imps <&> \imp -> SMTProofCheck
    { setup = group.setup
    , imp
    }

newtype SMTProofChecks a
  = SMTProofChecks { unwrap :: M.Map PairingId (ProofScript [SMTProofCheckGroup a]) }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

flattenSMTProofChecks :: SMTProofChecks a -> FlattenedSMTProofChecks a
flattenSMTProofChecks (SMTProofChecks byPairing) = FlattenedSMTProofChecks (M.map fold byPairing)

newtype FlattenedSMTProofChecks a
  = FlattenedSMTProofChecks { unwrap :: M.Map PairingId [SMTProofCheckGroup a] }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

data SMTProofCheckGroup a
  = SMTProofCheckGroup
      { setup :: [SExprWithPlaceholders]
      , imps :: [SMTProofCheckImp a]
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

data SMTProofCheck a
  = SMTProofCheck
      { setup :: [SExprWithPlaceholders]
      , imp :: SMTProofCheckImp a
      }
  deriving (Eq, Functor, Generic, NFData, Ord, Show)

data SMTProofCheckImp a
  = SMTProofCheckImp
      { meta :: a
      , term :: SExprWithPlaceholders
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

-- TODO deduplicate with one base function

traverseAndFilterChecksOfSMTProofCheckGroup
    :: forall f a b. Applicative f => (SMTProofCheck a -> f (Maybe b)) -> SMTProofCheckGroup a -> f (SMTProofCheckGroup b)
traverseAndFilterChecksOfSMTProofCheckGroup f group = traverseOf #imps g group
  where
    g :: [SMTProofCheckImp a] -> f [SMTProofCheckImp b]
    g imps = toListOf (folded % folded) <$> traverse (h . SMTProofCheck group.setup) imps
    h :: SMTProofCheck a -> f (Maybe (SMTProofCheckImp b))
    h check = f check <&> \maybeMeta -> maybeMeta <&> \meta -> SMTProofCheckImp meta check.imp.term

traverseChecksOfSMTProofCheckGroup
    :: forall f a b. Applicative f => (SMTProofCheck a -> f b) -> SMTProofCheckGroup a -> f (SMTProofCheckGroup b)
traverseChecksOfSMTProofCheckGroup f group = traverseOf #imps g group
  where
    g :: [SMTProofCheckImp a] -> f [SMTProofCheckImp b]
    g = traverse (h . SMTProofCheck group.setup)
    h :: SMTProofCheck a -> f (SMTProofCheckImp b)
    h check = f check <&> \meta -> SMTProofCheckImp meta check.imp.term

traverseAndFilterMetaOfSMTProofCheckGroup
    :: forall f a b. Applicative f => (a -> f (Maybe b)) -> SMTProofCheckGroup a -> f (SMTProofCheckGroup b)
traverseAndFilterMetaOfSMTProofCheckGroup f group = traverseOf #imps g group
  where
    g :: [SMTProofCheckImp a] -> f [SMTProofCheckImp b]
    g imps = toListOf (folded % folded) <$> traverse h imps
    h :: SMTProofCheckImp a -> f (Maybe (SMTProofCheckImp b))
    h imp = f imp.meta <&> \maybeMeta -> maybeMeta <&> \meta -> SMTProofCheckImp meta imp.term

traverseAndFilterChecksOfSMTProofCheckGroup_2
    :: forall f t a b. (Applicative f, Foldable t) => (SMTProofCheck a -> f (t (SMTProofCheckImp b))) -> SMTProofCheckGroup a -> f (SMTProofCheckGroup b)
traverseAndFilterChecksOfSMTProofCheckGroup_2 f group = traverseOf #imps g group
  where
    g :: [SMTProofCheckImp a] -> f [SMTProofCheckImp b]
    g imps = toListOf (folded % folded) <$> traverse (f . SMTProofCheck group.setup) imps
