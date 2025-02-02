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

newtype SMTProofChecks a
  = SMTProofChecks { unwrap :: M.Map PairingId (ProofScript [SMTProofCheckGroup a]) }
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

splitSMTProofCheckGroup :: SMTProofCheckGroup a -> [SMTProofCheck a]
splitSMTProofCheckGroup group = group.imps <&> \imp -> SMTProofCheck
    { setup = group.setup
    , imp
    }

newtype FlattenedSMTProofChecks a
  = FlattenedSMTProofChecks { unwrap :: M.Map PairingId [SMTProofCheckGroup a] }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

flattenSMTProofChecks :: SMTProofChecks a -> FlattenedSMTProofChecks a
flattenSMTProofChecks checks = FlattenedSMTProofChecks $ M.map fold checks.unwrap
