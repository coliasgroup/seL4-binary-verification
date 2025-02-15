{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SMTProofChecks
    ( AtomOrPlaceholder
    , SExprPlaceholder (..)
    , SExprWithPlaceholders
    , SMTProofCheck (..)
    , SMTProofCheckGroup (..)
    , SMTProofCheckImp (..)
    , SMTProofChecks (..)
    , readSExprWithPlaceholders
    , readSExprsWithPlaceholders
    , tryReadSExprWithPlaceholders
    , tryReadSExprsWithPlaceholders
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.ProofScript
import BV.Core.Types.SExprWithPlaceholders

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)

newtype SMTProofChecks a
  = SMTProofChecks { unwrap :: M.Map PairingId (ProofScript [SMTProofCheckGroup a]) }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

data SMTProofCheckGroup a
  = SMTProofCheckGroup
      { setup :: [SExprWithPlaceholders]
      , imps :: [SMTProofCheckImp a]
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SMTProofCheck a
  = SMTProofCheck
      { setup :: [SExprWithPlaceholders]
      , imp :: SMTProofCheckImp a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data SMTProofCheckImp a
  = SMTProofCheckImp
      { meta :: a
      , term :: SExprWithPlaceholders
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)
