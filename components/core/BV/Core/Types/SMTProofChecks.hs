{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.SMTProofChecks
    ( AtomOrPlaceholder
    , SExprPlaceholder (..)
    , SExprWithPlaceholders
    , SMTProofCheck (..)
    , SMTProofCheckGroup (..)
    , SMTProofCheckImp (..)
    , readSExprWithPlaceholders
    , readSExprsWithPlaceholders
    , tryReadSExprWithPlaceholders
    , tryReadSExprsWithPlaceholders
    ) where

import BV.Core.Types.SExprWithPlaceholders

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

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
