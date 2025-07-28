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
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.Map as M
import GHC.Generics (Generic)

newtype SMTProofChecks a
  = SMTProofChecks { unwrap :: M.Map PairingId' (ProofScript AsmRefineTag [SMTProofCheckGroup a]) }
  deriving (Eq, Functor, Generic, Ord, Show)
  deriving newtype (NFData)

data SMTProofCheckGroup a
  = SMTProofCheckGroup
      { setup :: [SExprWithPlaceholders]
      , imps :: [SMTProofCheckImp a]
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

-- TODO temporary
instance Binary a => Binary (SMTProofCheckGroup a) where

data SMTProofCheck a
  = SMTProofCheck
      { setup :: [SExprWithPlaceholders]
      , imp :: SMTProofCheckImp a
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

-- TODO temporary
instance Binary a => Binary (SMTProofCheck a) where

data SMTProofCheckImp a
  = SMTProofCheckImp
      { meta :: a
      , term :: SExprWithPlaceholders
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

-- TODO temporary
instance Binary a => Binary (SMTProofCheckImp a) where
