{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.ProofChecks
    ( EqHyp (..)
    , EqHypInduct (..)
    , EqHypSide (..)
    , Hyp (..)
    , PcImpHyp (..)
    , PcImpHypSide (..)
    , ProofCheck (..)
    , ProofChecks (..)
    , Restr (..)
    , SmtProofCheckGroup (..)
    , SmtProofChecks (..)
    , Visit (..)
    , VisitCount (..)
    , VisitWithTag (..)
    ) where

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Core.Types.Pairing
import BV.Core.Types.Program

newtype ProofChecks a
  = ProofChecks { unwrap :: M.Map PairingId [ProofCheck a] }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

data ProofCheck a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp]
      , hyp :: Hyp
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

data Hyp
  = HypPcImp PcImpHyp
  | HypEq
      { ifAt :: Bool
      , eq :: EqHyp
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PcImpHyp
  = PcImpHyp
      { lhs :: PcImpHypSide
      , rhs :: PcImpHypSide
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PcImpHypSide
  = PcImpHypSideBool Bool
  | PcImpHypSidePc VisitWithTag
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHyp
  = EqHyp
      { lhs :: EqHypSide
      , rhs :: EqHypSide
      , induct :: Maybe EqHypInduct
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypSide
  = EqHypSide
      { expr :: Expr
      , visit :: VisitWithTag
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypInduct
  = EqHypInduct
      { a :: Integer
      , b :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data VisitWithTag
  = VisitWithTag
      { visit :: Visit
      , tag :: Tag
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Visit
  = Visit
      { nodeId :: NodeId
      , restrs :: [Restr]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Restr
  = Restr
      { nodeId :: NodeId
      , visitCount :: VisitCount
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data VisitCount
  = VisitCount
      { numbers :: [Integer]
      , offsets :: [Integer]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

--

newtype SmtProofChecks
  = SmtProofChecks { unwrap :: M.Map PairingId [SmtProofCheckGroup] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

data SmtProofCheckGroup
  = SmtProofCheckGroup
      { setup :: [String]
      , imps :: [String]
      }
  deriving (Eq, Generic, NFData, Ord, Show)
