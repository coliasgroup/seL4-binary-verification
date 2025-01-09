module BV.Core.ProofChecks where

import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Core.Pairing
import BV.Core.Program

newtype ProofChecks a
  = ProofChecks { unwrap :: M.Map PairingId [ProofCheck a] }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

data ProofCheck a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp]
      , hyp :: Hyp
      }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

data Hyp
  = HypPcImp PcImpHyp
  | HypEq
      { ifAt :: Bool
      , eq :: EqHyp
      }
  deriving (Eq, Generic, Ord, Show)

data PcImpHyp
  = PcImpHyp
      { lhs :: PcImpHypSide
      , rhs :: PcImpHypSide
      }
  deriving (Eq, Generic, Ord, Show)

data PcImpHypSide
  = PcImpHypSideBool Bool
  | PcImpHypSidePc VisitWithTag
  deriving (Eq, Generic, Ord, Show)

data EqHyp
  = EqHyp
      { lhs :: EqHypSide
      , rhs :: EqHypSide
      , induct :: Maybe EqHypInduct
      }
  deriving (Eq, Generic, Ord, Show)

data EqHypSide
  = EqHypSide
      { expr :: Expr
      , visit :: VisitWithTag
      }
  deriving (Eq, Generic, Ord, Show)

data EqHypInduct
  = EqHypInduct
      { a :: Integer
      , b :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data VisitWithTag
  = VisitWithTag
      { visit :: Visit
      , tag :: Tag
      }
  deriving (Eq, Generic, Ord, Show)

data Visit
  = Visit
      { nodeId :: NodeId
      , restrs :: [Restr]
      }
  deriving (Eq, Generic, Ord, Show)

data Restr
  = Restr
      { nodeId :: NodeId
      , visitCount :: VisitCount
      }
  deriving (Eq, Generic, Ord, Show)

data VisitCount
  = VisitCount
      { numbers :: [Integer]
      , offsets :: [Integer]
      }
  deriving (Eq, Generic, Ord, Show)
