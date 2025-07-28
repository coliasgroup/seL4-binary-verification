{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.ProofChecks
    ( EqHyp (..)
    , EqHypInduct (..)
    , EqHypSide (..)
    , Hyp (..)
    , PcImpHyp (..)
    , PcImpHypSide (..)
    , ProofCheck (..)
    , ProofCheckDescription
    , ProofChecks (..)
    , Restr (..)
    , Visit (..)
    , VisitCount (..)
    , VisitWithTag (..)
    , checkVisits
    , hypVisits
    ) where

import BV.Core.Types.Pairing
import BV.Core.Types.Program
import BV.Core.Types.ProofScript

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

newtype ProofChecks t a
  = ProofChecks { unwrap :: M.Map (PairingId t) (ProofScript t [ProofCheck t a]) }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

instance AtPairingId t (ProofScript t [ProofCheck t a]) (ProofChecks t a) where
    atPairingId = atPairingId . (.unwrap)

data ProofCheck t a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp t]
      , hyp :: Hyp t
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

type ProofCheckDescription = String

data Hyp t
  = HypPcImp (PcImpHyp t)
  | HypEq
      { ifAt :: Bool
      , eq :: EqHyp t
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PcImpHyp t
  = PcImpHyp
      { lhs :: PcImpHypSide t
      , rhs :: PcImpHypSide t
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data PcImpHypSide t
  = PcImpHypSideBool Bool
  | PcImpHypSidePc (VisitWithTag t)
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHyp t
  = EqHyp
      { lhs :: EqHypSide t
      , rhs :: EqHypSide t
      , induct :: Maybe EqHypInduct
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypSide t
  = EqHypSide
      { expr :: Expr
      , visit :: VisitWithTag t
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypInduct
  = EqHypInduct
      { a :: Integer
      , b :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data VisitWithTag t
  = VisitWithTag
      { visit :: Visit
      , tag :: t
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
      { nodeAddr :: NodeAddr
      , visitCount :: VisitCount
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data VisitCount
  = VisitCount
      { numbers :: [Integer]
      , offsets :: [Integer]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Semigroup VisitCount where
    x <> y = VisitCount
        { numbers = x.numbers <> y.numbers
        , offsets = x.offsets <> y.offsets
        }

instance Monoid VisitCount where
    mempty = VisitCount [] []

hypVisits :: Traversal' (Hyp t) (VisitWithTag t)
hypVisits =
    (#_HypPcImp % (#lhs `adjoin` #rhs) % #_PcImpHypSidePc)
    `adjoin`
    (#_HypEq % _2 % (#lhs `adjoin` #rhs) % #visit)

checkVisits :: Traversal' (ProofCheck t a) (VisitWithTag t)
checkVisits = (#hyps % traversed `adjoin` #hyp) % hypVisits
