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
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

newtype ProofChecks a
  = ProofChecks { unwrap :: M.Map PairingId' (ProofScript [ProofCheck a]) }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving newtype (NFData)

instance AtPairingId (ProofScript [ProofCheck a]) (ProofChecks a) where
    atPairingId = atPairingId . (.unwrap)

data ProofCheck a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp]
      , hyp :: Hyp
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

type ProofCheckDescription = String

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
      , tag :: Tag'
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

hypVisits :: Traversal' Hyp VisitWithTag
hypVisits =
    (#_HypPcImp % (#lhs `adjoin` #rhs) % #_PcImpHypSidePc)
    `adjoin`
    (#_HypEq % _2 % (#lhs `adjoin` #rhs) % #visit)

checkVisits :: Traversal' (ProofCheck a) VisitWithTag
checkVisits = (#hyps % traversed `adjoin` #hyp) % hypVisits
