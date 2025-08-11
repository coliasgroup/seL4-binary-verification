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
    , ProofCheckGroup
    , ProofCheckGroupCheckIndices (..)
    , Restr (..)
    , Visit (..)
    , VisitCount (..)
    , checkVisits
    , hypVisits
    ) where

import BV.Core.Types.Program
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data ProofCheck t a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp t]
      , hyp :: Hyp t
      }
  deriving (Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

type ProofCheckGroup t a = [ProofCheck t a]

newtype ProofCheckGroupCheckIndices
  = ProofCheckGroupCheckIndices { unwrap :: S.Set Int }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Monoid, NFData, Semigroup)

instance Binary ProofCheckGroupCheckIndices

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
  | PcImpHypSidePc (WithTag t Visit)
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
      , visit :: WithTag t Visit
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypInduct
  = EqHypInduct
      { a :: Integer
      , b :: Integer
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

hypVisits :: Traversal' (Hyp t) (WithTag t Visit)
hypVisits =
    (#_HypPcImp % (#lhs `adjoin` #rhs) % #_PcImpHypSidePc)
    `adjoin`
    (#_HypEq % _2 % (#lhs `adjoin` #rhs) % #visit)

checkVisits :: Traversal' (ProofCheck t a) (WithTag t Visit)
checkVisits = (#hyps % traversed `adjoin` #hyp) % hypVisits
