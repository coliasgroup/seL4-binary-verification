{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Types.ProofCheck
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
    , RestrMap
    , Visit (..)
    , VisitCount (..)
    , checkVisits
    , debugShowRestrs
    , debugShowVisit
    , debugShowVisitCount
    , hypVisits
    ) where

import BV.Core.Types.Program
import BV.Core.Types.Tag

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.List (intercalate)
import qualified Data.Map as M
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
  | PcImpHypSidePc (Visit t)
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
      { expr :: GraphExpr
      , visit :: Visit t
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data EqHypInduct
  = EqHypInduct
      { n1 :: Integer
      , n2 :: Integer
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data Visit t
  = Visit
      { tag :: t
      , nodeId :: NodeId
      , restrs :: RestrMap
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type RestrMap = M.Map NodeAddr VisitCount

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

hypVisits :: Traversal' (Hyp t) (Visit t)
hypVisits =
    (#_HypPcImp % (#lhs `adjoin` #rhs) % #_PcImpHypSidePc)
    `adjoin`
    (#_HypEq % _2 % (#lhs `adjoin` #rhs) % #visit)

checkVisits :: Traversal' (ProofCheck t a) (Visit t)
checkVisits = (#hyps % traversed `adjoin` #hyp) % hypVisits

--

debugShowVisit :: Tag t => Visit t -> String
debugShowVisit visit = prettyTag visit.tag ++ ":" ++ prettyNodeId visit.nodeId ++ ":" ++ debugShowRestrs visit.restrs

debugShowRestrs :: RestrMap -> String
debugShowRestrs restrs = "[" ++ intercalate "," (map f (M.toList restrs)) ++ "]"
  where
    f (addr, vc) = prettyNodeAddr addr ++ "=" ++ debugShowVisitCount vc

debugShowVisitCount :: VisitCount -> String
debugShowVisitCount vc =
    intercalate "_" $ map showNumber vc.numbers ++ map showOffset vc.offsets
  where
    showNumber = show
    showOffset n = "i+" ++ show n
