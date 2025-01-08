module BV.ProofChecks where

import Control.Applicative (many, optional, (<|>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.String (fromString)
import GHC.Generics (Generic)
import Optics.Core
import Text.Megaparsec (manyTill, manyTill_, try)

import BV.Pairing
import BV.Parsing
import BV.Printing
import BV.Program

newtype ProofChecks a
  = ProofChecks (M.Map PairingId [ProofCheck a])
  deriving (Eq, Generic, Ord, Show)

data ProofCheck a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp]
      , hyp :: Hyp
      }
  deriving (Eq, Generic, Ord, Show)

data Hyp
  = HypPcImp PcImpHyp
  | HypEq
      { eq :: EqHyp
      , ifAt :: Bool
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
      , visitCound :: VisitCount
      }
  deriving (Eq, Generic, Ord, Show)

data VisitCount
  = VisitCount
      { numbers :: [Integer]
      , offsets :: [Integer]
      }
  deriving (Eq, Generic, Ord, Show)

--

instance ParseFile (ProofChecks String) where
    parseFile = undefined

instance BuildToFile (ProofChecks String) where
    buildToFile = undefined
