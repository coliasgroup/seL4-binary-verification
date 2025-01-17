module BV.Core.Stages.BuildProblem
    ( buildProblem
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

import BV.Core.Types
import BV.Core.Types (Node, NodeBySource)

buildProblem :: (Tag -> Ident -> Function) -> InlineScript -> PairingOf (Named Function) -> Problem
buildProblem = undefined

data ProblemBuilder
  = ProblemBuilder
      { sides :: PairingOf ProblemSide
      , nodeMapBuilder :: NodeMapBuilder
      }
  deriving (Eq, Generic, Ord, Show)

data NodeMapBuilder
  = NodeMapBuilder
      { nodes :: M.Map NodeAddr NodeWithMeta
      , nodesBySource :: M.Map NodeSource [NodeAddr]
      , vars :: S.Set Ident
      }
  deriving (Eq, Generic, Ord, Show)

data NodeWithMeta
  = NodeWithMeta
      { node :: Node
      , meta :: NodeMeta
      }
  deriving (Eq, Generic, Ord, Show)

data NodeMeta
  = NodeMeta
      { bySource :: Maybe NodeBySource
      }
  deriving (Eq, Generic, Ord, Show)
