module BV.Core.Stages.BuildProblem
    ( buildProblem
    ) where

import Control.DeepSeq (NFData)
import Optics
import qualified Data.Map as M
import GHC.Generics (Generic)

import BV.Core.Types

buildProblem :: (Tag -> Ident -> Function) -> InlineScript -> PairingOf (Named Function) -> Problem
buildProblem = undefined

data ProblemBuilder = ProblemBuilder
    { sides :: PairingOf ProblemSide
    , nodes :: M.Map NodeAddr NodeWithMeta
    }
  deriving (Eq, Generic, NFData, Ord, Show)

data NodeMapBuilder = ProblemBuilder
    { sides :: PairingOf ProblemSide
    , nodes :: M.Map NodeAddr NodeWithMeta
    }
  deriving (Eq, Generic, NFData, Ord, Show)
