module BV.Problem where

import Data.Map (Map)
import GHC.Generics (Generic)
import Optics.Core

import BV.Pairing
import BV.Program

data Problem
  = Problem
      { c :: ProblemSide
      , asm :: ProblemSide
      , nodes :: NodeMap
      }
  deriving (Eq, Generic, Ord, Show)

data ProblemSide
  = ProblemSide
      { name :: Ident
      , input :: [Argument]
      , output :: [Argument]
      , entryPoint :: NodeId
      }
  deriving (Eq, Generic, Ord, Show)

data NodeBySource
  = NodeBySource
      { nodeSource :: NodeSource
      , indexInProblem :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data NodeSource
  = NodeSource
      { tag :: Tag
      , functionName :: Ident
      , nodeAddr :: NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)
