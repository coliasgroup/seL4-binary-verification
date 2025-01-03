module BV.Problem where

import BV.Program
import Data.Map (Map)
import GHC.Generics (Generic)
import Optics.Core

data Problem
  = Problem
      { c :: ProblemSide
      , asm :: ProblemSide
      , nodes :: NodeMap
      }
  deriving (Generic, Show)

data ProblemSide
  = ProblemSide
      { name :: Ident
      , input :: [Argument]
      , output :: [Argument]
      , entryPoint :: NodeID
      }
  deriving (Generic, Show)
