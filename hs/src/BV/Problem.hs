module BV.Problem where

import Data.Map (Map)
import GHC.Generics (Generic)
import Optics.Core

import BV.Program

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
