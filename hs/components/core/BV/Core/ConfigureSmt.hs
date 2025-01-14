module BV.Core.ConfigureSmt
    ( SolverConfig (..)
    , SolverMemoryMode (..)
    , configureSExpr
    , smtConfigPreamble
    ) where

import GHC.Generics (Generic)

import BV.Core.Types
import BV.SMTLIB2.Types

data SolverConfig
  = SolverConfig
      { memoryMode :: SolverMemoryMode
      }
  deriving (Eq, Generic, Ord, Show)

data SolverMemoryMode
  = SolverMemoryModeWord8
  | SolverMemoryModeWord32
  deriving (Eq, Generic, Ord, Show)

configureSExpr :: SolverConfig -> SExprWithPlaceholders -> SExpr
configureSExpr = undefined

smtConfigPreamble :: SolverConfig -> [SExpr]
smtConfigPreamble = undefined
