{-# LANGUAGE TemplateHaskell #-}

module BV.Core.ModelConfig
    ( ModelConfig (..)
    , SolverMemoryMode (..)
    , configureSExpr
    , modelConfigPreamble
    ) where

import BV.Core.Types
import BV.SMTLIB2

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import GHC.Generics (Generic)

data ModelConfig
  = ModelConfig
      { memoryMode :: SolverMemoryMode
      }
  deriving (Eq, Generic, Ord, Show)

data SolverMemoryMode
  = SolverMemoryModeWord8
  | SolverMemoryModeWord32
  deriving (Eq, Generic, Ord, Show)

configureSExpr :: ModelConfig -> SExprWithPlaceholders -> SExpr
configureSExpr config ex = do
    atomOrPlaceholder <- ex
    case atomOrPlaceholder of
        AtomOrPlaceholderAtom atom -> return atom
        AtomOrPlaceholderPlaceholder placeholder -> case placeholder of
            SExprPlaceholderMemSort -> config'.memSort
            SExprPlaceholderMemDomSort -> config'.memDomSort
  where
    config' = memoryModeConfig config.memoryMode

modelConfigPreamble :: ModelConfig -> [SExpr]
modelConfigPreamble config = map (configureSExpr config) (memoryModeConfig config.memoryMode).preamble

memoryModeConfig :: SolverMemoryMode -> SolverMemoryModeConfig
memoryModeConfig = \case
    SolverMemoryModeWord8 -> word8Config
    SolverMemoryModeWord32 -> word32Config

data SolverMemoryModeConfig
  = SolverMemoryModeConfig
      { preamble :: [SExprWithPlaceholders]
      , memSort :: SExpr
      , memDomSort :: SExpr
      }
  deriving (Eq, Generic, Ord, Show)

word8Config :: SolverMemoryModeConfig
word8Config = SolverMemoryModeConfig
    { preamble = readSExprsWithPlaceholders
        $(makeRelativeToProject "components/core/data/word8-preamble.smt2" >>= embedStringFile)
    , memSort = readSExpr "(Array (_ BitVec 32) (_ BitVec 8))"
    , memDomSort = readSExpr "(Array (_ BitVec 32) (_ BitVec 1))"
    }

word32Config :: SolverMemoryModeConfig
word32Config = SolverMemoryModeConfig
    { preamble = readSExprsWithPlaceholders
        $(makeRelativeToProject "components/core/data/word32-preamble.smt2" >>= embedStringFile)
    , memSort = readSExpr "(Array (_ BitVec 30) (_ BitVec 32))"
    , memDomSort = readSExpr "(Array (_ BitVec 32) (_ BitVec 1))"
    }
