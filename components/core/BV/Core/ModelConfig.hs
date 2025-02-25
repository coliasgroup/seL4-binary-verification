{-# LANGUAGE TemplateHaskell #-}

module BV.Core.ModelConfig
    ( MemoryMode (..)
    , ModelConfig (..)
    , configureSExpr
    , modelConfigPreamble
    , prettyMemoryMode
    , prettyModelConfig
    ) where

import BV.Core.Types
import BV.SMTLIB2

import Data.Binary (Binary)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import GHC.Generics (Generic)

data ModelConfig
  = ModelConfig
      { memoryMode :: MemoryMode
      }
  deriving (Eq, Generic, Ord, Show)

instance Binary ModelConfig where

prettyModelConfig :: ModelConfig -> String
prettyModelConfig modelConfig = prettyMemoryMode modelConfig.memoryMode

data MemoryMode
  = MemoryModeWord8
  | MemoryModeWord32
  deriving (Eq, Generic, Ord, Show)

instance Binary MemoryMode where

prettyMemoryMode :: MemoryMode -> String
prettyMemoryMode = \case
    MemoryModeWord8 -> "word8"
    MemoryModeWord32 -> "word32"

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

memoryModeConfig :: MemoryMode -> MemoryModeConfig
memoryModeConfig = \case
    MemoryModeWord8 -> word8Config
    MemoryModeWord32 -> word32Config

data MemoryModeConfig
  = MemoryModeConfig
      { preamble :: [SExprWithPlaceholders]
      , memSort :: SExpr
      , memDomSort :: SExpr
      }
  deriving (Eq, Generic, Ord, Show)

word8Config :: MemoryModeConfig
word8Config = MemoryModeConfig
    { preamble = readSExprsWithPlaceholders
        $(makeRelativeToProject "components/core/data/word8-preamble.smt2" >>= embedStringFile)
    , memSort = readSExpr "(Array (_ BitVec 32) (_ BitVec 8))"
    , memDomSort = readSExpr "(Array (_ BitVec 32) (_ BitVec 1))"
    }

word32Config :: MemoryModeConfig
word32Config = MemoryModeConfig
    { preamble = readSExprsWithPlaceholders
        $(makeRelativeToProject "components/core/data/word32-preamble.smt2" >>= embedStringFile)
    , memSort = readSExpr "(Array (_ BitVec 30) (_ BitVec 32))"
    , memDomSort = readSExpr "(Array (_ BitVec 32) (_ BitVec 1))"
    }
