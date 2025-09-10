{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Experimental.Types
    ( Command (..)
    , InlineHint (..)
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics (Generic)

data Command
  = CommandDeclare NameTy
  | CommandDefine (Maybe InlineHint) NameTy Expr
  | CommandAssert Expr
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary Command

data InlineHint
  = InlineHintInline
  | InlineHintDontInline
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary InlineHint
