{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.RepGraph.New.ExprCommand
    ( ExprCommand (..)
    , ExprCommandInlineHint (..)
    ) where

import BV.Core.Types (Expr, NameTy)

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics (Generic)

data ExprCommand c
  = ExprCommandDeclare NameTy
  | ExprCommandDefine ExprCommandInlineHint NameTy (Expr c)
  | ExprCommandAssert (Expr c)
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary (ExprCommand c)

data ExprCommandInlineHint
  = ExprCommandInlineHintNever
  | ExprCommandInlineHintSometimes
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary ExprCommandInlineHint
