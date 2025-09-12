{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Experimental.Types
    ( Command (..)
    , InlineHint (..)
    , IntermediateSolverExpr
    , IntermediateSolverExprContext (..)
    , SolverExpr
    , SolverExprContext (..)
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Void (Void)
import GHC.Generics (Generic)

newtype SolverExprContext
  = SolverExprContext Void

type SolverExpr = Expr SolverExprContext

newtype IntermediateSolverExprContext
  = IntermediateSolverExprContext Void

type IntermediateSolverExpr = Expr IntermediateSolverExprContext

data Command c
  = CommandDeclare NameTy
  | CommandDefine (Maybe InlineHint) NameTy (Expr c)
  | CommandAssert (Expr c)
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary (Command c)

data InlineHint
  = InlineHintInline
  | InlineHintDontInline
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary InlineHint
