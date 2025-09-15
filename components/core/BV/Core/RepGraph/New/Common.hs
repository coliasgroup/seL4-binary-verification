{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module BV.Core.RepGraph.New.Common
    ( ExprCommand (..)
    , ExprCommandInlineHint (..)
    , MonadInner (..)
    ) where

import BV.Core.Types (Expr, NameTy)

import Control.DeepSeq (NFData)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
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

class (Monad m, Monad n) => MonadInner n m | m -> n where
    liftInner :: Monad m => n a -> m a

instance MonadInner n m => MonadInner n (MaybeT m) where
    liftInner = lift . liftInner
