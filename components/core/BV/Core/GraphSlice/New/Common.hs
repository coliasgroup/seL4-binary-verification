{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module BV.Core.GraphSlice.New.Common
    ( ExprCommand (..)
    , ExprCommandInlineHint (..)
    , MonadGraphSliceSendSExpr (..)
    , MonadInner (..)
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (WriterT, tell)
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

class Monad m => MonadGraphSliceSendSExpr m where
    sendSExpr :: SExprWithPlaceholders -> m ()

instance Monad m => MonadGraphSliceSendSExpr (WriterT [SExprWithPlaceholders] m) where
    sendSExpr s = tell [s]

instance MonadGraphSliceSendSExpr m => MonadGraphSliceSendSExpr (ExceptT e m) where
    sendSExpr = lift . sendSExpr

instance MonadGraphSliceSendSExpr m => MonadGraphSliceSendSExpr (ReaderT r m) where
    sendSExpr = lift . sendSExpr
