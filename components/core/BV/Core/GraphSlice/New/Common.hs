{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module BV.Core.GraphSlice.New.Common
    ( ExprCommand (..)
    , ExprCommandInlineHint (..)
    , MonadGraphSliceSendSExpr (..)
    , MonadLiftInner (..)
    , MonadMapInnermost (..)
    ) where

import BV.Core.Types

import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
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

class MonadLiftInner u t | t -> u where
    liftInner :: Monad m => u m a -> t m a

class MonadMapInnermost t where
    mapInnermost :: (forall a. m a -> n a) -> t m b -> t n b

class Monad m => MonadGraphSliceSendSExpr m where
    sendCommand :: SMTProofCheckCommand -> m ()

instance Monad m => MonadGraphSliceSendSExpr (WriterT [SMTProofCheckCommand] m) where
    sendCommand s = tell [s]

instance MonadGraphSliceSendSExpr m => MonadGraphSliceSendSExpr (ExceptT e m) where
    sendCommand = lift . sendCommand

instance MonadGraphSliceSendSExpr m => MonadGraphSliceSendSExpr (ReaderT r m) where
    sendCommand = lift . sendCommand
