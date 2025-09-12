module BV.Core.Experimental.RepGraph.AsmStackRep
    ( WithAsmStackRep
    , runWithAsmStackRep
    ) where

import BV.Core.Experimental.RepGraph.Core
import BV.Core.Experimental.RepGraph.Flatten

import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras

import Control.Monad (guard)
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (isPrefixOf)

newtype WithAsmStackRep m a
  = WithAsmStackRep { run :: ReaderT (ArgRenames AsmRefineTag) m a }
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadTrans)

runWithAsmStackRep :: MonadRepGraph AsmRefineTag m => ArgRenames AsmRefineTag -> WithAsmStackRep m a -> m a
runWithAsmStackRep argRenames m = runReaderT m.run argRenames

instance MonadRepGraphFlattenSend m => MonadRepGraphFlattenSend (WithAsmStackRep m) where
    sendCommand = WithAsmStackRep . sendCommand

instance MonadStructs m => MonadStructs (WithAsmStackRep m) where
    askLookupStruct = WithAsmStackRep askLookupStruct

instance MonadRepGraphFlatten m => MonadRepGraphFlatten (WithAsmStackRep m) where
    liftFlatten = WithAsmStackRep . liftFlatten

instance MonadRepGraph AsmRefineTag m => MonadRepGraphDefaultHelper AsmRefineTag m (WithAsmStackRep m)

instance MonadRepGraph AsmRefineTag m => MonadRepGraph AsmRefineTag (WithAsmStackRep m) where
    runProblemVarRepHook var kind _ = runMaybeT $ do
        tag <- askTag
        guard $ tag == Asm
        guard $ "stack" `isPrefixOf` var.name.unwrap
        guard $ var.ty == ExprTypeMem
        guard $ kind /= VarRepRequestKindInit
        argRenames <- lift $ lift $ WithAsmStackRep ask
        let quadrant = PairingEqSideQuadrant tag PairingEqDirectionIn
        return $ VarRepRequestSplitMem
            { addr = varE word32T (argRenames quadrant (Ident "r13"))
            }
