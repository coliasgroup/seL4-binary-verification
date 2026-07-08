module BV.Search.Core.ProofScript.Impl
    ( DiscoverProofScriptInput (..)
    , discoverProofScript
    ) where

import BV.Search.Core.GraphSlice
import BV.Search.Core.Solver

import BV.Core.Types
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.Program (LookupFunctionSignature)
import BV.Logging
import BV.Utils (todo)

import GHC.Generics (Generic)
import Text.Printf (printf)

data DiscoverProofScriptInput
  = DiscoverProofScriptInput
      { lookupSig :: LookupFunctionSignature AsmRefineTag
      , pairings :: Pairings'
      , graphSliceInput :: AsmRefineGraphSliceInput
      }
  deriving (Generic)

discoverProofScript
    :: forall m n.
       ( Monad m
       , MonadGraphSliceGetSExprValue n
       , MonadLoggerWithContext m
       , MonadLoggerWithContext n
       )
    => (forall a. n a -> m a)
    -> DiscoverProofScriptInput
    -> m (ProofScript AsmRefineTag ())
discoverProofScript run input = do
    let pairingId = pairingIdOfProblem input.graphSliceInput.repGraphInput.problem
    logInfo $ printf "searching for proof for %s" (prettyPairingId pairingId)
    todo
