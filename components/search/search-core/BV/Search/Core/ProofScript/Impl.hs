module BV.Search.Core.ProofScript.Impl
    ( DiscoverProofScriptInput (..)
    , discoverProofScript
    ) where

import BV.Search.Core.GraphSlice
import BV.Search.Core.Solver

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras.Expr (andE, eqE, falseE, ifThenElseE, minusE, notE,
                                  trueE, varE, word32E, word32T)
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.Program (FunctionSignature, LookupFunctionSignature,
                                     signatureOfFunction)
import BV.Core.Types.Extras.ProofCheck (eqH, eqSideH, pcFalseH)
import BV.Logging
import BV.Utils (ensure, ensureM, expecting, todo)

import Control.DeepSeq (NFData)
import qualified Data.Array as A
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (findIndex, genericIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Text.Printf (PrintfArg (formatArg), printf)

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
