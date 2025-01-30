module BV.System.Check
    ( AcceptableSatResult (..)
    , ExecuteChecksConfig (..)
    , MonadCache (..)
    , ProblemCheckError (..)
    , ProblemCheckResult
    , Report (..)
    , executeChecks
    ) where

import BV.Core.AdornProofScript
import BV.Core.ExecuteSMTProofChecks
import BV.Core.Types
import BV.SMTLIB2.Types.Command
import BV.System.SolversConfig
import BV.System.Throttle

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as M
import GHC.Generics (Generic)

data ExecuteChecksConfig
  = ExecuteChecksConfig
      { numCores :: Integer
      , solversConfig :: SolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

data Report
  = Report
      { unwrap :: M.Map PairingId ProblemCheckResult
      }
  deriving (Eq, Generic, Ord, Show)

type ProblemCheckResult = Maybe (NonEmpty (ProblemCheckError, NonEmpty ProofScriptNodeLocation))

data ProblemCheckError
  = NoSolversAnswered
  | SomeSolverAnsweredSat
  | AllSolversAnsweredUnknown
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadCache m where
    queryCache :: SMTProofCheck () -> m (Maybe AcceptableSatResult)
    updateCache :: SMTProofCheck () -> AcceptableSatResult -> m ()

-- newtype TrivialCacheT m a = TrivialCacheT { unwrap :: m a }

data AcceptableSatResult
  = AcceptableSatResultSat
  | AcceptableSatResultUnsat
  deriving (Eq, Generic, Ord, Show)

executeChecks
    :: forall m.
       (MonadUnliftIO m, MonadLogger m, MonadCache m)
    => ExecuteChecksConfig -> FlattenedSMTProofChecks ProofScriptNodeLocation -> m Report
executeChecks config checks = withRunInIO $ \runInIO ->
    withThrottling (Units config.numCores) $ \throttle -> runConcurrently $ do
        Report . M.fromList <$>
            traverse (Concurrently . runInIO . f throttle) (zip [0..] (M.toAscList checks.unwrap))
  where
    f :: Throttle -> (Int, (PairingId, [SMTProofCheckGroup ProofScriptNodeLocation])) -> m (PairingId, ProblemCheckResult)
    f throttle (pairingIx, (pairingId, checksForPairing)) =
        (,) pairingId . nonEmpty . concat <$> mapM (g throttle pairingIx) checksForPairing
    g :: Throttle -> Int -> SMTProofCheckGroup ProofScriptNodeLocation -> m [(ProblemCheckError, NonEmpty ProofScriptNodeLocation)]
    g throttle pairingIx group =
        let priority = negate (toInteger pairingIx)
         in undefined
