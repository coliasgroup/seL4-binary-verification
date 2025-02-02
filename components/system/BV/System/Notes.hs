{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BV.System.Notes
    (
    ) where

import BV.Core.AdornProofScript
import BV.Core.Types
import BV.System.CheckFrontend
import BV.System.TaskQueue

import GHC.Generics (Generic)

-- type ProofSearchResult = Either (ProofScript ()) ProofSearchError

-- data ProofSearchError
--   = ProofSearchError
--   deriving (Eq, Generic, Ord, Show)

-- data AnyTask
--   = TaskSMTProofCheckGroup (Task (SMTProofCheckGroup (SMTProofCheckDescription String)) (SMTProofCheckResult ()))
--   | TaskProofSearch (Task PairingId ProofSearchResult)
--   deriving (Eq, Generic)

--

-- data ProblemCheckError
--   = NoSolversAnswered
--   | SomeSolverAnsweredSat
--   | AllSolversAnsweredUnknown
--   deriving (Eq, Generic, Ord, Show)

-- data AcceptableSatResult
--   = AcceptableSatResultSat
--   | AcceptableSatResultUnsat
--   deriving (Eq, Generic, Ord, Show)

-- class Monad m => MonadCache n m | m -> n where
--     checkSMTProofCheckWithCache :: Units -> CacheWrapper n m (SMTProofCheckGroup ()) AcceptableSatResult

-- type CacheWrapper n m a b = (a -> n b) -> a -> m b
