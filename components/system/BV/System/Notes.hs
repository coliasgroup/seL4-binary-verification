{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.Notes
    (
    ) where

import BV.Core.AdornProofScript
import BV.Core.Types
import BV.System.CheckFrontend
import BV.System.TaskQueue

import GHC.Generics (Generic)

type ProofSearchResult = Either (ProofScript ()) ProofSearchError

data ProofSearchError
  = ProofSearchError
  deriving (Eq, Generic, Ord, Show)

data AnyTask
  = TaskSMTProofCheckGroup (Task (SMTProofCheckGroup (SMTProofCheckDescription String)) (SMTProofCheckResult ()))
  | TaskProofSearch (Task PairingId ProofSearchResult)
  deriving (Eq, Generic)
