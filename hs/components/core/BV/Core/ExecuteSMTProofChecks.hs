{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.ExecuteSMTProofChecks
    ( SolverConfig (..)
    , SolverMemoryMode (..)
    , executeSMTProofCheckGroupOffline
    , executeSMTProofCheckGroupOnline
    , executeSMTProofCheckOffline
    ) where

import Control.Monad.Logger (MonadLogger)
import GHC.Generics (Generic)

import BV.SMTLIB2.Types

import BV.Core.ConfigureSMT
import BV.Core.Types
import BV.SMTLIB2.Types.Command

executeSMTProofCheckOffline
    :: MonadSolver m
    => SolverConfig -> SMTProofCheck () -> m SatResult
executeSMTProofCheckOffline = undefined

executeSMTProofCheckGroupOffline
    :: MonadSolver m
    => SolverConfig -> SMTProofCheckGroup () -> m SatResult
executeSMTProofCheckGroupOffline = undefined

executeSMTProofCheckGroupOnline
    :: MonadSolver m
    => SolverConfig -> (Integer -> SatResult -> m ()) -> SMTProofCheckGroup () -> m ()
executeSMTProofCheckGroupOnline = undefined
