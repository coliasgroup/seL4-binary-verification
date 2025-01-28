module BV.Core.ExecuteSMTProofChecks
    ( SolverConfig (..)
    , SolverMemoryMode (..)
    , executeSMTProofCheckGroupOffline
    , executeSMTProofCheckGroupOnline
    , executeSMTProofCheckOffline
    ) where

import BV.Core.ConfigureSMT
import BV.Core.Types
import BV.SMTLIB2.Types
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
