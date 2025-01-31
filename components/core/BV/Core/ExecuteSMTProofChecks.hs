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
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheck a -> m (Maybe SatResult)
executeSMTProofCheckOffline config timeout check = undefined

executeSMTProofCheckGroupOffline
    :: MonadSolver m
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheckGroup a -> m (Maybe SatResult)
executeSMTProofCheckGroupOffline config timeout check = undefined

executeSMTProofCheckGroupOnline
    :: MonadSolver m
    => SolverConfig
    -> Maybe SolverTimeout
    -> SMTProofCheckGroup a
    -> m (SMTProofCheckGroup (Maybe SatResult, a))
executeSMTProofCheckGroupOnline config timeout group = undefined
