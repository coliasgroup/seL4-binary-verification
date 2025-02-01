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

import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Writer (runWriterT)
import Control.Monad.Writer (tell)
import Optics

executeSMTProofCheckOffline
    :: MonadSolver m
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheck a -> m (Maybe SatResult)
executeSMTProofCheckOffline config timeout check = undefined

executeSMTProofCheckGroupOffline
    :: MonadSolver m
    => SolverConfig -> Maybe SolverTimeout -> SMTProofCheckGroup a -> m (Maybe SatResult)
executeSMTProofCheckGroupOffline config timeout check = undefined

executeSMTProofCheckGroupOnline
    :: (MonadSolver m, MonadThrow m)
    => SolverConfig
    -> Maybe SolverTimeout
    -> SMTProofCheckGroup a
    -> m (Maybe a, [(a, SatResult)])
executeSMTProofCheckGroupOnline config timeout group = do
    sendSimpleCommandExpectingSuccess $ SetOption (PrintSuccessOption True)
    sendSimpleCommandExpectingSuccess $ SetLogic "QF_AUFBV"
    mapM_ sendExpectingSuccess (smtConfigPreamble config)
    mapM_ (sendExpectingSuccess . configureSExpr config) group.setup
    (timeoutInfo, results) <- runWriterT . runExceptT . forM_ group.imps $ \check -> do
        let meta = check.meta
        lift . sendSimpleCommandExpectingSuccess $ Push 1
        undefined -- TODO
        result <- ExceptT $ maybe (Left meta) Right <$> checkSatWithTimeout timeout
        tell [(meta, result)]
        lift . sendSimpleCommandExpectingSuccess $ Pop 1
        lift . sendSimpleCommandExpectingSuccess . Assert $
            undefined -- TODO
    return (timeoutInfo ^? _Left, results)
