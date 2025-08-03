{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module BV.System.Core.Utils.Logging
    ( augmentSolverContextWithLogging
    , runSolverWithLogging
    , withPushLogContextCheck
    , withPushLogContextCheckFingerprint
    , withPushLogContextCheckGroup
    , withPushLogContextCheckGroupFingerprint
    , withPushLogContextCheckSubgroup
    , withPushLogContextCheckSubgroupId
    , withPushLogContextPairing
    ) where

import BV.Core.Prelude
import BV.Logging
import BV.SMTLIB2.Process
import BV.SMTLIB2.SExpr.Build
import BV.System.Core.Fingerprinting
import BV.System.Core.Types

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Text.Lazy.Builder (toLazyText)
import System.Process (CreateProcess)

withPushLogContextPairing :: MonadLoggerWithContext m => PairingId' -> m a -> m a
withPushLogContextPairing pairingId = withPushLogContext $
    "pairing " ++ (getAsm pairingId).unwrap

withPushLogContextCheckGroup :: MonadLoggerWithContext m => CheckGroup -> m a -> m a
withPushLogContextCheckGroup group = withPushLogContextCheckGroupFingerprint group.fingerprint

withPushLogContextCheckGroupFingerprint :: MonadLoggerWithContext m => CheckGroupFingerprint -> m a -> m a
withPushLogContextCheckGroupFingerprint fingerprint = withPushLogContext $
    "group " ++ prettyCheckGroupFingerprintShort fingerprint

withPushLogContextCheckSubgroup :: MonadLoggerWithContext m => CheckSubgroup -> m a -> m a
withPushLogContextCheckSubgroup = withPushLogContextCheckSubgroupId . takeSubgroupId

withPushLogContextCheckSubgroupId :: MonadLoggerWithContext m => CheckSubgroupId -> m a -> m a
withPushLogContextCheckSubgroupId subgroupId = withPushLogContext $
    "subgroup " ++ prettyCheckSubgroupIdShort subgroupId

withPushLogContextCheck :: MonadLoggerWithContext m => Check -> m a -> m a
withPushLogContextCheck check = withPushLogContextCheckFingerprint check.fingerprint

withPushLogContextCheckFingerprint :: MonadLoggerWithContext m => CheckFingerprint -> m a -> m a
withPushLogContextCheckFingerprint fingerprint = withPushLogContext $
    "check " ++ prettyCheckFingerprintShort fingerprint

augmentSolverContextWithLogging :: MonadLoggerWithContext m => SolverContext m -> SolverContext m
augmentSolverContextWithLogging ctx =
    SolverContext
        { sendSExpr = \req -> withPushLogContext "send" $ do
            logTraceGeneric . toLazyText $ buildSExpr req
            ctx.sendSExpr req
        , recvSExprWithTimeout = \timeout -> do
            resp <- ctx.recvSExprWithTimeout timeout
            case resp of
                Nothing -> logTrace "timeout"
                Just sexpr -> withPushLogContext "recv" $ do
                    logTraceGeneric . toLazyText $ buildSExpr sexpr
            return resp
        }

runSolverWithLogging :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => CreateProcess -> SolverT m a -> m a
runSolverWithLogging =
    runSolverWith
        augmentSolverContextWithLogging
        (withPushLogContext "stderr" . logInfoGeneric)

deriving via (SolverTInner m) instance MonadLogger m => MonadLogger (SolverT m)

deriving via (SolverTInner m) instance MonadLoggerWithContext m => MonadLoggerWithContext (SolverT m)
