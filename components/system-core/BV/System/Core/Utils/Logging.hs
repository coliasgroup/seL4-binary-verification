{-# LANGUAGE OverloadedStrings #-}

module BV.System.Core.Utils.Logging
    ( augmentCacheContextWithLogging
    , augmentSolverContextWithLogging
    , runSolverWithLogging
    , withPushLogContextCheck
    , withPushLogContextCheckFingerprint
    , withPushLogContextCheckGroup
    , withPushLogContextCheckGroupFingerprint
    , withPushLogContextCheckSubgroup
    , withPushLogContextCheckSubgroupId
    , withPushLogContextPairing
    ) where

import BV.Core
import BV.Logging
import BV.SMTLIB2.Process
import BV.SMTLIB2.SExpr.Build
import BV.System.Core.Cache
import BV.System.Core.Fingerprinting
import BV.System.Core.WithFingerprints

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text.Lazy.Builder (toLazyText)
import System.Process (CreateProcess)

withPushLogContextPairing :: MonadLoggerWithContext m => PairingId -> m a -> m a
withPushLogContextPairing pairingId = withPushLogContext $
    "pairing " ++ pairingId.asm.unwrap

withPushLogContextCheckGroup :: MonadLoggerWithContext m => SMTProofCheckGroupWithFingerprints i -> m a -> m a
withPushLogContextCheckGroup group = withPushLogContextCheckGroupFingerprint group.fingerprint

withPushLogContextCheckGroupFingerprint :: MonadLoggerWithContext m => SMTProofCheckGroupFingerprint -> m a -> m a
withPushLogContextCheckGroupFingerprint fingerprint = withPushLogContext $
    "group " ++ prettySMTProofCheckGroupFingerprintShort fingerprint

withPushLogContextCheckSubgroup :: MonadLoggerWithContext m => SMTProofCheckSubgroupWithFingerprints i -> m a -> m a
withPushLogContextCheckSubgroup = withPushLogContextCheckSubgroupId . subgroupIdOf

withPushLogContextCheckSubgroupId :: MonadLoggerWithContext m => SMTProofCheckSubgroupId -> m a -> m a
withPushLogContextCheckSubgroupId subgroupId = withPushLogContext $
    "subgroup " ++ prettySMTProofCheckSubgroupIdShort subgroupId

withPushLogContextCheck :: MonadLoggerWithContext m => SMTProofCheckWithFingerprint i -> m a -> m a
withPushLogContextCheck check = withPushLogContextCheckFingerprint check.imp.meta.fingerprint

withPushLogContextCheckFingerprint :: MonadLoggerWithContext m => SMTProofCheckFingerprint -> m a -> m a
withPushLogContextCheckFingerprint fingerprint = withPushLogContext $
    "check " ++ prettySMTProofCheckFingerprintShort fingerprint

augmentSolverContextWithLogging :: MonadLoggerWithContext m => SolverContext m -> SolverContext m
augmentSolverContextWithLogging ctx =
    SolverContext
        { sendSExpr = \req -> withPushLogContext "send" $ do
            logTraceGeneric . toLazyText $ buildSExpr req
            ctx.sendSExpr req
        , recvSExprWithTimeout = \timeout -> withPushLogContext "recv" $ do
            resp <- ctx.recvSExprWithTimeout timeout
            case resp of
                Nothing -> logTrace "timeout"
                Just sexpr -> logTraceGeneric . toLazyText $ buildSExpr sexpr
            return resp
        }

runSolverWithLogging :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m) => CreateProcess -> SolverT m a -> m a
runSolverWithLogging =
    runSolverWith
        augmentSolverContextWithLogging
        (withPushLogContext "stderr" . logInfoGeneric)

augmentCacheContextWithLogging :: MonadLoggerWithContext m => CacheContext m -> CacheContext m
augmentCacheContextWithLogging ctx =
    CacheContext
        { queryCache = \check -> withPushLogContext "query" . withPushLogContextCheckFingerprint check $ do
            logTrace "querying"
            resp <- ctx.queryCache check
            logTrace $ "got: " ++ show resp
            return resp
        , updateCache = \result check -> withPushLogContext "update" . withPushLogContextCheckFingerprint check $ do
            logTrace $ "sending: " ++ show result
            ctx.updateCache result check
            logTrace "done"
        }
