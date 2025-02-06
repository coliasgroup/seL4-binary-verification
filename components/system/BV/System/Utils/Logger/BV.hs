{-# LANGUAGE OverloadedStrings #-}

module BV.System.Utils.Logger.BV
    ( addLoggingToCacheContext
    , withPushLogContextCheck
    , withPushLogContextCheckGroup
    , withPushLogContextPairing
    ) where

import BV.Core.Types
import BV.System.Cache
import BV.System.Fingerprinting
import BV.System.Utils.Logger

withPushLogContextPairing :: MonadLoggerWithContext m => PairingId -> m a -> m a
withPushLogContextPairing pairingId = withPushLogContext $
    "pairing " ++ pairingId.asm.unwrap

withPushLogContextCheckGroup :: MonadLoggerWithContext m => SMTProofCheckGroup b -> m a -> m a
withPushLogContextCheckGroup group = withPushLogContext $
    "group " ++ prettySMTProofCheckGroupFingerprintShort (smtProofCheckGroupFingerprint group)

withPushLogContextCheck :: MonadLoggerWithContext m => SMTProofCheck b -> m a -> m a
withPushLogContextCheck check = withPushLogContext $
    "check " ++ prettySMTProofCheckFingerprintShort (smtProofCheckFingerprint check)

addLoggingToCacheContext :: MonadLoggerWithContext m => CacheContext m -> CacheContext m
addLoggingToCacheContext ctx =
    CacheContext
        { queryCache = \check -> withPushLogContext "query" . withPushLogContextCheck check $ do
            logDebug "querying"
            resp <- ctx.queryCache check
            logDebug $ "got: " ++ show resp
            return resp
        , updateCache = \check result -> withPushLogContext "update" . withPushLogContextCheck check $ do
            logDebug $ "sending: " ++ show result
            ctx.updateCache check result
            logDebug "done"
        }
