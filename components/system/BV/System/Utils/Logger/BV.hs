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

withPushLogContextCheckGroup :: MonadLoggerWithContext m => SMTProofCheckGroupFingerprint -> m a -> m a
withPushLogContextCheckGroup fingerprint = withPushLogContext $
    "group " ++ prettySMTProofCheckGroupFingerprintShort fingerprint

withPushLogContextCheck :: MonadLoggerWithContext m => SMTProofCheckFingerprint -> m a -> m a
withPushLogContextCheck fingerprint = withPushLogContext $
    "check " ++ prettySMTProofCheckFingerprintShort fingerprint

addLoggingToCacheContext :: MonadLoggerWithContext m => CacheContext m -> CacheContext m
addLoggingToCacheContext ctx =
    CacheContext
        { queryCache = \check -> withPushLogContext "query" . withPushLogContextCheck check $ do
            logTrace "querying"
            resp <- ctx.queryCache check
            logTrace $ "got: " ++ show resp
            return resp
        , updateCache = \check result -> withPushLogContext "update" . withPushLogContextCheck check $ do
            logTrace $ "sending: " ++ show result
            ctx.updateCache check result
            logTrace "done"
        }
