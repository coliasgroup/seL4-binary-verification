{-# LANGUAGE OverloadedStrings #-}

module BV.System.Utils.Logger.BV
    ( withPushLogContextCheck
    , withPushLogContextCheckGroup
    , withPushLogContextPairing
    ) where

import BV.Core.Types
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
