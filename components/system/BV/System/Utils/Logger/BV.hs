{-# LANGUAGE OverloadedStrings #-}

module BV.System.Utils.Logger.BV
    ( augmentCacheContextWithLogging
    ) where

import BV.Core.Types
import BV.Logging
import BV.SMTLIB2.Process
import BV.SMTLIB2.SExpr.Build
import BV.System.Core.Cache
import BV.System.Core.Fingerprinting
import BV.System.Core.Utils.Logging
import BV.System.Core.WithFingerprints

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text.Lazy.Builder (toLazyText)
import System.Process (CreateProcess)

augmentCacheContextWithLogging :: MonadLoggerWithContext m => CacheContext m -> CacheContext m
augmentCacheContextWithLogging ctx =
    CacheContext
        { queryCacheUsingFingerprint = \check -> withPushLogContext "query" . withPushLogContextCheckFingerprint check $ do
            logTrace "querying"
            resp <- ctx.queryCacheUsingFingerprint check
            logTrace $ "got: " ++ show resp
            return resp
        , updateCacheUsingFingerprint = \result check -> withPushLogContext "update" . withPushLogContextCheckFingerprint check $ do
            logTrace $ "sending: " ++ show result
            ctx.updateCacheUsingFingerprint result check
            logTrace "done"
        }
