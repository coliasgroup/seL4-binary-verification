{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Local
    ( LocalConfig (..)
    , runLocal
    ) where

import BV.Logging
import BV.System.Core

import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception.Safe (MonadMask, bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIOOp)
import GHC.Generics (Generic)

data LocalConfig
  = LocalConfig
      { numJobs :: Integer
      , solversConfig :: SolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

runLocal
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => LocalConfig -> Checks -> m Report
runLocal config checks = do
    sem <- liftIO $ newQSemN (fromInteger config.numJobs)
    let throttle n =
            let n' = fromInteger n
             in liftIOOp $ bracket_ (waitQSemN sem n') (signalQSemN sem n')
    frontend throttle localSolverBackend config.solversConfig checks
