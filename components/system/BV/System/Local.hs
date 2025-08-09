{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.Local
    ( LocalConfig (..)
    , runLocal
    ) where

import BV.Logging
import BV.System.Core
import BV.System.Utils

import Control.Exception.Safe (MonadMask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import GHC.Generics (Generic)

data LocalConfig
  = LocalConfig
      { numEvalCores :: Integer
      , numSolverCores :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

runLocal
    :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadCache m, MonadMask m)
    => LocalConfig -> SolversConfig -> Checks -> m Report
runLocal config solversConfig checks = do
    gate <- liftIO $ newSemGate config.numSolverCores
    frontend config.numEvalCores (applySemGate gate) localSolverBackend solversConfig checks
