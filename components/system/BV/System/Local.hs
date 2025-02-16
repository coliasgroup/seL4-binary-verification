{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}

module BV.System.Local
    ( LocalConfig (..)
    , runLocal
    ) where

import BV.Logging
import BV.System.Core
import BV.System.Utils.Throttle
import BV.System.Utils.UnliftIO.Throttle

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
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
    withThrottlingUnliftIO (Units config.numJobs) $ \throttle -> do
        let withThrottleSimple units = withThrottleUnliftIO throttle 0 (Units units)
        frontend withThrottleSimple localSolverBackend config.solversConfig checks
