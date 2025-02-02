{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.System.LocalCheckBackend
    ( AcceptableSatResult (..)
    , LocalCheckBackendConfig (..)
    , MonadLocalCheckCache (..)
    , localCheckBackend
    ) where

import BV.Core.AdornProofScript
import BV.Core.ExecuteSMTProofChecks (SolverConfig)
import BV.Core.Types
import BV.System.CheckFingerprint
import BV.System.CheckFrontend
import BV.System.LocalCheckBackend.Cache
import BV.System.SolversConfig
import BV.System.TaskQueue
import BV.System.Throttle

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently),
                                 race)
import Control.Monad (forever)
import Control.Monad.Catch (MonadMask, MonadThrow, SomeException)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN)
import Control.Monad.State (evalState, state)
import Control.Monad.Trans (lift)
import Data.Functor (void)
import Data.List (uncons)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Void (absurd)
import GHC.Generics (Generic)
import Text.Printf (printf)

data LocalCheckBackendConfig
  = LocalCheckBackendConfig
      { numCores :: Integer
      , solversConfig :: SolversConfig
      }
  deriving (Eq, Generic, Ord, Show)

localCheckBackend
    :: (MonadUnliftIO m, MonadLogger m, MonadLocalCheckCache m)
    => LocalCheckBackendConfig -> FlattenedSMTProofChecks SMTProofCheckDescription -> m CheckReport
localCheckBackend config checks = withRunInIO $ \runInIO -> do
    (taskQueueIn, taskQueueControl) <- liftIO newTaskQueue
    let completeTasks = do
            withThrottling (Units config.numCores) $ \throttle -> do
                forever $ do
                    task <- acceptTask taskQueueControl
                    let (group, withResult) = useTask task
                    result <- runInIO $ checkGroup config throttle group
                    withResult result
    either absurd id <$> race completeTasks (runInIO (checkFrontend taskQueueIn checks))

checkGroup
    :: (MonadUnliftIO m, MonadLogger m, MonadLocalCheckCache m)
    => LocalCheckBackendConfig -> Throttle -> SMTProofCheckGroup SMTProofCheckDescription -> m (SMTProofCheckResult ())
checkGroup config throttle group = runExceptT $ do
    logDebugN . T.pack $ printf "Checking group: %s" (smtProofCheckGroupFingerprint (void group))
    filteredGroup <- ExceptT $ filterGroupM group
    undefined

filterGroupM :: MonadLocalCheckCache m => SMTProofCheckGroup a -> m (SMTProofCheckResult (SMTProofCheckGroup a))
filterGroupM = undefined

zipWithT :: Traversable f => [a] -> f b -> f (a, b)
zipWithT as f = flip evalState as $ traverse m f
  where
    m b = do
        a <- state (fromJust . uncons)
        return (a, b)

online
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadThrow m
       , MonadMask m
       )
    => SolverConfig
    -> SMTProofCheckGroup a
    -> m (Either SomeException (), [a])
online config group = do
    undefined
