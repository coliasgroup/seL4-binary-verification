{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BV.System.Core.RunSolvers.ConclusionT
    (
    ) where

import Control.Monad (filterM, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, liftEither,
                             mapExceptT, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import GHC.Generics (Generic)
import Optics

-- type ConclusionT c m a = ExceptT c m a

-- runConclusionT :: Monad m => ConclusionT c m () -> m (Maybe c)
-- runConclusionT m = preview _Left <$> runExceptT m

-- throwConclusion :: Monad m => c -> ConclusionT c m ()
-- throwConclusion = throwError

-- type ConcurrentlyUnliftIOC m c = ConcurrentlyUnliftIOE m c ()

-- makeConcurrentlyUnliftIOC :: MonadUnliftIO m => ConclusionT c m () -> ConcurrentlyUnliftIOC m c
-- makeConcurrentlyUnliftIOC m = makeConcurrentlyUnliftIOE (runExceptT m)

-- runConcurrentlyUnliftIOC :: MonadUnliftIO m => ConcurrentlyUnliftIOC m c -> ConclusionT c m ()
-- runConcurrentlyUnliftIOC m = ExceptT $ runConcurrentlyUnliftIOE m

-- concurrentlyUnliftIOC_ :: MonadUnliftIO m => ConclusionT c m () -> ConclusionT c m () -> ConclusionT c m ()
-- concurrentlyUnliftIOC_ left right = runConcurrentlyUnliftIOC $ makeConcurrentlyUnliftIOC left *> makeConcurrentlyUnliftIOC right
