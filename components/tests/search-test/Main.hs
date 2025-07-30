{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import BV.Core
import BV.Logging (MonadLoggerWithContext)
import BV.SMTLIB2.Process
import BV.System.Core
-- import BV.System.Core.Utils.Logging
import BV.System.Utils
import BV.System.Utils.UnliftIO.Async
import BV.Test.Utils

import Control.Concurrent.Async (Concurrently)
import Control.Exception.Safe
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import System.Process (CreateProcess)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testCase "inlining" testInlining
    , testCase "stack-bounds" testStackBounds
    ]

testInlining :: IO ()
testInlining = do
    _input <- seL4DefaultReadStagesInput referenceTargetDir
    return ()
  where
    referenceTargetDir =
        -- testSeL4TargetDirBig
        testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused

testStackBounds :: IO ()
testStackBounds = do
    _input <- seL4DefaultReadStagesInput referenceTargetDir
    return ()
  where
    referenceTargetDir =
        -- testSeL4TargetDirBig
        testSeL4TargetDirSmall
        -- testSeL4TargetDirFocused

-- runSolverInteractionConcurrently
--     :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
--     => SolverGate m -> ModelConfig -> CreateProcess -> (ModelConfig -> SolverT m a) -> ConcurrentlyUnliftIO m a
-- runSolverInteractionConcurrently gate modelConfig proc interaction =
--     makeConcurrentlyUnliftIO $ gate 1 $ runSolverInteraction modelConfig proc interaction

-- runSolverInteraction
--     :: (MonadUnliftIO m, MonadLoggerWithContext m, MonadMask m)
--     => ModelConfig -> CreateProcess -> (ModelConfig -> SolverT m a) -> m a
-- runSolverInteraction modelConfig proc interaction =
--     runSolverWithLogging proc (interaction modelConfig)
