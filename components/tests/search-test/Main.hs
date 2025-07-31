{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import BV.ConcreteSyntax
import BV.Core.Prelude
import BV.Core.Types
import BV.Logging
import BV.Search.Core
import BV.SMTLIB2.Monad
import BV.SMTLIB2.Process
import BV.System.Core
import BV.System.Search.Core
import BV.System.Utils
import BV.System.Utils.UnliftIO.Async
import BV.Test.Utils
import BV.Test.Utils.Logging

import Control.Concurrent (newMVar)
import Control.Concurrent.Async (Concurrently)
import Control.Concurrent.MVar (withMVar)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Resource
import Data.ByteString.Builder (Builder, hPutBuilder)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering,
                  openFile, stderr, withFile)
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

loggingOpts :: FilePath -> LoggingOpts
loggingOpts fname = LoggingOpts
    { stderrLogOpts = LogOpts
        { level = LevelInfo
        , format = LogFormatHuman
        }
    , fileLogOpts = Just $ FileLogOpts
        { dst = tmpDir </> "logs" </> fname
        , logOpts = LogOpts
            { level = levelTrace
            , format = LogFormatText
            }
        }
    }

solverConfig :: OnlineSolverConfig
solverConfig = OnlineSolverConfig
    { command = SolverCommand
        { path = "yices-smt2"
        , args = ["--incremental"]
        }
    , modelConfig = ModelConfig
        { memoryMode = MemoryModeWord32
        }
    , timeout = solverTimeoutFromSeconds 30
    }

testInlining :: IO ()
testInlining = withLoggingOpts (loggingOpts "inlining.log") $ do
    stagesInput <- liftIO $ seL4DefaultReadStagesInput referenceTargetDir
    let allInput = prepareAllDiscoverInlineScriptInput $ DiscoverAllInlineScriptsInput
            { programs = stagesInput.programs
            , objDumpInfo = stagesInput.objDumpInfo
            , rodata = stagesInput.rodata
            , earlyAsmFunctionFilter = stagesInput.earlyAsmFunctionFilter
            , asmFunctions = S.fromList $ map getAsm $ S.toList $ M.keysSet stagesInput.inlineScripts.unwrap
            -- , asmFunctions = S.fromList [Ident "handleVMFault"]
            , cFunctionPrefix = stagesInput.cFunctionPrefix
            }
    let f input = do
            r <- discoverInlineScript' solverConfig input
            case r of
                Right script -> return script
                Left failure -> liftIO $ assertFailure $ show failure
    scripts <- InlineScripts <$> traverse f allInput
    let reference = stagesInput.inlineScripts & #unwrap %~ flip M.restrictKeys (M.keysSet scripts.unwrap)
    unless (scripts == reference) $ liftIO $ do
        withFile (tmpOutDir </> "out-inline-scripts.json") WriteMode $ \h -> do
            CL.hPutStr h $ writeBVContents scripts
        withFile (tmpOutDir </> "in-inline-scripts.json") WriteMode $ \h -> do
            CL.hPutStr h $ writeBVContents reference
        assertBool "eq" False
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
