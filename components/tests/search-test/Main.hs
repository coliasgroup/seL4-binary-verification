{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main
    ( main
    ) where

import BV.ConcreteSyntax
import BV.Core.Prelude
import BV.Core.Types
import BV.Logging
import BV.Search
import BV.SMTLIB2.Monad
import BV.SMTLIB2.Process
import BV.System.Core
import BV.System.Core.Search
import BV.System.Utils
import BV.System.Utils.UnliftIO.Async
import BV.Test.Utils
import BV.Test.Utils.Logging

import Control.Concurrent (newMVar)
import Control.Concurrent.Async (Concurrently)
import Control.Concurrent.MVar (withMVar)
import Control.Exception.Safe
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Resource
import Data.ByteString.Builder (Builder, hPutBuilder)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering,
                  openFile, stderr)
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
testInlining = do
    stagesInput <- seL4DefaultReadStagesInput referenceTargetDir
    let allInput = prepareAllDiscoverInlineScriptInput $ DiscoverAllInlineScriptsInput
            { programs = stagesInput.programs
            , objDumpInfo = stagesInput.objDumpInfo
            , rodata = stagesInput.rodata
            , earlyAsmFunctionFilter = stagesInput.earlyAsmFunctionFilter
            , asmFunctions = S.fromList [Ident "handleVMFault"]
            , cFunctionPrefix = stagesInput.cFunctionPrefix
            }
    let f pairingId input = do
            withPushLogContext ("pairing " ++ (getAsm pairingId).unwrap) $ do
                r <- discoverInlineScript' solverConfig input
                case r of
                    Left failure -> liftIO $ assertFailure $ show failure
                    Right script -> return script
    script <- withLoggingOpts (loggingOpts "inlining.log") $ do
        InlineScripts <$> M.traverseWithKey f allInput
    putStrLn ""
    putStrLn $ CL.unpack $ writeBVContents script
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
