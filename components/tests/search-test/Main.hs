module Main
    ( main
    ) where

import BV.ConcreteSyntax
import BV.Core.Prelude
import BV.Core.Types
import BV.Logging
import BV.Search.Core
import BV.SMTLIB2.Monad
import BV.System.Core
import BV.System.Search.Core
import BV.System.Utils
import BV.System.Utils.UnliftIO.Async
import BV.Test.Utils

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as TL
import Optics
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = bvMain $ \opts -> testGroup "Tests"
    [ testCase "inlining" $ testInlining opts
    , testTreeWhen opts.includeWip $ testCase "stack-bounds" $ testStackBounds opts
    ]

testInlining :: CustomOpts -> IO ()
testInlining opts = withLoggingOpts (loggingOpts opts "inlining.log") $ do
    stagesInput <- liftIO $ seL4DefaultReadStagesInput opts.defaultTargetDirForSlowTests
    let allInput = prepareAllDiscoverInlineScriptInput $ DiscoverAllInlineScriptsInput
            { programs = stagesInput.programs
            , objDumpInfo = stagesInput.objDumpInfo
            , rodata = stagesInput.rodata
            , earlyAsmFunctionFilter = stagesInput.earlyAsmFunctionFilter
            , asmFunctions = S.fromList $ map getAsm $ S.toList $ M.keysSet stagesInput.inlineScripts.unwrap
            , cFunctionPrefix = stagesInput.cFunctionPrefix
            }
    gate <- liftIO $ newSemGate =<< numThreads
    let f input = makeConcurrentlyUnliftIO $ applySemGate gate 1 $ do
            r <- discoverInlineScript' solverConfig input
            case r of
                Right script -> return script
                Left failure -> liftIO $ assertFailure $ show failure
    scripts <- runConcurrentlyUnliftIO $ InlineScripts <$> traverse f allInput
    let reference = stagesInput.inlineScripts & #unwrap %~ flip M.restrictKeys (M.keysSet scripts.unwrap)
    unless (scripts == reference) $ liftIO $ do
        let mismatchDumpDir = mismatchOutDirOf opts </> "inline-scripts.json"
        ensureDir mismatchDumpDir
        withFile (mismatchDumpDir </> "actual.json") WriteMode $ \h -> do
            CL.hPutStr h $ writeBVContents scripts
        withFile (mismatchDumpDir </> "expected.json") WriteMode $ \h -> do
            CL.hPutStr h $ writeBVContents reference
        assertBool "eq" False
    return ()

testStackBounds :: CustomOpts -> IO ()
testStackBounds opts = withLoggingOpts (loggingOpts opts "stack-bounds.log") $ do
    stagesInput <- liftIO $ seL4DefaultReadStagesInput opts.defaultTargetDirForSlowTests
    let preparedInput = prepareDiscoverStackBoundsInput $ DiscoverAllStacFullDiscoverStackBoundsInputkBoundsInput
            { program = getAsm stagesInput.programs
            , rodata = stagesInput.rodata
            , earlyAsmFunctionFilter = stagesInput.earlyAsmFunctionFilter
            , include = M.keysSet stagesInput.stackBounds.unwrap
            }
    _gate <- liftIO $ newSemGate =<< numThreads
    let f :: DiscoverStackBoundsInput -> LoggingWithContextT IO StackBounds
        f _input = undefined
    bounds <- f preparedInput
    let reference = stagesInput.stackBounds & #unwrap %~ flip M.restrictKeys (M.keysSet bounds.unwrap)
    unless (bounds == reference) $ liftIO $ do
        let mismatchDumpDir = mismatchOutDirOf opts </> "inline-scripts.json"
        ensureDir mismatchDumpDir
        withFile (mismatchDumpDir </> "actual.json") WriteMode $ \h -> do
            TL.hPutStr h $ writeBVContents bounds
        withFile (mismatchDumpDir </> "expected.json") WriteMode $ \h -> do
            TL.hPutStr h $ writeBVContents reference
        assertBool "eq" False
    return ()

--

numThreads :: IO Integer
numThreads =
    return
        1
        -- 6
        -- 16

loggingOpts :: CustomOpts -> FilePath -> LoggingOpts
loggingOpts opts fname = LoggingOpts
    { stderrLogOpts = LogOpts
        { level =
            LevelInfo
            -- LevelDebug
        , format = LogFormatHuman
        }
    , fileLogOpts = Just $ FileLogOpts
        { dst = logOutDirOf opts </> fname
        , logOpts = LogOpts
            { level =
                LevelDebug
                -- levelTrace
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
