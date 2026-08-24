{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main
    ( main
    ) where

import BV.ConcreteSyntax
import BV.Core.Prelude
import BV.Core.Types
import BV.Core.Types.Extras.Aggregate
import BV.Core.Types.Extras.Program (signatureOfFunction)
import BV.Logging
import BV.Search.Core
import BV.Search.System.Core
import BV.SMTLIB2.Monad
import BV.System.Core
import BV.System.Core.Utils.Logging (withPushLogContextPairing)
import BV.System.Utils.SemGate
import BV.System.Utils.UnliftIO.Async
import BV.TargetDir (TargetDir (TargetDir))
import BV.Test.Utils
import BV.Test.Utils.Paths (defaultTestTargetDir)

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as TL
import Optics
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

main :: IO ()
main = bvMain $ \opts ->
    let ?opts = opts
     in testGroup "Tests"
            [ testCase "inlining" testInlining
            , testCase "stack-bounds" testStackBounds
            , testCase "proof-scripts" testProofScripts
            ]

testInlining :: (?opts :: CustomOpts) => IO ()
testInlining = withLoggingOpts (loggingOpts ?opts "inlining.log") $ do
    stagesInput <- liftIO $ seL4DefaultReadStagesInput $ TargetDir $ defaultTestTargetDir "coverage"
    let allInput = prepareAllDiscoverInlineScriptInput $ DiscoverAllInlineScriptsInput
            { programs = stagesInput.programs
            , objDumpInfo = stagesInput.objDumpInfo
            , rodata = stagesInput.rodata
            , cFunctionPrefix = stagesInput.cFunctionPrefix
            , earlyAsmFunctionFilter = stagesInput.earlyAsmFunctionFilter
            , asmFunctions = S.fromList $ map (.asm) $ S.toList $ M.keysSet stagesInput.inlineScripts.unwrap
            }
    gate <- liftIO $ newSemGate =<< numThreads
    let f input = makeConcurrentlyUnliftIO $ applySemGate gate 1 $ do
            discoverInlineScript' solverConfig input >>= assertSuccess
    scripts <- runConcurrentlyUnliftIO $ InlineScripts <$> traverse f allInput
    let reference = stagesInput.inlineScripts & #unwrap %~ flip M.restrictKeys (M.keysSet scripts.unwrap)
    -- let check = checkMatch
    let check = checkMatchWith $ \x y -> over _Left show $ inlineScriptMapsEquivalent x y
    check "inline-scripts" "json" scripts reference
    return ()

testStackBounds :: (?opts :: CustomOpts) => IO ()
testStackBounds = withLoggingOpts (loggingOpts ?opts "stack-bounds.log") $ do
    stagesInput <- liftIO $ seL4DefaultReadStagesInput $ TargetDir $ defaultTestTargetDir "coverage"
    let preparedInput = prepareDiscoverStackBoundsInput $ DiscoverAllStackFullDiscoverStackBoundsInputkBoundsInput
            { programs = stagesInput.programs
            , objDumpInfo = stagesInput.objDumpInfo
            , rodata = stagesInput.rodata
            , cFunctionPrefix = stagesInput.cFunctionPrefix
            , earlyAsmFunctionFilter = stagesInput.earlyAsmFunctionFilter
            , includeAsmFrom = M.keysSet stagesInput.stackBounds.unwrap
            -- , includeAsmFrom = S.fromList [Ident "handleVMFault"]
            }
    gate <- liftIO $ newSemGate =<< numThreads
    let f :: DiscoverStackBoundsInput -> LoggingWithContextT IO StackBounds
        f input = discoverStackBounds' (applySemGate gate 1) solverConfig input >>= assertSuccess
    bounds <- f preparedInput
    let reference = stagesInput.stackBounds & #unwrap %~ flip M.restrictKeys (M.keysSet bounds.unwrap)
    checkMatch "StackBounds" "txt" bounds reference
    return ()

testProofScripts :: (?opts :: CustomOpts) => IO ()
testProofScripts = withLoggingOpts (loggingOpts ?opts "proof-scripts.log") $ do
    let targetsArg = map Ident ?opts.proofScriptSearchTargets
    let targetsExcludeArg = map Ident ?opts.proofScriptSearchTargetsExclude
    stagesInput <- liftIO $ seL4DefaultReadStagesInput ?opts.defaultTargetDirForSlowTests
    let defaultTargets = map (viewAtTag Asm) $ M.keys stagesInput.proofScripts.unwrap
    let targets = filter (`notElem` targetsExcludeArg) $ case targetsArg of
            [] -> defaultTargets
            xs -> xs
    let stagesOutput = stages stagesInput
    let lookupFunction (WithTag _ funName) = stagesOutput.intermediate.functions.functions M.! funName
    let lookupSig = signatureOfFunction . lookupFunction
    solverGate <- liftIO $ newSemGate =<< numThreads
    targetGate <- liftIO $ newSemGate =<< numThreads
    forConcurrentlyUnliftIO_ targets $ \target -> applySemGate targetGate 1 $ do
        let [pairingId] =
                [ pid
                | pid <- M.keys stagesOutput.intermediate.pairings.unwrap
                , pid.asm == target
                ]
        withPushLogContextPairing pairingId $ do
            let input = DiscoverProofScriptInput
                    { graphSliceInput = stagesOutput.intermediate.graphSliceInput M.! pairingId
                    , pairings = stagesOutput.intermediate.pairings
                    , lookupSig
                    }
            let expected = stagesInput.proofScripts.unwrap M.! pairingId
            actual <- discoverProofScript' (applySemGate solverGate) solversConfig input
                >>= assertSuccess
            let actualTxt = writeBVContents actual
            logInfo $ printf "found proof script: %s" (TL.unpack actualTxt)
            checkMatch "proof-script" "txt" actual expected
    return ()

--

assertSuccess :: (MonadIO m, Show e) => Either e a -> m a
assertSuccess = either (liftIO . assertFailure . show) return

checkMatchWith :: (?opts :: CustomOpts, MonadIO m, MonadLogger m, WriteBVFile c a) => (a -> a -> Either String ()) -> String -> String -> a -> a -> m ()
checkMatchWith cmp fname ext actual expected = do
    case cmp actual expected of
        Right () -> return ()
        Left msg -> do
            let mismatchDumpDir = mismatchOutDirOf ?opts </> fname
            liftIO $ ensureDir mismatchDumpDir
            liftIO $ writeBVFile (mismatchDumpDir </> "actual" <.> ext) actual
            liftIO $ writeBVFile (mismatchDumpDir </> "expected" <.> ext) expected
            logError $ "mismatch: " ++ msg
            liftIO $ assertFailure "mismatch"

checkMatch :: (?opts :: CustomOpts, MonadIO m, MonadLogger m, Eq a, WriteBVFile c a) => String -> String -> a -> a -> m ()
checkMatch = checkMatchWith $ \x y ->
    if x == y
    then Right ()
    else Left "not equal"

--

numThreads :: IO Integer
numThreads =
    return
        -- 1
        -- 6
        16

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

solversConfig :: SolversConfig
solversConfig = SolversConfig
    { online = Just solverConfig
    , offline = OfflineSolversConfig
        { groups =
            [ defaultGroup
                & #commandName .~ "yices"
                & #command .~ SolverCommand "yices-smt2" ["--incremental"] -- TODO "--mode=one-shot" is apparently slow with ":produce-models"
            , defaultGroup
                & #commandName .~ "bitwuzla"
                & #command .~ SolverCommand "bitwuzla" []
            ]
        , timeout = solverTimeoutFromSeconds 6000
        }
    }
  where
    defaultGroup =
        OfflineSolverGroupConfig
            { scopes = [SolverScopeAll, SolverScopeHyp]
            , modelConfigs =
                [ ModelConfig MemoryModeWord8
                , ModelConfig MemoryModeWord32
                ]
            }
