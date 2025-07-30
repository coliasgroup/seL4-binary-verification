{-# LANGUAGE OverloadedLists #-}

module BV.Test.Utils where

import BV.Core
import BV.TargetDir

import Data.Maybe (fromJust)
import qualified Data.Set as S
import System.FilePath ((<.>), (</>))
import qualified Text.Show.Pretty as H

tmpDir :: FilePath
tmpDir = "tmp"

testTargetDir :: FilePath -> FilePath
testTargetDir name = tmpDir </> "test-target-dirs" </> name

graphRefineDir :: FilePath
graphRefineDir = "../graph-refine"

testSeL4TargetDirDefault :: TargetDir
testSeL4TargetDirDefault = testSeL4TargetDirBig

testSeL4TargetDirBig :: TargetDir
testSeL4TargetDirBig = TargetDir $ testTargetDir "big"

testSeL4TargetDirSmall :: TargetDir
testSeL4TargetDirSmall = TargetDir $ testTargetDir "small"

testSeL4TargetDirSmallTrace :: TargetDir
testSeL4TargetDirSmallTrace = TargetDir $ testTargetDir "small-trace"

testSeL4TargetDirSmallTraceOfflineOnly :: TargetDir
testSeL4TargetDirSmallTraceOfflineOnly = TargetDir $ testTargetDir "small-trace-offline-only"

testSeL4TargetDirFocused :: TargetDir
testSeL4TargetDirFocused = TargetDir $ testTargetDir "focused"

testSeL4TargetDirFocusedTrace :: TargetDir
testSeL4TargetDirFocusedTrace = TargetDir $ testTargetDir "focused-trace"

testHtmlPagesDir :: FilePath
testHtmlPagesDir = tmpDir </> "html" </> "pages"

testHtmlOpts :: H.HtmlOpts
testHtmlOpts = H.defaultHtmlOpts
    { H.dataDir = ".."
    }

writeHtml :: Show a => FilePath -> a -> IO ()
writeHtml fname val = writeFile dst html
  where
    dst = testHtmlPagesDir </> fname <.> "html"
    html = H.valToHtmlPage testHtmlOpts (fromJust (H.reify val))

tmpOutDir :: FilePath
tmpOutDir = tmpDir </> "out"

tmpOutPath :: FilePath -> FilePath
tmpOutPath = (tmpOutDir </>)

seL4DefaultEarlyAsmFunctionFilter :: AsmFunctionFilter
seL4DefaultEarlyAsmFunctionFilter = IncludeExcludeFilter
    { include = Nothing
    , exclude = S.fromList
        [ Ident "c_handle_syscall"
        ]
    }

seL4DefaultCFunctionPrefix :: String
seL4DefaultCFunctionPrefix = "Kernel_C."

seL4DefaultRODataInputRanges :: RODataInputRanges
seL4DefaultRODataInputRanges =
    [ (RODataInputRangeTypeSection, ".rodata")
    , (RODataInputRangeTypeSymbol, "kernel_device_frames")
    , (RODataInputRangeTypeSymbol, "avail_p_regs")
    ]

seL4DefaultReadStagesInput :: TargetDir -> IO StagesInput
seL4DefaultReadStagesInput =
    readStagesInput seL4DefaultEarlyAsmFunctionFilter seL4DefaultCFunctionPrefix seL4DefaultRODataInputRanges
