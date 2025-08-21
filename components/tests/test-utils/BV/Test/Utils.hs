module BV.Test.Utils
    ( module BV.Test.Utils.Logging
    , module BV.Test.Utils.Tasty
    , bvMain
    , ensureDir
    , ensureParent
    , htmlOutDirOf
    , logOutDirOf
    , miscOutDirOf
    , mismatchOutDirOf
    , seL4DefaultCFunctionPrefix
    , seL4DefaultEarlyAsmFunctionFilter
    , seL4DefaultRODataInputRanges
    , seL4DefaultReadStagesInput
    , writeHtml
    ) where

import BV.Test.Utils.Logging
import BV.Test.Utils.Tasty

import BV.Core.Prelude
import BV.TargetDir

import Data.Maybe (fromJust)
import qualified Data.Set as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>), takeDirectory)
import Test.Tasty (TestTree, withResource)
import qualified Text.Show.Pretty as H

ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True

ensureParent :: FilePath -> IO ()
ensureParent = ensureDir . takeDirectory

--

logOutDirOf :: CustomOpts -> FilePath
logOutDirOf opts = opts.outDir </> "logs"

mismatchOutDirOf :: CustomOpts -> FilePath
mismatchOutDirOf opts = opts.outDir </> "mismatch"

htmlOutDirOf :: CustomOpts -> FilePath
htmlOutDirOf opts = opts.outDir </> "html"

miscOutDirOf :: CustomOpts -> FilePath
miscOutDirOf opts = opts.outDir </> "misc"

--

writeHtml :: Show a => CustomOpts -> FilePath -> a -> IO ()
writeHtml opts fname val = do
    let dst = htmlOutDirOf opts </> fname <.> "html"
    ensureParent dst
    dataDir <- H.getDataDir
    let htmlOpts = H.defaultHtmlOpts
            { H.dataDir = dataDir
            }
    let html = H.valToHtmlPage htmlOpts (fromJust (H.reify val))
    writeFile dst html

--

seL4DefaultEarlyAsmFunctionFilter :: FunctionFilter
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

--

bvMain :: (CustomOpts -> TestTree) -> IO ()
bvMain f = defaultMainWithOpts $ \opts -> do
    let setup = do
            ensureDir $ logOutDirOf opts
            ensureDir $ mismatchOutDirOf opts
    withResource setup (const (return ())) $ \_ -> f opts
