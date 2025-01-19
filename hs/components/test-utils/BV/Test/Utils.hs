module BV.Test.Utils where

import Data.Maybe (fromJust)
import System.FilePath
import qualified Text.Show.Pretty as H

import BV.TargetDir

tmpDir :: FilePath
tmpDir = "tmp"

testTargetDir :: FilePath -> FilePath
testTargetDir name = tmpDir </> "test-target-dirs" </> name

graphRefineDir :: FilePath
graphRefineDir = "../../graph-refine"

testSeL4TargetDirDefault :: TargetDir
testSeL4TargetDirDefault = testSeL4TargetDirBig

testSeL4TargetDirBig :: TargetDir
testSeL4TargetDirBig = TargetDir $ testTargetDir "big"

testSeL4TargetDirSmall :: TargetDir
testSeL4TargetDirSmall = TargetDir $ testTargetDir "small"

testSeL4TargetDirSmallTrace :: TargetDir
testSeL4TargetDirSmallTrace = TargetDir $ testTargetDir "small-trace"

testSeL4TargetDirFocused :: TargetDir
testSeL4TargetDirFocused = TargetDir $ testTargetDir "focused"

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
