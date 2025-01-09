module BV.System.Test.Utils where

import Data.Maybe (fromJust)
import System.FilePath
import qualified Text.Show.Pretty as H

import BV.System.SeL4
import BV.System.TargetDir

tmpDir :: FilePath
tmpDir = "tmp"

parentTmpDir :: FilePath
parentTmpDir = "../tmp"

testSeL4TargetDirDefault :: TargetDir
testSeL4TargetDirDefault = testSeL4TargetDirBig

testSeL4TargetDirBig :: TargetDir
testSeL4TargetDirBig = defaultTestSeL4TargetDir testSeL4TargetDirBig'

testSeL4TargetDirSmall :: TargetDir
testSeL4TargetDirSmall = defaultTestSeL4TargetDir testSeL4TargetDirSmall'

testSeL4TargetDirBig' :: FilePath
testSeL4TargetDirBig' = tmpDir </> "target-big"

testSeL4TargetDirSmall' :: FilePath
testSeL4TargetDirSmall' = tmpDir </> "target-small"

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

graphRefineDir :: FilePath
graphRefineDir = "../../graph-refine"
