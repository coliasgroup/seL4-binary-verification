module BV.Test.Utils where

import Data.Maybe (fromJust)
import System.FilePath
import qualified Text.Show.Pretty as H

import BV.Parsing
import BV.Printing
import BV.SeL4
import BV.TargetDir

tmpDir :: FilePath
tmpDir = "tmp"

parentTmpDir :: FilePath
parentTmpDir = "../tmp"

testSeL4TargetDir' :: FilePath
testSeL4TargetDir' = parentTmpDir </> "target"

testSeL4TargetDir :: TargetDir
testSeL4TargetDir = defaulttestSeL4TargetDir testSeL4TargetDir'

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

newtype InBlockAsFile a
  = InBlockAsFile { unwrap :: a }

instance ParseInBlock a => ParseFile (InBlockAsFile a) where
    parseFile = InBlockAsFile <$> parseInBlock

newtype InLineAsInBlock a
  = InLineAsInBlock { unwrap :: a }

instance ParseInLine a => ParseInBlock (InLineAsInBlock a) where
    parseInBlock = InLineAsInBlock <$> line parseInLine
