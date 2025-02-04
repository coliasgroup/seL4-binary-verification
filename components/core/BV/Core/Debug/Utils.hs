module BV.Core.Debug.Utils
    ( unsafePerformIO
    , writeHtml
    ) where

import Data.Maybe (fromJust)
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Show.Pretty as H

tmpDir :: FilePath
tmpDir = "tmp"

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
