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

testHtmlDir :: FilePath
testHtmlDir = tmpDir </> "html"

writeHtml :: Show a => FilePath -> a -> IO ()
writeHtml fname val = do
    let dst = testHtmlDir </> fname <.> "html"
    dataDir <- H.getDataDir
    let opts = H.defaultHtmlOpts
            { H.dataDir = dataDir
            }
    let html = H.valToHtmlPage opts (fromJust (H.reify val))
    writeFile dst html
