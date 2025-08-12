module BV.Test.Utils.Paths
    ( defaultGraphRefineDir
    , defaultOutDir
    , defaultTestTargetDir
    ) where

import System.FilePath ((</>))

defaultOutDir :: FilePath
defaultOutDir = "tmp" </> "test-out"

defaultTestTargetDir :: FilePath -> FilePath
defaultTestTargetDir name = "tmp" </> "test-target-dirs" </> name

defaultGraphRefineDir :: FilePath
defaultGraphRefineDir = "../graph-refine"
