module BV.Test.Utils.Tasty
    ( CustomOpts (..)
    , defaultMainWithOpts
    , testTreeWhen
    ) where

import BV.TargetDir
import BV.Test.Utils.Paths (defaultGraphRefineDir, defaultOutDir,
                            defaultTestTargetDir)

import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.Options

data CustomOpts
  = CustomOpts
      { outDir :: FilePath
      , includeExtra :: Bool
      , includeBroken :: Bool
      , includeWip :: Bool
      , defaultTargetDirForFastTests :: TargetDir
      , defaultTargetDirForSlowTests :: TargetDir
      , defaultTargetDirForTestsRequiringTrace :: TargetDir
      , graphRefineDir :: FilePath
      }

defaultMainWithOpts :: (CustomOpts -> TestTree) -> IO ()
defaultMainWithOpts f = defaultMainWithIngredients ingredients $
    askOption $ \(OutDir outDir) ->
    askOption $ \(IncludeExtra includeExtra) ->
    askOption $ \(IncludeBroken includeBroken) ->
    askOption $ \(IncludeWip includeWip) ->
    askOption $ \(DefaultTargetDirForFastTests defaultTargetDirForFastTests) ->
    askOption $ \(DefaultTargetDirForSlowTests defaultTargetDirForSlowTests) ->
    askOption $ \(DefaultTargetDirForTestsRequiringTrace defaultTargetDirForTestsRequiringTrace) ->
    askOption $ \(GraphRefineDir graphRefineDir) ->
    do
        let opts = CustomOpts
                { outDir
                , includeExtra
                , includeBroken
                , includeWip
                , defaultTargetDirForFastTests
                , defaultTargetDirForSlowTests
                , defaultTargetDirForTestsRequiringTrace
                , graphRefineDir
                }
        f opts
  where
    ingredients =
        includingOptions
            [ Option (Proxy :: Proxy OutDir)
            , Option (Proxy :: Proxy IncludeExtra)
            , Option (Proxy :: Proxy IncludeBroken)
            , Option (Proxy :: Proxy IncludeWip)
            , Option (Proxy :: Proxy DefaultTargetDirForFastTests)
            , Option (Proxy :: Proxy DefaultTargetDirForSlowTests)
            , Option (Proxy :: Proxy GraphRefineDir)
            ]
                : defaultIngredients

newtype OutDir
  = OutDir FilePath
  deriving (Eq, Ord, Typeable)

instance IsOption OutDir where
  defaultValue = OutDir defaultOutDir
  parseValue = pure . OutDir
  optionName = return "out-dir"
  optionHelp = return "Out dir"

newtype IncludeExtra
  = IncludeExtra Bool
  deriving (Eq, Ord, Typeable)

instance IsOption IncludeExtra where
  defaultValue = IncludeExtra False
  parseValue = fmap IncludeExtra . safeReadBool
  optionName = return "include-extra"
  optionHelp = return "Whether to include extra tests"
  optionCLParser = flagCLParser Nothing (IncludeExtra True)

newtype IncludeBroken
  = IncludeBroken Bool
  deriving (Eq, Ord, Typeable)

instance IsOption IncludeBroken where
  defaultValue = IncludeBroken False
  parseValue = fmap IncludeBroken . safeReadBool
  optionName = return "include-broken"
  optionHelp = return "Whether to include broken tests"
  optionCLParser = flagCLParser Nothing (IncludeBroken True)

newtype IncludeWip
  = IncludeWip Bool
  deriving (Eq, Ord, Typeable)

instance IsOption IncludeWip where
  defaultValue = IncludeWip False
  parseValue = fmap IncludeWip . safeReadBool
  optionName = return "include-wip"
  optionHelp = return "Whether to include WIP tests"
  optionCLParser = flagCLParser Nothing (IncludeWip True)

newtype DefaultTargetDirForFastTests
  = DefaultTargetDirForFastTests TargetDir
  deriving (Eq, Ord, Typeable)

instance IsOption DefaultTargetDirForFastTests where
  defaultValue = DefaultTargetDirForFastTests $ TargetDir $ defaultTestTargetDir "big"
  parseValue = pure . DefaultTargetDirForFastTests . TargetDir
  optionName = return "for-fast"
  optionHelp = return "Default target dir for fast tests"

newtype DefaultTargetDirForSlowTests
  = DefaultTargetDirForSlowTests TargetDir
  deriving (Eq, Ord, Typeable)

instance IsOption DefaultTargetDirForSlowTests where
  defaultValue = DefaultTargetDirForSlowTests $ TargetDir $ defaultTestTargetDir "small"
  parseValue = pure . DefaultTargetDirForSlowTests . TargetDir
  optionName = return "for-slow"
  optionHelp = return "Default target dir for slow tests"

newtype DefaultTargetDirForTestsRequiringTrace
  = DefaultTargetDirForTestsRequiringTrace TargetDir
  deriving (Eq, Ord, Typeable)

instance IsOption DefaultTargetDirForTestsRequiringTrace where
  defaultValue = DefaultTargetDirForTestsRequiringTrace $ TargetDir $ defaultTestTargetDir "small-trace"
  parseValue = pure . DefaultTargetDirForTestsRequiringTrace . TargetDir
  optionName = return "for-trace"
  optionHelp = return "Default target dir for tests requiring trace"

newtype GraphRefineDir
  = GraphRefineDir FilePath
  deriving (Eq, Ord, Typeable)

instance IsOption GraphRefineDir where
  defaultValue = GraphRefineDir defaultGraphRefineDir
  parseValue = pure . GraphRefineDir
  optionName = return "graph-refine-dir"
  optionHelp = return "Graph refine"

testTreeWhen :: Bool -> TestTree -> TestTree
testTreeWhen cond tree = if cond then tree else testGroup "skipped" []
