{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import BV.Logging

import qualified Control.Monad.Logger as L

import Control.Applicative (many)
import Data.Attoparsec.ByteString.Lazy
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "trivial" $ return ()
    , testCase "json" $ testLoggingWith formatLogEntryJSON parseLogEntryJSON dummyWithLoc
    , testCase "text" $ testLoggingWith formatLogEntryText parseLogEntryText dummy
    , testCase "human" $ testLoggingWith formatLogEntryHuman parseLogEntryHumanBestEffort dummy
    ]

testLoggingWith
    :: (LogEntry -> Builder)
    -> Parser LogEntry
    -> (forall m. MonadLoggerWithContext m => m ())
    -> IO ()
testLoggingWith ser de m = do
    entriesRef <- newIORef []
    runLoggingWithContextT m $ \entry -> modifyIORef entriesRef (++ [entry])
    entries <- readIORef entriesRef
    let serEntries = toLazyByteString $ foldMap ser entries
    BL.putStr serEntries
    let deEntries = either error id $ parseOnly (many de <* endOfInput) serEntries
    assertEqual "" entries deEntries

dummyWithLoc :: MonadLoggerWithContext m => m ()
dummyWithLoc = do
    withPushLogContext "this" $ do
        logDebug "foo 1"
        withPushLogContext "that" $ do
            $(L.logWarn) "foo 2"
            withCleanLogContext $ do
                logTrace "foo 3"

dummy :: MonadLoggerWithContext m => m ()
dummy = do
    withPushLogContext "this" $ do
        logDebug "foo 1"
        withPushLogContext "that" $ do
            logWarn "foo 2"
            withCleanLogContext $ do
                logTrace "foo 3"
