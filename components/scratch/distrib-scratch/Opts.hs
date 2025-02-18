{-# LANGUAGE ApplicativeDo #-}

module Opts
    ( DriverOpts (..)
    , Opts (..)
    , WorkerOpts (..)
    , parseOpts
    ) where

import Options.Applicative

import GHC.Generics (Generic)

data Opts
  = CommandDriver DriverOpts
  | CommandWorker WorkerOpts
  deriving (Generic, Show)

data DriverOpts
  = DriverOpts
  deriving (Generic, Show)

data WorkerOpts
  = WorkerOpts
  deriving (Generic, Show)

parseOpts :: IO Opts
parseOpts = customExecParser
    (prefs (subparserInline <> helpShowGlobals))
    optsParserInfo

optsParserInfo :: ParserInfo Opts
optsParserInfo =
    info (optsParser <**> helper) mempty

optsParser :: Parser Opts
optsParser =
    subparser (mconcat
        [ command "driver"
            (info (CommandDriver <$> parseDriverOpts) mempty)
        , command "worker"
            (info (CommandWorker <$> parseWorkerrOpts) mempty)
        ])

parseDriverOpts :: Parser DriverOpts
parseDriverOpts = do
    pure DriverOpts

parseWorkerrOpts :: Parser WorkerOpts
parseWorkerrOpts = do
    pure WorkerOpts
