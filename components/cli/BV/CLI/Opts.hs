{-# LANGUAGE ApplicativeDo #-}

module BV.CLI.Opts
    ( CheckOpts (..)
    , CommandOpts (..)
    , ExtractSMTDirection (..)
    , ExtractSMTOpts (..)
    , FileLogOpts (..)
    , FingerprintPattern
    , GlobalOpts (..)
    , LogFormat (..)
    , LogOpts (..)
    , LoggingOpts (..)
    , Opts (..)
    , parseOpts
    ) where

import BV.Core.Types
import BV.Logging
import BV.SMTLIB2

import Options.Applicative

import GHC.Generics (Generic)
import Text.Printf (printf)

--

data Opts
  = Opts
      { justDumpOptions :: Bool
      , globalOpts :: GlobalOpts
      , commandOpts :: CommandOpts
      }
  deriving (Generic, Show)

data GlobalOpts
  = GlobalOpts
      { loggingOpts :: LoggingOpts
      , numCores :: Maybe Int
      }
  deriving (Generic, Show)

data LoggingOpts
  = LoggingOpts
      { stderrLogOpts :: LogOpts
      , fileLogOpts :: Maybe FileLogOpts
      }
  deriving (Generic, Show)

data FileLogOpts
  = FileLogOpts
      { dst :: FilePath
      , logOpts :: LogOpts
      }
  deriving (Generic, Show)

data LogOpts
  = LogOpts
      { level :: LogLevel
      , format :: LogFormat
      }
  deriving (Generic, Show)

data LogFormat
  = LogFormatText
  | LogFormatHuman
  | LogFormatJSON
  deriving (Generic, Show)

data CommandOpts
  = CommandOptsCheck CheckOpts
  | CommandOptsExtractSMT ExtractSMTOpts
  deriving (Generic, Show)

data CheckOpts
  = CheckOpts
      { maxNumConcurrentSolvers :: Maybe Int
      , solvers :: FilePath
      , onlineSolverTimeout :: SolverTimeout
      , offlineSolverTimeout :: SolverTimeout
      , inputTargetDir :: FilePath
      , dumpTargetDir :: Maybe FilePath
      , mismatchDir :: Maybe FilePath
      , includeFunctions :: [Ident]
      , includeGroups :: [FingerprintPattern]
      , includeChecks :: [FingerprintPattern]
      }
  deriving (Generic, Show)

type FingerprintPattern = String

data ExtractSMTOpts
  = ExtractSMTOpts
      { match :: String
      , direction :: ExtractSMTDirection
      }
  deriving (Generic, Show)

data ExtractSMTDirection
  = Send
  | Recv
  deriving (Generic, Show)

--

parseOpts :: IO Opts
parseOpts = customExecParser
    (prefs (subparserInline <> helpShowGlobals))
    optsParserInfo

--

commonInfo :: InfoMod a
commonInfo = header "sel4-bv: seL4 Binary Verification"

optsParserInfo :: ParserInfo Opts
optsParserInfo =
    info
        (optsParser <**> helper)
        (commonInfo <> fullDesc)

optsParser :: Parser Opts
optsParser = do
    justDumpOptions <- switch (internal <> long "just-dump-options")
    globalOpts <- globalOptsParser
    commandOpts <- commandOptsParser
    pure $ Opts
        { justDumpOptions
        , globalOpts
        , commandOpts
        }

globalOptsParser :: Parser GlobalOpts
globalOptsParser = do
    loggingOpts <- loggingOptsParser
    numCores <- optional $ option' auto
        [ long "cores"
        , short 'N'
        , metavar "NUM_CORES"
        , help "Number of cores to use"
        ]
    pure $ GlobalOpts
        { loggingOpts
        , numCores
        }

loggingOptsParser :: Parser LoggingOpts
loggingOptsParser = do
    stderrLogOpts <- logOptsParser "stderr" ""
    fileLogOpts <- optional $ do
            dst <- option' str
                [ long "file-log"
                , metavar "FILE"
                , help "Destination file for file log"
                , action "file"
                ]
            logOpts <- logOptsParser "file" "file-"
            return $ FileLogOpts { dst, logOpts }
    return $ LoggingOpts { stderrLogOpts, fileLogOpts }

logOptsParser :: String -> String -> Parser LogOpts
logOptsParser logName prefix = do
    level <- option' logLevelReader
        [ long (prefix ++ "log-level")
        , metavar "LEVEL"
        , value defaultLogLevel
        , help (printf "Log level for %s log" logName)
        , completeWith logLevelValues
        ]
    format <- option' logFormatReader
        [ long "file-log-format"
        , metavar "FORMAT"
        , value defaultLogFormat
        , help "Log format for file log"
        , completeWith logFormatValues
        ]
    return $ LogOpts { level, format }

logLevelReader :: ReadM LogLevel
logLevelReader = maybeReader $ \case
    "error" -> Just LevelError
    "warn" -> Just LevelWarn
    "info" -> Just LevelInfo
    "debug" -> Just LevelDebug
    "trace" -> Just levelTrace
    _ -> Nothing

logLevelValues :: [String]
logLevelValues =
    [ "error"
    , "warn"
    , "info"
    , "debug"
    , "trace"
    ]

defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

logFormatReader :: ReadM LogFormat
logFormatReader = maybeReader $ \case
    "text" -> Just LogFormatText
    "human" -> Just LogFormatHuman
    "json" -> Just LogFormatJSON
    _ -> Nothing

logFormatValues :: [String]
logFormatValues =
    [ "text"
    , "human"
    , "json"
    ]

defaultLogFormat :: LogFormat
defaultLogFormat = LogFormatText

commandOptsParser :: Parser CommandOpts
commandOptsParser =
    subparser (mconcat
        [ command "check"
            (info (CommandOptsCheck <$> checkOptsParser)
                (commonInfo <> progDesc "Check proof scripts"))
        , command "extract-smt"
            (info (CommandOptsExtractSMT <$> extractSMTOptsParser)
                (commonInfo <> progDesc "Extract SMT from a 'check' log"))
        ])

checkOptsParser :: Parser CheckOpts
checkOptsParser = do
    solvers <- option' str
        [ long "solvers"
        , short 's'
        , metavar "FILE"
        , help "Solvers config file"
        , action "file"
        ]
    onlineSolverTimeout <- option' (solverTimeoutFromSeconds <$> auto)
        [ long "online-solver-timeout"
        , metavar "SECONDS"
        , value defaultOnlineSolverTimeout
        , help "Timeout for online solvers"
        ]
    offlineSolverTimeout <- option' (solverTimeoutFromSeconds <$> auto)
        [ long "offline-solver-timeout"
        , metavar "SECONDS"
        , value defaultOfflineSolverTimeout
        , help "Timeout for offline solvers"
        ]
    inputTargetDir <- option' str
        [ long "target-dir"
        , short 'd'
        , metavar "DIRECTORY"
        , help "Input target directory"
        , action "directory"
        ]
    includeFunctions <- many $ option' (Ident <$> str)
        [ long "include-function"
        , metavar "SYMBOL"
        ]
    includeGroups <- many $ option' str
        [ long "include-group"
        , metavar "FINGERPRINT"
        ]
    includeChecks <- many $ option' str
        [ long "include-check"
        , metavar "FINGERPRINT"
        ]
    maxNumConcurrentSolvers <- optional $ option' auto
        [ long "jobs"
        , short 'j'
        , metavar "NUM_JOBS"
        , help "Maximun number of concurrent solvers"
        ]
    dumpTargetDir <- optional $ option' str
        [ long "dump-target-dir"
        , metavar "DIRECTORY"
        , action "directory"
        ]
    mismatchDir <- optional $ option' str
        [ long "mismatch-dir"
        , metavar "DIRECTORY"
        , action "directory"
        ]
    return $ CheckOpts
        { maxNumConcurrentSolvers
        , solvers
        , onlineSolverTimeout
        , offlineSolverTimeout
        , inputTargetDir
        , dumpTargetDir
        , mismatchDir
        , includeFunctions
        , includeGroups
        , includeChecks
        }

defaultOnlineSolverTimeout :: SolverTimeout
defaultOnlineSolverTimeout = solverTimeoutFromSeconds 30

defaultOfflineSolverTimeout :: SolverTimeout
defaultOfflineSolverTimeout = solverTimeoutFromSeconds 6000

extractSMTOptsParser :: Parser ExtractSMTOpts
extractSMTOptsParser = do
    match <- argument' str
        [ metavar "MATCH"
        ]
    direction <-
            flag' Recv (long "recv" <> short 'r')
        <|> flag' Send (long "send" <> short 's')
        <|> pure Send
    return $ ExtractSMTOpts { match, direction }

--

option' :: ReadM a -> [Mod OptionFields a] -> Parser a
option' r = option r . mconcat

argument' :: ReadM a -> [Mod ArgumentFields a] -> Parser a
argument' r = argument r . mconcat
