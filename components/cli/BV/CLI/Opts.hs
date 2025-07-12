{-# LANGUAGE ApplicativeDo #-}

module BV.CLI.Opts
    ( CheckOpts (..)
    , CommandOpts (..)
    , ExtractSMTDirection (..)
    , ExtractSMTOpts (..)
    , FileLogOpts (..)
    , FormatSMTOpts (..)
    , GlobalOpts (..)
    , LineWrappingOpts (..)
    , LogFormat (..)
    , LogOpts (..)
    , LoggingOpts (..)
    , NotWorkerCommandOpts (..)
    , NotWorkerGlobalOpts (..)
    , Opts (..)
    , WorkerOpts (..)
    , logEntryFormatterFor
    , logEntryParserFor
    , parseOpts
    ) where

import BV.Core.Types
import BV.Logging
import BV.SMTLIB2
import BV.SMTLIB2.SExpr.Build.Pretty
import BV.System.Core

import Options.Applicative

import qualified Data.Attoparsec.ByteString as A (Parser)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as C
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
  deriving (Generic, Show)

data NotWorkerGlobalOpts
  = NotWorkerGlobalOpts
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
  = LogFormatJSON
  | LogFormatText
  | LogFormatHuman
  deriving (Generic, Show)

logEntryFormatterFor :: LogFormat -> LogEntry -> Builder
logEntryFormatterFor = \case
    LogFormatJSON -> formatLogEntryJSON
    LogFormatText -> formatLogEntryText
    LogFormatHuman -> formatLogEntryHuman

logEntryParserFor :: LogFormat -> A.Parser LogEntry
logEntryParserFor = \case
    LogFormatJSON -> parseLogEntryJSON
    LogFormatText -> parseLogEntryText
    LogFormatHuman -> parseLogEntryHumanBestEffort

data CommandOpts
  = CommandOptsNotWorker NotWorkerGlobalOpts NotWorkerCommandOpts
  | CommandOptsWorker WorkerOpts
  deriving (Generic, Show)

data NotWorkerCommandOpts
  = CommandOptsCheck CheckOpts
  | CommandOptsExtractSMT ExtractSMTOpts
  | CommandOptsFormatSMT FormatSMTOpts
  deriving (Generic, Show)

data CheckOpts
  = CheckOpts
      { maxNumConcurrentSolvers :: Maybe Int
      , solvers :: FilePath
      , workers :: Maybe FilePath
      , onlineSolverTimeout :: SolverTimeout
      , offlineSolverTimeout :: SolverTimeout
      , sqliteCache :: Maybe String
      , postgresCache :: Maybe String
      , inputTargetDir :: FilePath
      , forceEvalStages :: Bool
      , referenceTargetDir :: Maybe FilePath
      , dumpTargetDir :: Maybe FilePath
      , mismatchDir :: Maybe FilePath
      , rodataSections :: [String]
      , rodataSymbols :: [String]
      , includeFunctions :: [Ident]
      , ignoreFunctions :: [Ident]
      , ignoreFunctionsEarly :: [Ident]
      , includeGroups :: [CheckGroupFingerprintPattern]
      , includeChecks :: [CheckFingerprintPattern]
      , reportFile :: Maybe FilePath
      , justCompareChecks :: Bool
      }
  deriving (Generic, Show)

data ExtractSMTOpts
  = ExtractSMTOpts
      { match :: String
      , direction :: ExtractSMTDirection
      , format :: LogFormat
      , lineWrappingOpts :: Maybe LineWrappingOpts
      }
  deriving (Generic, Show)

data ExtractSMTDirection
  = Send
  | Recv
  deriving (Generic, Show)

data FormatSMTOpts
  = FormatSMTOpts
      { lineWrappingOpts :: Maybe LineWrappingOpts
      }
  deriving (Generic, Show)

data LineWrappingOpts
  = LineWrappingOpts
      { preferredMaxLineWidth :: Integer
      , indentWidth :: Integer
      , minBreak :: Maybe Integer
      , color :: Bool
      }
  deriving (Generic, Show)

data WorkerOpts
  = WorkerOpts
      { addr :: String
      }
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
    pure GlobalOpts

notWorkerGlobalOptsParser :: Parser NotWorkerGlobalOpts
notWorkerGlobalOptsParser = do
    loggingOpts <- loggingOptsParser
    numCores <- optional $ option' auto
        [ long "cores"
        , short 'N'
        , metavar "NUM_CORES"
        , help "Number of cores to use"
        ]
    pure $ NotWorkerGlobalOpts
        { loggingOpts
        , numCores
        }

loggingOptsParser :: Parser LoggingOpts
loggingOptsParser = do
    stderrLogOpts <- logOptsParser defaultLogFormatForStderr "stderr" ""
    fileLogOpts <- optional $ do
        dst <- option' str
            [ long "file-log"
            , metavar "FILE"
            , help "Destination file for file log"
            , action "file"
            ]
        logOpts <- logOptsParser defaultLogFormatForFile "file" "file-"
        pure $ FileLogOpts { dst, logOpts }
    pure $ LoggingOpts { stderrLogOpts, fileLogOpts }

logOptsParser :: LogFormat -> String -> String -> Parser LogOpts
logOptsParser defaultLogFormat logName prefix = do
    level <- option' logLevelReader
        [ long (prefix ++ "log-level")
        , metavar "LEVEL"
        , value defaultLogLevel
        , help (printf "Log level for %s log" logName)
        , completeWith logLevelValues
        ]
    format <- option' logFormatReader
        [ long (prefix ++ "log-format")
        , metavar "FORMAT"
        , value defaultLogFormat
        , help "Log format for file log"
        , completeWith logFormatValues
        ]
    pure $ LogOpts { level, format }

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
    "json" -> Just LogFormatJSON
    "text" -> Just LogFormatText
    "human" -> Just LogFormatHuman
    _ -> Nothing

logFormatValues :: [String]
logFormatValues =
    [ "json"
    , "text"
    , "human"
    ]

defaultLogFormatForStderr :: LogFormat
defaultLogFormatForStderr = LogFormatHuman

defaultLogFormatForFile :: LogFormat
defaultLogFormatForFile = LogFormatText

commandOptsParser :: Parser CommandOpts
commandOptsParser =
    subparser (mconcat
        [ command "check"
            (info (notWorker CommandOptsCheck checkOptsParser)
                (commonInfo <> progDesc "Check proof scripts"))
        , command "extract-smt"
            (info (notWorker CommandOptsExtractSMT extractSMTOptsParser)
                (commonInfo <> progDesc "Extract SMT from a 'check' log"))
        , command "format-smt"
            (info (notWorker CommandOptsFormatSMT formatSMTOptsParser)
                (commonInfo <> progDesc "Format SMT s-expressions"))
        , command "worker"
            (info (CommandOptsWorker <$> workerOptsParser)
                (commonInfo <> progDesc "Spawn worker"))
        ])
  where
    notWorker constr p = do
        opts <- constr <$> p
        notWorkerGlobalOpts <- notWorkerGlobalOptsParser
        pure $ CommandOptsNotWorker notWorkerGlobalOpts opts

checkOptsParser :: Parser CheckOpts
checkOptsParser = do
    solvers <- option' str
        [ long "solvers"
        , short 's'
        , metavar "FILE"
        , help "Solvers config file"
        , action "file"
        ]
    workers <- optional $ option' str
        [ long "workers"
        , short 'w'
        , metavar "FILE"
        , help "Workers config file"
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
    maxNumConcurrentSolvers <- optional $ option' auto
        [ long "jobs"
        , short 'j'
        , metavar "NUM_JOBS"
        , help "Maximun number of concurrent solvers"
        ]
    sqliteCache <- optional $ option' str
        [ long "sqlite-cache"
        , metavar "DATABASE"
        , help "SQLite database to use as a cache"
        ]
    postgresCache <- optional $ option' str
        [ long "postgres-cache"
        , metavar "DATABASE"
        , help "PostgreSQL database to use as a cache"
        ]
    rodataSections <- many $ option' str
        [ long "rodata-section"
        , metavar "SECTION"
        ]
    rodataSymbols <- many $ option' str
        [ long "rodata-symbol"
        , metavar "SYMBOL"
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
    ignoreFunctions <- many $ option' (Ident <$> str)
        [ long "ignore-function"
        , metavar "SYMBOL"
        ]
    ignoreFunctionsEarly <- many $ option' (Ident <$> str)
        [ long "ignore-function-early"
        , metavar "SYMBOL"
        ]
    includeGroups <- many $ CheckGroupFingerprintPattern <$> option' (eitherReader (B16.decode . C.pack))
        [ long "include-group"
        , metavar "FINGERPRINT"
        ]
    includeChecks <- many $ CheckFingerprintPattern <$> option' (eitherReader (B16.decode . C.pack))
        [ long "include-check"
        , metavar "FINGERPRINT"
        ]
    reportFile <- optional $ option' str
        [ long "report-file"
        , metavar "FILE"
        , help "Output file for report"
        , action "file"
        ]
    forceEvalStages <- switch (long "force-eval-stages" <> help "Force evaluation of stages")
    dumpTargetDir <- optional $ option' str
        [ long "dump-target-dir"
        , metavar "DIRECTORY"
        , action "directory"
        ]
    referenceTargetDir <- optional $ option' str
        [ long "reference-target-dir"
        , metavar "DIRECTORY"
        , action "directory"
        ]
    mismatchDir <- optional $ option' str
        [ long "mismatch-dir"
        , metavar "DIRECTORY"
        , action "directory"
        ]
    justCompareChecks <- switch (long "just-compare-checks" <> help "Just compare checks to reference")
    pure $ CheckOpts
        { maxNumConcurrentSolvers
        , solvers
        , workers
        , onlineSolverTimeout
        , offlineSolverTimeout
        , sqliteCache
        , postgresCache
        , inputTargetDir
        , forceEvalStages
        , dumpTargetDir
        , referenceTargetDir
        , mismatchDir
        , rodataSections
        , rodataSymbols
        , includeFunctions
        , ignoreFunctions
        , ignoreFunctionsEarly
        , includeGroups
        , includeChecks
        , reportFile
        , justCompareChecks
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
    format <- option' logFormatReader
        [ long "format"
        , metavar "FORMAT"
        , value defaultLogFormatForFile
        , help "Log format for input log"
        , completeWith logFormatValues
        ]
    lineWrappingOpts <- optional lineWrappingOptsParser
    pure $ ExtractSMTOpts { match, direction, format, lineWrappingOpts }

formatSMTOptsParser :: Parser FormatSMTOpts
formatSMTOptsParser = do
    lineWrappingOpts <- optional lineWrappingOptsParser
    pure $ FormatSMTOpts { lineWrappingOpts }

lineWrappingOptsParser :: Parser LineWrappingOpts
lineWrappingOptsParser = do
    flag' () (long "wrap" <> help "Wrap long lines")
    preferredMaxLineWidth <- option' auto
        [ long "line-width"
        , short 'n'
        , metavar "N"
        , value defaultPrettySExprConfig.preferredMaxLineWidth
        , help "Preferred maximum line width"
        ]
    indentWidth <- option' auto
        [ long "indent-width"
        , short 'i'
        , metavar "N"
        , value defaultPrettySExprConfig.indentWidth
        , help "Indent width"
        ]
    minBreakOpt <- optional $ option' auto
        [ long "min-break"
        , metavar "N"
        , help "Indent width"
        ]
    color <- switch (long "color" <> help "Add color")
    pure $ LineWrappingOpts
        { preferredMaxLineWidth
        , indentWidth
        , minBreak = minBreakOpt <|> defaultPrettySExprConfig.minBreak
        , color
        }

workerOptsParser :: Parser WorkerOpts
workerOptsParser = do
    addr <- strArgument $ mconcat
        [ metavar "ADDRESS"
        , help "Worker endpoint address"
        ]
    return $ WorkerOpts
        { addr
        }

--

option' :: ReadM a -> [Mod OptionFields a] -> Parser a
option' r = option r . mconcat

argument' :: ReadM a -> [Mod ArgumentFields a] -> Parser a
argument' r = argument r . mconcat
