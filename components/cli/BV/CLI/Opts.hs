{-# LANGUAGE ApplicativeDo #-}

module BV.CLI.Opts
    ( CheckNumCoresOpts (..)
    , CheckOpts (..)
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

import Control.Monad.Except (liftEither, runExcept, throwError)
import qualified Data.Attoparsec.ByteString as A (Parser)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as C
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Optics
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
      { numCoresOpts :: CheckNumCoresOpts
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
      , cFunctionPrefix :: String
      , rodataSections :: [String]
      , rodataSymbols :: [String]
      , includeFunctions :: [Ident]
      , ignoreFunctions :: [Ident]
      , includeFunctionsEarly :: [Ident]
      , ignoreFunctionsEarly :: [Ident]
      , includeGroups :: [(Ident, CheckGroupFingerprintPattern)]
      , includeChecks :: [(Ident, CheckFingerprintPattern)]
      , reportFile :: Maybe FilePath
      , justCompareChecks :: Bool
      }
  deriving (Generic, Show)

data CheckNumCoresOpts
  = CheckNumCoresOpts
      { numEvalCores :: Maybe Integer
      , numSolverCores :: Maybe Integer
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
commonInfo = header "seL4 Binary Verification"

optsParserInfo :: ParserInfo Opts
optsParserInfo =
    info
        (optsParser <**> helper)
        (commonInfo <> fullDesc)

optsParser :: Parser Opts
optsParser = do
    justDumpOptions <- switch'
        [ internal
        , long "just-dump-options"
        ]
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
    pure $ NotWorkerGlobalOpts
        { loggingOpts
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
        , help (printf "Log level for %s log (error|warn|info|debug|trace)" logName)
        , completeWith logLevelValues
        ]
    format <- option' logFormatReader
        [ long (prefix ++ "log-format")
        , metavar "FORMAT"
        , value defaultLogFormat
        , help "Log format for file log (human|text|json)"
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
    hsubparser (mconcat
        [ command "check"
            (info
                (notWorker CommandOptsCheck checkOptsParser)
                (commonInfo <> progDesc "Check proof scripts"))
        , command "format-smt"
            (info
                (notWorker CommandOptsFormatSMT formatSMTOptsParser)
                (commonInfo <> progDesc "Format SMT-LIB 2.0 S-expressions"))

        , command "extract-smt"
            (info
                (notWorker CommandOptsExtractSMT extractSMTOptsParser)
                (commonInfo <> progDesc "Extract solver input or output from a 'check' log"))
        , command "_worker"
            (info
                (CommandOptsWorker <$> workerOptsParser)
                (commonInfo <> progDesc "(internal) Spawn worker"))
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
        , metavar "FILE"
        , action "file"
        , help "Solvers config file"
        ]
    workers <- optional $ option' str
        [ long "workers"
        , metavar "FILE"
        , action "file"
        , help "Workers config file, for external workers mode"
        ]
    onlineSolverTimeout <- option' (solverTimeoutFromSeconds <$> auto)
        [ long "online-solver-timeout"
        , metavar "SECONDS"
        , value defaultOnlineSolverTimeout
        , help "Timeout for online solver"
        ]
    offlineSolverTimeout <- option' (solverTimeoutFromSeconds <$> auto)
        [ long "offline-solver-timeout"
        , metavar "SECONDS"
        , value defaultOfflineSolverTimeout
        , help "Timeout for offline solvers"
        ]
    numCoresOpts <- do
        numEvalCores <- optional $ option' auto
            [ long "num-eval-cores"
            , metavar "N"
            , help "Number of cores to use for Haskell evaluation"
            ]
        numSolverCores <- optional $ option' auto
            [ long "num-solver-cores"
            , metavar "N"
            , help "Number cores to use for SMT solvers, for embedded worker mode"
            ]
        pure $ CheckNumCoresOpts
            { numEvalCores
            , numSolverCores
            }
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
    cFunctionPrefix <- option' str
        [ long "c-function-prefix"
        , metavar "C_FUNCTION_PREFIX"
        , help "Prefix used for deriving C function name from ASM function name"
        ]
    rodataSections <- many $ option' str
        [ long "rodata-section"
        , metavar "SECTION"
        , help "Declare ELF section as rodata"
        ]
    rodataSymbols <- many $ option' str
        [ long "rodata-symbol"
        , metavar "SYMBOL"
        , help "Declare ELF symbol as rodata"
        ]
    inputTargetDir <- option' str
        [ long "target-dir"
        , metavar "DIRECTORY"
        , action "directory"
        , help "Input target directory"
        ]
    includeFunctions <- many $ option' (Ident <$> str)
        [ long "include-function"
        , metavar "SYMBOL"
        , help "Only check ASM functions specified by this option"
        ]
    ignoreFunctions <- many $ option' (Ident <$> str)
        [ long "ignore-function"
        , metavar "SYMBOL"
        , help "Don't check this ASM function"
        ]
    includeFunctionsEarly <- many $ option' (Ident <$> str)
        [ long "include-function-early"
        , metavar "SYMBOL"
        , help "Only acknowledge ASM functions specified by this option"
        ]
    ignoreFunctionsEarly <- many $ option' (Ident <$> str)
        [ long "ignore-function-early"
        , metavar "SYMBOL"
        , help "Don't acknowledge this ASM function"
        ]
    includeGroups <- many $ over _2 CheckGroupFingerprintPattern <$> option' (eitherReader parseIncludeFingerprintPattern)
        [ long "include-group"
        , metavar "FUNCTION:GROUP_FINGERPRINT"
        , help "Only check check groups specified by this option"
        ]
    includeChecks <- many $ over _2 CheckFingerprintPattern <$> option' (eitherReader parseIncludeFingerprintPattern)
        [ long "include-check"
        , metavar "FUNCTION:CHECK_FINGERPRINT"
        , help "Only check checks specified by this option"
        ]
    reportFile <- optional $ option' str
        [ long "report-file"
        , metavar "FILE"
        , action "file"
        , help "Output file for report"
        ]
    dumpTargetDir <- optional $ option' str
        [ long "dump-target-dir"
        , metavar "DIRECTORY"
        , action "directory"
        , help "Dump stages into this directory"
        ]
    referenceTargetDir <- optional $ option' str
        [ long "reference-target-dir"
        , metavar "DIRECTORY"
        , action "directory"
        , help "Check stages against those found in this"
        ]
    mismatchDir <- optional $ option' str
        [ long "mismatch-dir"
        , metavar "DIRECTORY"
        , action "directory"
        , help "Dump stage mismatches into this directory"
        ]
    justCompareChecks <- switch'
        [ long "just-compare-to-reference"
        , help "Just compare stages to reference, skipping solver invocations"
        ]
    forceEvalStages <- switch'
        [ long "force-eval-all-stages"
        , help "Force evaluation of all stages"
        ]
    pure $ CheckOpts
        { numCoresOpts
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
        , cFunctionPrefix
        , rodataSections
        , rodataSymbols
        , includeFunctions
        , ignoreFunctions
        , includeFunctionsEarly
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
        , help "Log format for input log (human|text|json)"
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
    color <- switch'
        [ long "color"
        , help "Add color"
        ]
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

switch' :: [Mod FlagFields Bool] -> Parser Bool
switch' = switch . mconcat

parseIncludeFingerprintPattern :: String -> Either String (Ident, C.ByteString)
parseIncludeFingerprintPattern s = runExcept $ do
    case Seq.breakr (== ':') (Seq.fromList s) of
        (fingerprintStr, ident Seq.:|> ':') -> do
            fingerprint <- liftEither $ B16.decode (C.pack (toList fingerprintStr))
            return (Ident (toList ident), fingerprint)
        _ -> throwError $ "malformed fingerprint: " ++ show s
