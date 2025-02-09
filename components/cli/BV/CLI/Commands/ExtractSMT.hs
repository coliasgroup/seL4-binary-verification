{-# LANGUAGE OverloadedStrings #-}

module BV.CLI.Commands.ExtractSMT
    ( runExtractSMT
    ) where

import BV.CLI.Opts
import BV.Logging

import Data.Attoparsec.Text
import Optics

import Control.Applicative (optional)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (fromLogStr)
import Data.Conduit (Flush (Chunk, Flush), runConduit, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Options.Applicative (many)
import System.Exit (die)
import System.IO (stdout)

-- TODO ensure that all contexts of a given run are the same

runExtractSMT :: (MonadThrow m, MonadIO m, MonadLoggerWithContext m) => ExtractSMTOpts -> m ()
runExtractSMT opts = do
    let r = parseOnly (parsePattern <* endOfInput) (T.pack opts.match)
    patternPrefix <- case r of
        Left err -> liftIO . die $ "failed to parse pattern: " ++ err
        Right pattern -> return pattern
    let pattern = patternPrefix ++ patternSuffix
    let preparedPattern = preparePattern pattern
    runConduit $
        C.stdin
        .| conduitParser (logEntryParserFor opts.format)
        .| CL.mapMaybe (\(_, entry) -> if matches preparedPattern entry.context then Just entry.msg else Nothing)
        .| CL.concatMap (\msg -> [Chunk (fromLogStr msg), Chunk "\n", Flush])
        .| C.sinkHandleFlush stdout
  where
    directionCtxEntry = case opts.direction of
        Send -> "send"
        Recv -> "recv"
    patternSuffix = [(True, directionCtxEntry, True)]

type PatternEntry = (Bool, String, Bool)

type Pattern = [PatternEntry]

type PreparedPattern = [PatternEntry]

matches :: PreparedPattern -> LogContext -> Bool
matches preparedPattern ctx =
    length preparedPattern <= length ctx && and (zipWith entryMatches preparedPattern preparedCtx)
  where
    preparedCtx = reverse ctx

entryMatches :: PatternEntry -> LogContextEntry -> Bool
entryMatches (leftAnchor, pattern, rightAnchor) (LogContextEntry entry) =
    f pattern entry
  where
    f = case (leftAnchor, rightAnchor) of
        (True, True) -> (==)
        (True, False) -> isPrefixOf
        (False, True) -> isSuffixOf
        (False, False) -> isInfixOf

preparePattern :: Pattern -> PreparedPattern
preparePattern = reverse

parsePattern :: Parser Pattern
parsePattern = do
    leftAnchor <- isJust <$> optional "["
    segments <- many (notChar ']') `sepBy` "] ["
    rightAnchor <- isJust <$> optional "]"
    return $
        [ (True, seg, True) | seg <- segments ]
            & singular traversed % _1 .~ leftAnchor
            & singular (backwards traversed) % _3 .~ rightAnchor
