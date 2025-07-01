{-# LANGUAGE OverloadedStrings #-}

module BV.CLI.Commands.ExtractSMT
    ( runExtractSMT
    ) where

import BV.CLI.Commands.FormatSMT
import BV.CLI.Opts
import BV.ConcreteSyntax
import BV.Logging

import Data.Attoparsec.Text
import Optics

import Control.Applicative (optional)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (fromLogStr)
import Control.Monad.State (get, put)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Lift (evalStateLC)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT (decodeUtf8)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Options.Applicative (many)
import System.Exit (die)
import System.IO (stdout)

runExtractSMT :: (MonadThrow m, MonadIO m, MonadLoggerWithContext m) => ExtractSMTOpts -> m ()
runExtractSMT opts = do
    let r = parseOnly (parsePattern <* endOfInput) (T.pack opts.match)
    patternPrefix <- case r of
        Left err -> liftIO . die $ "failed to parse pattern: " ++ err
        Right pat -> return pat
    let pat = patternPrefix ++ patternSuffix
    let preparedPattern = preparePattern pat
    runConduit $
        C.stdin
        .| conduitParser (logEntryParserFor opts.format)
        .| evalStateLC Nothing (CL.mapMaybeM (\(_, entry) -> do
                if matches preparedPattern entry.context
                then do
                    let cur = entry.context
                    prev <- get
                    case prev of
                        Just prevContext -> when (prevContext /= cur) $ do
                            liftIO $ die "TODO"
                        Nothing -> do
                            return ()
                    put (Just cur)
                    return (Just entry.msg)
                else do
                    return Nothing))
        .| CL.map fromLogStr
        .| CT.decodeUtf8
        .| conduitParser parseSExprWithPlaceholdersFaster
        .| CL.map snd
        .| formatSMTC opts.lineWrappingOpts
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
entryMatches (leftAnchor, pat, rightAnchor) (LogContextEntry entry) =
    f pat entry
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
            & _head % _1 .~ leftAnchor
            & _last % _3 .~ rightAnchor
