{-# LANGUAGE OverloadedStrings #-}

module BV.Logging.Parsing
    ( parseLogEntryHumanBestEffort
    , parseLogEntryJSON
    , parseLogEntryText
    ) where

import BV.Logging.Aeson ()
import BV.Logging.Types

import Control.Applicative (optional)
import Control.Monad.Logger (Loc, LogLevel (..), LogStr, ToLogStr (toLogStr))
import Data.Aeson (parseJSON)
import Data.Aeson.Parser (json)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

parseLogEntryJSON :: Parser LogEntry
parseLogEntryJSON = do
    val <- json <* endOfLine
    either fail return $ parseEither parseJSON val

parseLogEntryTextWith :: Parser (LogStr, Maybe Loc) -> Parser LogEntry
parseLogEntryTextWith p = do
    level <- parseLevel <$> ("[" *> A.takeWhile (`B.notElem` "#]"))
    source <- fmap (fromMaybe defaultLogSource) . optional $ do
        "#"
        bytes <- A.takeWhile (`B.notElem` "]") <* "]"
        either (fail . show) return $ T.decodeUtf8' bytes
    "] "
    context <- many' $ do
        bytes <- "[" *> A.takeWhile (`B.notElem` "]") <* "] "
        either (fail . show) (return . makeLogContextEntry . T.unpack) $ T.decodeUtf8' bytes
    (msg, maybeLoc) <- p
    return $ LogEntry
        { level
        , source
        , context
        , msg
        , loc = fromMaybe defaultLoc maybeLoc
        }

parseLevel :: B.ByteString -> LogLevel
parseLevel = \case
    "Error" -> LevelError
    "Warn" -> LevelWarn
    "Info" -> LevelInfo
    "Debug" -> LevelDebug
    other -> LevelOther (T.decodeUtf8 other)

-- HACK `monad-logger` leaves out the `loc_end` field in `defaultLogStr`, so we ignore the entire `Loc` type here
parseLogEntryText :: Parser LogEntry
parseLogEntryText = parseLogEntryTextWith $ do
    msgLen <- "(" *> decimal <* ") "
    msg <- toLogStr <$> A.take msgLen
    optional $ do
        " @(" *> A.takeWhile (`B.notElem` ")")  <* ")"
    endOfLine
    return (msg, Nothing)

parseLogEntryHumanBestEffort :: Parser LogEntry
parseLogEntryHumanBestEffort = parseLogEntryTextWith $ do
    msg <- toLogStr . B.pack <$> manyTill anyWord8 endOfLine
    return (msg, Nothing)
