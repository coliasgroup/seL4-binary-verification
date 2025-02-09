{-# LANGUAGE OverloadedStrings #-}

module BV.Logging.Formatting
    ( adaptLogEntryFormatterWith
    , formatLogEntryHuman
    , formatLogEntryJSON
    , formatLogEntryText
    ) where

import BV.Logging.Aeson ()
import BV.Logging.Types

import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr,
                             defaultLogStr, fromLogStr, toLogStr)
import Data.Aeson (encode)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString, lazyByteString)

formatLogEntryJSON :: LogEntry -> Builder
formatLogEntryJSON entry = lazyByteString $ encode entry <> "\n"

formatLogEntryText :: LogEntry -> Builder
formatLogEntryText = byteString . fromLogStr . adaptLogEntryFormatterWith True defaultLogStr

formatLogEntryHuman :: LogEntry -> Builder
formatLogEntryHuman = byteString . fromLogStr . adaptLogEntryFormatterWith False defaultLogStr

adaptLogEntryFormatterWith :: Bool -> (Loc -> LogSource -> LogLevel -> LogStr -> a) -> LogEntry -> a
adaptLogEntryFormatterWith includeMsgLengths f entry =
    f entry.loc entry.source entry.level msg
  where
    msg = buildContextAndMessageWith includeMsgLengths entry.context entry.msg

buildContextAndMessageWith :: Bool -> [LogContextEntry] -> LogStr -> LogStr
buildContextAndMessageWith includeMsgLengths ctx msg =
    -- LogStr is implemented as a bytestring builder, so a round trip is actually the most efficient here
    let msgBytes = fromLogStr (toLogStr msg)
    in foldMap (\ctxEntry -> "[" <> toLogStr ctxEntry.unwrap <> "] ") ctx
        <> (if includeMsgLengths then "(" <> toLogStr (show (B.length msgBytes)) <> ") " else mempty)
        <> toLogStr msgBytes
