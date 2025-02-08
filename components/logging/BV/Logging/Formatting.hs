{-# LANGUAGE OverloadedStrings #-}

module BV.Logging.Formatting
    ( adaptWith
    , humanLogFormatter
    , jsonLogFormatter
    , textLogFormatter
    ) where

import BV.Logging.Aeson ()
import BV.Logging.Types

import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr,
                             defaultLogStr, fromLogStr, toLogStr)
import Data.Aeson (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

textLogFormatter :: LogEntry -> BL.ByteString
textLogFormatter = BL.fromStrict . fromLogStr . adaptWith True defaultLogStr

humanLogFormatter :: LogEntry -> BL.ByteString
humanLogFormatter = BL.fromStrict . fromLogStr . adaptWith False defaultLogStr

jsonLogFormatter :: LogEntry -> BL.ByteString
jsonLogFormatter = encode

adaptWith :: Bool -> (Loc -> LogSource -> LogLevel -> LogStr -> a) -> LogEntry -> a
adaptWith includeMsgLengths f entry =
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
