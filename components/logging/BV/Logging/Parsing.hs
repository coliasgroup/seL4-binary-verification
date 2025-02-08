{-# LANGUAGE OverloadedStrings #-}

module BV.Logging.Parsing
    ( parseTextLogEntry
    ) where

import BV.Logging.Types

import Data.Attoparsec.ByteString

parseTextLogEntry :: Parser LogEntry
parseTextLogEntry = undefined
