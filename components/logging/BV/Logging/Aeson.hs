{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module BV.Logging.Aeson
    (
    ) where

import BV.Logging.Types

import Control.Monad.Logger (Loc (..), LogLevel (..), fromLogStr, toLogStr)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject,
                   withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

instance ToJSON LogEntry where
    toJSON v = object $
        [ "context" .= v.context
        , "level" .= case v.level of
            LevelError -> "Error"
            LevelWarn -> "Warn"
            LevelInfo -> "Info"
            LevelDebug -> "Debug"
            LevelOther other -> other
        , "msg" .= decodeUtf8 (fromLogStr v.msg)
        ] ++ unless' (isDefaultLoc v.loc)
        [ "loc" .= encodeLoc v.loc
        ] ++ unless' (isDefaultLogSource v.source)
        [ "source" .= v.source
        ]
      where
        unless' p xs = if p then [] else xs

encodeLoc :: Loc -> Value
encodeLoc v = object
    [ "filename" .= v.loc_filename
    , "package" .= v.loc_package
    , "module" .= v.loc_module
    , "start" .= v.loc_start
    , "end" .= v.loc_end
    ]

instance FromJSON LogEntry where
    parseJSON = withObject "LogEntry" $ \v -> LogEntry
        <$> v .: "context"
        <*> ((v .:? "loc") >>= traverse decodeLoc) .!= defaultLoc
        <*> v .:? "source" .!= defaultLogSource
        <*> ((v .: "level") >>= decodeLogLevel)
        <*> (toLogStr <$> ((v .: "msg") :: Parser Text))

decodeLoc :: Value -> Parser Loc
decodeLoc = withObject "Loc" $ \v -> Loc
    <$> v .: "filename"
    <*> v .: "package"
    <*> v .: "module"
    <*> v .: "start"
    <*> v .: "end"

decodeLogLevel :: Value -> Parser LogLevel
decodeLogLevel = withText "LogLevel" $ \s -> pure $ case s of
    "Error" -> LevelError
    "Warn" -> LevelWarn
    "Info" -> LevelInfo
    "Debug" -> LevelDebug
    other -> LevelOther other
