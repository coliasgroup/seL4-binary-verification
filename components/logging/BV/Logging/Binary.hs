{-# OPTIONS_GHC -Wno-orphans #-}

module BV.Logging.Binary
    (
    ) where

import BV.Logging.Types

import Control.Monad.Logger (Loc (..), LogLevel (..), LogStr, fromLogStr,
                             toLogStr)
import Data.Binary
import Data.ByteString (StrictByteString)

instance Binary LogEntry where

    put entry = do
        put entry.context
        putLoc entry.loc
        put entry.source
        putLevel entry.level
        putLogStr entry.msg

    get = LogEntry <$> get <*> getLoc <*> get <*> getLevel <*> getLogStr

putLoc :: Loc -> Put
putLoc loc = do
    put loc.loc_filename
    put loc.loc_package
    put loc.loc_module
    put loc.loc_start
    put loc.loc_end

getLoc :: Get Loc
getLoc = Loc <$> get <*> get <*> get <*> get <*> get

putLevel :: LogLevel -> Put
putLevel = \case
    LevelDebug -> put (0 :: Word8)
    LevelInfo -> put (1 :: Word8)
    LevelWarn -> put (2 :: Word8)
    LevelError -> put (3 :: Word8)
    LevelOther s -> put (4 :: Word8) >> put s

getLevel :: Get LogLevel
getLevel = do
    t :: Word8 <- get
    case t of
        0 -> return LevelDebug
        1 -> return LevelInfo
        2 -> return LevelWarn
        3 -> return LevelError
        4 -> LevelOther <$> get
        _ -> fail "unexpected tag"

putLogStr :: LogStr -> Put
putLogStr = put . fromLogStr

getLogStr :: Get LogStr
getLogStr = do
    s :: StrictByteString <- get
    return $ toLogStr s
