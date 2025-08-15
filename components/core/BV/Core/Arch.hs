module BV.Core.Arch
    ( archPtrSizeBytes
    , archWordSizeBits
    ) where

-- TODO make configurable

archWordSizeBits :: Integer
archWordSizeBits = 32

archPtrSizeBytes :: Integer
archPtrSizeBytes = archWordSizeBits `div` 8
