{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Printing
    ( BlockBuilder
    , BuildInBlock (..)
    , BuildInLine (..)
    , BuildToFile (..)
    , LineBuilder
    , buildBlock
    , buildFile
    , buildLine
    , intersperse
    , lineInBlock
    , put
    , putDec
    , putHex
    , putManyWith
    , putWord
    ) where

import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import qualified Data.DList as D

import BV.Utils

class BuildToFile a where
    buildToFile :: a -> Builder

buildFile :: BuildToFile a => a -> L.Text
buildFile = toLazyText . buildToFile

newtype BlockBuilder
  = BlockBuilder { unwrapBlockBuilder :: D.DList LineBuilder }
  deriving (Monoid, Semigroup)

buildBlock :: BlockBuilder -> Builder
buildBlock blockBuilder = mconcat (map buildLine (D.toList blockBuilder.unwrapBlockBuilder))

lineInBlock :: LineBuilder -> BlockBuilder
lineInBlock = BlockBuilder . D.singleton

class BuildInBlock a where
    buildInBlock :: a -> BlockBuilder

newtype LineBuilder
  = LineBuilder { unwrapLineBuilder :: D.DList Builder }
  deriving (Monoid, Semigroup)

instance IsString LineBuilder where
    fromString = putWord

buildLine :: LineBuilder -> Builder
buildLine lineBuilder = intersperse " " (D.toList lineBuilder.unwrapLineBuilder) <> "\n"

put :: BuildInLine a => a -> LineBuilder
put = buildInLine

putWord :: String -> LineBuilder
putWord = putBuilder . fromString

putBuilder :: Builder -> LineBuilder
putBuilder = LineBuilder . D.singleton

putDec :: Integral a => a -> LineBuilder
putDec = putBuilder . decimal

putHex :: Integral a => a -> LineBuilder
putHex = putBuilder . ("0x" <>). hexadecimal

putManyWith :: (a -> LineBuilder) -> [a] -> LineBuilder
putManyWith f xs = putDec (length xs) <> mconcat (map f xs)

class BuildInLine a where
    buildInLine :: a -> LineBuilder

instance BuildInLine a => BuildInLine [a] where
    buildInLine = putManyWith buildInLine
