{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.Printing
    ( BlockBuilder
    , BuildInBlock (..)
    , BuildInLine (..)
    , BuildToFile (..)
    , LineBuilder
    , buildBlock
    , buildBlocksFile
    , buildBlocksFileWithTypicalKeyFormat
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

import qualified Data.DList as D
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)

import BV.Utils

buildBlocksFile :: (k -> Builder) -> (v -> BlockBuilder) -> [(k, v)] -> Builder
buildBlocksFile bk bv = mconcat . map bkv
  where
    bkv (k, v) = bk k <> " {\n" <> buildBlock (bv v) <> "}\n"

buildBlocksFileWithTypicalKeyFormat :: [String] -> (k -> Builder) -> (v -> BlockBuilder) -> [(k, v)] -> Builder
buildBlocksFileWithTypicalKeyFormat nesting = buildBlocksFile . bk' nesting
  where
    bk' [] bk k = bk k
    bk' (x:xs) kb k = fromString x <> " (" <> bk' xs kb k <> ")"

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
putDec = putIntegralWith decimal

putHex :: Integral a => a -> LineBuilder
putHex = putIntegralWith $ ("0x" <>) . hexadecimal

putIntegralWith :: Integral a => (a -> Builder) -> a -> LineBuilder
putIntegralWith putAbs n = putBuilder $ (if n < 0 then "-" else mempty) <> putAbs (abs n)

putManyWith :: (a -> LineBuilder) -> [a] -> LineBuilder
putManyWith f xs = putDec (length xs) <> mconcat (map f xs)

class BuildInLine a where
    buildInLine :: a -> LineBuilder

instance BuildInLine a => BuildInLine [a] where
    buildInLine = putManyWith buildInLine

instance (BuildInLine a, BuildInLine b) => BuildInLine (a, b) where
    buildInLine (a, b)= put a <> put b
