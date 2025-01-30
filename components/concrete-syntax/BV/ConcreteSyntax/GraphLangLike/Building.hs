{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.GraphLangLike.Building
    ( BlockBuilder
    , BuildInBlock (..)
    , BuildInLine (..)
    , BuildToFile (..)
    , LineBuilder
    , buildBlock
    , buildFile
    , buildLine
    , buildStandaloneLine
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
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)

intersperse :: Monoid a => a -> [a] -> a
intersperse _ [] = mempty
intersperse sep (x:xs) = x <> foldMap (sep <>) xs

class BuildToFile a where
    buildToFile :: a -> Builder

buildFile :: BuildToFile a => a -> TL.Text
buildFile = toLazyText . buildToFile

newtype BlockBuilder
  = BlockBuilder { unwrapBlockBuilder :: D.DList LineBuilder }
  deriving (Monoid, Semigroup)

buildBlock :: BlockBuilder -> Builder
buildBlock blockBuilder = foldMap buildLine (D.toList blockBuilder.unwrapBlockBuilder)

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
buildLine lineBuilder = buildStandaloneLine lineBuilder <> "\n"

buildStandaloneLine :: LineBuilder -> Builder
buildStandaloneLine lineBuilder = intersperse " " (D.toList lineBuilder.unwrapLineBuilder)

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
putManyWith f xs = putDec (length xs) <> foldMap f xs

class BuildInLine a where
    buildInLine :: a -> LineBuilder

instance BuildInLine a => BuildInLine [a] where
    buildInLine = putManyWith buildInLine

instance (BuildInLine a, BuildInLine b) => BuildInLine (a, b) where
    buildInLine (a, b)= put a <> put b
