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

import Data.Monoid
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)

intersperse :: Monoid a => a -> [a] -> a
intersperse _ [] = mempty
intersperse sep (x:xs) = x <> mconcat (map (sep <>) xs)

newtype DList a
  = DList { applyDList :: Endo [a] }
  deriving (Monoid, Semigroup)

singleton :: a -> DList a
singleton = DList . Endo . (:)

fromList :: [a] -> DList a
fromList = DList . Endo . (++)

toList :: DList a -> [a]
toList = ($ []) . appEndo . (.applyDList)

--

class BuildToFile a where
    buildToFile :: a -> Builder

buildFile :: BuildToFile a => a -> L.Text
buildFile = toLazyText . buildToFile

newtype BlockBuilder
  = BlockBuilder { unwrapBlockBuilder :: DList LineBuilder }
  deriving (Monoid, Semigroup)

buildBlock :: BlockBuilder -> Builder
buildBlock blockBuilder = mconcat (map buildLine (toList blockBuilder.unwrapBlockBuilder))

lineInBlock :: LineBuilder -> BlockBuilder
lineInBlock = BlockBuilder . singleton

class BuildInBlock a where
    buildInBlock :: a -> BlockBuilder

newtype LineBuilder
  = LineBuilder { unwrapLineBuilder :: DList Builder }
  deriving (Monoid, Semigroup)

instance IsString LineBuilder where
    fromString = putWord

buildLine :: LineBuilder -> Builder
buildLine lineBuilder = intersperse " " (toList lineBuilder.unwrapLineBuilder) <> "\n"

put :: BuildInLine a => a -> LineBuilder
put = buildInLine

putWord :: String -> LineBuilder
putWord = putBuilder . fromString

putBuilder :: Builder -> LineBuilder
putBuilder = LineBuilder . singleton

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
