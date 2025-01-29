module BV.ConcreteSyntax.GraphLangLike.Adapters
    ( InBlockAsFile (..)
    , InLineAsInBlock (..)
    , ParseInBlock
    , ParseInLine
    ) where

import BV.ConcreteSyntax.GraphLangLike.Building
import BV.ConcreteSyntax.GraphLangLike.Parsing

import GHC.Generics (Generic)

newtype InBlockAsFile a
  = InBlockAsFile { unwrap :: a }
  deriving (Eq, Generic, Ord, Show)

instance ParseInBlock a => ParseFile (InBlockAsFile a) where
    parseFile = InBlockAsFile <$> parseInBlock

instance BuildInBlock a => BuildToFile (InBlockAsFile a) where
    buildToFile = buildBlock . buildInBlock . (.unwrap)

newtype InLineAsInBlock a
  = InLineAsInBlock { unwrap :: a }
  deriving (Eq, Generic, Ord, Show)

instance ParseInLine a => ParseInBlock (InLineAsInBlock a) where
    parseInBlock = InLineAsInBlock <$> line parseInLine

instance BuildInLine a => BuildInBlock (InLineAsInBlock a) where
    buildInBlock = lineInBlock . buildInLine . (.unwrap)
