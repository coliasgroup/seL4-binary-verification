module BV.ConcreteSyntax
    ( BuildInBlock
    , BuildInLine
    , BuildToFile (..)
    , InBlockAsFile (..)
    , InLineAsInBlock (..)
    , ParseFile (..)
    , ParseFileFast (..)
    , ParseInBlock
    , ParseInLine
    , buildFile
    , parseWholeFile
    , parseWholeFileFast
    ) where

import GHC.Generics (Generic)

import BV.ConcreteSyntax.FastInstances ()
import BV.ConcreteSyntax.FastParsing
import BV.ConcreteSyntax.Instances ()
import BV.ConcreteSyntax.Parsing
import BV.ConcreteSyntax.Printing

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
