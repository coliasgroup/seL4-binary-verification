{-# LANGUAGE DerivingVia #-}

module BV.ConcreteSyntax.Classes
    ( IsContents (..)
    , ReadBVFile (..)
    , WriteBVFile (..)
    , readBVFile
    , writeBVFile
    ) where

import BV.ConcreteSyntax.GraphLangLike.Adapters
import BV.ConcreteSyntax.GraphLangLike.Building (BuildToFile, buildToFile)
import BV.ConcreteSyntax.GraphLangLike.Instances ()
import BV.ConcreteSyntax.GraphLangLike.Parsing (ParseFile, parseWholeFile)
import BV.ConcreteSyntax.JSON ()
import BV.Core.Types

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL

class IsContents a where
    readContentsFromFile :: FilePath -> IO a
    writeContentsToFile :: FilePath -> a -> IO ()

instance IsContents BL.ByteString where
    readContentsFromFile = BL.readFile
    writeContentsToFile = BL.writeFile

instance IsContents TL.Text where
    readContentsFromFile = TL.readFile
    writeContentsToFile = TL.writeFile

class IsContents c => ReadBVFile c a | a -> c where
    readBVContents :: FilePath -> c -> Either String a

class IsContents c => WriteBVFile c a | a -> c where
    writeBVContents :: a -> c

readBVFile :: ReadBVFile c a => FilePath -> IO (Either String a)
readBVFile fp = readBVContents fp <$> readContentsFromFile fp

writeBVFile :: WriteBVFile c a => FilePath -> a -> IO ()
writeBVFile fp a = writeContentsToFile fp (writeBVContents a)

newtype JSONBVFile a
  = JSONBVFile a

instance (FromJSON a, ToJSON a) => ReadBVFile BL.ByteString (JSONBVFile a) where
    readBVContents _fp c = JSONBVFile <$> eitherDecode c

instance (FromJSON a, ToJSON a) => WriteBVFile BL.ByteString (JSONBVFile a) where
    writeBVContents (JSONBVFile a) = encodePretty' config a
      where
        config = defConfig
            { confCompare = compare
            , confTrailingNewline = True
            }

newtype GraphLangLikeBVFile a
  = GraphLangLikeBVFile a

instance (ParseFile a, BuildToFile a) => ReadBVFile TL.Text (GraphLangLikeBVFile a) where
    readBVContents fp c = GraphLangLikeBVFile <$> parseWholeFile fp c

instance (ParseFile a, BuildToFile a) => WriteBVFile TL.Text (GraphLangLikeBVFile a) where
    writeBVContents (GraphLangLikeBVFile a) = TB.toLazyText (buildToFile a)

--

deriving via (GraphLangLikeBVFile Program) instance ReadBVFile TL.Text Program
deriving via (GraphLangLikeBVFile Program) instance WriteBVFile TL.Text Program

deriving via (GraphLangLikeBVFile StackBounds) instance ReadBVFile TL.Text StackBounds
deriving via (GraphLangLikeBVFile StackBounds) instance WriteBVFile TL.Text StackBounds

deriving via (GraphLangLikeBVFile Problems) instance ReadBVFile TL.Text Problems
deriving via (GraphLangLikeBVFile Problems) instance WriteBVFile TL.Text Problems

deriving via (JSONBVFile InlineScripts) instance ReadBVFile BL.ByteString InlineScripts
deriving via (JSONBVFile InlineScripts) instance WriteBVFile BL.ByteString InlineScripts

deriving via (JSONBVFile (Proofs ())) instance ReadBVFile BL.ByteString (Proofs ())
deriving via (JSONBVFile (Proofs ())) instance WriteBVFile BL.ByteString (Proofs ())

deriving via (JSONBVFile (Pairings t)) instance RefineTag t => ReadBVFile BL.ByteString (Pairings t)
deriving via (JSONBVFile (Pairings t)) instance RefineTag t => WriteBVFile BL.ByteString (Pairings t)

deriving via (JSONBVFile CompatProofChecks) instance ReadBVFile BL.ByteString CompatProofChecks
deriving via (JSONBVFile CompatProofChecks) instance WriteBVFile BL.ByteString CompatProofChecks

deriving via (JSONBVFile CompatSMTProofChecks) instance ReadBVFile BL.ByteString CompatSMTProofChecks
deriving via (JSONBVFile CompatSMTProofChecks) instance WriteBVFile BL.ByteString CompatSMTProofChecks

-- HACK just for test in graph-refine/loop-example

deriving via (GraphLangLikeBVFile (InBlockAsFile (InLineAsInBlock (ProofScript t ()))))
    instance RefineTag t => ReadBVFile TL.Text (ProofScript t ())

deriving via (GraphLangLikeBVFile (InBlockAsFile (InLineAsInBlock (ProofScript t ()))))
    instance RefineTag t => WriteBVFile TL.Text (ProofScript t ())
