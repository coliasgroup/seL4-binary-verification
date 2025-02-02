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

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
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
    writeBVContents (JSONBVFile a) = encode a

newtype GraphLangLikeBVFile a
  = GraphLangLikeBVFile a

instance (ParseFile a, BuildToFile a) => ReadBVFile TL.Text (GraphLangLikeBVFile a) where
    readBVContents fp c = GraphLangLikeBVFile <$> parseWholeFile fp c

instance (ParseFile a, BuildToFile a) => WriteBVFile TL.Text (GraphLangLikeBVFile a) where
    writeBVContents (GraphLangLikeBVFile a) = TL.toLazyText (buildToFile a)

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

deriving via (JSONBVFile Pairings) instance ReadBVFile BL.ByteString Pairings
deriving via (JSONBVFile Pairings) instance WriteBVFile BL.ByteString Pairings

deriving via (JSONBVFile (CompatProofChecks String)) instance ReadBVFile BL.ByteString (CompatProofChecks String)
deriving via (JSONBVFile (CompatProofChecks String)) instance WriteBVFile BL.ByteString (CompatProofChecks String)

deriving via (JSONBVFile (CompatSMTProofChecks ())) instance ReadBVFile BL.ByteString (CompatSMTProofChecks ())
deriving via (JSONBVFile (CompatSMTProofChecks ())) instance WriteBVFile BL.ByteString (CompatSMTProofChecks ())

-- HACK just for test in graph-refine/loop-example

deriving via (GraphLangLikeBVFile (InBlockAsFile (InLineAsInBlock (ProofScript ())))) instance ReadBVFile TL.Text (ProofScript ())
deriving via (GraphLangLikeBVFile (InBlockAsFile (InLineAsInBlock (ProofScript ())))) instance WriteBVFile TL.Text (ProofScript ())
