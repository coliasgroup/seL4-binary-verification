{-# LANGUAGE OverloadedStrings #-}

module BV.Core.ProofChecks where

import Control.Applicative (many, optional, (<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import GHC.Generics (Generic)
import Optics.Core
import Text.Megaparsec (eof, manyTill, manyTill_, parse, try)

import BV.Core.Inputs (ProblemAndProof (problem))
import BV.Core.Pairing
import BV.Core.Program
import BV.ConcreteSyntax.Parsing
import BV.ConcreteSyntax.Printing
import BV.System.Utils
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Text.Internal.Builder (Builder)
import Text.Megaparsec.Error (errorBundlePretty)

newtype ProofChecks a
  = ProofChecks { unwrap :: M.Map PairingId [ProofCheck a] }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

data ProofCheck a
  = ProofCheck
      { meta :: a
      , hyps :: [Hyp]
      , hyp :: Hyp
      }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

data Hyp
  = HypPcImp PcImpHyp
  | HypEq
      { ifAt :: Bool
      , eq :: EqHyp
      }
  deriving (Eq, Generic, Ord, Show)

data PcImpHyp
  = PcImpHyp
      { lhs :: PcImpHypSide
      , rhs :: PcImpHypSide
      }
  deriving (Eq, Generic, Ord, Show)

data PcImpHypSide
  = PcImpHypSideBool Bool
  | PcImpHypSidePc VisitWithTag
  deriving (Eq, Generic, Ord, Show)

data EqHyp
  = EqHyp
      { lhs :: EqHypSide
      , rhs :: EqHypSide
      , induct :: Maybe EqHypInduct
      }
  deriving (Eq, Generic, Ord, Show)

data EqHypSide
  = EqHypSide
      { expr :: Expr
      , visit :: VisitWithTag
      }
  deriving (Eq, Generic, Ord, Show)

data EqHypInduct
  = EqHypInduct
      { a :: Integer
      , b :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

data VisitWithTag
  = VisitWithTag
      { visit :: Visit
      , tag :: Tag
      }
  deriving (Eq, Generic, Ord, Show)

data Visit
  = Visit
      { nodeId :: NodeId
      , restrs :: [Restr]
      }
  deriving (Eq, Generic, Ord, Show)

data Restr
  = Restr
      { nodeId :: NodeId
      , visitCount :: VisitCount
      }
  deriving (Eq, Generic, Ord, Show)

data VisitCount
  = VisitCount
      { numbers :: [Integer]
      , offsets :: [Integer]
      }
  deriving (Eq, Generic, Ord, Show)

--

instance ParseInLine Hyp where
    parseInLine = word >>= \case
        "PCImp" -> HypPcImp <$> parseInLine
        "Eq" -> HypEq False <$> parseInLine
        "EqIfAt" -> HypEq True <$> parseInLine
        _ -> fail "invalid pc imp hyp side"

instance BuildInLine Hyp where
    buildInLine = \case
        HypPcImp hyp -> putWord "PCImp" <> put hyp
        HypEq False hyp -> putWord "Eq" <> put hyp
        HypEq True hyp -> putWord "EqIfAt" <> put hyp

instance ParseInLine PcImpHyp where
    parseInLine = PcImpHyp <$> parseInLine <*> parseInLine

instance BuildInLine PcImpHyp where
    buildInLine hyp = put hyp.lhs <> put hyp.rhs

instance ParseInLine PcImpHypSide where
    parseInLine = word >>= \case
        "True" -> return $ PcImpHypSideBool True
        "False" -> return $ PcImpHypSideBool False
        "PC" -> PcImpHypSidePc <$> parseInLine
        _ -> fail "invalid pc imp hyp side"

instance BuildInLine PcImpHypSide where
    buildInLine = \case
        PcImpHypSideBool val -> putWord (show val)
        PcImpHypSidePc visit -> putWord "PC" <> put visit

instance ParseInLine EqHyp where
    parseInLine = do
        lhs <- parseInLine
        rhs <- parseInLine
        induct <- try (Nothing <$ (inLineSymbol "None" *> inLineSymbol "None"))
            <|> (Just <$> (EqHypInduct <$> parseInLine <*> parseInLine))
        return $ EqHyp lhs rhs induct

instance BuildInLine EqHyp where
    buildInLine hyp = put hyp.lhs <> put hyp.rhs <> case hyp.induct of
        Just induct -> putDec induct.a <> putDec induct.b
        Nothing -> putWord "None" <> putWord "None"

instance ParseInLine EqHypSide where
    parseInLine = EqHypSide <$> parseInLine <*> parseInLine

instance BuildInLine EqHypSide where
    buildInLine side = put side.expr <> put side.visit

instance ParseInLine VisitWithTag where
    parseInLine = VisitWithTag <$> parseInLine <*> parseInLine

instance BuildInLine VisitWithTag where
    buildInLine visit = put visit.visit <> put visit.tag

instance ParseInLine Visit where
    parseInLine = Visit <$> parseInLine <*> parseInLine

instance BuildInLine Visit where
    buildInLine visit = put visit.nodeId <> put visit.restrs

instance ParseInLine Restr where
    parseInLine = Restr <$> parseInLine <*> parseInLine

instance BuildInLine Restr where
    buildInLine restr = put restr.nodeId <> put restr.visitCount

instance ParseInLine VisitCount where
    parseInLine = inLineSymbol "VC" *> (VisitCount <$> parseInLine <*> parseInLine)

instance BuildInLine VisitCount where
    buildInLine visitCount = "VC" <> putManyWith putDec visitCount.numbers <> putManyWith putDec visitCount.offsets

--

type ProofChecksJsonAdapter = [ProofChecksJsonAdapterEntry]

data ProofChecksJsonAdapterEntry
  = ProofChecksJsonAdapterEntry
      { problem_name :: String
      , checks :: [ProofChecksJsonAdapterEntryChecks]
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON ProofChecksJsonAdapterEntry where
instance FromJSON ProofChecksJsonAdapterEntry where

data ProofChecksJsonAdapterEntryChecks
  = ProofChecksJsonAdapterEntryChecks
      { name :: String
      , hyp :: String
      , hyps :: [String]
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON ProofChecksJsonAdapterEntryChecks where
instance FromJSON ProofChecksJsonAdapterEntryChecks where

parseProofChecksForOneFile :: T.Text -> Either String (PairingId, [ProofCheck String])
parseProofChecksForOneFile s = do
    adapterEntry <- A.eitherDecodeStrict @ProofChecksJsonAdapterEntry (T.encodeUtf8 s)
    decodeAdapterEntry adapterEntry

parseProofChecksForManyFile :: T.Text -> Either String (ProofChecks String)
parseProofChecksForManyFile s = do
    adapterEntries <- decodeMany @ProofChecksJsonAdapterEntry s
    ProofChecks . M.fromList <$> traverse decodeAdapterEntry adapterEntries

parseWithin :: Parser a -> String -> T.Text -> Either String a
parseWithin p path = first errorBundlePretty . parse (p <* eof) path

decodeAdapterEntry :: ProofChecksJsonAdapterEntry -> Either String (PairingId, [ProofCheck String])
decodeAdapterEntry entry = do
    pairingId <- parseWithin
        (parseTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId)
        "a pairing id"
        (T.pack entry.problem_name)
    checks <- traverse (decodeAdapterCheck pairingId) entry.checks
    return $ (pairingId, checks)

decodeAdapterCheck :: PairingId -> ProofChecksJsonAdapterEntryChecks -> Either String (ProofCheck String)
decodeAdapterCheck pairingId check = do
    hyp <- parseWithin parseInLine (prettyPairingId pairingId ++ " hyp") (T.pack check.hyp)
    hyps <- traverse (parseWithin parseInLine (prettyPairingId pairingId ++ " hyps") . T.pack) check.hyps
    return $ ProofCheck
        { meta = check.name
        , hyp
        , hyps
        }

buildProofChecksForManyFile :: ProofChecks String -> Builder
buildProofChecksForManyFile checks = mconcat (map (uncurry buildProofChecksForOneFile) (M.toList checks.unwrap))

buildProofChecksForOneFile :: PairingId -> [ProofCheck String] -> Builder
buildProofChecksForOneFile pairingId checksForOne = A.encodeToTextBuilder entry <> "\n"
  where
    entry = ProofChecksJsonAdapterEntry
        { problem_name = L.unpack . B.toLazyText $ buildTypicalKeyFormat ["Problem", "Pairing"] (fromString (prettyPairingId pairingId))
        , checks = map g checksForOne
        }
    g check = ProofChecksJsonAdapterEntryChecks
        { name = check.meta
        , hyp = f check.hyp
        , hyps = map f check.hyps
        }
    f = L.unpack . B.toLazyText . buildStandaloneLine . buildInLine
