{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.FastInstances
    (
    ) where

import Data.Aeson (FromJSON, Result (..), ToJSON, fromJSON)
import Data.Aeson.Parser (json)
import qualified Data.Aeson.Text as A
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void)
import GHC.Generics
import qualified Text.Megaparsec as MP

import BV.Core.Types

import BV.ConcreteSyntax.FastParsing
import BV.ConcreteSyntax.Instances (parsePrettyPairingId)
import BV.ConcreteSyntax.Parsing (parseInLine, parseTypicalKeyFormat)
import BV.ConcreteSyntax.Printing
import Data.Monoid (Endo (..))

parseWithin :: MP.Parsec Void T.Text a -> String -> T.Text -> Either String a
parseWithin p path = first MP.errorBundlePretty . MP.parse (p <* MP.eof) path

decodeMany :: FromJSON a => Parser [a]
decodeMany = (many' $ ((fromJSON <$> (json <?> "json")) >>= \case
    Success x -> return x
    Error err -> fail err) <* space)

instance ParseFileFast (ProofChecks String) where
    parseFileFast = do
        checks <- decodeMany >>= mapM (\x -> case decodeAdapterEntry x of
            Right x -> return x
            Left err -> fail err)
        return $ ProofChecks (M.fromList checks)

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

decodeAdapterEntry :: ProofChecksJsonAdapterEntry -> Either String (PairingId, [ProofCheck String])
decodeAdapterEntry entry = do
    pairingId <- parseWithin
        (parseTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId)
        "a pairing id"
        (T.pack entry.problem_name)
    checks <- traverse (decodeAdapterCheck pairingId) entry.checks
    return (pairingId, checks)

decodeAdapterCheck :: PairingId -> ProofChecksJsonAdapterEntryChecks -> Either String (ProofCheck String)
decodeAdapterCheck pairingId check = do
    hyp <- parseWithin parseInLine (prettyPairingId pairingId ++ " hyp") (T.pack check.hyp)
    hyps <- traverse (parseWithin parseInLine (prettyPairingId pairingId ++ " hyps") . T.pack) check.hyps
    return $ ProofCheck
        { meta = check.name
        , hyp
        , hyps
        }

instance BuildToFile (ProofChecks String) where
    buildToFile = buildProofChecksForManyFile

buildProofChecksForManyFile :: ProofChecks String -> Builder
buildProofChecksForManyFile checks = mconcat (map (uncurry buildProofChecksForOneFile) (M.toList checks.unwrap))

buildProofChecksForOneFile :: PairingId -> [ProofCheck String] -> Builder
buildProofChecksForOneFile pairingId checksForOne = A.encodeToTextBuilder entry <> "\n"
  where
    entry = ProofChecksJsonAdapterEntry
        { problem_name = TL.unpack . TB.toLazyText $ buildTypicalKeyFormat ["Problem", "Pairing"] (fromString (prettyPairingId pairingId))
        , checks = map g checksForOne
        }
    g check = ProofChecksJsonAdapterEntryChecks
        { name = check.meta
        , hyp = f check.hyp
        , hyps = map f check.hyps
        }
    f = TL.unpack . TB.toLazyText . buildStandaloneLine . buildInLine

-- --

instance ParseFileFast SmtProofChecks where
    parseFileFast = do
        groups <- decodeMany >>= mapM (\x -> case decodeSmtAdapterEntry x of
            Right x -> return x
            Left err -> fail err)
        let x = map (\(k, v) -> M.insertWith (++) k [v]) groups
        return . SmtProofChecks . ($ M.empty) . appEndo . mconcat . map Endo $ x


-- >>> import Data.Aeson (Value)
-- >>> import qualified Data.ByteString.Char8 as B
-- >>> x <- B.readFile "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.json"
-- >>> parseOnly (parseFileFast @(SmtProofChecks)) x


-- >>> :{
--     do
--      x <- B.readFile "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.json"
--      print $ parseOnly (decodeMany @(Value)) x
-- :}
-- Couldn't match expected type `ByteString'
--             with actual type `IO ByteString'
-- In the second argument of `parseOnly', namely
--   `(readFile
--       "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.json")'
-- In the expression:
--   parseOnly
--     (decodeMany @(Value))
--     (readFile
--        "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.json")
-- In an equation for `it_amR0s':
--     it_amR0s
--       = parseOnly
--           (decodeMany @(Value))
--           (readFile
--              "/home/x/i/v/seL4-verification-reproducibility/projects/bv-sandbox/hs/tmp/target-small-smt/smt-proof-checks.json")

--

data SmtProofChecksJsonAdapterEntry
  = SmtProofChecksJsonAdapterEntry
      { problem_name :: String
      , check_group :: SmtProofChecksJsonAdapterEntryCheckGroup
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON SmtProofChecksJsonAdapterEntry where
instance FromJSON SmtProofChecksJsonAdapterEntry where

data SmtProofChecksJsonAdapterEntryCheckGroup
  = SmtProofChecksJsonAdapterEntryCheckGroup
      { setup :: [String]
      , imps :: [String]
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON SmtProofChecksJsonAdapterEntryCheckGroup where
instance FromJSON SmtProofChecksJsonAdapterEntryCheckGroup where

decodeSmtAdapterEntry :: SmtProofChecksJsonAdapterEntry -> Either String (PairingId, SmtProofCheckGroup)
decodeSmtAdapterEntry entry = do
    pairingId <- parseWithin
        (parseTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId)
        "a pairing id"
        (T.pack entry.problem_name)
    group <- decodeSmtAdapterCheckGroup pairingId entry.check_group
    return (pairingId, group)

decodeSmtAdapterCheckGroup :: PairingId -> SmtProofChecksJsonAdapterEntryCheckGroup -> Either String SmtProofCheckGroup
decodeSmtAdapterCheckGroup pairingId group = do
    setup <- return group.setup
    imps <- return group.imps
    return $ SmtProofCheckGroup
        { setup
        , imps
        }

instance BuildToFile SmtProofChecks where
    buildToFile = buildSmtProofChecksForManyFile

buildSmtProofChecksForManyFile :: SmtProofChecks -> Builder
buildSmtProofChecksForManyFile checks = mconcat $ mconcat (map (\(k, v) -> map (buildSmtProofChecksForOneFile k) v) (M.toList checks.unwrap))

buildSmtProofChecksForOneFile :: PairingId -> SmtProofCheckGroup -> Builder
buildSmtProofChecksForOneFile pairingId group = A.encodeToTextBuilder entry <> "\n"
  where
    entry = SmtProofChecksJsonAdapterEntry
        { problem_name = TL.unpack . TB.toLazyText $ buildTypicalKeyFormat ["Problem", "Pairing"] (fromString (prettyPairingId pairingId))
        , check_group = SmtProofChecksJsonAdapterEntryCheckGroup
            { setup = group.setup
            , imps = group.imps
            }
        }
