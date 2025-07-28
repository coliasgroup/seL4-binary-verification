{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module BV.ConcreteSyntax.JSON
    (
    ) where

import BV.ConcreteSyntax.GraphLangLike.Building (BuildInLine, buildInLine,
                                                 buildStandaloneLine)
import BV.ConcreteSyntax.GraphLangLike.Instances ()
import BV.ConcreteSyntax.GraphLangLike.Parsing (ParseInLine, parseInLine)
import BV.ConcreteSyntax.SExprWithPlaceholders (buildSExprWithPlaceholders)
import BV.ConcreteSyntax.SExprWithPlaceholdersFaster (parseSExprWithPlaceholdersFaster)
import BV.Core.Types

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), FromJSONKey (..),
                   FromJSONKeyFunction (FromJSONKeyTextParser), ToJSON (..),
                   ToJSONKey (..), ToJSONKeyFunction (ToJSONKeyText),
                   Value (..), object, withObject, withText, (.:), (.=))
import Data.Aeson.Encoding as A
import Data.Aeson.Types (Parser)
import qualified Data.Attoparsec.Text as AP
import Data.Char (isSpace)
import Data.String (fromString)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

instance RefineTag t => FromJSON (PairingId t) where
    parseJSON = parseText parsePrettyPairingId

instance RefineTag t => ToJSON (PairingId t) where
    toJSON = fromString . prettyPairingId

instance RefineTag t => FromJSONKey (PairingId t) where
    fromJSONKey = FromJSONKeyTextParser $ parseText' parsePrettyPairingId

instance RefineTag t => ToJSONKey (PairingId t) where
    toJSONKey = ToJSONKeyText (fromString . prettyPairingId) (A.string . prettyPairingId)

deriving instance FromJSON InlineScripts

deriving instance ToJSON InlineScripts

instance FromJSON (InlineScriptEntry AsmRefineTag) where
    parseJSON = parseLine

instance ToJSON (InlineScriptEntry AsmRefineTag) where
    toJSON = buildLine

deriving instance FromJSON Pairings

deriving instance ToJSON Pairings

instance RefineTag t => FromJSON (Pairing t) where
    parseJSON = withObject "Pairing" $ \v -> Pairing
        <$> v .: "in"
        <*> v .: "out"

instance RefineTag t => ToJSON (Pairing t) where
    toJSON v = object [ "in" .= v.inEqs, "out" .= v.outEqs ]

instance RefineTag t => FromJSON (PairingEq t) where
    parseJSON = parseLine

instance RefineTag t => ToJSON (PairingEq t) where
    toJSON = buildLine

deriving instance FromJSON (Proofs ())

deriving instance ToJSON (Proofs ())

instance FromJSON (ProofScript ()) where
    parseJSON = parseLine

instance ToJSON (ProofScript ()) where
    toJSON = buildLine

deriving instance FromJSON CompatProofChecks

deriving instance ToJSON CompatProofChecks

instance FromJSON (ProofCheck String) where

instance ToJSON (ProofCheck String) where

instance FromJSON Hyp where
    parseJSON = parseLine

instance ToJSON Hyp where
    toJSON = buildLine

deriving instance FromJSON CompatSMTProofChecks

deriving instance ToJSON CompatSMTProofChecks

instance FromJSON (SMTProofCheckGroup ()) where

instance ToJSON (SMTProofCheckGroup ()) where

instance FromJSON (SMTProofCheckImp ()) where

instance ToJSON (SMTProofCheckImp ()) where

instance FromJSON SExprWithPlaceholders where
    parseJSON = parseTextFaster parseSExprWithPlaceholdersFaster

instance ToJSON SExprWithPlaceholders where
    toJSON = fromBuilder . buildSExprWithPlaceholders

parseText :: M.Parsec Void TL.Text a -> Value -> Parser a
parseText p = withText "x" $ parseText' p

parseText' :: M.Parsec Void TL.Text a -> TS.Text -> Parser a
parseText' p = either (fail . M.errorBundlePretty) pure . M.parse (p <* M.eof) "" . TL.fromStrict

parseTextFaster :: AP.Parser a -> Value -> Parser a
parseTextFaster p = withText "x" $ parseTextFaster' p

parseTextFaster' :: AP.Parser a -> TS.Text -> Parser a
parseTextFaster' p = either fail pure <$> AP.parseOnly (p <* AP.endOfInput)

parseLine :: ParseInLine a => Value -> Parser a
parseLine = parseText parseInLine

buildLine :: BuildInLine a => a -> Value
buildLine = fromBuilder . buildStandaloneLine . buildInLine

fromBuilder :: Builder -> Value
fromBuilder = String . TL.toStrict . toLazyText

parsePrettyPairingId :: forall t. RefineTag t => M.Parsec Void TL.Text (PairingId t)
parsePrettyPairingId = do
    l <- ident
    M.hspace *> tag leftTag *> M.hspace *> "<=" *> M.hspace
    r <- ident
    M.hspace *> tag rightTag
    return $ byRefineTag l r
  where
    ident = Ident <$> M.some (M.satisfy isIdentChar)
    isIdentChar c = not (isSpace c || c == '(' || c == ')')
    tag :: t -> M.Parsec Void TL.Text ()
    tag t = do
        "("
        M.try (fromString (prettyTag t)) <|> fail "unrecognized tag"
        ")"
        return ()
