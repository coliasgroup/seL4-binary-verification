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

import Data.Aeson
import Data.Aeson.Encoding as A
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as AP
import Data.Char (isSpace)
import Data.String (fromString)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

instance FromJSON PairingId where
    parseJSON = parseText parsePrettyPairingId

instance ToJSON PairingId where
    toJSON = fromString . prettyPairingId

instance FromJSONKey PairingId where
    fromJSONKey = FromJSONKeyTextParser $ parseText' parsePrettyPairingId

instance ToJSONKey PairingId where
    toJSONKey = ToJSONKeyText (fromString . prettyPairingId) (A.string . prettyPairingId)

deriving instance FromJSON InlineScripts

deriving instance ToJSON InlineScripts

instance FromJSON InlineScriptEntry where
    parseJSON = parseLine

instance ToJSON InlineScriptEntry where
    toJSON = buildLine

deriving instance FromJSON Pairings

deriving instance ToJSON Pairings

instance FromJSON Pairing where
    parseJSON = withObject "Pairing" $ \v -> Pairing
        <$> v .: "in"
        <*> v .: "out"

instance ToJSON Pairing where
    toJSON v = object [ "in" .= v.inEqs, "out" .= v.outEqs ]

instance FromJSON PairingEq where
    parseJSON = parseLine

instance ToJSON PairingEq where
    toJSON = buildLine

deriving instance FromJSON (Proofs ())

deriving instance ToJSON (Proofs ())

instance FromJSON (ProofScript ()) where
    parseJSON = parseLine

instance ToJSON (ProofScript ()) where
    toJSON = buildLine

deriving instance FromJSON (CompatProofChecks String)

deriving instance ToJSON (CompatProofChecks String)

instance FromJSON (ProofCheck String) where

instance ToJSON (ProofCheck String) where

instance FromJSON Hyp where
    parseJSON = parseLine

instance ToJSON Hyp where
    toJSON = buildLine

deriving instance FromJSON (CompatSMTProofChecks ())

deriving instance ToJSON (CompatSMTProofChecks ())

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

parsePrettyPairingId :: M.Parsec Void TL.Text PairingId
parsePrettyPairingId = do
    asm <- ident
    M.hspace *> "(ASM)" *> M.hspace *> "<=" *> M.hspace
    c <- ident
    M.hspace *> "(C)"
    return $ PairingOf { asm, c }
  where
    ident = Ident <$> M.some (M.satisfy isIdentChar)
    isIdentChar c = not (isSpace c || c == '(' || c == ')')
