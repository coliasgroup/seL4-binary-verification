{-# LANGUAGE OverloadedStrings #-}

module BV.ConcreteSyntax.FastInstances
    (
    ) where

import Data.Aeson (FromJSON, Result (..), ToJSON, fromJSON)
import Data.Aeson.Parser (json, json')
import qualified Data.Aeson.Text as A
import Data.Attoparsec.Text
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Monoid (Endo (..))
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Void (Void)
import GHC.Generics
import qualified Text.Megaparsec as MP

import BV.Core.Types

import BV.ConcreteSyntax.FastParsing
import qualified BV.ConcreteSyntax.Instances as Slow
import qualified BV.ConcreteSyntax.Parsing as Slow
import BV.ConcreteSyntax.Printing
import BV.ConcreteSyntax.SExpr
import Data.Char (isSpace)

parsePrettyPairingId :: Parser PairingId
parsePrettyPairingId = do
    asm <- ident
    hspace *> "(ASM)" *> hspace *> "<=" *> hspace
    c <- ident
    hspace *> "(C)"
    return $ PairingId { asm, c }
  where
    ident = Ident <$> many1 (satisfy isIdentChar)
    isIdentChar c = not (isSpace c || c == '(' || c == ')')

--

parseWithin :: MP.Parsec Void T.Text a -> String -> T.Text -> Either String a
parseWithin p path = first MP.errorBundlePretty . MP.parse (p <* MP.eof) path

parseWithin' :: Parser a -> String -> T.Text -> Either String a
parseWithin' p path = parseOnly (p <* endOfInput)

instance ParseFileFast (ProofChecks String) where
    parseFileFast = undefined

instance BuildToFile (ProofChecks String) where
    buildToFile = undefined

-- --

instance ParseFileFast SmtProofChecks where
    parseFileFast = do
        blocks <- parseBlocksFileWithTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId $ do
            setupLen <- decimal <* endOfLine
            impsLen <- decimal <* endOfLine
            setup <- count setupLen parseSExpr
            imps <- count impsLen parseSExpr
            return $ SmtProofCheckGroup { setup, imps }
        let x = map (\(k, v) -> M.insertWith (++) k [v]) blocks
        return . SmtProofChecks . ($ M.empty) . appEndo . mconcat . map Endo $ x

instance BuildToFile SmtProofChecks where
    buildToFile checks = mconcat $ mconcat (map (\(k, v) -> map (buildGroup k) v) (M.toList checks.unwrap))
      where
        buildGroup pairingId (SmtProofCheckGroup { setup, imps }) =
            mconcat . (map (<> "\n")) $
                [ buildTypicalKeyFormat ["Problem", "Pairing"] (TB.fromString (prettyPairingId pairingId)) <> " {"
                , TB.decimal (length setup)
                , TB.decimal (length imps)
                ] ++ f setup ++ f imps ++
                [ "}"
                ]
        f ss = map buildSExpr ss
