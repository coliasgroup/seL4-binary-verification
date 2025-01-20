{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module BV.ConcreteSyntax.FastInstances
    (
    ) where

import Data.Attoparsec.Text
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Monoid (Endo (..))

import BV.Core.Types

import BV.ConcreteSyntax.FastParsing
import BV.ConcreteSyntax.SExprWithPlaceholdersFast

parsePrettyPairingId :: Parser PairingId
parsePrettyPairingId = do
    asm <- ident
    hspace *> "(ASM)" *> hspace *> "<=" *> hspace
    c <- ident
    hspace *> "(C)"
    return $ PairingOf { asm, c }
  where
    ident = Ident <$> many1 (satisfy isIdentChar)
    isIdentChar c = not (isSpace c || c == '(' || c == ')')

instance ParseFileFast (FlattenedSMTProofChecks ()) where
    parseFileFast = do
        blocks <- parseBlocksFileWithTypicalKeyFormat ["Problem", "Pairing"] parsePrettyPairingId $ do
            setupLen <- decimal <* endOfLine
            impsLen <- decimal <* endOfLine
            setup <- count setupLen (parseSExprWithPlaceholdersFast <* ignoredLines)
            imps <- count impsLen (SMTProofCheckImp () <$> parseSExprWithPlaceholdersFast <* ignoredLines)
            return $ SMTProofCheckGroup { setup, imps }
        let x = map (\(k, v) -> M.insertWith (++) k [v]) blocks
        return . FlattenedSMTProofChecks . ($ M.empty) . appEndo . foldMap Endo $ x
