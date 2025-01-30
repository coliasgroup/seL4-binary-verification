{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.ConcreteSyntax.ObjDump
    (
    ) where

import BV.ConcreteSyntax.Classes
import BV.Core.Types

import Data.Bifunctor (first)
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Monoid (Endo (Endo, appEndo))
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void TL.Text

parseLines :: Parser [(String, Symbol)]
parseLines = do
    _ <- skipManyTill anySingle (string "SYMBOL TABLE:" *> eol)
    lines <- many $ do
        addr <- L.hexadecimal
        _ <- char ' ' *> skipCount 7 anySingle *> char ' '
        section <- ident
        _ <- char '\t'
        size <- L.hexadecimal
        _ <- char ' '
        name <- ident
        _ <- eol
        let symbol = Symbol
                { addr
                , size
                , section
                }
        return (name, symbol)
    _ <- skipSome eol
    return lines
  where
    ident = some (satisfy (not . isSpace))

makeObjDumpInfo :: [(String, Symbol)] -> ObjDumpInfo
makeObjDumpInfo lines =
    ObjDumpInfo
        { symbols
        , sections
        }
  where
    symbols = M.fromList lines
    sections = appEndo (foldMap (Endo . f) (M.elems symbols)) M.empty
    f symbol = flip M.alter symbol.section $ \entry -> Just $ case entry of
        Just section ->
            let addr = min section.addr symbol.addr
                end = max (sectionEnd section) (symbolEnd symbol)
             in Section
                    { addr
                    , size = end - addr
                    }
        Nothing ->
            Section
                { addr = symbol.addr
                , size = symbol.size
                }

parseObjDumpInfo :: Parsec Void TL.Text ObjDumpInfo
parseObjDumpInfo = makeObjDumpInfo <$> parseLines

instance ReadBVFile TL.Text ObjDumpInfo where
    readBVContents fp = first errorBundlePretty . parse (parseObjDumpInfo <* eof) fp
