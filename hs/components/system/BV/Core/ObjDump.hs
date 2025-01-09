{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.Core.ObjDump
    ( ObjDumpInfo (..)
    , Section (..)
    , Symbol (..)
    , parseObjDumpInfo
    ) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Endo (Endo, appEndo))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics.Core
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import BV.ConcreteSyntax.Parsing

data ObjDumpInfo
  = ObjDumpInfo
      { symbols :: Map String Symbol
      , sections :: Map String Section
      }
  deriving (Eq, Generic, Ord, Show)

data Symbol
  = Symbol
      { addr :: Integer
      , size :: Integer
      , section :: String
      }
  deriving (Eq, Generic, Ord, Show)

data Section
  = Section
      { addr :: Integer
      , size :: Integer
      }
  deriving (Eq, Generic, Ord, Show)

symbolEnd :: Symbol -> Integer
symbolEnd = (+) <$> view #addr <*> view #size

sectionEnd :: Section -> Integer
sectionEnd = (+) <$> view #addr <*> view #size

parseLines :: Parser [(String, Symbol)]
parseLines = do
    _ <- skipManyTill anySingle (string "SYMBOL TABLE:" *> eol)
    lines <- many $ do
        addr <- hexadecimal
        _ <- char ' ' *> skipCount 7 anySingle *> char ' '
        section <- ident
        _ <- char '\t'
        size <- hexadecimal
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
    sections = appEndo (mconcat (map (Endo . f) (M.elems symbols))) M.empty
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

parseObjDumpInfo :: Parsec Void Text ObjDumpInfo
parseObjDumpInfo = makeObjDumpInfo <$> parseLines

instance ParseFile ObjDumpInfo where
    parseFile = parseObjDumpInfo
