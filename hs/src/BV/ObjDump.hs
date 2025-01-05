{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}

module BV.ObjDump
    ( ObjDumpInfo (..)
    , Section (..)
    , Symbol (..)
    , parseObjDumpInfo
    ) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Endo (Endo, appEndo))
import GHC.Generics (Generic)
import Numeric (readHex)
import Optics.Core
import Text.Parsec
import Text.Parsec.Text

data ObjDumpInfo
  = ObjDumpInfo
      { symbols :: Map String Symbol
      , sections :: Map String Section
      }
  deriving (Generic, Show)

data Symbol
  = Symbol
      { addr :: Integer
      , size :: Integer
      , section :: String
      }
  deriving (Generic, Show)

data Section
  = Section
      { addr :: Integer
      , size :: Integer
      }
  deriving (Generic, Show)

symbolEnd :: Symbol -> Integer
symbolEnd = (+) <$> view #addr <*> view #size

sectionEnd :: Section -> Integer
sectionEnd = (+) <$> view #addr <*> view #size

parseLines :: Parser [(String, Symbol)]
parseLines = do
    _ <- manyTill anyChar (try (string "SYMBOL TABLE:" >> newline))
    lines <- many $ do
        addr <- hex
        _ <- char ' ' >> count 7 anyChar >> char ' '
        section <- ident
        _ <- tab
        size <- hex
        _ <- char ' '
        name <- ident
        _ <- newline
        let symbol = Symbol
                { addr
                , size
                , section
                }
        return (name, symbol)
    _ <- many newline
    eof
    return lines
  where
    ident = many1 (satisfy (not . isSpace))
    hex = readHex' <$> many1 hexDigit

readHex' :: (Eq a, Num a) => String -> a
readHex' s = let [(x, "")] = readHex s in x

makeObjDumpInfo :: [(String, Symbol)] -> ObjDumpInfo
makeObjDumpInfo lines =
    ObjDumpInfo
        { symbols
        , sections
        }
  where
    symbols = M.fromList lines
    sections = appEndo (mconcat (map (Endo . f) (M.elems symbols))) M.empty
    f symbol = flip M.alter (symbol ^. #section) $ \entry -> Just $ case entry of
        Just section ->
            let addr = min (view #addr section) (view #addr symbol)
                end = max (sectionEnd section) (symbolEnd symbol)
             in Section
                    { addr
                    , size = end - addr
                    }
        Nothing ->
            Section
                { addr = view #addr symbol
                , size = view #size symbol
                }

parseObjDumpInfo :: Parser ObjDumpInfo
parseObjDumpInfo = makeObjDumpInfo <$> parseLines
