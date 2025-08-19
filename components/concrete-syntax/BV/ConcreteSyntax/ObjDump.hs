{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BV.ConcreteSyntax.ObjDump
    ( readROData
    ) where

import BV.ConcreteSyntax.Classes
import BV.Core.Types

import Control.Monad (guard, (<$!>))
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Endo (Endo, appEndo))
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void TL.Text

readUsingParser :: Parsec Void TL.Text a -> FilePath -> TL.Text -> Either String a
readUsingParser p fp = first errorBundlePretty . parse (p <* eof) fp

--

type SymtabLine = (String, Symbol)

parseSymtabLines :: Parser [SymtabLine]
parseSymtabLines = do
    _ <- skipManyTill anySingle (string "SYMBOL TABLE:" *> eol)
    lines_ <- many $ do
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
    return lines_
  where
    ident = some (satisfy (not . isSpace))

makeObjDumpInfo :: [SymtabLine] -> ObjDumpInfo
makeObjDumpInfo lines_ =
    ObjDumpInfo
        { symbols
        , sections
        }
  where
    symbols = M.fromList lines_
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
parseObjDumpInfo = makeObjDumpInfo <$> parseSymtabLines

instance ReadBVFile TL.Text ObjDumpInfo where
    readBVContents = readUsingParser parseObjDumpInfo

--

data RODataFile
  = RODataFile
      { wordSize :: Integer
      , map :: [(Integer, Integer)]
      }

parseRODataFile :: Parser RODataFile
parseRODataFile = do
    skipMany eol
    skipManyTill anySingle ":"
    skipManyTill anySingle "file format "
    formatName <- manyTill anySingle eol
    let wordSize = case formatName of
            "elf32-littlearm" -> 4
            _ -> error $ "unrecognized format: " ++ formatName
    map_ <- catMaybes <$> many (optional (try assocP) <* skipManyTill anySingle eol)
    return $ RODataFile
        { wordSize
        , map = map_
        }
  where
    assocP = (,) <$> (L.hexadecimal <* ":\t") <*> L.hexadecimal

makeROData :: ObjDumpInfo -> RODataInputRanges -> RODataFile -> ROData
makeROData objDumpInfo inputRanges file =
    ROData
        { ranges
        , rodata
        }
  where
    ranges = sort [ lookupRODataRange objDumpInfo ty name | (ty, name) <- inputRanges ]
    rangesMap = M.fromList . normalizeRanges . sort $
        [ (range.addr, range.addr + range.size)
        | range <- ranges
        ]
    rodata = M.fromList . flip filter file.map $ \(addr, _value) ->
        containsRange (addr, addr + file.wordSize) rangesMap

lookupRODataRange :: ObjDumpInfo -> RODataInputRangeType -> String -> RODataRange
lookupRODataRange objDumpInfo ty name = case ty of
    RODataInputRangeTypeSection ->
        let section = objDumpInfo.sections M.! name
        in RODataRange
            { addr = section.addr
            , size = section.size
            }
    RODataInputRangeTypeSymbol ->
        let symbol = objDumpInfo.symbols M.! name
        in RODataRange
            { addr = symbol.addr
            , size = symbol.size
            }

normalizeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
normalizeRanges ranges = reverse $ go [] ranges
  where
    go xs [] = xs
    go [] (y:ys) = go [y] ys
    go (x@(startX, endX):xs) (y@(startY, endY):ys) =
        if endX < startY
        then go (y:x:xs) ys
        else go ((startX, endY):ys) ys

containsRange :: (Integer, Integer) -> M.Map Integer Integer -> Bool
containsRange (start, end) ranges = fromJust $ do
    case M.lookupGT start ranges of
        Just (start', _) -> guard $ end <= start'
        Nothing -> return ()
    case M.lookupLE start ranges of
        Just (_, end') ->
            if end' <= start
            then return False
            else if end' < end
            then empty
            else return True
        Nothing -> return False

parseROData :: ObjDumpInfo -> RODataInputRanges -> Parsec Void TL.Text ROData
parseROData objDumpInfo ranges = makeROData objDumpInfo ranges <$!> parseRODataFile

readROData :: ObjDumpInfo -> RODataInputRanges -> FilePath -> TL.Text -> Either String ROData
readROData objDumpInfo ranges = readUsingParser $ parseROData objDumpInfo ranges
