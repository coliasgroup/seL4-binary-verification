module BV.ConcreteSyntax.FastParsing
    ( ParseFileFast (..)
    , parseWholeFileFast
    , parseWholeFileFastWith
    ) where

import Data.Attoparsec.ByteString
import Data.ByteString as B

class ParseFileFast a where
    parseFileFast :: Parser a

parseWholeFileFast :: ParseFileFast a => B.ByteString -> Either String a
parseWholeFileFast = parseWholeFileFastWith parseFileFast

parseWholeFileFastWith :: Parser a -> B.ByteString -> Either String a
parseWholeFileFastWith p = parseOnly (p <* endOfInput)
