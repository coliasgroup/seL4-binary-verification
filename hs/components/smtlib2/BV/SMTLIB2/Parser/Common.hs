module BV.SMTLIB2.Parser.Common
    ( isValidSExprKeywordChar
    ) where

import Data.Char

isValidSExprKeywordChar :: Char -> Bool
isValidSExprKeywordChar c = isLetter c || isDigit c || c `elem` ("~!@$%^&*_-+=<>.?/" :: String)
