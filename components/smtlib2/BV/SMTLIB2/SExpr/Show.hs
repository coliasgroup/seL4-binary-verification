module BV.SMTLIB2.SExpr.Show
    ( showAtom
    , showGenericSExpr
    , showSExpr
    , showUncheckedSExpr
    ) where

import BV.SMTLIB2.SExpr
import BV.SMTLIB2.SExpr.Build

import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)

builderToString :: Builder -> String
builderToString = TL.unpack . toLazyText

showSExpr :: SExpr -> String
showSExpr = builderToString . buildSExpr

showUncheckedSExpr :: UncheckedSExpr -> String
showUncheckedSExpr = builderToString . buildUncheckedSExpr

showAtom :: Atom -> String
showAtom = builderToString . buildAtom

showGenericSExpr :: (a -> String) -> GenericSExpr a -> String
showGenericSExpr f sexpr = builderToString $ buildGenericSExpr (fromString . f) sexpr
