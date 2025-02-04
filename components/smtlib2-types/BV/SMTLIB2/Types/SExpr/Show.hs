module BV.SMTLIB2.Types.SExpr.Show
    ( showSExpr
    , showUncheckedSExpr
    , showAtom
    ) where

import BV.SMTLIB2.Types.SExpr
import BV.SMTLIB2.Types.SExpr.Build

import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText, Builder)

builderToString :: Builder -> String
builderToString = TL.unpack . toLazyText

showSExpr :: SExpr -> String
showSExpr = builderToString . buildSExpr

showUncheckedSExpr :: UncheckedSExpr -> String
showUncheckedSExpr = builderToString . buildUncheckedSExpr

showAtom :: Atom -> String
showAtom = builderToString . buildAtom
