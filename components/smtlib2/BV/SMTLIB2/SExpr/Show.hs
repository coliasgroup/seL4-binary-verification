module BV.SMTLIB2.SExpr.Show
    ( showAtom
    , showSExpr
    , showUncheckedSExpr
    ) where

import BV.SMTLIB2.SExpr
import BV.SMTLIB2.SExpr.Build

import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)

builderToString :: Builder -> String
builderToString = TL.unpack . toLazyText

showSExpr :: SExpr -> String
showSExpr = builderToString . buildSExpr

showUncheckedSExpr :: UncheckedSExpr -> String
showUncheckedSExpr = builderToString . buildUncheckedSExpr

showAtom :: Atom -> String
showAtom = builderToString . buildAtom
