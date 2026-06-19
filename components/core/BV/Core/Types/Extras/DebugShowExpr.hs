{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Types.Extras.DebugShowExpr
    ( debugExprToSExpr
    , debugShowExpr
    ) where

import BV.Core.Types
import BV.SMTLIB2
import BV.Utils (ensure)

import Text.Printf (printf)
import Data.Maybe (fromMaybe)

debugShowExpr :: Expr c -> String
debugShowExpr = showSExpr . debugExprToSExpr

debugExprToSExpr :: Expr c -> SExpr
debugExprToSExpr expr = case expr.value of
    ExprValueVar name -> Atom $ stringAtom name.unwrap
    ExprValueNum n ->
        let ExprTypeWord bits = expr.ty
         in wordToSExpr bits n
    ExprValueToken tok -> [Atom (keywordAtom "token"), Atom (stringAtom tok.unwrap)]
    ExprValueOp op args ->
        let op' = fromMaybe (Atom (stringAtom ("?" ++ show op))) $ opToSExpr expr.ty op (map (.ty) args)
            args' = map debugExprToSExpr args
         in case args' of
                [] -> op'
                _ -> List $ [op'] ++ args'
    ExprValueType ty ->
        -- TODO hack
        Atom $ stringAtom $ show ty

wordToSExpr :: Integer -> Integer -> SExpr
wordToSExpr bits n =
    ensure (bits > 0 && n >= 0 && n < 2^bits) $
        Atom $ f (printf ("%0." ++ show cols ++ fmt) n)
  where
    (f, cols, fmt) = case bits `quotRem` 4 of
        (q, 0) -> (hexadecimalAtom, q, "x")
        _ -> (binaryAtom, bits, "b")

opToSExpr :: ExprType -> Op -> [ExprType] -> Maybe SExpr
opToSExpr ty op argTypes = case op of
    OpEquals -> Just "="
    OpPlus -> Just "bvadd"
    OpMinus -> Just "bvsub"
    OpTimes -> Just "bvmul"
    OpModulus -> Just "bvurem"
    OpDividedBy -> Just "bvudiv"
    OpBWAnd -> Just "bvand"
    OpBWOr -> Just "bvor"
    OpBWXOR -> Just "bvxor"
    OpAnd -> Just "and"
    OpOr -> Just "or"
    OpImplies -> Just "=>"
    OpLess -> Just "bvult"
    OpLessEquals -> Just "bvule"
    OpSignedLess -> Just "bvslt"
    OpSignedLessEquals -> Just "bvsle"
    OpShiftLeft -> Just "bvshl"
    OpShiftRight -> Just "bvlshr"
    OpSignedShiftRight -> Just "bvashr"
    OpNot -> Just "not"
    OpBWNot -> Just "bvnot"
    OpTrue -> Just "true"
    OpFalse -> Just "false"
    OpUnspecifiedPrecond -> Just "unspecified-precond"
    OpIfThenElse -> Just "ite"
    OpMemDom -> Just "mem-dom"
    OpWordArrayAccess -> Just "select"
    OpWordArrayUpdate -> Just "store"
    OpCountLeadingZeroes -> Just "bvclz"
    OpWordCast -> Just "bvucast"
    OpWordCastSigned -> Just "bvscast"
    OpWordReverse -> Just "bvrev"
    OpMemAcc -> Just $
        let ExprTypeWord bits = ty
         in Atom $ symbolAtom $ "load-word" ++ show bits
    OpMemUpdate -> Just $
        let [_, _, ExprTypeWord bits] = argTypes
         in Atom $ symbolAtom $ "store-word" ++ show bits
    _ -> Nothing
    -- _ -> error $ "unhandled: " ++ show op
