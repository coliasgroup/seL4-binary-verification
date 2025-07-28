
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.CompileProofChecks.Structs
    ( globalWrapperT
    , initStructsEnv
    ) where

import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils

import Data.Map (Map)
import qualified Data.Map as M
import Optics
import Text.Printf (printf)

initStructsEnv :: ROData -> Problem' -> Map Ident Struct -> (Ident -> Struct)
initStructsEnv rodata problem cStructs = (M.!) $ augmentStructs rodata problem cStructs

augmentStructs :: ROData -> Problem' -> Map Ident Struct -> Map Ident Struct
augmentStructs rodata problem cStructs =
    nonGlobal <> global
  where
    rodataStructs = rodataStructsOf rodata
    rodataStructTypes = [ structT name | name <- M.keys rodataStructs ]
    nonGlobal = cStructs <> rodataStructs
    global = M.fromList
        [ (globalWrapperStructNameOf ty, globalWrapperStructUsing nonGlobal ty)
        | ty <- toWrap
        ]
    toWrap = pglobalValidsToWrap <> rodataStructTypes
    pglobalValidsToWrap = problem.nodes ^.. folded % traverseTopLevelLevelExprs % foldExprs % afolding isPGlobalValid
    isPGlobalValid expr = case expr.value of
        ExprValueOp OpPGlobalValid args ->
            let [_, tyExpr, _] = args
                Expr { ty = ExprTypeType, value = ExprValueType ty } = tyExpr
             in Just ty
        _ -> Nothing

globalWrapperStructNameOf :: ExprType -> Ident
globalWrapperStructNameOf ty = Ident $ printf "Global (%s)" (nameOfType ty)

globalWrapperStructUsing :: Map Ident Struct -> ExprType -> Struct
globalWrapperStructUsing structs ty = Struct
    { size = withStructs (structs !@) $ sizeOfType ty
    , align = withStructs (structs !@) $ alignOfType ty
    , fields = M.fromList
        [ ( Ident "v"
          , StructField
                { ty
                , offset = 0
                }
          )
        ]
    }

nameOfType :: ExprType -> String
nameOfType = go
  where
    go a = case a of
        ExprTypeBool -> "Bool"
        ExprTypeMem -> "Mem"
        ExprTypeDom -> "Dom"
        ExprTypeHtd -> "HTD"
        ExprTypePms -> "PMS"
        ExprTypeUnit -> "UNIT"
        ExprTypeType -> "Type"
        ExprTypeToken -> "Token"
        ExprTypeRelWrapper -> "RelWrapper"
        ExprTypeWord { bits } -> unwords ["Word", show bits]
        ExprTypeWordArray { len, bits } -> unwords ["WordArray", show len, show bits]
        ExprTypeArray { ty, len } -> unwords ["Array", go ty, show len]
        ExprTypeStruct ident -> unwords ["Struct", ident.unwrap]
        ExprTypePtr ty -> unwords ["Ptr", go ty]

globalWrapperT :: ExprType -> ExprType
globalWrapperT = structT . globalWrapperStructNameOf
