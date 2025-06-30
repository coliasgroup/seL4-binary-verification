{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.CompileProofChecks.Structs
    ( globalWrapperT
    , initStructsEnv
    , rodataPtrsOf
    ) where

import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Optics
import Text.Printf (printf)

initStructsEnv :: ROData -> Map Ident Struct -> Problem -> (Ident -> Struct)
initStructsEnv rodata cStructs problem = (M.!) $ augmentStructs rodata cStructs problem

augmentStructs :: ROData -> Map Ident Struct -> Problem -> Map Ident Struct
augmentStructs rodata cStructs problem =
    nonGlobal <> global
  where
    rodataStructs = rodataStructsWith rodata
    rodataStructTypes = [ structT name | name <- M.keys rodataStructs ]
    nonGlobal = cStructs <> rodataStructs
    global = M.fromList
        [ (globalWrapperStructName ty, globalWrapperStructWith nonGlobal ty)
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

globalWrapperStructName :: ExprType -> Ident
globalWrapperStructName ty = Ident $ printf "Global (%s)" (typeName ty)

globalWrapperStructWith :: Map Ident Struct -> ExprType -> Struct
globalWrapperStructWith structs ty = Struct
    { size = withStructs (structs !@) $ sizeOfType ty
    , align = withStructs (structs !@) $ alignOfType ty
    , fields =
        [ ( "v"
          , StructField
                { ty
                , offset = 0
                }
          )
        ]
    }

rodataStructsWith :: ROData -> Map Ident Struct
rodataStructsWith rodata =
    M.fromList
        [ let struct = Struct
                { size = range.size
                , align = 1
                , fields = M.empty
                }
           in (structName, struct)
        | (structName, range) <- zip (rodataStructNames rodata) rodata.ranges
        ]

rodataStructNames :: ROData -> [Ident]
rodataStructNames rodata =
    [ Ident $ case rodata.ranges of
            [_] -> "rodata_struct"
            _ -> printf "rodata_struct_%d" i
    | (i :: Integer, _) <- zip [1..] rodata.ranges
    ]

typeName :: ExprType -> String
typeName = go
  where
    join = intercalate " "
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
        ExprTypeWord { bits } -> join ["Word", show bits]
        ExprTypeWordArray { len, bits } -> join ["WordArray", show len, show bits]
        ExprTypeArray { ty, len } -> join ["Array", go ty, show len]
        ExprTypeStruct ident -> join ["Struct", ident.unwrap]
        ExprTypePtr ty -> join ["Ptr", go ty]

--

globalWrapperT :: ExprType -> ExprType
globalWrapperT = structT . globalWrapperStructName

rodataPtrsOf :: ROData -> [(Expr, ExprType)]
rodataPtrsOf rodata =
    [ (machineWordE range.addr, globalWrapperT (structT structName))
    | (structName, range) <- zip (rodataStructNames rodata) rodata.ranges
    ]
