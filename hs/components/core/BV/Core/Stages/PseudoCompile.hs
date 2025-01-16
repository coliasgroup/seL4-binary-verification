module BV.Core.Stages.PseudoCompile
    ( pseudoCompile
    ) where

import Control.Exception (assert)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Optics

import BV.Core.Arch
import BV.Core.ExprConstruction
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Utils

pseudoCompile :: ObjDumpInfo -> Program -> Program
pseudoCompile objDumpInfo prog = assert (M.null prog.structs && M.null prog.constGlobals) $
    (compilePValidAlignExprs . compileSymbolReferences objDumpInfo) prog

compileSymbolReferences :: ObjDumpInfo -> Program -> Program
compileSymbolReferences objDumpInfo = walkFunctionExprs (over #value f)
  where
    f = \case
        ExprValueSymbol ident -> ExprValueNum . fromJust $
            objDumpInfo ^? #symbols % ix ident.unwrap % #addr
        v -> v

compilePValidAlignExprs :: Program -> Program
compilePValidAlignExprs = walkFunctionExprs f
  where
    f expr@(Expr _ val) = case val of
        ExprValueOp OpPAlignValid args -> case args of
            [ Expr ExprTypeType (ExprValueType tyVal)
                , ptr@(Expr (ExprTypeWord wordSize) (ExprValueVar _))
                ] | wordSize == archWordSizeBits -> alignValidIneqE tyVal ptr
            _ -> error ""
        _ -> expr

walkFunctionExprs :: (Expr -> Expr) -> Program -> Program
walkFunctionExprs f =
    #functions % traversed % #body % traversed % #nodes % traversed % traverseTopLevelLevelExprs
        %~ walkExprsI f

alignValidIneqE :: ExprType -> Expr -> Expr
alignValidIneqE ty p =
    assert (align `elem` [1, 4, 8]) $
        foldr1 andE conj
  where
    size = machineWordE (sizeOfType ty)
    align = alignOfType ty
    w0 = machineWordE 0
    conj = optionals (align > 1) [bitwiseAndE p (machineWordE (align - 1)) `eqE` w0] ++
        [ notE (p `eqE` w0)
        , (w0 `lessE` size) `impliesE` (p `lessEqE` negE size)
        ]
