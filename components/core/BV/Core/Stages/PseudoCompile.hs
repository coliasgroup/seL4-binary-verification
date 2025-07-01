module BV.Core.Stages.PseudoCompile
    ( pseudoCompile
    ) where

import BV.Core.Arch
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Utils

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Optics

pseudoCompile :: ObjDumpInfo -> Program -> Program
pseudoCompile objDumpInfo prog = ensure (M.null prog.constGlobals) $
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
                ] | wordSize == archWordSizeBits -> withoutStructs $ alignValidIneq (PValidTypeType tyVal) ptr
            _ -> error ""
        _ -> expr

walkFunctionExprs :: (Expr -> Expr) -> Program -> Program
walkFunctionExprs f =
    #functions % traversed % #body % traversed % #nodes % traversed % traverseTopLevelLevelExprs
        %~ walkExprsI f
