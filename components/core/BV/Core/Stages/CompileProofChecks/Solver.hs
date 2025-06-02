{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.Core.Stages.CompileProofChecks.Solver
    ( MonadSolver (..)
    , SolverContext
    , SolverEnv
    , SolverOutput
    , SolverState
    , initSolverEnv
    , initSolverState
    , smtExprM
    ) where

import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.SMTLIB2.SExpr

import BV.Core.Arch
import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState, modify)
import Control.Monad.Writer (MonadWriter, tell)
import Data.List (intercalate)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Set (Set)
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

type SolverContext = MonadRWS SolverEnv SolverOutput SolverState

class Monad m => MonadSolver m where
    liftSolver :: (forall n. SolverContext n => n a) -> m a

data SolverEnv
  = SolverEnv
      { rodata :: ROData
      , structs :: Map Ident Struct
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type SolverOutput = [SExprWithPlaceholders]

data SolverState
  = SolverState
      { smtDerivedOps :: Map (Op, Integer) String
      , namesUsed :: Set Ident
      , externalNames :: Set Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

initSolverEnv :: ROData -> Map Ident Struct -> Problem -> SolverEnv
initSolverEnv rodata cStructs problem = SolverEnv
    { rodata
    , structs = augmentStructs rodata cStructs problem
    }

initSolverState :: SolverState
initSolverState = SolverState
    { smtDerivedOps = mempty
    , namesUsed = mempty
    , externalNames = mempty
    }

send :: MonadSolver m => SExprWithPlaceholders -> m ()
send sexpr = liftSolver $ tell [sexpr]

lookupStruct :: MonadReader SolverEnv m => Ident -> m Struct
lookupStruct name = gview $ #structs % at name % unwrapped

typeSizeWith :: Map Ident Struct -> ExprType -> Integer
typeSizeWith structs = go
  where
    go = \case
        ExprTypeWord { bits } ->
            let (bytes, 0) = bits `divMod` 8
            in bytes
        ExprTypeArray { ty, length } -> go ty * length
        ExprTypeStruct ident -> (structs ! ident).size
        ExprTypePtr _ -> archPtrSizeBytes

typeSize :: MonadReader SolverEnv m => ExprType -> m Integer
typeSize ty = do
    structs <- gview #structs
    return $ typeSizeWith structs ty

typeAlignWith :: Map Ident Struct -> ExprType -> Integer
typeAlignWith structs ty = case ty of
    ExprTypeWord { } -> typeSizeWith structs ty
    ExprTypeArray { ty } -> typeAlignWith structs ty
    ExprTypeStruct ident -> (structs ! ident).align
    ExprTypePtr _ -> archPtrSizeBytes

typeAlign :: MonadReader SolverEnv m => ExprType -> m Integer
typeAlign ty = do
    structs <- gview #structs
    return $ typeSizeWith structs ty

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
        ExprTypeWordArray { length, bits } -> join ["WordArray", show length, show bits]
        ExprTypeArray { ty, length } -> join ["Array", go ty, show length]
        ExprTypeStruct ident -> join ["Struct", ident.unwrap]
        ExprTypePtr ty -> join ["Ptr", go ty]

globalWrapperStructName :: ExprType -> Ident
globalWrapperStructName ty = Ident $ printf "Global (%s)" (typeName ty)

globalWrapperStructWith :: Map Ident Struct -> ExprType -> Struct
globalWrapperStructWith structs ty = Struct
    { size = typeSizeWith structs ty
    , align = typeAlignWith structs ty
    , fields =
        [ ( "v"
          , StructField
                { ty
                , offset = 0
                }
          )
        ]
    }

globalWrapperT :: ExprType -> ExprType
globalWrapperT = structT . globalWrapperStructName

rodataStructNames :: ROData -> [Ident]
rodataStructNames rodata =
    [ Ident $ case rodata.ranges of
            [_] -> "rodata_struct"
            _ -> printf "rodata_struct_%d" i
    | (i :: Integer, _) <- zip [1..] rodata.ranges
    ]

rodataPtrsWith :: ROData -> [(Expr, ExprType)]
rodataPtrsWith rodata =
    [ (machineWordE range.addr, globalWrapperT (structT structName))
    | (structName, range) <- zip (rodataStructNames rodata) rodata.ranges
    ]

rodataPtrs :: MonadReader SolverEnv m => m [(Expr, ExprType)]
rodataPtrs = do
    rodata <- gview #rodata
    return $ rodataPtrsWith rodata

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

-- rodataStructs :: MonadReader SolverEnv m => m (Map Ident Struct)
-- rodataStructs = do
--     rodata <- gview $ #rodata
--     return $ rodataStructsWith rodata

augmentStructs :: ROData -> Map Ident Struct -> Problem -> Map Ident Struct
augmentStructs rodata cStructs problem =
    nonGlobal <> global
  where
    rodataStructs = rodataStructsWith rodata
    nonGlobal = cStructs <> rodataStructs
    global = M.fromList
        [ (globalWrapperStructName ty, globalWrapperStructWith cStructs ty)
        | ty <- toWrap
        ]
    toWrap = problem.nodes ^.. folded % traverseTopLevelLevelExprs % foldExprs % afolding isPGlobalValid
    isPGlobalValid expr = case expr.value of
        ExprValueOp OpPGlobalValid args ->
            let [_, tyExpr, _] = args
                Expr { ty = ExprTypeType, value = ExprValueType ty } = tyExpr
             in Just ty
        _ -> Nothing

smtExprM :: MonadSolver m => Map (Ident, ExprType) S -> Expr -> m S
smtExprM env expr = do
    case expr.value of
        ExprValueOp op args -> case op of
            _ | op == OpWordCast || op == OpWordCastSigned -> do
                    let [v] = args
                    let ExprTypeWord bitsExpr = expr.ty
                    let ExprTypeWord bitsV = v.ty
                    ex <- smtExprM env v
                    return $ if
                        | bitsExpr == bitsV -> ex
                        | bitsExpr < bitsV -> [["_", "extract", intS (bitsExpr - 1), intS 0], ex]
                        | otherwise ->
                            case op of
                                OpWordCast -> [["_", "zero_extend", intS (bitsExpr - bitsV), ex]]
                                OpWordCastSigned -> [["_", "sign_extend", intS (bitsExpr - bitsV), ex]]
            _ | op == OpToFloatingPoint || op == OpToFloatingPointSigned ||
                op == OpToFloatingPointUnsigned || op == OpFloatingPointCast -> do
                    error "unsupported"
            _ | op == OpCountLeadingZeroes || op == OpWordReverse -> do
                    let [v] = args
                    v' <- smtExprM env v
                    op' <- getSMTDerivedOpM op (expr.ty ^. expecting #_ExprTypeWord)
                    return [symbolS op', v']
            _ | op == OpCountTrailingZeroes -> do
                    let [v] = args
                    smtExprM env $ clzE (wordReverseE v)
            _ -> do
                -- undefined
                return ["TODO"]
        ExprValueNum n -> do
            return $ intWithWidthS (wordTBits expr.ty) n
        ExprValueVar var -> do
            let envKey = (var, expr.ty)
            return $ case env !? envKey of
                Just sexpr ->
                    let check = case sexpr of
                            List ("SplitMem":_) -> True
                            Atom (AtomOrPlaceholderAtom atom)
                                | isJust (preview #_SymbolAtom (viewAtom atom)) -> True
                            _ -> False
                     in ensure check sexpr
                Nothing -> error $ "env miss: " ++ show envKey
        ExprValueSMTExpr sexpr -> do
            return sexpr
        ExprValueToken tok -> do
            undefined

getSMTDerivedOpM :: MonadSolver m => Op -> Integer -> m String
getSMTDerivedOpM op n = do
    opt <- liftSolver $ use $ #smtDerivedOps % at (op, n)
    case opt of
        Just fname -> return fname
        Nothing -> do
            let fname = case op of
                    OpCountLeadingZeroes -> printf "bvclz_%d" n
                    OpWordReverse -> printf "bvrev_%d" n
            body <- case n of
                    1 -> return $ case op of
                            OpCountLeadingZeroes -> iteS ("x" `eqS` hexS "0") (hexS "1") (hexS "0")
                            OpWordReverse -> "x"
                    _ -> do
                        let m = n `div` 2
                        topAppOp <- getSMTDerivedOpM op (n - m)
                        botAppOp <- getSMTDerivedOpM op m
                        let top = [ixS "extract" [intS (n - 1), intS m], "x"]
                            bot = [ixS "extract" [intS (m - 1), intS 0], "x"]
                            topApp = [symbolS topAppOp, top]
                            topAppX = [ixS "zero_extend" [intS m], topApp]
                            botApp = [symbolS botAppOp, bot]
                            botAppX = [ixS "zero_extend" [intS (n - m)], botApp]
                        return $ case op of
                                OpCountLeadingZeroes ->
                                    iteS
                                        (top `eqS` intWithWidthS (n - m) 0)
                                        (bvaddS botAppX (intWithWidthS n m))
                                        topAppX
                                OpWordReverse ->
                                    concatS botApp topApp
            send $ defineFunS fname [("x", bitVecS 1)] (bitVecS 1) body
            liftSolver $ modify $ #smtDerivedOps % at (op, n) ?~ fname
            return fname
