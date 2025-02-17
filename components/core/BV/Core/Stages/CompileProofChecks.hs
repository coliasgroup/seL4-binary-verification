{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- TODO
{-# OPTIONS -Wno-all #-}

module BV.Core.Stages.CompileProofChecks
    ( CheckGroupKey
    , compileProofChecks
    , proofCheckGroupsWithKeys
    ) where

import BV.Core.Stages.Utils
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils
import BV.SMTLIB2

import Control.DeepSeq (NFData)
import Control.Monad.RWS (RWS, modify, runRWS, tell)
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.Functor (void)
import Data.List (sort, sortOn)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import GHC.Generics (Generic)
import Optics
import Text.Printf (printf)

type ProofCheckGroup a = [ProofCheck a]

compileProofChecks :: Problem -> [ProofCheck a] -> [SMTProofCheckGroup a]
compileProofChecks problem checks =
    map (compileProofCheckGroup problem) (proofCheckGroups checks)

proofCheckGroups :: [ProofCheck a] -> [ProofCheckGroup a]
proofCheckGroups = toList . proofCheckGroupsWithKeys

proofCheckGroupsWithKeys :: [ProofCheck a] -> Map CheckGroupKey (ProofCheckGroup a)
proofCheckGroupsWithKeys =
    foldMap (\check -> M.singleton (compatOrdKey (groupKeyOf check)) [check])

newtype CheckGroupKey
  = CheckGroupKey { unwrap :: [((String, [(Integer, ([Integer], [Integer]))]), String)] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

groupKeyOf :: ProofCheck a -> Set VisitWithTag
groupKeyOf check = S.fromList (check ^.. checkVisits)

compatOrdKey :: Set VisitWithTag -> CheckGroupKey
compatOrdKey visits = CheckGroupKey $ sort
    [ ((prettyNodeId visit.nodeId, []), prettyTag tag)
    | VisitWithTag visit tag <- S.toList visits
    ]

--

compileProofCheckGroup :: Problem -> [ProofCheck a] -> SMTProofCheckGroup a
compileProofCheckGroup problem group = SMTProofCheckGroup setup imps
  where
    (imps, _, setup) = runRWS (interpretGroupM group) problem state0

type M = RWS Problem [SExprWithPlaceholders] State

data State
  = State
      { smtDerivedOps :: Map (Op, Integer) String
      , namesUsed :: Set Ident
      , externalNames :: Set Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

state0 :: State
state0 = State
    { smtDerivedOps = mempty
    , namesUsed = mempty
    , externalNames = mempty
    }

interpretGroupM :: [ProofCheck a] -> M [SMTProofCheckImp a]
interpretGroupM group = do
    mapM interpretCheckM group

interpretCheckM :: ProofCheck a -> M (SMTProofCheckImp a)
interpretCheckM check = do
    concl <- interpretHypM check.hyp
    term <- interpretHypImpsM check.hyps concl
    sexpr <- smtExprM mempty term
    return $ SMTProofCheckImp check.meta sexpr

interpretHypImpsM :: [Hyp] -> Expr -> M Expr
interpretHypImpsM hyps concl = do
    hyps' <- mapM interpretHypM hyps
    return $ strengthenHyp' (nImpliesE hyps' concl)

strengthenHyp' :: Expr -> Expr
strengthenHyp' = strengthenHyp 1

strengthenHyp :: Integer ->  Expr -> Expr
strengthenHyp sign expr = case expr.value of
    ExprValueOp op args -> case op of
        _ | op == OpAnd || op == OpOr ->
            Expr expr.ty (ExprValueOp op (map goWith args))
        OpImplies ->
            let [l, r] = args
             in goAgainst l `impliesE` goWith r
        OpNot ->
            let [x] = args
             in notE (goAgainst x)
        OpStackEquals -> case sign of
            1 -> boolE (ExprValueOp OpImpliesStackEquals args)
            -1 -> boolE (ExprValueOp OpStackEqualsImplies args)
        OpROData -> case sign of
            1 -> boolE (ExprValueOp OpImpliesROData args)
            -1 -> expr
        OpEquals | isBoolT (head args).ty ->
            let [_l, r] = args
                args' = applyWhen (r `elem` ([trueE, falseE] :: [Expr])) reverse args
                [l', r'] = args'
             in if l' == trueE then goWith r'
                else if l' == falseE then goWith (notE r')
                else expr
        _ -> expr
    _ -> expr
  where
    goWith = strengthenHyp sign
    goAgainst = strengthenHyp (-sign)

smtExprM :: Map (Ident, ExprType) S -> Expr -> M S
smtExprM env expr = do
    case expr.value of
        ExprValueOp op args -> case op of
            _ | op == OpWordCast || op == OpWordCastSigned -> do
                    let [v] = args
                    let ExprTypeWord bitsExpr = expr.ty
                    let ExprTypeWord bitsV = v.ty
                    ex <- smtExprM env v
                    return $
                        if bitsExpr == bitsV
                        then ex
                        else if bitsExpr < bitsV
                        then [["_", "extract", intS (bitsExpr - 1), intS 0], ex]
                        else case op of
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

getSMTDerivedOpM :: Op -> Integer -> M String
getSMTDerivedOpM op n = do
    opt <- use $ #smtDerivedOps % at (op, n)
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
            let def = defineFunS fname [("x", bitVecS 1)] (bitVecS 1) body
            tell [def]
            modify $ #smtDerivedOps % at (op, n) ?~ fname
            return fname

interpretHypM :: Hyp -> M Expr
interpretHypM hyp = do
    -- undefined
    return $ Expr boolT (ExprValueSMTExpr ["TODO"])
