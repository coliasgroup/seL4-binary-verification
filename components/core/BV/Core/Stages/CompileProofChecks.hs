{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BV.Core.Stages.CompileProofChecks
    ( FunctionSignature (..)
    , FunctionSignatures
    , compileProofChecks
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.Grouping
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types
import BV.Core.Types.Extras

import BV.Core.Utils
import Control.Monad.Reader (runReaderT)
import Control.Monad.RWS (RWS, runRWS)
import Data.Function (applyWhen)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import GHC.Generics (Generic)
import Optics

compileProofChecks :: Map Ident Struct -> FunctionSignatures -> Pairings -> ROData -> Problem -> [ProofCheck a] -> [SMTProofCheckGroup a]
compileProofChecks cStructs functionSigs pairings rodata problem checks =
    map
        (compileProofCheckGroup cStructs functionSigs pairings rodata problem)
        (proofCheckGroups checks)

compileProofCheckGroup :: Map Ident Struct -> FunctionSignatures -> Pairings -> ROData -> Problem -> ProofCheckGroup a -> SMTProofCheckGroup a
compileProofCheckGroup cStructs functionSigs pairings rodata problem group =
    SMTProofCheckGroup setup imps
  where
    env = initEnv rodata cStructs functionSigs pairings problem
    state = initState
    (imps, _, setup) = runRWS (compileProofCheckGroupM group).run env state

newtype M a
  = M { run :: RWS Env SolverOutput State a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

data Env
  = Env
      { solver :: SolverEnv
      , repGraph :: RepGraphEnv
      }
  deriving (Generic)

data State
  = State
      { solver :: SolverState
      , repGraph :: RepGraphState
      }
  deriving (Generic)

initEnv :: ROData -> Map Ident Struct -> FunctionSignatures -> Pairings -> Problem -> Env
initEnv rodata cStructs functionSigs pairings problem = Env
    { solver = initSolverEnv rodata cStructs problem
    , repGraph = initRepGraphEnv functionSigs pairings problem
    }

initState :: State
initState = State
    { solver = initSolverState
    , repGraph = initRepGraphState
    }

instance MonadStructs M where
    askLookupStruct = askLookupStructForSolver

instance MonadSolver M where
    liftSolver m = M . zoom #solver . magnify #solver $ m

instance MonadRepGraph M where
    liftRepGraph m = M . zoom #repGraph . magnify #repGraph $ m

compileProofCheckGroupM :: ProofCheckGroup a -> M [SMTProofCheckImp a]
compileProofCheckGroupM group = do
    imps <- interpretGroupM group
    addPValidDomAssertionsM
    return imps

interpretGroupM :: ProofCheckGroup a -> M [SMTProofCheckImp a]
interpretGroupM group = mapM interpretCheckM group

interpretCheckM :: ProofCheck a -> M (SMTProofCheckImp a)
interpretCheckM check = do
    concl <- interpretHypM check.hyp
    term <- interpretHypImpsM check.hyps concl
    sexpr <- runReaderT (smtExprNoSplitM term) M.empty
    return $ SMTProofCheckImp check.meta sexpr

interpretHypImpsM :: [Hyp] -> Expr -> M Expr
interpretHypImpsM hyps concl = do
    hyps' <- mapM interpretHypM hyps
    return $ strengthenHyp (nImpliesE hyps' concl)

strengthenHyp :: Expr -> Expr
strengthenHyp = go 1
  where
    go sign expr = case expr.value of
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
                in if
                    | l' == trueE -> goWith r'
                    | l' == falseE -> goWith (notE r')
                    | otherwise -> expr
            _ -> expr
        _ -> expr
      where
        goWith = go sign
        goAgainst = go (-sign)

interpretHypM :: Hyp -> M Expr
interpretHypM = \case
    HypPcImp hyp -> do
        let f = \case
                PcImpHypSideBool v -> return $ fromBoolE v
                PcImpHypSidePc vt -> getPcM vt.visit (Just vt.tag)
        pc1 <- f hyp.lhs
        pc2 <- f hyp.rhs
        return $ impliesE pc1 pc2
    HypEq { ifAt, eq } -> do
        (x, y) <- case eq.induct of
            Nothing -> return (eq.lhs.expr, eq.rhs.expr)
            Just induct -> do
                v <- getInductVarM induct
                let x = substInduct eq.lhs.expr v
                let y = substInduct eq.rhs.expr v
                return $ (x, y)
        x_pc_env <- getNodePcEnvM eq.lhs.visit.visit (Just eq.lhs.visit.tag)
        y_pc_env <- getNodePcEnvM eq.rhs.visit.visit (Just eq.rhs.visit.tag)
        case (x_pc_env, y_pc_env) of
            (Just (_, xenv), Just (_, yenv)) -> do
                eq' <- instEqWithEnvs (x, xenv) (y, yenv)
                if ifAt
                    then do
                        x_pc <- getPcM eq.lhs.visit.visit (Just eq.lhs.visit.tag)
                        y_pc <- getPcM eq.rhs.visit.visit (Just eq.rhs.visit.tag)
                        return $ nImpliesE [x_pc, y_pc] eq'
                    else do
                        return eq'
            _ -> do
                return $ fromBoolE ifAt
