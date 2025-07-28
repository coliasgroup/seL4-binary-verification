{-# LANGUAGE MultiWayIf #-}

module BV.Core.Stages.CompileProofChecks.RepGraph.AddFunc
    ( WithAddFunc
    , runWithAddFunc
    ) where

import BV.Core.Logic
import BV.Core.Stages.CompileProofChecks.RepGraph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils

import Control.Monad (when)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (for_, toList)
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

newtype WithAddFunc t m a
  = WithAddFunc { run :: StateT (State t) (ReaderT (Env t) m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

instance MonadTrans (WithAddFunc t) where
    lift = WithAddFunc . lift . lift

data Env t
  = Env
      { pairings :: Pairings t
      , pairingsAccess :: M.Map Ident (PairingId t)
      }
  deriving (Generic)

data State t
  = State
      { funcs :: M.Map Visit (ExprEnv, ExprEnv, Expr)
      , funcsByName :: M.Map (ByTag t Ident) [Visit]
      }
  deriving (Generic)

runWithAddFunc :: RefineTag t => MonadRepGraph t m => Pairings t -> WithAddFunc t m a -> m a
runWithAddFunc pairings m = runReaderT (evalStateT m.run initState) (initEnv pairings)

initEnv :: RefineTag t => Pairings t -> Env t
initEnv pairings = Env
    { pairings
    , pairingsAccess = M.fromListWith (error "unexpected") $
        concat [ [(getRight p, p), (getLeft p, p)] | p <- M.keys pairings.unwrap]
    }

initState :: State t
initState = State
    { funcs = M.empty
    , funcsByName = M.empty
    }

instance MonadSolverSend m => MonadSolverSend (WithAddFunc t m) where
    sendSExprWithPlaceholders = WithAddFunc . sendSExprWithPlaceholders

instance MonadStructs m => MonadStructs (WithAddFunc t m) where
    askLookupStruct = WithAddFunc askLookupStruct

instance MonadSolver m => MonadSolver (WithAddFunc t m) where
    liftSolver = WithAddFunc . liftSolver

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (WithAddFunc t m) where

instance (RefineTag t, MonadRepGraph t m) => MonadRepGraph t (WithAddFunc t m) where
    runPostEmitCallNodeHook = addFunc

--

addFunc :: RefineTag t => MonadRepGraph t m => Visit -> ExprEnv -> ExprEnv -> Expr -> WithAddFunc t m ()
addFunc visit inputs outputs success = do
    WithAddFunc $ #funcs %= M.insertWith (error "unexpected") visit (inputs, outputs, success)
    name <- askFnName visit
    pairingIdOpt <- WithAddFunc $ gview $ #pairingsAccess % at name
    whenJust_ pairingIdOpt $ \pairingId -> do
        group <- WithAddFunc $ use $ #funcsByName % to (fromMaybe [] . M.lookup pairingId)
        for_ group $ \visit2 -> do
            ok <- isJust <$> getFuncPairing visit visit2
            when ok $ do
                addFuncAssert visit visit2
        WithAddFunc $ #funcsByName %= M.insert pairingId (group ++ [visit])

getFuncPairingNoCheck :: (RefineTag t, MonadRepGraph t m) => Visit -> Visit -> WithAddFunc t m (Maybe (Pairing t, ByTag t Visit))
getFuncPairingNoCheck visit visit2 = do
    fname <- askFnName visit
    fname2 <- askFnName visit2
    pairingId <- WithAddFunc $ gview $ #pairingsAccess % at fname % unwrapped
    p <- WithAddFunc $ gview $ #pairings % #unwrap % at pairingId % unwrapped
    return $ (p ,) <$> if
        | pairingId == byRefineTag fname fname2 -> Just $ byRefineTag visit visit2
        | pairingId == byRefineTag fname2 fname -> Just $ byRefineTag visit2 visit
        | otherwise -> Nothing

getFuncPairing :: (RefineTag t, MonadRepGraph t m) => Visit -> Visit -> WithAddFunc t m (Maybe (Pairing t, ByTag t Visit))
getFuncPairing visit visit2 = do
    opt <- getFuncPairingNoCheck visit visit2
    whenJustThen opt $ \(p, visits) -> do
        (lin, _, _) <- WithAddFunc $ use $ #funcs % at (getLeft visits) % unwrapped
        (rin, _, _) <- WithAddFunc $ use $ #funcs % at (getRight visits) % unwrapped
        lcalls <- scanMemCalls lin
        rcalls <- scanMemCalls rin
        (compatible, _s) <- memCallsCompatible $ byRefineTag lcalls rcalls
        -- unless compatible $ do
        --     warn _s
        return $ if compatible then Just (p, visits) else Nothing

getFuncAssert :: (RefineTag t, MonadRepGraph t m) => Visit -> Visit -> WithAddFunc t m Expr
getFuncAssert visit visit2 = do
    (pairing, visits) <- fromJust <$> getFuncPairing visit visit2
    (lin, lout, lsucc) <- WithAddFunc $ use $ #funcs % at (getLeft visits) % unwrapped
    (rin, rout, rsucc) <- WithAddFunc $ use $ #funcs % at (getRight visits) % unwrapped
    _lpc <- getPc (getLeft visits) Nothing
    rpc <- getPc (getRight visits) Nothing
    let envs = \case
            PairingEqSideQuadrant t PairingEqDirectionIn | t == leftTag -> lin
            PairingEqSideQuadrant t PairingEqDirectionIn | t == rightTag -> rin
            PairingEqSideQuadrant t PairingEqDirectionOut | t == leftTag -> lout
            PairingEqSideQuadrant t PairingEqDirectionOut | t == rightTag -> rout
            _ -> error "unreachable"
    inEqs <- instEqs pairing.inEqs envs
    outEqs <- instEqs pairing.outEqs envs
    let succImp = impliesE rsucc lsucc
    return $ impliesE
        (foldr1 andE (inEqs ++ [rpc]))
        (foldr1 andE (outEqs ++ [succImp]))
  where
    instEqs :: MonadSolver m => [PairingEq t] -> (PairingEqSideQuadrant t -> ExprEnv) -> m [Expr]
    instEqs eqs envs = for eqs $ \eq ->
        instEqWithEnvs (eq.lhs.expr, envs eq.lhs.quadrant) (eq.rhs.expr, envs eq.rhs.quadrant)

addFuncAssert :: (RefineTag t, MonadRepGraph t m) => Visit -> Visit -> WithAddFunc t m ()
addFuncAssert visit visit2 = do
    imp <- weakenAssert <$> getFuncAssert visit visit2
    withoutEnv $ assertFact imp

memCallsCompatible :: (RefineTag t, MonadRepGraph t m) => ByTag t (Maybe MemCalls) -> WithAddFunc t m (Bool, Maybe String)
memCallsCompatible byTag = case toList byTag of
    [Just lcalls, Just rcalls] -> do
        rcastcalls <- fmap (M.fromList . catMaybes) $ for (M.toAscList lcalls) $ \(fname, calls) -> do
            pairingId <- WithAddFunc $ gview $ #pairingsAccess % at fname % unwrapped
            let rfname = getRight pairingId
            rsig <- askFunctionSigs <&> ($ WithTag rightTag rfname)
            return $
                if any (\arg -> arg.ty == memT) rsig.output
                then Just (rfname, calls)
                else Nothing
        let isIncompat fname =
                let rcast = fromMaybe zeroMemCallsForFunction $ rcastcalls !? fname
                    ractual = fromMaybe zeroMemCallsForFunction $ rcalls !? fname
                    x = case rcast.max of
                            Just n -> n < ractual.min
                            _ -> False
                    y = case ractual.max of
                            Just n -> n < rcast.min
                            _ -> False
                    in x || y
        let incompat = any isIncompat $ S.toList $ M.keysSet rcastcalls <> M.keysSet rcalls
        return $ if incompat then (False, error "unimplemented") else (True, Nothing)
    _ -> return (True, Nothing)

--

askFnName :: MonadRepGraph t m => Visit -> m Ident
askFnName v = do
    p <- askProblem
    return $ p ^. #nodes % at (nodeAddrFromNodeId v.nodeId) % unwrapped % expecting #_NodeCall % #functionName
