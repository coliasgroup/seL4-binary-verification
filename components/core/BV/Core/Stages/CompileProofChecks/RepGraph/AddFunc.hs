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
import Data.Foldable (for_)
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

newtype WithAddFunc m a
  = WithAddFunc { run :: StateT State (ReaderT Env m) a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

instance MonadTrans WithAddFunc where
    lift = WithAddFunc . lift . lift

data Env
  = Env
      { pairings :: Pairings
      , pairingsAccess :: M.Map Ident PairingId
      }
  deriving (Generic)

data State
  = State
      { funcs :: M.Map Visit (ExprEnv, ExprEnv, Expr)
      , funcsByName :: M.Map (PairingOf Ident) [Visit]
      }
  deriving (Generic)

runWithAddFunc :: MonadRepGraph m => Pairings -> WithAddFunc m a -> m a
runWithAddFunc pairings m = runReaderT (evalStateT m.run initState) (initEnv pairings)

initEnv :: Pairings -> Env
initEnv pairings = Env
    { pairings
    , pairingsAccess = M.fromListWith (error "unexpected") $
        concat [ [(p.c, p), (p.asm, p)] | p <- M.keys pairings.unwrap]
    }

initState :: State
initState = State
    { funcs = M.empty
    , funcsByName = M.empty
    }

instance MonadSolverSend m => MonadSolverSend (WithAddFunc m) where
    sendSExprWithPlaceholders = WithAddFunc . sendSExprWithPlaceholders

instance MonadStructs m => MonadStructs (WithAddFunc m) where
    askLookupStruct = WithAddFunc askLookupStruct

instance MonadSolver m => MonadSolver (WithAddFunc m) where
    liftSolver = WithAddFunc . liftSolver

instance MonadRepGraph m => MonadRepGraphDefaultHelper m (WithAddFunc m) where

instance MonadRepGraph m => MonadRepGraph (WithAddFunc m) where
    runPostEmitCallNodeHook = addFunc

--

addFunc :: MonadRepGraph m => Visit -> ExprEnv -> ExprEnv -> Expr -> WithAddFunc m ()
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

getFuncPairingNoCheck :: MonadRepGraph m => Visit -> Visit -> WithAddFunc m (Maybe (Pairing, PairingOf Visit))
getFuncPairingNoCheck visit visit2 = do
    fname <- askFnName visit
    fname2 <- askFnName visit2
    pairingId <- WithAddFunc $ gview $ #pairingsAccess % at fname % unwrapped
    p <- WithAddFunc $ gview $ #pairings % #unwrap % at pairingId % unwrapped
    return $ (p ,) <$> if
        | pairingId == PairingOf { asm = fname, c = fname2 } -> Just $ PairingOf { asm = visit, c = visit2 }
        | pairingId == PairingOf { asm = fname2, c = fname } -> Just $ PairingOf { asm = visit2, c = visit }
        | otherwise -> Nothing

getFuncPairing :: MonadRepGraph m => Visit -> Visit -> WithAddFunc m (Maybe (Pairing, PairingOf Visit))
getFuncPairing visit visit2 = do
    opt <- getFuncPairingNoCheck visit visit2
    whenJustThen opt $ \(p, visits) -> do
        (lin, _, _) <- WithAddFunc $ use $ #funcs % at visits.asm % unwrapped
        (rin, _, _) <- WithAddFunc $ use $ #funcs % at visits.c % unwrapped
        lcalls <- scanMemCalls lin
        rcalls <- scanMemCalls rin
        (compatible, _s) <- memCallsCompatible $ PairingOf
            { asm = lcalls
            , c = rcalls
            }
        -- unless compatible $ do
        --     warn _s
        return $ if compatible then Just (p, visits) else Nothing

getFuncAssert :: MonadRepGraph m => Visit -> Visit -> WithAddFunc m Expr
getFuncAssert visit visit2 = do
    (pairing, visits) <- fromJust <$> getFuncPairing visit visit2
    (lin, lout, lsucc) <- WithAddFunc $ use $ #funcs % at visits.asm % unwrapped
    (rin, rout, rsucc) <- WithAddFunc $ use $ #funcs % at visits.c % unwrapped
    _lpc <- getPc visits.asm Nothing
    rpc <- getPc visits.c Nothing
    let envs = \case
            PairingEqSideQuadrant Asm PairingEqDirectionIn -> lin
            PairingEqSideQuadrant C PairingEqDirectionIn -> rin
            PairingEqSideQuadrant Asm PairingEqDirectionOut -> lout
            PairingEqSideQuadrant C PairingEqDirectionOut -> rout
    inEqs <- instEqs pairing.inEqs envs
    outEqs <- instEqs pairing.outEqs envs
    let succImp = impliesE rsucc lsucc
    return $ impliesE
        (foldr1 andE (inEqs ++ [rpc]))
        (foldr1 andE (outEqs ++ [succImp]))
  where
    instEqs :: MonadSolver m => [PairingEq] -> (PairingEqSideQuadrant -> ExprEnv) -> m [Expr]
    instEqs eqs envs = for eqs $ \eq ->
        instEqWithEnvs (eq.lhs.expr, envs eq.lhs.quadrant) (eq.rhs.expr, envs eq.rhs.quadrant)

addFuncAssert :: MonadRepGraph m => Visit -> Visit -> WithAddFunc m ()
addFuncAssert visit visit2 = do
    imp <- weakenAssert <$> getFuncAssert visit visit2
    withoutEnv $ assertFact imp

memCallsCompatible :: MonadRepGraph m => PairingOf (Maybe MemCalls) -> WithAddFunc m (Bool, Maybe String)
memCallsCompatible = \case
    PairingOf { asm = Just lcalls, c = Just rcalls } -> do
        rcastcalls <- fmap (M.fromList . catMaybes) $ for (M.toAscList lcalls) $ \(fname, calls) -> do
            pairingId <- WithAddFunc $ gview $ #pairingsAccess % at fname % unwrapped
            let rfname = pairingId.c
            rsig <- askFunctionSigs <&> ($ WithTag C rfname)
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

askFnName :: MonadRepGraph m => Visit -> m Ident
askFnName v = do
    p <- askProblem
    return $ p ^. #nodes % at (nodeAddrFromNodeId v.nodeId) % unwrapped % expecting #_NodeCall % #functionName
