module BV.Core.RepGraph.AddFunc
    ( FunctionSignatures
    , WithAddFunc
    , runWithAddFunc
    ) where

import BV.Core.Logic
import BV.Core.RepGraph.Core
import BV.Core.RepGraph.Solver
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (expecting, unwrapped)

import Control.Monad (when)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (for_)
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

type FunctionSignatures t = WithTag t Ident -> FunctionSignature

type WithAddFuncInner t m = StateT (State t) (ReaderT (Env t) m)

newtype WithAddFunc t m a
  = WithAddFunc { run :: WithAddFuncInner t m a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

instance MonadTrans (WithAddFunc t) where
    lift = WithAddFunc . lift . lift

data Env t
  = Env
      { functionSigs :: FunctionSignatures t
      , pairings :: Pairings t
      , pairingsAccess :: ByTag t (M.Map Ident (PairingId t))
      }
  deriving (Generic)

data State t
  = State
      { funcsByName :: M.Map (WithTag t Ident) [Visit]
      }
  deriving (Generic)

runWithAddFunc :: RefineTag t => MonadRepGraph t m => FunctionSignatures t -> Pairings t -> WithAddFunc t m a -> m a
runWithAddFunc functionSigs pairings m =
    runReaderT
        (evalStateT m.run initState)
        (initEnv functionSigs pairings)

initEnv :: RefineTag t => FunctionSignatures t -> Pairings t -> Env t
initEnv functionSigs pairings = Env
    { functionSigs
    , pairings
    , pairingsAccess = byTagFrom $ \tag ->
        M.fromList [ (viewAtTag tag p, p) | p <- M.keys pairings.unwrap]
    }

initState :: State t
initState = State
    { funcsByName = M.empty
    }

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (WithAddFunc t m) where
    sendSExprWithPlaceholders = WithAddFunc . sendSExprWithPlaceholders

instance MonadStructs m => MonadStructs (WithAddFunc t m) where
    askLookupStruct = WithAddFunc askLookupStruct

instance MonadRepGraphSolver m => MonadRepGraphSolver (WithAddFunc t m) where
    liftSolver = WithAddFunc . liftSolver

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (WithAddFunc t m)

instance (RefineTag t, MonadRepGraph t m) => MonadRepGraph t (WithAddFunc t m) where
    runPostEmitCallNodeHook = addFunc

--

addFunc :: RefineTag t => MonadRepGraph t m => Visit -> ForTag t (WithAddFunc t m) ()
addFunc visit = do
    tag <- askTag
    name <- askFunName visit
    pairingIdOpt <- liftWithAddFunc $ gview $ #pairingsAccess % atTag tag % at name
    for_ pairingIdOpt $ \pairingId -> do
        group <- liftWithAddFunc $ use $ #funcsByName %
            to (M.findWithDefault [] (viewAtTag (otherTag tag) (withTags pairingId)))
        for_ group $ \otherVisit -> do
            byTag <- orient visit otherVisit
            lift $ do
                compat <- areCompatible byTag
                when compat $ do
                    pairing <- WithAddFunc $ gview $ #pairings % #unwrap % at pairingId % unwrapped
                    imp <- weakenAssert <$> getFuncAssert byTag pairingId pairing
                    withoutEnv $ assertFact imp
        liftWithAddFunc $ #funcsByName %= M.insertWith (flip (<>)) (viewAtTag tag (withTags pairingId)) [visit]

areCompatible :: (RefineTag t, MonadRepGraph t m) => ByTag t Visit -> WithAddFunc t m Bool
areCompatible visits = do
    calls <- for (withTags visits) $ \(WithTag tag visit) -> do
        (in_, _, _) <- runForTag tag $ getFuncRaw visit
        scanMemCalls in_
    (compatible, _s) <- memCallsCompatible calls
    -- unless compatible $ do
    --     warn _s
    return compatible

getFuncAssert :: (RefineTag t, MonadRepGraph t m) => ByTag t Visit -> PairingId t -> Pairing t -> WithAddFunc t m Expr
getFuncAssert visits pairingId pairing = do
    ((lin, lout, lsucc), (rin, rout, rsucc)) <- fmap viewByRefineTag $ for (withTags visits) $ \(WithTag tag visit) -> do
        (rawIn, rawOut, succ_) <- runForTag tag $ getFuncRaw visit
        sigs <- WithAddFunc $ gview #functionSigs
        let sig = sigs $ viewWithTag tag pairingId
        let in_ = M.fromList $ zip sig.input $ map snd rawIn
        let out = M.fromList $ zip sig.output $ map snd rawOut
        return (in_, out, succ_)
    (_lpc, rpc) <- fmap viewByRefineTag $ for (withTags visits) getPcWithTag
    let envs = \case
            PairingEqSideQuadrant t PairingEqDirectionIn | t == leftTag -> lin
            PairingEqSideQuadrant t PairingEqDirectionIn | t == rightTag -> rin
            PairingEqSideQuadrant t PairingEqDirectionOut | t == leftTag -> lout
            PairingEqSideQuadrant t PairingEqDirectionOut | t == rightTag -> rout
            _ -> undefined
    inEqs <- instEqs pairing.inEqs envs
    outEqs <- instEqs pairing.outEqs envs
    return $ impliesE
        (foldr1 andE (inEqs ++ [rpc]))
        (foldr1 andE (outEqs ++ [impliesE rsucc lsucc]))
  where
    instEqs eqs envs = for eqs $ \eq ->
        instEqWithEnvs (eq.lhs.expr, envs eq.lhs.quadrant) (eq.rhs.expr, envs eq.rhs.quadrant)

memCallsCompatible :: (RefineTag t, MonadRepGraph t m) => ByTag t (Maybe MemCalls) -> WithAddFunc t m (Bool, Maybe String)
memCallsCompatible byTag = case viewByRefineTag byTag of
    (Just lcalls, Just rcalls) -> do
        rcastcalls <- fmap (M.fromList . catMaybes) $ for (M.toList lcalls) $ \(fname, calls) -> do
            pairingId <- WithAddFunc $ gview $ #pairingsAccess % atTag leftTag % at fname % unwrapped
            let rfname = pairingId.right
            functionSigs <- WithAddFunc $ gview #functionSigs
            let rsig = functionSigs $ WithTag rightTag rfname
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

orient :: (RefineTag t, MonadRepGraphForTag t m) => a -> a -> m (ByTag t a)
orient x y = do
    tag <- askTag
    return $ byTagFrom $ \tag' -> if tag' == tag then x else y

askFunName :: MonadRepGraph t m => Visit -> m Ident
askFunName v = do
    p <- askProblem
    return $ p ^. #nodes % at (nodeAddrOf v.nodeId) % unwrapped % expecting #_NodeCall % #functionName

liftWithAddFunc :: MonadRepGraph t m => WithAddFuncInner t m a -> ForTag t (WithAddFunc t m) a
liftWithAddFunc = lift . WithAddFunc
