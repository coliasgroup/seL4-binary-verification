module BV.Core.RepGraph.FunAsserts
    ( WithFunAsserts
    , runWithFunAsserts
    ) where

import BV.Core.Logic
import BV.Core.RepGraph.Core
import BV.Core.RepGraph.Solver
import BV.Core.Structs
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (expecting, expectingAt, unwrapped)

import Control.Monad (when, (>=>))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (for_, toList)
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

type WithFunAssertsInner t m = StateT (State t) (ReaderT (Env t) m)

newtype WithFunAsserts t m a
  = WithFunAsserts { run :: WithFunAssertsInner t m a }
  deriving (Functor)
  deriving newtype (Applicative, Monad)

instance MonadTrans (WithFunAsserts t) where
    lift = WithFunAsserts . lift . lift

data Env t
  = Env
      { lookupSig :: LookupFunctionSignature t
      , pairings :: Pairings t
      , pairingsAccess :: M.Map (WithTag t Ident) (PairingId t)
      }
  deriving (Generic)

data State t
  = State
      { funcsByName :: M.Map (WithTag t Ident) [Visit]
      }
  deriving (Generic)

runWithFunAsserts :: RefineTag t => MonadRepGraph t m => LookupFunctionSignature t -> Pairings t -> WithFunAsserts t m a -> m a
runWithFunAsserts lookupSig pairings m =
    runReaderT
        (evalStateT m.run initState)
        (initEnv lookupSig pairings)

initEnv :: RefineTag t => LookupFunctionSignature t -> Pairings t -> Env t
initEnv lookupSig pairings = Env
    { lookupSig
    , pairings
    , pairingsAccess = M.fromList $ concatMap toList
        [ (,p) <$> withTags p | p <- M.keys pairings.unwrap]
    }

initState :: State t
initState = State
    { funcsByName = M.empty
    }

instance MonadRepGraphSolverSend m => MonadRepGraphSolverSend (WithFunAsserts t m) where
    sendSExprWithPlaceholders = WithFunAsserts . sendSExprWithPlaceholders

instance MonadStructs m => MonadStructs (WithFunAsserts t m) where
    askLookupStruct = WithFunAsserts askLookupStruct

instance MonadRepGraphSolver m => MonadRepGraphSolver (WithFunAsserts t m) where
    liftSolver = WithFunAsserts . liftSolver

instance MonadRepGraph t m => MonadRepGraphDefaultHelper t m (WithFunAsserts t m)

instance (RefineTag t, MonadRepGraph t m) => MonadRepGraph t (WithFunAsserts t m) where
    runPostEmitCallNodeHook = askWithTag >=> lift . addFunAsserts

--

addFunAsserts :: RefineTag t => MonadRepGraph t m => WithTag t Visit -> WithFunAsserts t m ()
addFunAsserts (WithTag tag visit) = do
    funName <- WithTag tag <$> askFunName visit
    pairingIdOpt <- WithFunAsserts $ gview $ #pairingsAccess % at funName
    for_ pairingIdOpt $ \pairingId -> do
        let otherFunName = viewAtTag (otherTag tag) (withTags pairingId)
        group <- WithFunAsserts $ use $ #funcsByName % to (M.findWithDefault [] otherFunName)
        for_ group $ \otherVisit -> do
            let visits = byTagFrom $ \tag' -> if tag' == tag then visit else otherVisit
            compat <- areFunCallsCompatible visits
            when compat $ do
                imp <- getFunAssert visits
                withoutEnv $ assertFact $ weakenAssert imp
        WithFunAsserts $ #funcsByName %= M.insertWith (flip (<>)) funName [visit]

areFunCallsCompatible :: (RefineTag t, MonadRepGraph t m) => ByTag t Visit -> WithFunAsserts t m Bool
areFunCallsCompatible visits = do
    lowLevelInfoByTag <- getFunCallInfoRawByTag visits
    memCalls <- for lowLevelInfoByTag $ \lowLevelInfo -> scanMemCalls lowLevelInfo.ins
    memCallsCompatible memCalls

memCallsCompatible :: (RefineTag t, MonadRepGraph t m) => ByTag t MemCallsIfKnown -> WithFunAsserts t m Bool
memCallsCompatible memCalls = case sequenceA memCalls of
    Nothing -> return True
    Just calls -> do
        rcastcalls <- fmap (M.fromList . catMaybes) $ for (M.toList calls.left) $ \(lname, lcallsForFun) -> do
            pairingId <- WithFunAsserts $ gview $ #pairingsAccess % expectingAt (WithTag leftTag lname)
            let rname = pairingId.right
            lookupSig <- WithFunAsserts $ gview #lookupSig
            let rsig = lookupSig $ WithTag rightTag rname
            return $
                if any (\arg -> arg.ty == memT) rsig.output
                then Just (rname, lcallsForFun)
                else Nothing
        let compat rname =
                let rcast = fromMaybe zeroMemCallsRange $ rcastcalls !? rname
                    ractual = fromMaybe zeroMemCallsRange $ calls.right !? rname
                 in maybe True (ractual.min <=) rcast.max && maybe True (rcast.min <=) ractual.max
        return $ all compat $ S.toList $ M.keysSet calls.right <> M.keysSet rcastcalls

getFunAssert :: (RefineTag t, MonadRepGraph t m) => ByTag t Visit -> WithFunAsserts t m GraphExpr
getFunAssert visits = do
    pairingId <- traverse askFunName visits
    pairing <- WithFunAsserts $ gview $ #pairings % #unwrap % expectingAt pairingId
    lookupSig <- WithFunAsserts $ gview #lookupSig
    let sigs = lookupSig <$> withTags pairingId
    lowLevelInfo <- getFunCallInfoRawByTag visits
    let info = augmentFunCallInfo <$> sigs <*> lowLevelInfo
    pcs <- traverse getPcWithTag (withTags visits)
    let instEqs eqs = for eqs $ \eq ->
            instEqWithEnvs
                (eq.lhs.expr, envForQuadrant eq.lhs.quadrant info)
                (eq.rhs.expr, envForQuadrant eq.rhs.quadrant info)
    inEqs <- instEqs pairing.inEqs
    outEqs <- instEqs pairing.outEqs
    return $ impliesE
        (foldr1 andE (inEqs ++ [pcs.right]))
        (foldr1 andE (outEqs ++ [info.right.success `impliesE` info.left.success]))

--

askFunName :: MonadRepGraph t m => Visit -> m Ident
askFunName v = do
    p <- askProblem
    return $ p ^. #nodes % at (nodeAddrOf v.nodeId) % unwrapped % expecting #_NodeCall % #functionName

getFunCallInfoRawByTag
    :: (RefineTag t, MonadRepGraph t m)
    => ByTag t Visit
    -> WithFunAsserts t m (ByTag t FunCallInfo)
getFunCallInfoRawByTag visits = for (withTags visits) $ \(WithTag tag visit) ->
    runForTag tag $ getFunCallInfoRaw visit

data FunCallInfoWithNames
  = FunCallInfoWithNames
      { ins :: ExprEnv
      , outs :: ExprEnv
      , success :: GraphExpr
      }
  deriving (Eq, Generic, Ord, Show)

augmentFunCallInfo :: FunctionSignature -> FunCallInfo -> FunCallInfoWithNames
augmentFunCallInfo sig info = FunCallInfoWithNames
    { ins = M.fromList (zip sig.input (map snd info.ins))
    , outs = M.fromList (zip sig.output (map snd info.outs))
    , success = info.success
    }

envForQuadrant :: Tag t => PairingEqSideQuadrant t -> ByTag t FunCallInfoWithNames -> ExprEnv
envForQuadrant (PairingEqSideQuadrant t direction) = view $ atTag t % directionLabel
  where
    directionLabel = case direction of
        PairingEqDirectionIn -> #ins
        PairingEqDirectionOut -> #outs
