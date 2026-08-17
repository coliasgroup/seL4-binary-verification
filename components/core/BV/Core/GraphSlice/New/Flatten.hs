{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module BV.Core.GraphSlice.New.Flatten
    ( ExprEnv
    , GraphSliceHooks
    , GraphSliceT
    , NormalizedVisit
    , PcEnv (..)
    , VisitTooGeneral
    , askProblemWithAnalysis
    , defaultGraphSliceHooks
    , flattenExpr
    , getAllPcEnvsNorm
    , getCallOrderCompatNorm
    , getCallsByFunNameNorm
    , getInductVar
    , getPcEnvNorm
    , getSuccessVarNorm
    , normalizeGeneralVisit
    , normalizeVisit
    , runGraphSliceTStep
    , unwrapNormalizedVisit
    , withAddFunAsserts
    , withConstRetAssumptions
    , withFast
    , withIsStack
    ) where

import BV.Core.GraphSlice.New.Common
import BV.Core.GraphSlice.New.Flat
import BV.Core.GraphSlice.New.Flatten.CallCount
import BV.Core.GraphSlice.New.Flatten.NameHint
import BV.Core.GraphSlice.New.Flatten.Visit
import BV.Core.GraphSlice.New.Flatten.VisitInfo
import BV.Core.GraphSlice.New.SendFlatExprCommand (FlatExpr)

import BV.Core.Logic (eqHandlingRelWrapper, weakenAssert)
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils (withMapSlotWith)
import BV.Utils

import Control.Monad (guard, when, (>=>))
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (Reader, ReaderT, mapReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, mapStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.List (genericIndex)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

--

type T = GraphSliceT

type InnerT = GraphSliceFlatT

type C t m = (Tag t, MonadGraphSliceSendSExpr m)

--

newtype GraphSliceT t m a
  = GraphSliceT { run :: StateT (TState t) (ReaderT (TEnv t) (InnerT m)) a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans (T t) where
    lift = liftInner . lift

instance MonadLiftInner InnerT (T t) where
    liftInner = GraphSliceT . lift . lift

instance MonadMapInnermost (T t) where
    mapInnermost f = #run %~ mapStateT (mapReaderT (mapInnermost f))

runGraphSliceTStep
    :: (Tag t, Monad m)
    => ProblemWithAnalysis t
    -> GraphSliceHooks t
    -> T t m a
    -> InnerT m a
runGraphSliceTStep pwa hooks =
      flip runReaderT (initEnv pwa hooks)
    . flip evalStateT initState
    . (.run)

data TEnv t
  = TEnv
      { pwa :: ProblemWithAnalysis t
      , hooks :: GraphSliceHooks t
      }
  deriving (Generic)

data GraphSliceHooks t
  = GraphSliceHooks
      { isStack :: WithTag t Ident -> FunctionSignatureDirection -> Integer -> Bool
      , addFunAsserts :: AddFunAssertsHook t
      , fast :: Bool
      , constRetAssumptions :: WithTag t Ident -> Integer -> Maybe Integer
      }
  deriving (Generic)

data TState t
  = TState
      { inputEnvs :: Map t ExprEnv
      , inboundVisitInfo :: Map (NormalizedVisit t) (Maybe VisitInfo)
      , outboundVisitInfo :: Map (NormalizedVisit t) (Maybe (Map NodeId VisitInfo))
      , successVars :: Map (NormalizedVisit t) FlatExpr
      , inductVars :: Map EqHypInduct FlatExpr
      , callsByFunName :: Map (WithTag t Ident) (S.Set (NormalizedVisit t))
      , callOrderCompat :: Seq (NormalizedVisit t)
      }
  deriving (Generic)

initEnv :: Tag t => ProblemWithAnalysis t -> GraphSliceHooks t -> TEnv t
initEnv pwa hooks = TEnv
    { pwa
    , hooks
    }

defaultGraphSliceHooks :: GraphSliceHooks t
defaultGraphSliceHooks = GraphSliceHooks
    { addFunAsserts = AddFunAssertsHook $ \_ -> return ()
    , isStack = \_ _ _ -> False
    , fast = False
    , constRetAssumptions = \_ _ -> Nothing
    }

withAddFunAsserts
    :: RefineTag t
    => LookupFunctionSignature t
    -> Pairings t
    -> GraphSliceHooks t
    -> GraphSliceHooks t
withAddFunAsserts lookupSig pairings = #addFunAsserts .~ addFunAssertsHook lookupSig pairings

withIsStack
    :: (WithTag t Ident -> FunctionSignatureDirection -> Integer -> Bool)
    -> GraphSliceHooks t
    -> GraphSliceHooks t
withIsStack = set #isStack

withFast :: GraphSliceHooks t -> GraphSliceHooks t
withFast = #fast .~ True

withConstRetAssumptions
    :: (WithTag t Ident -> Integer -> Maybe Integer)
    -> GraphSliceHooks t
    -> GraphSliceHooks t
withConstRetAssumptions = set #constRetAssumptions

initState :: TState t
initState = TState
    { inputEnvs = M.empty
    , inboundVisitInfo = M.empty
    , outboundVisitInfo = M.empty
    , successVars = M.empty
    , inductVars = M.empty
    , callsByFunName = M.empty
    , callOrderCompat = Seq.empty
    }

--

liftFlat :: Monad m => InnerT m a -> T t m a
liftFlat = liftInner

liftPure :: Monad m => StateT (TState t) (Reader (TEnv t)) a -> T t m a
liftPure = GraphSliceT . mapStateT (mapReaderT (return . runIdentity))

withMapSlot :: (C t m, Ord k) => Lens' (TState t) (Map k v) -> k -> T t m v -> T t m v
withMapSlot = withMapSlotWith $ liftPure . mapStateT (return . runIdentity)

--

normTag :: Tag t => NormalizedVisit t -> t
normTag norm = (unwrapNormalizedVisit norm).tag

withNormTag :: Tag t => NormalizedVisit t -> a -> WithTag t a
withNormTag = WithTag . normTag

--

askProblemWithAnalysis :: Monad m => T t m (ProblemWithAnalysis t)
askProblemWithAnalysis = liftPure $ gview #pwa

askProblemSide :: (Tag t, Monad m) => t -> T t m ProblemSide
askProblemSide tag = do
    p <- askProblemWithAnalysis
    return $ viewAtTag tag p.problem.sides

askProblemSideWithAnalysis :: (Tag t, Monad m) => t -> T t m ProblemSideWithAnalysis
askProblemSideWithAnalysis tag = problemSideWithAnalysis tag <$> askProblemWithAnalysis

askNode :: (Tag t, Monad m) => NormalizedVisit t -> T t m Node
askNode norm = do
    side <- askProblemSide $ normTag norm
    return $ side.nodes ! (nodeAddrOf visit.nodeId)
  where
    visit = unwrapNormalizedVisit norm

askCallNode :: (Tag t, Monad m) => NormalizedVisit t -> T t m CallNode
askCallNode norm = view (expecting #_NodeCall) <$> askNode norm

askNodeFunName :: (Tag t, Monad m) => NormalizedVisit t -> T t m Ident
askNodeFunName norm = (.functionName) <$> askCallNode norm

askHook :: C t m => Lens' (GraphSliceHooks t) a -> T t m a
askHook l = liftPure $ gview $ #hooks % l

askIsStack :: C t m => WithTag t Ident -> FunctionSignatureDirection -> Integer -> T t m Bool
askIsStack funName dir i = do
    isStackHook <- askHook #isStack
    return $ isStackHook funName dir i

--

flattenExpr :: ExprEnv -> GraphExpr -> FlatExpr
flattenExpr = flip go
  where
    go = traverseOf (exprArgs % traversed) go >=> \expr -> case expr.value of
        ExprValueVar name -> (! name)
        _ -> return expr

flattenAndAddDef :: C t m => ExprEnv -> GraphExpr -> NameHint -> T t m FlatExpr
flattenAndAddDef env expr hint = liftFlat $ addDef hint $ flattenExpr env expr

contractVisitInfo :: C t m => NormalizedVisit t -> VisitInfo -> T t m VisitInfo
contractVisitInfo norm (VisitInfo pc env callCount) = do
    pc' <- liftFlat $ cacheExpr (pathCondName norm) pc
    let f name val = liftFlat $ cacheExprInline (localNameBefore norm name) val
    env' <- M.traverseWithKey f env
    return $ VisitInfo pc' env' callCount

addOpaqueVar :: C t m => NameHint -> ExprType -> T t m FlatExpr
addOpaqueVar hint ty = liftFlat $ varFromNameTyE <$> addVar hint ty

addOpaqueVarWithIsStack
    :: C t m
    => (Ident -> NameHint)
    -> NameTy
    -> Bool
    -> Bool
    -> T t m FlatExpr
addOpaqueVarWithIsStack f var isStack splitStack = do
    if isStack
    then ensure (isMemT var.ty) $ markedStackE <$> add (if splitStack then stackT else memT)
    else add var.ty
  where
    add = addOpaqueVar $ f var.name

getIsExprStack :: (Tag t, Monad m) => FlatExpr -> T t m Bool
getIsExprStack =
    \expr -> if isMemT expr.ty then go expr else return False
  where
    go expr' = case expr'.value of
        ExprValueOp OpMemUpdate [m, _, _] -> go m
        ExprValueOp OpIfThenElse [_, l, r] -> ensureEq <$> go l <*> go r
        ExprValueOp (OpExt OpExtMarkedStack) _ -> return True
        ExprValueVar name -> liftFlat (lookupDef name) >>= maybe (return False) go
        _ -> unexpected
    ensureEq x y = ensure (x == y) x

--

getPcEnvNorm :: C t m => NormalizedVisit t -> T t m (Maybe PcEnv)
getPcEnvNorm norm = fmap visitInfoPcEnv <$> getInboundVisitInfo norm

getAllPcEnvsNorm :: (Tag t, Monad m) => T t m [(NormalizedVisit t, PcEnv)]
getAllPcEnvsNorm = do
    inboundVisitInfo <- liftPure $ use #inboundVisitInfo
    return
        [ (norm, visitInfoPcEnv info)
        | (norm, Just info) <- M.toList inboundVisitInfo
        ]

getCallsByFunNameNorm :: (Tag t, Monad m) => T t m (M.Map (WithTag t Ident) [NormalizedVisit t])
getCallsByFunNameNorm = liftPure $ fmap toList <$> use #callsByFunName

getCallOrderCompatNorm :: (Tag t, Monad m) => T t m [NormalizedVisit t]
getCallOrderCompatNorm = liftPure $ toList <$> use #callOrderCompat

getSuccessVarNorm :: C t m => NormalizedVisit t -> T t m FlatExpr
getSuccessVarNorm norm = withMapSlot #successVars norm $ do
    funName <- askNodeFunName norm
    addOpaqueVar (successName norm funName) boolT

getInductVar :: C t m => EqHypInduct -> T t m FlatExpr
getInductVar induct = withMapSlot #inductVars induct $
    addOpaqueVar (inductVarName induct) word32T

--

getInboundVisitInfo :: C t m => NormalizedVisit t -> T t m (Maybe VisitInfo)
getInboundVisitInfo norm = withMapSlot #inboundVisitInfo norm $
    visitKind norm <$> askProblemWithAnalysis >>= \case
        VisitKindEntryPoint -> do
            env <- getInputEnv tag
            return $ Just $ VisitInfo trueE env emptyCallCount
        VisitKindPostLoop preLoop -> do
            getInboundVisitInfo preLoop >>= traverse (getPostLoopVisitInfo norm)
        VisitKindNormal preds -> do
            optArcs <- for preds $ \pred_ ->
                (>>= M.lookup nodeId) <$> getOutboundVisitInfo pred_
            case catMaybes optArcs of
                [] -> return Nothing
                arcs -> Just <$> do
                    let optimize = case nodeId of
                            Err -> traversed % #env .~ M.empty
                            _ -> id
                    contractVisitInfo norm $ mergeVisitInfo $ optimize arcs
  where
    visit = unwrapNormalizedVisit norm
    nodeId = visit.nodeId
    tag = visit.tag

getInputEnv :: C t m => t -> T t m ExprEnv
getInputEnv tag = withMapSlot #inputEnvs tag $ do
    side <- askProblemSide tag
    fmap M.fromList $ for (zip [0..] side.input) $ \(i, sigVar) -> (sigVar.name,) <$> do
        isStack <- askIsStack (WithTag tag side.name) FunctionSignatureDirectionIn i
        addOpaqueVarWithIsStack (initName tag) sigVar isStack False

getPostLoopVisitInfo :: C t m => NormalizedVisit t -> VisitInfo -> T t m VisitInfo
getPostLoopVisitInfo norm preLoopInfo = do
    side <- askProblemSideWithAnalysis tag
    fast <- askHook #fast
    constRetAssumptions <- (. WithTag tag) <$> askHook #constRetAssumptions
    let loop = fromJust $ outermostLoopContaining side.analysis.loopData visitAddr
    let isConst var =
            let alwaysCheck = case var.ty of
                    ExprTypeHtd -> True
                    ExprTypeDom -> True
                    _ -> False
             in not (loopIsComplex loop)
                    && (alwaysCheck || fast)
                    && isSyntacticConstant side constRetAssumptions var loop visitAddr
    env <- ifor preLoopInfo.env $ \name val ->
        let var = NameTy name val.ty
         in if isConst var
            then return val
            else do
                isStack <- getIsExprStack $ preLoopInfo.env ! name
                addOpaqueVarWithIsStack (localNameAfterLoop norm) var isStack True
    pc <- addOpaqueVar (pathCondNameAfterLoop norm) boolT
    return $ VisitInfo
        { pc
        , env
        , callCount =
            let calledFuns =
                    [ side.problem.nodes ! addr | addr <- toList loop.members ]
                        ^.. folded % #_NodeCall % #functionName
             in foldl (flip addUnboundedCalls) preLoopInfo.callCount calledFuns
        }
  where
    visit = unwrapNormalizedVisit norm
    tag = visit.tag
    visitAddr = nodeAddrOf visit.nodeId

getOutboundVisitInfo :: C t m => NormalizedVisit t -> T t m (Maybe (Map NodeId VisitInfo))
getOutboundVisitInfo norm = withMapSlot #outboundVisitInfo norm $ do
    infoOpt <- getInboundVisitInfo norm
    for infoOpt $ \info@(VisitInfo { pc, env }) -> M.fromList <$> do
        askNode norm >>= \case
            node | pc == falseE -> return
                [ (cont, VisitInfo falseE M.empty emptyCallCount)
                | cont <- node ^.. nodeConts
                ]
            NodeBasic basicNode -> do
                updates <- for basicNode.varUpdates $ \update ->
                    (update.var.name,) <$> case update.val.value of
                        ExprValueVar name -> return $ env ! name
                        _ -> flattenAndAddDef env update.val $ localName norm update.var.name
                let env' = M.union (M.fromList updates) env
                return [(basicNode.next, info & #env .~ env')]
            NodeCond condNode ->
                if  | condNode.left == condNode.right -> return [(condNode.left, info)]
                    | condNode.expr == trueE -> return
                        [ (condNode.left, info)
                        , (condNode.right, info & #pc .~ falseE)
                        ]
                    -- TODO why doesn't this work?
                    -- | condNode.expr == falseE -> return
                    --     [ (condNode.left, info & #pc .~ falseE)
                    --     , (condNode.right, info)
                    --     ]
                    | otherwise -> do
                        cond <- flattenAndAddDef env condNode.expr $ condName norm
                        return
                            [ (condNode.left, info & #pc .~ andE cond pc)
                            , (condNode.right, info & #pc .~ andE (notE cond) pc)
                            ]
            NodeCall callNode -> do
                info' <- getPostCallVisitInfo norm info callNode
                success <- getSuccessVarNorm norm
                AddFunAssertsHook addFunAsserts <- askHook #addFunAsserts
                addFunAsserts $ withNormTag norm $ FunAssertInput
                    { callNode
                    , inboundInfo = info
                    , outboundInfo = info'
                    , success
                    }
                liftPure $ do
                    #callsByFunName %=
                        M.insertWith (<>) (withNormTag norm callNode.functionName) (S.singleton norm)
                    #callOrderCompat %=
                        (Seq.|> norm)
                return [(callNode.next, info')]

getPostCallVisitInfo
    :: forall t m. C t m
    => NormalizedVisit t
    -> VisitInfo
    -> CallNode
    -> T t m VisitInfo
getPostCallVisitInfo norm preCallInfo callNode = do
    fast <- askHook #fast
    constRetAssumptions <- askHook #constRetAssumptions
    newVars <- for (zip [0..] callNode.output) $ \(i, sigVar) -> (sigVar.name,) <$>
        case guard fast >> constRetAssumptions funName i of
            Just j -> return $ flattenExpr preCallInfo.env $ callNode.input `genericIndex` j
            Nothing -> do
                isStack <- askIsStack funName FunctionSignatureDirectionOut i
                addOpaqueVarWithIsStack (localName norm) sigVar isStack True
    return $ VisitInfo
        { pc = preCallInfo.pc
        , env = M.union (M.fromList newVars) preCallInfo.env
        , callCount = addCall callNode.functionName preCallInfo.callCount
        }
  where
    funName = withNormTag norm callNode.functionName

--

type AddFunAssertsHookFn t =
    forall m. MonadGraphSliceSendSExpr m
    => WithTag t FunAssertInput
    -> T t m ()

newtype AddFunAssertsHook t
  = AddFunAssertsHook (AddFunAssertsHookFn t)

addFunAssertsHook :: RefineTag t => LookupFunctionSignature t -> Pairings t -> AddFunAssertsHook t
addFunAssertsHook lookupSig pairings = AddFunAssertsHook $
    addFunAssertsImpl lookupSig (lookupPairingIdMap !?) pairings
  where
    lookupPairingIdMap = M.fromList $ concatMap toList
        [ (,p) <$> withTags p | p <- M.keys pairings.unwrap]

addFunAssertsImpl
    :: forall t m. (RefineTag t, MonadGraphSliceSendSExpr m)
    => LookupFunctionSignature t
    -> (WithTag t Ident -> Maybe (PairingId t))
    -> Pairings t
    -> WithTag t FunAssertInput
    -> T t m ()
addFunAssertsImpl lookupSig lookupPairingId pairings (WithTag tag input) = do
    let byTag :: forall a. a -> a -> ByTag t a
        byTag x y = byTagFrom $ \t -> if t == tag then x else y
    for_ (lookupPairingId (WithTag tag input.callNode.functionName)) $ \pairingId -> do
        let otherFunName = viewWithTag (otherTag tag) pairingId
        group <- liftPure $ use $ #callsByFunName % to (M.findWithDefault S.empty otherFunName)
        for_ group $ \otherNorm -> do
            otherInboundInfo <- fromJust <$> getInboundVisitInfo otherNorm
            let compat = areCallCountsCompatible
                    lookupPairingId
                    ((.callCount) <$> byTag input.inboundInfo otherInboundInfo)
            when compat $ do
                otherInput <- do
                    callNode <- askCallNode otherNorm
                    outboundInfo <- deconstructSingleton . fromJust <$> getOutboundVisitInfo otherNorm
                    success <- getSuccessVarNorm otherNorm
                    return $ FunAssertInput
                        { callNode
                        , inboundInfo = otherInboundInfo
                        , outboundInfo
                        , success
                        }
                let pairing = pairings.unwrap ! pairingId
                let sigs = lookupSig <$> withTags pairingId
                let imp = funAssert pairing sigs (byTag input otherInput)
                liftFlat $ assertFlatExpr $ weakenAssert imp
  where
    deconstructSingleton m = case toList m of
        [x] -> x
        _ -> error "unexpected"

data FunAssertInput
  = FunAssertInput
      { callNode :: CallNode
      , inboundInfo :: VisitInfo
      , outboundInfo :: VisitInfo
      , success :: FlatExpr
      }
  deriving (Generic)

funAssert
    :: RefineTag t
    => Pairing t
    -> ByTag t FunctionSignature
    -> ByTag t FunAssertInput
    -> FlatExpr
funAssert pairing sigs inputs = impliesE
    (foldr1 andE (instEqs pairing.inEqs ++ [inputs.right.inboundInfo.pc]))
    (foldr1 andE (instEqs pairing.outEqs ++ [inputs.right.success `impliesE` inputs.left.success]))
  where
    instEqs eqs =
        [ let f side = flattenExpr (envForQuadrant side.quadrant) side.expr
           in (eqHandlingRelWrapper `on` f) eq.lhs eq.rhs
        | eq <- eqs
        ]
    envForQuadrant (PairingEqSideQuadrant tag direction) =
        let FunAssertInput { .. } = viewAtTag tag inputs
            sig = viewAtTag tag sigs
         in case direction of
                PairingEqDirectionIn ->
                    M.fromList $
                        zip
                            (map (.name) sig.input)
                            [ flattenExpr inboundInfo.env in_ | in_ <- callNode.input ]
                PairingEqDirectionOut ->
                    M.fromList $
                        zip
                            (map (.name) sig.output)
                            [ outboundInfo.env ! out.name | out <- callNode.output ]
