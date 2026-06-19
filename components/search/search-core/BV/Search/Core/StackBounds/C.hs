{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Search.Core.StackBounds.C
    ( getRecursionIdents
    ) where

import BV.Search.Core.StackBounds.Base

import BV.Search.Core.GraphSlice
import BV.Search.Core.Solver

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Types.Extras.Expr (andE, eqE, falseE, isWordT, notE, trueE,
                                  varFromNameTyE)
import BV.Core.Types.Extras.Problem
import BV.Core.Types.Extras.ProofCheck (eqH, eqSideH, pcTrueH)
import BV.Logging
import BV.Utils (ensureM, expecting, expectingAt)

import Control.Monad (filterM)
import Control.Monad.Extra (findM, whenJust, whileM)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))
import Text.Printf (printf)

getRecursionIdents
    :: forall m n. (Monad m, MonadGraphSliceGetSExprValue n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => RunnerFn n m
    -> M.Map Ident Function
    -> m (M.Map Ident [GraphExpr])
getRecursionIdents runner functions =
    flip execStateT M.empty $ for_ (recursionGroups (makeCallGraph functions)) $ \(group, prevs) -> do
        logInfo $ printf "Doing recursion analysis for function group:"
        logInfo $ printf "  %s" $ intercalate ", " $ map (.unwrap) $ S.toList group
        for_ prevs $ \prev -> do
            whileM $ do
                idents <- get
                let env = Env
                        { runner = Runner runner
                        , functions
                        , group
                        , idents
                        }
                newIdentOpt <- lift $ runReaderT (addRecursionIdent prev) env
                for_ newIdentOpt $ \(fname, newIdent) -> do
                    modify $ M.insertWith (\new old -> old ++ new) fname [newIdent]
                return $ isJust newIdentOpt

type RunnerFn n m = forall t a. HasTagIsAsm t => Problem t -> GraphSliceT t n a -> m a

newtype Runner n m
  = Runner (RunnerFn n m)

data Env n m
  = Env
      { runner :: Runner n m
      , functions :: M.Map Ident Function
      , group :: S.Set Ident
      , idents :: M.Map Ident [GraphExpr]
      }
  deriving (Generic)

run :: (Monad m, HasTagIsAsm t) => Problem t -> GraphSliceT t n a -> ReaderT (Env n m) m a
run p m = do
    Runner f <- gview #runner
    lift $ f p m

data ChainState
  = ChainState
      { problemBuilder :: ProblemBuilder (FunTag C)
      , chain :: [Ident]
      , assns :: [Hyp (FunTag C)]
      }
  deriving (Generic)

addRecursionIdent
    :: forall m n. (Monad m, MonadGraphSliceGetSExprValue n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => Ident
    -> ReaderT (Env n m) m (Maybe (Ident, GraphExpr))
addRecursionIdent f =
    evalStateT inner initState
  where
    initState :: ChainState
    initState = ChainState
        { problemBuilder = initProblemBuilder
        , chain = []
        , assns = []
        }

    inner :: StateT ChainState (ReaderT (Env n m) m) (Maybe (Ident, GraphExpr))
    inner = do
        buildChain Nothing f
        chain <- use #chain
        case length chain of
            1 -> return Nothing
            _ -> do
                deepestTag <- getDeepestTag
                let deepestFunName:_ = chain
                p <- use $ #problemBuilder % to extractProblemWithAnalysis
                assns <- use #assns
                lift $ modelStuff p assns deepestTag deepestFunName

    buildChain :: Maybe (WithTag (FunTag C) NodeAddr) -> Ident -> StateT ChainState (ReaderT (Env n m) m) ()
    buildChain callSiteOpt fname = do
        #chain %= (:) fname
        tag <- getDeepestTag
        p <- zoom #problemBuilder $ do
            functions <- gview #functions
            addEntrypoint $ WithTag tag $ lookupNamed (functions M.!) fname
            doAnalysis
            gets extractProblemWithAnalysis
        whenJust callSiteOpt $ \callSite -> do
            #assns %= (++ functionLinkAssns p callSite tag)
        assns <- use #assns
        callSiteOpt' <- lift $ findUnknownRecursion p tag assns
        for_ callSiteOpt' $ \(callSiteAddr', callSiteNode') -> do
            buildChain (Just (WithTag tag callSiteAddr')) callSiteNode'.functionName

    getDeepestTag :: StateT ChainState (ReaderT (Env n m) m) (FunTag C)
    getDeepestTag = do
        chain <- use #chain
        return $ FunTag (length chain - 1)

modelStuff
    :: forall m n tv. (KnownAsmRefineTag tv, Monad m, MonadGraphSliceGetSExprValue n, MonadGraphSliceSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => ProblemWithAnalysis (FunTag tv)
    -> [Hyp (FunTag tv)]
    -> FunTag tv
    -> Ident
    -> ReaderT (Env n m) m (Maybe (Ident, GraphExpr))
modelStuff p assns deepestTag deepestFunName = do
    unconditional <- isNothing <$> findUnknownRecursion p deepestTag []
    if unconditional
        then return $ Just (deepestFunName, trueE)
        else do
            deepestFun <- gview $ #functions % expectingAt deepestFunName
            let deepestSide = viewAtTag deepestTag p.problem.sides
            let wordArgs =
                    [ (renamed, orig)
                    | (renamed, orig) <- zip deepestSide.input deepestFun.input
                    , isWordT renamed.ty
                    ]
            let entryVis = Visit deepestSide.entryPoint []
            stable <- run p.problem $ runTagged deepestTag $ do
                pcEnv <- fromJust <$> getNodePcEnv entryVis
                liftUntagged (testHypWhyps falseE assns) >>= ensureM . not
                candidates <- for wordArgs $ \(renamed, orig) -> do
                    let argFlat = flattenExpr pcEnv.env $ varFromNameTyE renamed
                    val <- liftUntagged $ getFlatExprValue argFlat
                    return (renamed, orig, argFlat, val)
                flip filterM candidates $ \(_, _, argFlat, val) -> do
                    liftUntagged $ testHypWhyps (argFlat `eqE` flattenExpr M.empty val) assns
            (_, orig, _, val) <- fmap fromJust $ flip findM stable $ \(renamed, _, _, val) -> do
                let assn = eqSideH (varFromNameTyE renamed) (WithTag deepestTag entryVis)
                            `eqH` eqSideH val (WithTag deepestTag entryVis)
                isNothing <$> findUnknownRecursion p deepestTag (assns ++ [assn])
            return $ Just (deepestFunName, varFromNameTyE orig `eqE` val)

functionLinkAssns
    :: ProblemWithAnalysis (FunTag t)
    -> WithTag (FunTag t) NodeAddr
    -> FunTag t
    -> [Hyp (FunTag t)]
functionLinkAssns p callSite newTag = pcTrueH callVis : eqHyps
  where
    newSide = viewAtTag newTag p.problem.sides
    entryVis = WithTag newTag $ Visit newSide.entryPoint []
    callVis = WithTag callSite.tag $ defaultVisit p.analysis.loopData (Addr callSite.value)
    callNode = p.problem ^. #nodes % expectingAt callSite.value % expecting #_NodeCall
    eqHyps =
        [ eqSideH callInput callVis `eqH` eqSideH entryInput entryVis
        | (callInput, entryInput) <- zip callNode.input (map varFromNameTyE newSide.input)
        , case callInput.ty of
            ExprTypeWord {} -> True
            ExprTypeMem -> True
            ExprTypeWordArray {} -> True
            _ -> False
        ]

findUnknownRecursion
    :: forall m n tv. (KnownAsmRefineTag tv, Monad m, MonadGraphSliceSolverInteract n, MonadLoggerWithContext m, MonadLoggerWithContext n)
    => ProblemWithAnalysis (FunTag tv)
    -> FunTag tv
    -> [Hyp (FunTag tv)]
    -> ReaderT (Env n m) m (Maybe (NodeAddr, CallNode))
findUnknownRecursion p tag assns = do
    functions <- gview #functions
    group <- gview #group
    idents <- gview #idents
    let callNodes =
            [ (addr, callNode)
            | (addr, NodeCall callNode) <- M.toAscList p.problem.nodes
            , p.analysis.nodeTag addr == tag
            , callNode.functionName `S.member` group
            ]
    let isUnknown (addr, callNode) = do
            getNodePcEnv (defaultVisit p.analysis.loopData (Addr addr)) >>= \case
                Nothing -> return False
                Just (PcEnv pc env) -> do
                    let calledFunName = callNode.functionName
                    let calledFunInput = (functions M.! calledFunName).input
                    let inpEnv = M.fromList $ zip
                            (map (.name) calledFunInput)
                            (map (flattenExpr env) callNode.input)
                    let calledIdents = map (flattenExpr inpEnv) $ M.findWithDefault [] calledFunName idents
                    let new = foldr1 andE $ pc : map notE calledIdents
                    liftUntagged $ not <$> testHypWhyps (notE new) assns
    run p.problem $ runTagged tag $ findM isUnknown callNodes
