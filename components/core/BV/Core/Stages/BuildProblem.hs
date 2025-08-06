{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.BuildProblem
    ( ProblemBuilder
    , addEntrypoint
    , addEntrypoints
    , buildProblem
    , doAnalysis
    , extractAnalysis
    , extractProblem
    , extractProblemWithAnalysis
    , initProblemBuilder
    , inlineAtPoint
    ) where

import BV.Core.GenerateFreshName
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (ensureM, expecting, expectingIx, unwrapped)

import Control.Monad (forM, unless)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Foldable (forM_, for_)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

buildProblem :: Tag t => (WithTag t Ident -> Function) -> InlineScript t -> ByTag t (Named Function) -> Problem t
buildProblem lookupFun inlineScript funs = runIdentity . flip evalStateT initProblemBuilder $ do
    addEntrypoints funs
    doAnalysis
    for_ inlineScript $ \entry -> do
        inline lookupFun entry
        doAnalysis
    padMergePoints
    doAnalysis
    gets extractProblem

data ProblemBuilder t
  = ProblemBuilder
      { sides :: M.Map t ProblemSide
      , nodes :: M.Map NodeAddr (Maybe (NodeWithMeta t))
      , nodesBySource :: M.Map (t, NodeSource) [NodeAddr]
      , vars :: S.Set Ident
      }
  deriving (Generic)

data NodeWithMeta t
  = NodeWithMeta
      { node :: Node
      , meta :: NodeMeta t
      }
  deriving (Eq, Generic, Ord, Show)

data NodeMeta t
  = NodeMeta
      { tag :: t
      , sourceWithIndex :: Maybe (NodeSource, Int)
      }
  deriving (Eq, Generic, Ord, Show)

initProblemBuilder :: ProblemBuilder t
initProblemBuilder = ProblemBuilder
    { sides = M.empty
    , nodes = M.singleton 0 Nothing -- HACK graph_refine.problem starts at 1
    , nodesBySource = M.empty
    , vars = S.empty
    }

extractProblem :: Tag t => ProblemBuilder t -> Problem t
extractProblem builder = Problem
    { sides = byTagFromN (M.size builder.sides) (builder.sides M.!)
    , nodes = M.mapMaybe (preview (_Just % #node)) builder.nodes
    }

extractAnalysis :: Tag t => ProblemBuilder t -> ProblemAnalysis t
extractAnalysis builder = analyzeProblemFromPartial nodeTag builder.vars (extractProblem builder)
  where
    nodeTag = (M.!) $ M.mapMaybe (fmap (view (#meta % #tag))) builder.nodes

extractProblemWithAnalysis :: Tag t => ProblemBuilder t -> ProblemWithAnalysis t
extractProblemWithAnalysis = ProblemWithAnalysis <$> extractProblem <*> extractAnalysis

nodeAt :: NodeAddr -> Lens' (ProblemBuilder t) Node
nodeAt nodeAddr = nodeWithMetaAt nodeAddr % #node

nodeWithMetaAt :: NodeAddr -> Lens' (ProblemBuilder t) (NodeWithMeta t)
nodeWithMetaAt nodeAddr = #nodes % at nodeAddr % unwrapped % unwrapped

reserveNodeAddr :: (Tag t, Monad m) => StateT (ProblemBuilder t) m NodeAddr
reserveNodeAddr = do
    addr <- gets $ ((+ 1) . fst) . M.findMax . (.nodes)
    modify $ #nodes % at addr ?~ Nothing
    return addr

insertNodeWithMeta :: Monad m => NodeAddr -> (NodeWithMeta t) -> StateT (ProblemBuilder t) m ()
insertNodeWithMeta addr nodeWithMeta = do
    zoom (#nodes % at addr) $ do
        gets isJust >>= ensureM
        put $ Just (Just nodeWithMeta)

insertNode :: (Tag t, Monad m) => NodeAddr -> Node -> t -> Maybe NodeSource -> StateT (ProblemBuilder t) m ()
insertNode addr node tag nodeSourceOpt = do
    sourceWithIndexOpt <- runMaybeT $ do
        nodeSource <- hoistMaybe nodeSourceOpt
        indexInProblem <- lift $ do
            zoom (#nodesBySource % at (tag, nodeSource)) $ do
                v <- gets (fromMaybe [])
                let indexInProblem = length v
                put (Just (v ++ [addr]))
                return indexInProblem
        return (nodeSource, indexInProblem)
    insertNodeWithMeta addr (NodeWithMeta node (NodeMeta tag sourceWithIndexOpt))

appendNode :: (Tag t, Monad m) => Node -> t -> Maybe NodeSource -> StateT (ProblemBuilder t) m NodeAddr
appendNode node tag nodeSourceOpt = do
    addr <- reserveNodeAddr
    insertNode addr node tag nodeSourceOpt
    return addr

getFreshName :: Monad m => Ident -> StateT (ProblemBuilder t) m Ident
getFreshName hint = do
    zoom #vars $ do
        taken <- get
        let name = Ident $ generateFreshName (flip S.member taken . Ident) hint.unwrap
        modify $ S.insert name
        return name

data AddFunctionRenames
  = AddFunctionRenames
      { var :: Map Ident Ident
      , nodeAddr :: Map NodeAddr NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

addFunction
    :: (Tag t, Monad m) => WithTag t (Named Function) -> NodeId -> StateT (ProblemBuilder t) m AddFunctionRenames
addFunction (WithTag tag (Named funName fun)) retTarget = do
    varRenames <- M.fromList <$>
        forM (S.toAscList origVars) (\name -> (name,) <$> getFreshName name)
    nodeAddrRenames <- M.fromList <$>
        forM (S.toAscList origNodeAddrs) (\addr -> (addr,) <$> reserveNodeAddr)
    let renames = AddFunctionRenames
            { var = varRenames
            , nodeAddr = nodeAddrRenames
            }
        adaptNodeId = \case
            Ret -> retTarget
            Err -> Err
            Addr addr -> Addr (renames.nodeAddr ! addr)
        adaptNode = (varNamesOf %~ (renames.var !)) . (nodeConts %~ adaptNodeId)
    forM_ origNodeAddrs $ \origAddr ->
        let newNodeAddr = renames.nodeAddr ! origAddr
            newNode = adaptNode (funBody.nodes ! origAddr)
            nodeSource = NodeSource funName origAddr
         in insertNode newNodeAddr newNode tag (Just nodeSource)
    return renames
  where
    funBody = fun ^. #body % unwrapped
    funGraph = makeNodeGraph funBody.nodes
    origNodeAddrs = S.fromList $ reachableFrom funGraph funBody.entryPoint ^.. traversed % #_Addr
    origVars = S.fromList $ fun ^.. varDeclsOf % #name

addEntrypoint :: (Tag t, Monad m) => WithTag t (Named Function) -> StateT (ProblemBuilder t) m ()
addEntrypoint namedFun@(WithTag tag (Named name fun)) = do
    renames <- addFunction namedFun Ret
    let renameArgs = traversed % #name %~ (renames.var !)
    let side = ProblemSide
            { name = name
            , entryPoint = (fromJust fun.body).entryPoint & #_Addr %~ (renames.nodeAddr !)
            , input = renameArgs fun.input
            , output = renameArgs fun.output
            }
    #sides %= M.insert tag side

addEntrypoints :: (Tag t, Monad m) => ByTag t (Named Function) -> StateT (ProblemBuilder t) m ()
addEntrypoints funs = for_ (withTags funs) $ \fun -> addEntrypoint fun

--

inline :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> InlineScriptEntry t -> StateT (ProblemBuilder t) m ()
inline lookupFun entry = do
    nodeAddr <- use $
        #nodesBySource % at (entry.tag, entry.nodeSource) % unwrapped
            % expectingIx entry.indexInProblem
    inlineInner lookupFun nodeAddr entry

inlineAtPoint
    :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> NodeAddr -> StateT (ProblemBuilder t) m (InlineScriptEntry t)
inlineAtPoint lookupFun nodeAddr = do
    meta <- use $ #nodes % at nodeAddr % unwrapped % unwrapped % #meta
    inlinedFunctionName <- use $ nodeAt nodeAddr % expecting #_NodeCall % #functionName
    let Just (nodeSource, indexInProblem) = meta.sourceWithIndex
    let entry = InlineScriptEntry
            { tag = meta.tag
            , nodeSource
            , indexInProblem
            , inlinedFunctionName
            }
    inlineInner lookupFun nodeAddr entry
    return entry

inlineInner
    :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> NodeAddr -> InlineScriptEntry t -> StateT (ProblemBuilder t) m ()
inlineInner lookupFun nodeAddr entry = do
    callNode <- use $ nodeAt nodeAddr % expecting #_NodeCall
    ensureM $ callNode.functionName == entry.inlinedFunctionName
    let fun = lookupFun (WithTag entry.tag callNode.functionName)
    exitNodeAddr <- reserveNodeAddr
    renames <- addFunction (WithTag entry.tag (Named callNode.functionName fun)) (Addr exitNodeAddr)
    let entryNodeAddr = renames.nodeAddr ! (fun ^. #body % unwrapped % #entryPoint % expecting #_Addr)
    let newNode = NodeBasic $ BasicNode
            { next = Addr entryNodeAddr
            , varUpdates =
                [ VarUpdate
                    { var = NameTy
                        { name = renames.var ! arg.name
                        , ty = arg.ty
                        }
                    , val = callInput
                    }
                | (arg, callInput) <- zip fun.input callNode.input
                ]
            }
    insertNode nodeAddr newNode entry.tag Nothing
    let exitNode = NodeBasic $ BasicNode
            { next = callNode.next
            , varUpdates =
                [ VarUpdate
                    { var = NameTy
                        { name = callOutput.name
                        , ty = arg.ty
                        }
                    , val = varE arg.ty (renames.var ! arg.name)
                    }
                | (arg, callOutput) <- zip fun.output callNode.output
                ]
            }
    insertNode exitNodeAddr exitNode entry.tag Nothing

--

doAnalysis :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
doAnalysis = do
    forceSimpleLoopReturns

forceSimpleLoopReturns :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
forceSimpleLoopReturns = do
    ProblemWithAnalysis problem analysis <- gets extractProblemWithAnalysis
    for_ (loopsOf analysis.loopData) $ \loop -> do
        let tag = analysis.nodeTag loop.head
        let rets = S.toList $ S.filter (`S.member` loop.body) $ viewAtTag tag analysis.preds (Addr loop.head)
        let alreadySimple = [ isNodeNoop (problem.nodes ! ret) | ret <- rets ] == [True]
        unless alreadySimple $ do
            simpleRetNodeAddr <- appendNode (trivialNode (Addr loop.head)) tag Nothing
            forM_ rets $ \ret -> modifying (nodeAt ret % nodeConts) $ \cont ->
                if cont == Addr loop.head
                then Addr simpleRetNodeAddr
                else cont

padMergePoints :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
padMergePoints = do
    ProblemWithAnalysis problem analysis <- gets extractProblemWithAnalysis
    let mergePreds =
            M.filter
                (\preds -> S.size preds > 1)
                (M.fromSet
                    (\n -> viewAtTag (analysis.nodeTag n) analysis.preds (Addr n))
                    (M.keysSet problem.nodes))
    nonTrivialEdgesToMergePoints <- fmap concat . forM (M.toAscList mergePreds) $ \(nodeAddr, nodePreds) -> do
        fmap concat . forM (S.toAscList nodePreds) $ \predNodeAddr -> do
            predNode <- use $ nodeAt predNodeAddr
            return $ case predNode of
                NodeBasic (BasicNode { varUpdates = [] }) -> []
                _ -> [(predNodeAddr, nodeAddr)]
    forM_ nonTrivialEdgesToMergePoints $ \(predNodeAddr, nodeAddr) -> do
        let paddingNode = NodeBasic $ BasicNode
                { next = Addr nodeAddr
                , varUpdates = []
                }
        paddingNodeAddr <- appendNode paddingNode (analysis.nodeTag nodeAddr) Nothing
        modifying (nodeAt predNodeAddr % nodeConts % #_Addr) $ \contNodeAddr ->
            if contNodeAddr == nodeAddr
            then paddingNodeAddr
            else contNodeAddr
