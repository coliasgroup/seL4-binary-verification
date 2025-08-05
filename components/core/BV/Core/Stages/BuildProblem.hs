{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.BuildProblem
    ( ProblemBuilder
    , addEntrypoint
    , addEntrypoints
    , buildProblem
    , doAnalysis
    , extractProblem
    , initProblemBuilder
    , inline
    , inlineAtPoint
    ) where

import BV.Core.GenerateFreshName
import BV.Core.Graph
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (ensureM, expecting, expectingIx, unwrapped)

import Control.Monad (forM, unless)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Foldable (forM_, for_, toList)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=))

buildProblem :: Tag t => (WithTag t Ident -> Function) -> InlineScript t -> ByTag t (Named Function) -> Problem t
buildProblem lookupFun inlineScript funs = runIdentity . flip evalStateT initProblemBuilder $ do
    addEntrypoints funs
    doAnalysis
    for_ inlineScript $ \entry -> do
        entry' <- inline lookupFun entry.nodeBySource
        ensureM $ entry' == entry
        doAnalysis
    padMergePoints
    doAnalysis
    gets extractProblem

data ProblemBuilder t
  = ProblemBuilder
      { sides :: M.Map t ProblemSide
      , nodes :: M.Map NodeAddr (Maybe (NodeWithMeta t))
      , nodeSources :: M.Map NodeAddr (NodeBySource t)
      , nodesBySource :: M.Map (NodeSource t) [NodeAddr]
      , vars :: S.Set Ident
      }
  deriving (Generic, Show)

data NodeWithMeta t
  = NodeWithMeta
      { node :: Node
      , meta :: NodeMeta t
      }
  deriving (Eq, Generic, Ord, Show)

data NodeMeta t
  = NodeMeta
      { bySource :: Maybe (NodeBySource t)
      }
  deriving (Eq, Generic, Ord, Show)

initProblemBuilder :: ProblemBuilder t
initProblemBuilder = ProblemBuilder
    { sides = M.empty
    , nodes = M.singleton 0 Nothing -- HACK graph_refine.problem starts at 1
    , nodeSources = M.empty
    , nodesBySource = M.empty
    , vars = S.empty
    }

extractProblem :: Tag t => ProblemBuilder t -> Problem t
extractProblem builder = Problem
    { sides = byTagFromN (M.size builder.sides) (builder.sides M.!)
    , nodes = M.mapMaybe (preview (_Just % #node)) builder.nodes
    }

nodeAt :: NodeAddr -> Lens' (ProblemBuilder t) Node
nodeAt nodeAddr = nodeWithMetaAt nodeAddr % #node

nodeWithMetaAt :: NodeAddr -> Lens' (ProblemBuilder t) (NodeWithMeta t)
nodeWithMetaAt nodeAddr =
    #nodes % at nodeAddr % unwrapped % unwrapped

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

insertNode :: (Tag t, Monad m) => NodeAddr -> Node -> Maybe (NodeSource t) -> StateT (ProblemBuilder t) m ()
insertNode addr node maybeNodeSource = do
    bySource <- runMaybeT $ do
        nodeSource <- hoistMaybe maybeNodeSource
        indexInProblem <- lift $ do
            zoom (#nodesBySource % at nodeSource) $ do
                v <- gets (fromMaybe [])
                let indexInProblem = length v
                put (Just (v ++ [addr]))
                return indexInProblem
        return (NodeBySource nodeSource indexInProblem)
    insertNodeWithMeta addr (NodeWithMeta node (NodeMeta bySource))

appendNode :: (Tag t, Monad m) => Node -> Maybe (NodeSource t) -> StateT (ProblemBuilder t) m NodeAddr
appendNode node maybeNodeSource = do
    addr <- reserveNodeAddr
    insertNode addr node maybeNodeSource
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
            nodeSource = NodeSource tag funName origAddr
         in insertNode newNodeAddr newNode (Just nodeSource)
    return renames
  where
    funBody = fun ^. #body % unwrapped
    funGraph = makeNodeGraph (M.toAscList funBody.nodes)
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

inline :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> NodeBySource t -> StateT (ProblemBuilder t) m (InlineScriptEntry t)
inline lookupFun nodeBySource = do
    nodeAddr <- use $
        #nodesBySource % at nodeBySource.nodeSource % unwrapped
            % expectingIx nodeBySource.indexInProblem
    inlineInner lookupFun nodeAddr nodeBySource

inlineAtPoint
    :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> NodeAddr -> StateT (ProblemBuilder t) m (InlineScriptEntry t)
inlineAtPoint lookupFun nodeAddr = do
    nodeBySource <- use $
        #nodes % at nodeAddr % unwrapped % unwrapped % #meta % #bySource % unwrapped
    inlineInner lookupFun nodeAddr nodeBySource

inlineInner
    :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> NodeAddr -> NodeBySource t -> StateT (ProblemBuilder t) m (InlineScriptEntry t)
inlineInner lookupFun nodeAddr nodeBySource = do
    callNode <- use $ nodeAt nodeAddr % expecting #_NodeCall
    let tag = nodeBySource.nodeSource.tag
    let fun = lookupFun (WithTag tag callNode.functionName)
    exitNodeAddr <- reserveNodeAddr
    renames <- addFunction (WithTag tag (Named callNode.functionName fun)) (Addr exitNodeAddr)
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
    insertNode nodeAddr newNode Nothing
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
    insertNode exitNodeAddr exitNode Nothing
    return $ InlineScriptEntry
        { nodeBySource
        , inlinedFunctionName = callNode.functionName
        }

--

doAnalysis :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
doAnalysis = forceSimpleLoopReturns

forceSimpleLoopReturns :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
forceSimpleLoopReturns = do
    entryPoints <- use $ #sides % to (fmap (view #entryPoint))
    nodeGraph <-
        -- TODO ugly
        makeNodeGraph . map (\(k, v) -> (k, v.node)). mapMaybe (\(k, v) -> (k,) <$> v) . M.toAscList <$> use #nodes
    preds <- gets computePreds
    forM_ (loopHeadsFrom nodeGraph (toList entryPoints)) $ \(loopHead, scc) -> do
        let rets = filter (`S.member` scc) (S.toAscList (preds ! loopHead))
        retsIsSimple <- do
            case rets of
                [ret] -> isNodeNoop <$> use (nodeAt ret)
                _ -> return False
        unless retsIsSimple $ do
            simpleRetNodeAddr <- appendNode (trivialNode (Addr loopHead)) Nothing
            forM_ rets $ \ret -> do
                modifying (nodeAt ret % nodeConts) $ \cont ->
                    if cont == Addr loopHead
                    then Addr simpleRetNodeAddr
                    else cont

padMergePoints :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
padMergePoints = do
    preds <- gets computePreds
    let mergePreds = M.filter (\nodePreds -> S.size nodePreds > 1) preds
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
        paddingNodeAddr <- appendNode paddingNode Nothing
        modifying (nodeAt predNodeAddr % nodeConts % #_Addr) $ \contNodeAddr ->
            if contNodeAddr == nodeAddr
            then paddingNodeAddr
            else contNodeAddr

computePreds :: ProblemBuilder t -> Map NodeAddr (Set NodeAddr)
computePreds builder = M.fromListWith (<>) $ concat
    [ [ (cont, S.singleton nodeAddr)
      | cont <- node ^.. nodeConts % #_Addr
      ]
    | (nodeAddr, Just (NodeWithMeta { node })) <- M.toAscList builder.nodes
    ]
