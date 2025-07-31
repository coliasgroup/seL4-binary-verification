{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.BuildProblem
    ( Inliner
    , buildInlineScript
    , buildProblem
    ) where

import BV.Core.GenerateFreshName
import BV.Core.Graph
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (ensureM, expecting, expectingIx, unwrapped)

import Control.Monad (forM, unless)
import Control.Monad.State (State, StateT, evalStateT, execState, get, gets,
                            modify, put, runState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Foldable (forM_, toList)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics

buildProblem :: Tag t => (WithTag t Ident -> Function) -> InlineScript t -> ByTag t (Named Function) -> Problem t
buildProblem lookupFun inlineScript funs = build builder
  where
    builder = flip execState (beginProblemBuilder funs) $ do
        forceSimpleLoopReturns
        forM_ inlineScript $ \entry -> do
            inline lookupFun entry.nodeBySource

type Inliner t m = Problem t -> m (Maybe [NodeAddr])

buildInlineScript :: forall t m. (Tag t, Monad m) => Inliner t m -> (WithTag t Ident -> Function) -> ByTag t (Named Function) -> m (InlineScript t)
buildInlineScript inliner lookupFun funs = do
    flip evalStateT (beginProblemBuilder funs) $ do
        forceSimpleLoopReturns
        let go :: [InlineScriptEntry t] -> StateT (ProblemBuilder t) m [InlineScriptEntry t]
            go acc = do
                p <- gets build
                lift (inliner p) >>= \case
                    Nothing -> return acc
                    Just addrs -> do
                        entries <- for addrs $ \addr -> do
                            nodeBySource <- use $
                                #nodeMapBuilder % #nodes % at addr % unwrapped % unwrapped % #meta % #bySource % unwrapped
                            inlinedFunctionName <- use $
                                #nodeMapBuilder % #nodes % at addr % unwrapped % unwrapped % #node % expecting #_NodeCall % #functionName
                            inline lookupFun nodeBySource
                            return $ InlineScriptEntry
                                    { nodeBySource
                                    , inlinedFunctionName
                                    }
                        go (acc ++ entries)
        go []

data ProblemBuilder t
  = ProblemBuilder
      { sides :: ByTag t ProblemSide
      , nodeMapBuilder :: NodeMapBuilder t
      }
  deriving (Generic)

data NodeMapBuilder t
  = NodeMapBuilder
      { nodes :: M.Map NodeAddr (Maybe (NodeWithMeta t))
      , nodesBySource :: M.Map (NodeSource t) [NodeAddr]
      , vars :: S.Set Ident
      }
  deriving (Eq, Generic, Ord, Show)

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

beginProblemBuilder :: Tag t => ByTag t (Named Function) -> ProblemBuilder t
beginProblemBuilder funs = ProblemBuilder
    { sides
    , nodeMapBuilder
    }
  where
    renameSide (WithTag tag namedFun) = do
        renames <- addFunction (WithTag tag namedFun) Ret
        let renameArgs = traversed % #name %~ (renames.var !)
        return $ ProblemSide
            { name = namedFun.name
            , entryPoint = (fromJust namedFun.value.body).entryPoint & #_Addr %~ (renames.nodeAddr !)
            , input = renameArgs namedFun.value.input
            , output = renameArgs namedFun.value.output
            }
    (sides, nodeMapBuilder) = flip runState emptyNodeMapBuilder $ do
        _ <- reserveNodeAddr -- HACK graph_refine.problem starts at 1
        traverse renameSide $ withTags funs

forceSimpleLoopReturns :: (Tag t, Monad m) => StateT (ProblemBuilder t) m ()
forceSimpleLoopReturns = do
    entryPoints <- use $ #sides % to (fmap (view #entryPoint))
    zoom #nodeMapBuilder $ do
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

inline :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> NodeBySource t -> StateT (ProblemBuilder t) m ()
inline lookupFun nodeBySource = do
    zoom #nodeMapBuilder $ nodeMapBuilderInline lookupFun nodeBySource
    forceSimpleLoopReturns

build :: Tag t => ProblemBuilder t -> Problem t
build builder =
    Problem
        { sides = finalBuilder.sides
        , nodes = M.mapMaybe (\case
                Just (NodeWithMeta x _) -> Just x
                _ -> Nothing
            ) finalBuilder.nodeMapBuilder.nodes
        }
  where
    finalBuilder = flip execState builder $ do
        forceSimpleLoopReturns
        zoom #nodeMapBuilder padMergePoints
        forceSimpleLoopReturns

emptyNodeMapBuilder :: NodeMapBuilder t
emptyNodeMapBuilder = NodeMapBuilder
    { nodes = M.empty
    , nodesBySource = M.empty
    , vars = S.empty
    }

nodeAt :: NodeAddr -> Lens' (NodeMapBuilder t) Node
nodeAt nodeAddr = nodeWithMetaAt nodeAddr % #node

nodeWithMetaAt :: NodeAddr -> Lens' (NodeMapBuilder t) (NodeWithMeta t)
nodeWithMetaAt nodeAddr =
    #nodes % at nodeAddr % unwrapped % unwrapped

reserveNodeAddr :: Monad m => StateT (NodeMapBuilder t) m NodeAddr
reserveNodeAddr = do
    addr <- gets $ maybe 0 ((+ 1) . fst) . M.lookupMax . (.nodes)
    modify $ #nodes % at addr ?~ Nothing
    return addr

insertNodeWithMeta :: Monad m => NodeAddr -> (NodeWithMeta t) -> StateT (NodeMapBuilder t) m ()
insertNodeWithMeta addr nodeWithMeta = do
    zoom (#nodes % at addr) $ do
        gets isJust >>= ensureM
        put $ Just (Just nodeWithMeta)

insertNode :: (Tag t, Monad m) => NodeAddr -> Node -> Maybe (NodeSource t) -> StateT (NodeMapBuilder t) m ()
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

appendNode :: (Tag t, Monad m) => Node -> Maybe (NodeSource t) -> StateT (NodeMapBuilder t) m NodeAddr
appendNode node maybeNodeSource = do
    addr <- reserveNodeAddr
    insertNode addr node maybeNodeSource
    return addr

data AddFunctionRenames
  = AddFunctionRenames
      { var :: Map Ident Ident
      , nodeAddr :: Map NodeAddr NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

addFunction
    :: (Tag t, Monad m) => WithTag t (Named Function) -> NodeId -> StateT (NodeMapBuilder t) m AddFunctionRenames
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

nodeMapBuilderInlineAtPoint
    :: (Tag t, Monad m) => NodeAddr -> Function -> StateT (NodeMapBuilder t) m ()
nodeMapBuilderInlineAtPoint nodeAddr fun = do
    nodeWithMeta <- use $ nodeWithMetaAt nodeAddr
    let tag = nodeWithMeta ^. #meta % #bySource % unwrapped % #nodeSource % #tag
    let NodeCall callNode = nodeWithMeta.node
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

nodeMapBuilderInline
    :: (Tag t, Monad m) => (WithTag t Ident -> Function) -> NodeBySource t -> StateT (NodeMapBuilder t) m ()
nodeMapBuilderInline lookupFun nodeBySource = do
    let tag = nodeBySource.nodeSource.tag
    nodeAddr <- use $
        #nodesBySource % at nodeBySource.nodeSource % unwrapped
            % expectingIx nodeBySource.indexInProblem
    funName <- use $ nodeAt nodeAddr % expecting #_NodeCall % #functionName
    nodeMapBuilderInlineAtPoint nodeAddr (lookupFun (WithTag tag funName))

padMergePoints :: Tag t => State (NodeMapBuilder t) ()
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

computePreds :: NodeMapBuilder t -> Map NodeAddr (Set NodeAddr)
computePreds builder = M.fromListWith (<>) $ concat
        [ [ (cont, S.singleton nodeAddr)
          | cont <- node ^.. nodeConts % #_Addr
          ]
        | (nodeAddr, Just (NodeWithMeta { node })) <- M.toAscList builder.nodes
        ]

getFreshName :: Monad m => Ident -> StateT (NodeMapBuilder t) m Ident
getFreshName hint = do
    zoom #vars $ do
        taken <- get
        let name = Ident $ generateFreshName (flip S.member taken . Ident) hint.unwrap
        modify $ S.insert name
        return name
