{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.BuildProblem
    ( buildProblem
    ) where

import Control.Exception (assert)
import Control.Monad (forM, unless, when)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Foldable (forM_, toList)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Debug.Trace
import GHC.Generics (Generic)
import Optics

import BV.Core.ExprConstruction
import BV.Core.Graph
import BV.Core.Logic
import BV.Core.Types
import BV.Core.Utils

import BV.Core.Debug.Utils

buildProblem :: (Tag -> Ident -> Function) -> InlineScript -> PairingOf (Named Function) -> Problem
buildProblem lookupFun inlineScript funs = build builder
  where
    builder = flip execState (beginProblemBuilder funs) $ do
        -- !_ <- traceShowM ((.name) <$> funs)
        -- !_ <- do
        --     x <- get
        --     !_ <- return $ unsafePerformIO (writeHtml "zzz" x)
        --     return ()
        -- !_ <- traceShowM inlineScript
        forM_ inlineScript $ \entry -> do
            -- !_ <- traceShowM entry
            inline lookupFun entry.nodeBySource

data ProblemBuilder
  = ProblemBuilder
      { sides :: PairingOf ProblemSide
      , nodeMapBuilder :: NodeMapBuilder
      }
  deriving (Eq, Generic, Ord, Show)

data NodeMapBuilder
  = NodeMapBuilder
      { nodes :: M.Map NodeAddr (Maybe NodeWithMeta)
      , nodesBySource :: M.Map NodeSource [NodeAddr]
      , vars :: S.Set Ident
      }
  deriving (Eq, Generic, Ord, Show)

data NodeWithMeta
  = NodeWithMeta
      { node :: Node
      , meta :: NodeMeta
      }
  deriving (Eq, Generic, Ord, Show)

data NodeMeta
  = NodeMeta
      { bySource :: Maybe NodeBySource
      }
  deriving (Eq, Generic, Ord, Show)

beginProblemBuilder :: PairingOf (Named Function) -> ProblemBuilder
beginProblemBuilder funs = ProblemBuilder
    { sides
    , nodeMapBuilder
    }
  where
    renameSide (WithTag tag namedFun) = do
        renames <- addFunction (WithTag tag namedFun) Ret
        -- !_ <- do
        --     x <- get
        --     !_ <- return $ unsafePerformIO (writeHtml "zz1" x)
        --     return ()
        let renameArgs = traversed % #name %~ (renames.var !)
        return $ ProblemSide
            { name = namedFun.name
            , entryPoint = (fromJust namedFun.value.body).entryPoint & #_Addr %~ (renames.nodeAddr !)
            , input = renameArgs namedFun.value.input
            , output = renameArgs namedFun.value.output
            }
    (sides, nodeMapBuilder) = flip runState emptyNodeMapBuilder $ do
        _ <- reserveNodeAddr -- HACK graph_refine.problem stats at 1
        -- s <- reserveNodeAddr
        -- s <- reserveNodeAddr
        -- !_ <- traceShowM $ "FOO " ++ show s
        asm <- renameSide $ pairingSideWithTag Asm funs
        c <- renameSide $ pairingSideWithTag C funs
        return $ PairingOf { c, asm }

forceSimpleLoopReturns :: State ProblemBuilder ()
forceSimpleLoopReturns = do
    entryPoints <- use $ #sides % to (fmap (view #entryPoint))
    zoom #nodeMapBuilder $ do
        nodeGraph <-
            -- TODO ugly
            makeNodeGraph . map (\(k, v) -> (k, v.node)). mapMaybe (\(k, v) -> (k,) <$> v) . M.toAscList <$> use #nodes
        preds <- gets computePreds
        forM_ (loopHeads nodeGraph (toList entryPoints)) $ \(head, scc) -> do
            let rets = filter (`S.member` scc) (S.toAscList (preds ! head))
            retsIsSimple <- do
                case rets of
                    [ret] -> isNodeNoop <$> use (nodeAt ret)
                    _ -> return False
            unless retsIsSimple $ do
                simpleRetNodeAddr <- appendNode (trivialNode (Addr head)) Nothing
                forM_ rets $ \ret -> do
                    modifying (nodeAt ret % nodeConts) $ \cont ->
                        if cont == Addr head
                        then Addr simpleRetNodeAddr
                        else cont

inline :: (Tag -> Ident -> Function) -> NodeBySource -> State ProblemBuilder ()
inline lookupFun nodeBySource = do
    zoom #nodeMapBuilder $ nodeMapBuilderInline lookupFun nodeBySource
    forceSimpleLoopReturns

build :: ProblemBuilder -> Problem
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

emptyNodeMapBuilder :: NodeMapBuilder
emptyNodeMapBuilder = NodeMapBuilder
    { nodes = M.empty
    , nodesBySource = M.empty
    , vars = S.empty
    }

nodeAt :: NodeAddr -> Lens' NodeMapBuilder Node
nodeAt nodeAddr = nodeWithMetaAt nodeAddr % #node

nodeWithMetaAt :: NodeAddr -> Lens' NodeMapBuilder NodeWithMeta
nodeWithMetaAt nodeAddr =
    #nodes % at nodeAddr % partially (castOptic _Just) % partially (castOptic _Just)

allNodesWithMeta :: Traversal' NodeMapBuilder NodeWithMeta
allNodesWithMeta = #nodes % traversed % _Just

allNodes :: Traversal' NodeMapBuilder Node
allNodes = allNodesWithMeta % #node

reserveNodeAddr :: State NodeMapBuilder NodeAddr
reserveNodeAddr = do
    addr <- maybe 0 ((+ 1) . fst) . M.lookupMax <$> gets (.nodes)
    modify $ #nodes % at addr .~ (Just Nothing)
    -- !_ <- do
    --     x <- get
    --     !_ <- return $ unsafePerformIO (writeHtml "zzx" x)
    --     return ()
    return addr

insertNodeWithMeta :: NodeAddr -> NodeWithMeta -> State NodeMapBuilder ()
insertNodeWithMeta addr nodeWithMeta = do
    zoom (#nodes % at addr) $ do
        -- _ <- traceShowM (addr, nodeWithMeta)
        _ <- assert . isJust <$> get
        put $ Just (Just nodeWithMeta)

insertNode :: NodeAddr -> Node -> Maybe NodeSource -> State NodeMapBuilder ()
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

appendNode :: Node -> Maybe NodeSource -> State NodeMapBuilder NodeAddr
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
    :: WithTag (Named Function) -> NodeId -> State NodeMapBuilder AddFunctionRenames
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
    origNodeAddrs = S.fromList $ reachable funGraph funBody.entryPoint ^.. traversed % #_Addr
    origVars = S.fromList . map fst $ fun ^.. varDeclsOf

nodeMapBuilderInlineAtPoint
    :: NodeAddr -> Function -> State NodeMapBuilder ()
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
                    { varName = renames.var ! arg.name
                    , ty = arg.ty
                    , expr = callInput
                    }
                | (arg, callInput) <- zip fun.input callNode.input
                ]
            }
    insertNode nodeAddr newNode Nothing
    let exitNode = NodeBasic $ BasicNode
            { next = callNode.next
            , varUpdates =
                [ VarUpdate
                    { varName = callOutput.name
                    , ty = arg.ty
                    , expr = varE arg.ty (renames.var ! arg.name)
                    }
                | (arg, callOutput) <- zip fun.output callNode.output
                ]
            }
    insertNode exitNodeAddr exitNode Nothing

nodeMapBuilderInline
    :: (Tag -> Ident -> Function) -> NodeBySource -> State NodeMapBuilder ()
nodeMapBuilderInline lookupFun nodeBySource = do
    -- !_ <- do
    --     x <- get
    --     !_ <- traceM "xxx"
    --     !_ <- return $ unsafePerformIO (writeHtml "xyz" x)
    --     return ()
    let tag = nodeBySource.nodeSource.tag
    nodeAddr <- use $
        #nodesBySource % at nodeBySource.nodeSource % unwrapped
            % expectingIx nodeBySource.indexInProblem
    -- !_ <- traceShowM nodeAddr
    !funName <- use $ nodeAt nodeAddr % expecting #_NodeCall % #functionName
    -- !_ <- traceM "yyy"
    nodeMapBuilderInlineAtPoint nodeAddr (lookupFun tag funName)
    return ()

padMergePoints
    :: State NodeMapBuilder ()
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

computePreds :: NodeMapBuilder -> Map NodeAddr (Set NodeAddr)
computePreds builder = M.fromListWith (<>) $ concat
        [ [ (cont, S.singleton nodeAddr)
          | cont <- node ^.. nodeConts % #_Addr
          ]
        | (nodeAddr, Just (NodeWithMeta { node })) <- M.toAscList builder.nodes
        ]

getFreshName :: Ident -> State NodeMapBuilder Ident
getFreshName hint = do
    zoom #vars $ do
        taken <- get
        let name = chooseFreshName taken hint
        modify $ S.insert name
        return name

-- Implementation matches graph_refine.syntax.fresh_name
chooseFreshName :: S.Set Ident -> Ident -> Ident
chooseFreshName taken n =
    if n `S.notMember` taken
    then n
    else loop1 1 1
  where
    isTaken = (`S.member` taken)
    fmt x = Ident (n.unwrap ++ "." ++ show x)
    loop1 x y =
        if isTaken (fmt x)
        then loop1 y (x * 2)
        else loop2 x y
    loop2 x y =
        if x < y
        then
            let z = (y + x) `div` 2
             in if isTaken (fmt z)
                then loop2 x (z + 1)
                else loop2 z y
        else
            let n' = fmt x
             in assert (not (isTaken n')) n'
