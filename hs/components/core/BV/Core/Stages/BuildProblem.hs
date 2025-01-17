{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Stages.BuildProblem
    ( buildProblem
    ) where

import Control.Exception (assert)
import Control.Monad (forM)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Foldable (forM_)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

import BV.Core.ExprConstruction
import BV.Core.Graph
import BV.Core.Types
import BV.Core.Utils
import Data.Traversable (for)

buildProblem :: (Tag -> Ident -> Function) -> InlineScript -> PairingOf (Named Function) -> Problem
buildProblem = undefined

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
    (sides, nodeMapBuilder) = flip runState emptyNodeMapBuilder $ do
        _ <- reserveNodeAddr -- HACK graph_refine.problem stats at 1
        asm <- do
            renames <- addFunction (WithTag Asm funs.asm) Ret
            let renameArgs = traversed % #name %~ (renames.var !)
            return $ ProblemSide
                { name = funs.asm.name
                , entryPoint = (fromJust funs.asm.value.body).entryPoint & #_Addr %~ (renames.nodeAddr !)
                , input = renameArgs funs.asm.value.input
                , output = renameArgs funs.asm.value.output
                }
        c <- undefined
        return $ PairingOf { c, asm }

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
    addr <- maybe 0 fst . M.lookupMax <$> gets (.nodes)
    modify $ #nodes % at addr ?~ Nothing
    return addr

insertNodeWithMeta :: NodeAddr -> NodeWithMeta -> State NodeMapBuilder ()
insertNodeWithMeta addr nodeWithMeta = do
    zoom (#nodes % at addr) $ do
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
        forM (S.toList origVars) (\name -> (name,) <$> getFreshName name)
    nodeAddrRenames <- M.fromList <$>
        forM (S.toList origNodeAddrs) (\addr -> (addr,) <$> reserveNodeAddr)
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
    funGraph = makeNodeGraph funBody.nodes
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
    let tag = nodeBySource.nodeSource.tag
    nodeAddr <- use $
        #nodesBySource % at nodeBySource.nodeSource % unwrapped
            % expectingIx nodeBySource.indexInProblem
    funName <- use $ nodeAt nodeAddr % expecting #_NodeCall % #functionName
    nodeMapBuilderInlineAtPoint nodeAddr (lookupFun tag funName)

padMergePoints
    :: State NodeMapBuilder ()
padMergePoints = do
    preds <- gets computePreds
    let mergePreds = M.filter (\nodePreds -> S.size nodePreds > 1) preds
    nonTrivialEdgesToMergePoints <- fmap concat . forM (M.toList mergePreds) $ \(nodeAddr, nodePreds) -> do
        fmap concat . forM (S.toList nodePreds) $ \predNodeAddr -> do
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
        | (nodeAddr, Just (NodeWithMeta { node })) <- M.toList builder.nodes
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
