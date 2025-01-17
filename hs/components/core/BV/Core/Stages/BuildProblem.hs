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

emptyNodeMapBuilder :: NodeMapBuilder
emptyNodeMapBuilder = NodeMapBuilder
    { nodes = M.empty
    , nodesBySource = M.empty
    , vars = S.empty
    }

nodeMapBuilderInsert :: NodeAddr -> Node -> Maybe NodeSource -> State NodeMapBuilder ()
nodeMapBuilderInsert addr node maybeNodeSource = do
    bySource <- runMaybeT $ do
        nodeSource <- hoistMaybe maybeNodeSource
        indexInProblem <- lift $ do
            zoom (#nodesBySource % at nodeSource) $ do
                v <- gets (fromMaybe [])
                let indexInProblem = length v
                put (Just (v ++ [addr]))
                return indexInProblem
        return (NodeBySource nodeSource indexInProblem)
    zoom (#nodes % at addr) $ do
        _ <- assert . isJust <$> get
        put $ Just (Just (NodeWithMeta node (NodeMeta bySource)))

nodeMapBuilderReserve :: State NodeMapBuilder NodeAddr
nodeMapBuilderReserve = do
    addr <- maybe 0 fst . M.lookupMax <$> gets (.nodes)
    modify $ #nodes % at addr ?~ Nothing
    return addr

data AddFunctionRenames
  = AddFunctionRenames
      { var :: Map Ident Ident
      , nodeAddr :: Map NodeAddr NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

nodeMapBuilderAddFunction
    :: WithTag (Named Function) -> NodeId -> State NodeMapBuilder AddFunctionRenames
nodeMapBuilderAddFunction (WithTag tag (Named funName fun)) retTarget = do
    varRenames <- M.fromList <$>
        forM (S.toList origVars) (\name -> (name,) <$> getFreshName name)
    nodeAddrRenames <- M.fromList <$>
        forM (S.toList origNodeAddrs) (\addr -> (addr,) <$> nodeMapBuilderReserve)
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
         in nodeMapBuilderInsert newNodeAddr newNode (Just nodeSource)
    return renames
  where
    funBody = fun ^. #body % unwrap
    funGraph = makeNodeGraph funBody.nodes
    origNodeAddrs = S.fromList $ reachable funGraph funBody.entryPoint ^.. traversed % #_Addr
    origVars = S.fromList . map fst $ fun ^.. varDeclsOf

nodeMapBuilderInlineAtPoint
    :: NodeAddr -> Function -> State NodeMapBuilder ()
nodeMapBuilderInlineAtPoint nodeAddr fun = do
    nodeWithMeta <- fmap fromJust . preuse $ #nodes % at nodeAddr %? to fromJust
    let Just tag = nodeWithMeta ^? #meta % #bySource %? #nodeSource % #tag
    let CallNode { next, functionName, input, output } = nodeWithMeta.node
    exitNodeAddr <- nodeMapBuilderReserve
    renames <- nodeMapBuilderAddFunction (WithTag tag (Named functionName fun)) (Addr exitNodeAddr)
    let entryNodeAddr = renames.nodeAddr ! fromJust (fun ^? #body % _Just % #entryPoint % #_Addr)
    let newNode = BasicNode
            { next = Addr entryNodeAddr
            , varUpdates =
                [ VarUpdate
                    { varName = renames.var ! arg.name
                    , ty = arg.ty
                    , expr = callInput
                    }
                | (arg, callInput) <- zip fun.input input
                ]
            }
    nodeMapBuilderInsert nodeAddr newNode Nothing
    let exitNode = BasicNode
            { next = next
            , varUpdates =
                [ VarUpdate
                    { varName = callOutput.name
                    , ty = arg.ty
                    , expr = varE arg.ty (renames.var ! arg.name)
                    }
                | (arg, callOutput) <- zip fun.output output
                ]
            }
    nodeMapBuilderInsert exitNodeAddr exitNode Nothing

nodeMapBuilderInline
    :: (Tag -> Ident -> Function) -> NodeBySource -> State NodeMapBuilder ()
nodeMapBuilderInline lookupFun nodeBySource = do
    let tag = nodeBySource.nodeSource.tag
    nodeAddr <-
        -- TODO ugly
        fmap fromJust . preuse $
            #nodesBySource % at nodeBySource.nodeSource % to fromJust % ix nodeBySource.indexInProblem
    node <- fmap (.node) . fmap fromJust . use $ #nodes % at nodeAddr % to fromJust
    let Just funName = node ^? #_CallNode % _2
    nodeMapBuilderInlineAtPoint nodeAddr (lookupFun tag funName)

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
