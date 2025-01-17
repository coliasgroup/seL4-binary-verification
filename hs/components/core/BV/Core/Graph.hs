{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Graph
    ( NodeGraph (..)
    , NodeGraphEdges
    , makeNodeGraph
    , nodeGraphEdges
    , nodeGraphEdgesWith
    , reachable
    ) where

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics.Core

import BV.Core.Types
import Data.Foldable (fold, toList)
import Data.List (find)

data NodeGraph
  = NodeGraph
      { graph :: Graph
      , nodeIdMap :: Vertex -> NodeId
      , nodeIdMapRev :: NodeId -> Maybe Vertex
      }
  deriving (Generic)

type NodeGraphEdges = [((), NodeId, [NodeId])]

nodeGraphEdgesWith :: (a -> Node) -> Map NodeAddr a -> NodeGraphEdges
nodeGraphEdgesWith f nodeMap =
      ((), Ret, [])
    : ((), Err, [])
    : (M.assocs nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts (f node)))

nodeGraphEdges :: NodeMap -> NodeGraphEdges
nodeGraphEdges = nodeGraphEdgesWith id

makeNodeGraph :: NodeGraphEdges -> NodeGraph
makeNodeGraph edges =
    NodeGraph
        { graph
        , nodeIdMap = view _2 . nodeIdMap'
        , nodeIdMapRev
        }
  where
    (graph, nodeIdMap', nodeIdMapRev) = G.graphFromEdges edges


-- Algorithms

reachable :: NodeGraph -> NodeId -> [NodeId]
reachable g from = map g.nodeIdMap $ G.reachable g.graph (fromJust (g.nodeIdMapRev from))

loopHeads :: NodeGraph -> [NodeId] -> [(NodeId, [NodeId])]
loopHeads g entryPoints =
    [ (g.nodeIdMap (findHead comp), map g.nodeIdMap (S.toList comp))
    | comp <- sccs
    ]
  where
    sccs = do
        comp <- S.fromList . toList <$> G.scc g.graph
        False <- return $ S.null comp
        return comp
    findHead comp =
        fromJust $ find (`S.member` comp) inOrder
      where
        entryPointsAsVertices = map (fromJust . g.nodeIdMapRev) entryPoints
        inOrder = foldMap toList $ G.dfs g.graph entryPointsAsVertices
