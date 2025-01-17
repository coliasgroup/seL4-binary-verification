{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Graph
    ( NodeGraph (..)
    , NodeGraphEdges
    , makeNodeGraph
    , makeNodeGraphFromEdges
    , makeNodeGraphEdges
    , reachable
    , loopHeads
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

makeNodeGraphEdges:: [(NodeAddr, Node)] -> NodeGraphEdges
makeNodeGraphEdges nodeMap =
      ((), Ret, [])
    : ((), Err, [])
    : (nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts node))

makeNodeGraphFromEdges :: NodeGraphEdges -> NodeGraph
makeNodeGraphFromEdges edges =
    NodeGraph
        { graph
        , nodeIdMap = view _2 . nodeIdMap'
        , nodeIdMapRev
        }
  where
    (graph, nodeIdMap', nodeIdMapRev) = G.graphFromEdges edges

makeNodeGraph :: [(NodeAddr, Node)] -> NodeGraph
makeNodeGraph = makeNodeGraphFromEdges . makeNodeGraphEdges

-- Algorithms

reachable :: NodeGraph -> NodeId -> [NodeId]
reachable g from = map g.nodeIdMap $ G.reachable g.graph (fromJust (g.nodeIdMapRev from))

loopHeads :: NodeGraph -> [NodeId] -> [(NodeId, S.Set NodeId)]
loopHeads g entryPoints =
    [ (g.nodeIdMap (findHead comp), S.map g.nodeIdMap comp)
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
