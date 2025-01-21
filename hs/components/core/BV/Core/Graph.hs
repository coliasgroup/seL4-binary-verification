{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Graph
    ( NodeGraph (..)
    , NodeGraphEdges
    , isReachableFrom
    , loopHeads
    , makeNodeGraph
    , makeNodeGraphEdges
    , makeNodeGraphFromEdges
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
import BV.Core.Utils
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

isReachableFrom :: NodeGraph -> NodeId -> NodeId -> Bool
isReachableFrom g to from = G.path g.graph (fromJust (g.nodeIdMapRev from)) (fromJust (g.nodeIdMapRev to))

loopHeads :: NodeGraph -> [NodeId] -> [(NodeAddr, S.Set NodeAddr)]
loopHeads g entryPoints =
    [ (toNodeAddr (findHead comp), S.map toNodeAddr comp)
    | comp <- sccs
    ]
  where
    toNodeAddr v = g.nodeIdMap v ^. expecting #_Addr
    sccs = do
        comp <- S.fromList . toList <$> G.scc g.graph
        True <- return $ S.size comp > 1
        return comp
    findHead comp =
        fromJust $ find (`S.member` comp) inOrder
      where
        entryPointsAsVertices = map (fromJust . g.nodeIdMapRev) entryPoints
        inOrder = foldMap toList $ G.dfs g.graph entryPointsAsVertices
