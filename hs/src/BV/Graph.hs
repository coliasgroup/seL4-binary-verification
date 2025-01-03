module BV.Graph where

import BV.Program
import Data.Graph (Graph, Vertex)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Arr (Array)
import qualified GHC.Arr as A
import GHC.Generics (Generic)
import Optics.Core

data NodeGraph
  = NodeGraph
      { graph :: Graph
      , nodeIDMap :: Array Vertex NodeID
      , nodeIDMapRev :: Map NodeID Vertex
      }
  deriving (Generic, Show)

makeNodeGraph :: Map NodeAddr Node -> NodeGraph
makeNodeGraph nodeMap = NodeGraph
    { graph
    , nodeIDMap
    , nodeIDMapRev
    }
  where
    vertices = zip [0..]
      ( (Ret, Nothing)
      : (Err, Nothing)
      : (M.assocs nodeMap <&> (_1 %~ Addr) . (_2 %~ Just))
      )
    (graphList, nodeIDMapList, nodeIDMapRevList) = unzip3
      [ ( node ^.. _Just % nodeConts % to (nodeIDMapRev M.!)
        , nodeID
        , (nodeID, i)
        )
      | (i, (nodeID, node)) <- vertices
      ]
    bounds = (0, M.size nodeMap + 2)
    graph = A.listArray bounds graphList
    nodeIDMap = A.listArray bounds nodeIDMapList
    nodeIDMapRev = M.fromList nodeIDMapRevList
