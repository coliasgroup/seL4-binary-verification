module BV.Core.Graph
    ( LoopData (..)
    , LoopDataMap
    , NodeGraph (..)
    , NodeGraphEdges
    , createLoopDataMap
    , isReachableFrom
    , loopBodyInnerLoops
    , loopBodyOf
    , loopHeadOf
    , loopHeadsFrom
    , loopHeadsIncludingInner
    , loopHeadsOf
    , makeNodeGraph
    , makeNodeGraphEdges
    , makeNodeGraphFromEdges
    , nodeTagMap
    , reachableFrom
    ) where

import BV.Core.Types
import BV.Core.Types.Extras
import BV.Utils (expecting, expectingAt, unwrapped)

import Data.Foldable (toList)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

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

reachableFrom :: NodeGraph -> NodeId -> [NodeId]
reachableFrom g from = map g.nodeIdMap $ G.reachable g.graph (fromJust (g.nodeIdMapRev from))

isReachableFrom :: NodeGraph -> NodeId -> NodeId -> Bool
isReachableFrom g from to_ = G.path g.graph (fromJust (g.nodeIdMapRev from)) (fromJust (g.nodeIdMapRev to_))

loopHeadsFrom :: NodeGraph -> [NodeId] -> [(NodeAddr, S.Set NodeAddr)]
loopHeadsFrom g entryPoints =
    [ (toNodeAddr (findHead comp), S.map toNodeAddr comp)
    | comp <- sccs
    ]
  where
    toNodeAddr v = nodeAddrFromNodeId $ g.nodeIdMap v
    sccs =
        [ comp
        | comp <- map (S.fromList . toList) (G.scc g.graph)
        , S.size comp > 1
        ]
    findHead comp =
        fromJust $ find (`S.member` comp) inOrder
      where
        entryPointsAsVertices = map (fromJust . g.nodeIdMapRev) entryPoints
        inOrder = foldMap toList $ G.dfs g.graph entryPointsAsVertices

nodeTagMap :: Tag t => Problem t -> NodeGraph -> M.Map NodeAddr t
nodeTagMap problem nodeGraph =
    M.fromListWith (error "unexpected") $ byTag ^.. folded % folded
  where
    byTag = withTags problem.sides <&> \(WithTag tag side) ->
        [ (addr, tag)
        | addr <- reachableFrom nodeGraph side.entryPoint ^.. folded % #_Addr
        ]

type LoopDataMap = M.Map NodeAddr LoopData

data LoopData
  = LoopHead (S.Set NodeAddr)
  | LoopMember NodeAddr
  deriving (Eq, Generic, Ord, Show)

createLoopDataMap :: Tag t => Problem t -> NodeGraph -> LoopDataMap
createLoopDataMap problem nodeGraph =
    M.fromList $ flip foldMap heads $ \(loopHead, scc) ->
        [(loopHead, LoopHead scc)] <> flip mapMaybe (S.toList scc) (\member ->
            if member == loopHead then Nothing else Just (member, LoopMember loopHead))
  where
    heads = loopHeadsFrom nodeGraph $ problem.sides ^.. folded % #entryPoint

loopHeadsOf :: LoopDataMap -> [NodeAddr]
loopHeadsOf loopDataMap = flip mapMaybe (M.toList loopDataMap) $ \(k, v) -> case v of
    LoopHead _ -> Just k
    LoopMember _ -> Nothing

loopHeadOf :: NodeAddr -> LoopDataMap -> Maybe NodeAddr
loopHeadOf addr loopDataMap = M.lookup addr loopDataMap <&> \case
    LoopHead _ -> addr
    LoopMember addr' -> addr'

loopBodyOf :: NodeAddr -> LoopDataMap -> S.Set NodeAddr
loopBodyOf n loopDataMap =
    loopDataMap ^. expectingAt (fromJust (loopHeadOf n loopDataMap)) % expecting #_LoopHead

--

loopBodyInnerLoops :: NodeMap -> NodeAddr -> S.Set NodeAddr -> [(NodeAddr, S.Set NodeAddr)]
loopBodyInnerLoops nodes loopHead loopBody =
    loopHeadsFrom g $ nodes ^.. at loopHead % unwrapped % nodeConts
  where
    g = makeNodeGraph $ M.toList $ M.restrictKeys nodes $ S.delete loopHead loopBody

loopHeadsIncludingInner :: NodeMap -> LoopDataMap -> [NodeAddr]
loopHeadsIncludingInner nodes m =
    let heads = loopHeadsOf m
     in go heads [ (h, loopBodyOf h m ) | h <- heads ]
  where
    go heads [] = heads
    go heads ((h, body):check) =
        let comps = loopBodyInnerLoops nodes h body
         in go
                (heads ++ map fst comps)
                (check ++ comps)
