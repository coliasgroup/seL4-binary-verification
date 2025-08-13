{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Types.Extras.Problem
    ( ArgRenames
    , Loop (..)
    , LoopData
    , NodeGraph
    , ProblemAnalysis (..)
    , ProblemWithAnalysis (..)
    , allInnerLoops
    , analyzeProblem
    , analyzeProblemFromPartial
    , augmentProblem
    , innerLoopsOf
    , isReachableFrom
    , loopBodyOf
    , loopContainingOf
    , loopHeadOf
    , loopHeadsOf
    , loopsFrom
    , loopsOf
    , makeNodeGraph
    , pairingIdOfProblem
    , problemArgRenames
    , reachableFrom
    , varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Program
import BV.Utils

import Control.Monad (guard)
import Control.Monad.Writer (execWriter, tell)
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

type ArgRenames t = PairingEqSideQuadrant t -> Ident -> Ident

problemArgRenames :: Tag t => Problem t -> ByTag t FunctionSignature -> ArgRenames t
problemArgRenames problem sigs quadrant mangledName =
    fromJust $ lookup mangledName (zip (map (.name) mangledArgs) (map (.name) origArgs))
  where
    sig = viewAtTag quadrant.tag sigs
    origArgs = case quadrant.direction of
        PairingEqDirectionIn -> sig.input
        PairingEqDirectionOut -> sig.output
    probSide = viewAtTag quadrant.tag problem.sides
    mangledArgs = case quadrant.direction of
        PairingEqDirectionIn -> probSide.input
        PairingEqDirectionOut -> probSide.output

pairingIdOfProblem :: Problem t -> PairingId t
pairingIdOfProblem problem = view #name <$> problem.sides

varNamesOfProblem :: Tag t => Traversal' (Problem t) Ident
varNamesOfProblem =
    (#sides % traversed % (#input `adjoin` #output) % traversed % varNamesOf)
        `adjoin` (#nodes % traversed % varNamesOf)

--

data ProblemWithAnalysis t
  = ProblemWithAnalysis
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      }
  deriving (Generic)

data ProblemAnalysis t
  = ProblemAnalysis
      { nodeGraph :: NodeGraph
      , nodeTag :: NodeAddr -> t
      , loopData :: LoopData
      , preds :: ByTag t (NodeId -> S.Set NodeAddr)
      , varNames :: S.Set Ident
      }
  deriving (Generic)

analyzeProblem :: Tag t => Problem t -> ProblemAnalysis t
analyzeProblem problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData = makeLoopData problem nodeGraph
    , preds = computePreds problem nodeTag
    , varNames = S.fromList $ toListOf varNamesOfProblem problem
    }
  where
    nodeGraph = makeNodeGraph problem.nodes
    nodeTag = (M.!) $ nodeTagMap problem nodeGraph

analyzeProblemFromPartial :: Tag t => (NodeAddr -> t) -> S.Set Ident -> Problem t -> ProblemAnalysis t
analyzeProblemFromPartial nodeTag varNames problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData = makeLoopData problem nodeGraph
    , preds = computePreds problem nodeTag
    , varNames
    }
  where
    nodeGraph = makeNodeGraph problem.nodes

augmentProblem :: Tag t => Problem t -> ProblemWithAnalysis t
augmentProblem problem = ProblemWithAnalysis
    { problem
    , analysis = analyzeProblem problem
    }

--

data NodeGraph
  = NodeGraph
      { graph :: Graph
      , vertexToNodeId :: Vertex -> NodeId
      , nodeIdToVertex :: NodeId -> Vertex
      }
  deriving (Generic)

type NodeGraphEdges = [((), NodeId, [NodeId])]

makeNodeGraphEdges :: NodeMap -> NodeGraphEdges
makeNodeGraphEdges nodeMap =
      ((), Ret, [])
    : ((), Err, [])
    : (M.toList nodeMap <&> \(addr, node) -> ((), Addr addr, toListOf nodeConts node))

makeNodeGraphFromEdges :: NodeGraphEdges -> NodeGraph
makeNodeGraphFromEdges edges =
    NodeGraph
        { graph
        , vertexToNodeId = view _2 . vertexToNodeId'
        , nodeIdToVertex = fromJust . nodeIdToVertex'
        }
  where
    (graph, vertexToNodeId', nodeIdToVertex') = G.graphFromEdges edges

makeNodeGraph :: NodeMap -> NodeGraph
makeNodeGraph = makeNodeGraphFromEdges . makeNodeGraphEdges

--

reachableFrom :: NodeGraph -> NodeId -> [NodeId]
reachableFrom g from = map g.vertexToNodeId $ G.reachable g.graph (g.nodeIdToVertex from)

isReachableFrom :: NodeGraph -> NodeId -> NodeId -> Bool
isReachableFrom g from to_ = G.path g.graph (g.nodeIdToVertex from) (g.nodeIdToVertex to_)

--

nodeTagMap :: Tag t => Problem t -> NodeGraph -> M.Map NodeAddr t
nodeTagMap problem nodeGraph =
    M.fromListWith undefined $ byTag ^.. folded % folded
  where
    byTag = withTags problem.sides <&> \(WithTag tag side) ->
        [ (addr, tag)
        | addr <- reachableFrom nodeGraph side.entryPoint ^.. folded % #_Addr
        ]

--

loopsFromGeneric :: G.Graph -> [Vertex] -> [(Vertex, S.Set Vertex)]
loopsFromGeneric g entryPoints = do
    body <- S.fromList . toList <$> G.scc g
    guard $ S.size body > 1
    Just h <- return $ find (`S.member` body) inOrder
    return (h, body)
  where
    inOrder = foldMap toList $ G.dfs g entryPoints

data LoopData
  = LoopData
      { inOrder :: [Loop]
        -- HACK for compatibility
      , heads :: M.Map NodeAddr Loop
      , members :: M.Map NodeAddr LoopDataForNode
      }
  deriving (Eq, Generic, Ord, Show)

data LoopDataForNode
  = LoopDataForNode
      { role :: LoopRole
      , loop :: Loop
      }
  deriving (Eq, Generic, Ord, Show)

data LoopRole
  = LoopRoleHead
  | LoopRoleBody
  deriving (Eq, Generic, Ord, Show)

data Loop
  = Loop
      { head :: NodeAddr
      , body :: S.Set NodeAddr
      }
  deriving (Eq, Generic, Ord, Show)

loopsFrom :: NodeGraph -> [NodeId] -> [Loop]
loopsFrom g entryPoints =
    [ Loop
        { head = toNodeAddr h
        , body = S.map toNodeAddr body
        }
    | (h, body) <- loopsFromGeneric g.graph (map g.nodeIdToVertex entryPoints)
    ]
  where
    toNodeAddr = nodeAddrOf . g.vertexToNodeId

makeLoopData :: Tag t => Problem t -> NodeGraph -> LoopData
makeLoopData problem nodeGraph = LoopData
    { inOrder = loops
    , heads = M.fromList [ (loop.head, loop) | loop <- loops ]
    , members = M.fromList $ flip concatMap loops $ \loop ->
        [ let role = if n == loop.head then LoopRoleHead else LoopRoleBody
           in (n, LoopDataForNode role loop)
        | n <- S.toList loop.body
        ]
    }
  where
    loops = loopsFrom nodeGraph $ toListOf (folded % #entryPoint) problem.sides

loopsOf :: LoopData -> [Loop]
loopsOf d = d.inOrder

loopContainingOf :: LoopData -> NodeAddr -> Maybe Loop
loopContainingOf d n = d ^? #members % at n % _Just % #loop

loopHeadsOf :: LoopData -> [NodeAddr]
loopHeadsOf = map (.head) . loopsOf

loopHeadOf :: NodeAddr -> LoopData -> Maybe NodeAddr
loopHeadOf n d = (.head) <$> loopContainingOf d n

loopBodyOf :: NodeAddr -> LoopData -> S.Set NodeAddr
loopBodyOf n d = (fromJust (loopContainingOf d n)).body

innerLoopsOf :: NodeMap -> Loop -> [Loop]
innerLoopsOf nodes loop =
    loopsFrom g entryPoints
  where
    entryPoints = nodes ^.. at loop.head % unwrapped % nodeConts
    bodyWithoutHead = S.delete loop.head loop.body
    g = makeNodeGraphFromEdges
            [ ((), src, dsts)
            | (src, dsts) <- M.toList (M.fromListWith (<>) edges)
            ]
    edges = do
        ((), Addr src, dsts) <- makeNodeGraphEdges nodes
        guard $ S.member src bodyWithoutHead
        Addr dst <- dsts
        guard $ S.member dst bodyWithoutHead
        return (Addr src, [Addr dst])

allInnerLoops :: NodeMap -> LoopData -> [Loop]
allInnerLoops nodes d = execWriter (go (loopsOf d))
  where
    go = \case
        [] -> return ()
        loop:loops -> do
            tell [loop]
            go (loops ++ innerLoopsOf nodes loop)

--

computePreds :: Tag t => Problem t -> (NodeAddr -> t) -> ByTag t (NodeId -> S.Set NodeAddr)
computePreds problem nodeTag = withTags problem.sides <&> \(WithTag tag _) nodeId ->
    applyWhen
        (not (is #_Addr nodeId))
        (S.filter ((==) tag . nodeTag))
        (M.findWithDefault S.empty nodeId clobbered)
  where
    clobbered = M.fromListWith (<>) $ concat
        [ [ (cont, S.singleton nodeAddr)
          | cont <- node ^.. nodeConts
          ]
        | (nodeAddr, node) <- M.toList problem.nodes
        ]
