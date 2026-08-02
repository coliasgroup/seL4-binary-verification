{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BV.Core.Types.Extras.Problem
    ( ArgRenames
    , Loop (..)
    , LoopData (..)
    , NodeGraph
    , ProblemAnalysis (..)
    , ProblemSideAnalysis (..)
    , ProblemSideWithAnalysis (..)
    , ProblemWithAnalysis (..)
    , allLoopsOf
    , augmentProblem
    , inlineScriptsEquivalent
    , innermostLoopContaining
    , isNonTriviallyReachableFrom
    , isReachableFrom
    , isSyntacticConstant
    , loopIsComplex
    , loopIsSimple
    , makeNodeGraph
    , outermostLoopContaining
    , pairingIdOfProblem
    , problemArgRenames
    , problemSideWithAnalysis
    , problemSidesWithAnalysis
    , reachableFrom
    , varNamesOfProblem
    ) where

import BV.Core.Types
import BV.Core.Types.Extras.Expr (varFromNameTyE)
import BV.Core.Types.Extras.Program
import BV.Utils

import Control.Monad (unless, when)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State (evalStateT, get, modify)
import Data.Either (isRight)
import Data.Foldable (for_, toList)
import Data.Function (applyWhen, on)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (elemIndex, find, genericIndex, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

-- TODO don't abuse PairingEqSideQuadrant
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
        `adjoin` (#sides % traversed % #nodes % traversed % varNamesOf)

--

data ProblemSideAnalysis
  = ProblemSideAnalysis
      { nodeGraph :: NodeGraph
      , loopData :: LoopData
      , preds :: NodeId -> S.Set NodeAddr
      , isNonTriviallyReachableFrom :: NodeAddr -> NodeId -> Bool
      }
  deriving (Generic)

data ProblemAnalysis t
  = ProblemAnalysis
      { sides :: ByTag t ProblemSideAnalysis
      }
  deriving (Generic)

-- TODO rename to AnalyzedProblem
data ProblemWithAnalysis t
  = ProblemWithAnalysis
      { problem :: Problem t
      , analysis :: ProblemAnalysis t
      }
  deriving (Generic)

-- TODO rename to AnalyzedProblemSide
data ProblemSideWithAnalysis
  = ProblemSideWithAnalysis
      { problem :: ProblemSide
      , analysis :: ProblemSideAnalysis
      }
  deriving (Generic)

analyzeProblemSide :: ProblemSide -> ProblemSideAnalysis
analyzeProblemSide side = ProblemSideAnalysis
    { nodeGraph
    , loopData
    , preds = computePreds side
    , isNonTriviallyReachableFrom = makeIsNonTriviallyReachableFrom side nodeGraph loopData
    }
  where
    nodeGraph = makeNodeGraph side.nodes
    loopData = makeLoopData side nodeGraph

augmentProblem :: Tag t => Problem t -> ProblemWithAnalysis t
augmentProblem problem = ProblemWithAnalysis
    { problem
    , analysis = ProblemAnalysis $ analyzeProblemSide <$> problem.sides
    }

problemSideWithAnalysis :: Tag t => t -> ProblemWithAnalysis t -> ProblemSideWithAnalysis
problemSideWithAnalysis tag pwa = ProblemSideWithAnalysis
    { problem = viewAtTag tag pwa.problem.sides
    , analysis = viewAtTag tag pwa.analysis.sides
    }

problemSidesWithAnalysis :: Tag t => ProblemWithAnalysis t -> ByTag t ProblemSideWithAnalysis
problemSidesWithAnalysis pwa = withTags pwa.problem.sides <&> \(WithTag tag side) ->
    ProblemSideWithAnalysis
        { problem = side
        , analysis = viewAtTag tag pwa.analysis.sides
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

isNonTriviallyReachableFrom :: ProblemSideWithAnalysis -> NodeAddr -> NodeId -> Bool
isNonTriviallyReachableFrom swa from to_ =
    if Addr from /= to_
    then isReachableFrom swa.analysis.nodeGraph (Addr from) to_
    else from `M.member` swa.analysis.loopData.byMember

makeIsNonTriviallyReachableFrom :: ProblemSide -> NodeGraph -> LoopData -> NodeAddr -> NodeId -> Bool
makeIsNonTriviallyReachableFrom side g loopData = \from to_ -> to_ `S.member` (m M.! from)
  where
    m = M.mapWithKey f side.nodes
    f from _ = applyWhen (not keepSelf) (S.delete n) s
      where
        n = Addr from
        s = S.fromList $ reachableFrom g n
        keepSelf = from `M.member` loopData.byMember

--

loopsFromGeneric :: G.Graph -> Vertex -> [(Vertex, S.Set Vertex)]
loopsFromGeneric g entryPoint =
    [ (h, body)
    | scc <- G.scc g
    , let body = S.fromList (toList scc)
    , S.size body > 1
    , let Just h = find (`S.member` body) inOrder
    ]
  where
    inOrder = foldMap toList $ G.dfs g [entryPoint]

data LoopData
  = LoopData
      { outermostLoops :: [Loop]
      , byHead :: M.Map NodeAddr Loop
      , byMember :: M.Map NodeAddr Loop
      }
  deriving (Generic)

data Loop
  = Loop
      { head :: NodeAddr
      , members :: S.Set NodeAddr
      , parent :: Maybe Loop
      , children :: [Loop]
      }
  deriving (Generic)

makeLoopData :: ProblemSide -> NodeGraph -> LoopData
makeLoopData side nodeGraph = LoopData
    { outermostLoops
    , byHead = M.fromList $ allLoops <&> \loop -> (loop.head, loop)
    , byMember = M.fromList $ concat $ allLoops <&> \loop ->
        [ (n, loop)
        | n <- S.toList loop.members
        ]
    }
  where
    outermostLoops = go nodeGraph Nothing side.entryPoint
    allLoops = flattenLoops outermostLoops
    go g parent entryPoint = sortOn (.head)
        [ let toNodeAddr = nodeAddrOf . g.vertexToNodeId
              loop = Loop
                { head = toNodeAddr h
                , members = S.map toNodeAddr body
                , parent
                , children =
                    let g' = makeNodeGraphFromEdges
                            [ ((), Addr src, dsts)
                            | src <- S.toList loop.members
                            , let dsts =
                                    [ Addr dst
                                    | Addr dst <- side.nodes ^.. at src % unwrapped % nodeConts
                                    , dst `S.member` loop.members
                                    , dst /= loop.head
                                    ]
                            ]
                     in go g' (Just loop) (Addr loop.head)
                }
          in loop
        | (h, body) <- loopsFromGeneric g.graph (g.nodeIdToVertex entryPoint)
        ]

flattenLoops :: [Loop] -> [Loop]
flattenLoops loops =
    loops ++
        concat
            [ flattenLoops loop.children
            | loop <- loops
            ]

allLoopsOf :: LoopData -> [Loop]
allLoopsOf d = flattenLoops d.outermostLoops

innermostLoopContaining :: LoopData -> NodeAddr -> Maybe Loop
innermostLoopContaining d n = M.lookup n d.byMember

outermostLoopContaining :: LoopData -> NodeAddr -> Maybe Loop
outermostLoopContaining d n = go <$> innermostLoopContaining d n
  where
    go loop = maybe loop go loop.parent

loopIsSimple :: Loop -> Bool
loopIsSimple loop = null loop.children

loopIsComplex :: Loop -> Bool
loopIsComplex = not . loopIsSimple

--

isSyntacticConstant
    :: ProblemSideWithAnalysis
    -> (Ident -> Integer -> Maybe Integer)
    -> NameTy
    -> Loop
    -> NodeAddr
    -> Bool
isSyntacticConstant swa constRetAssumptions var loop split =
    ensure (not (loopIsComplex loop)) $
    isRight $
        runExcept
            (evalStateT
                (go (var.name, split))
                (S.singleton (var.name, split)))
  where
    go (name, addr) = do
        let localVar = NameTy name var.ty
        let node = swa.problem.nodes M.! addr
        predName <- fromMaybe name <$> case node of
            NodeCall callNode -> do
                let isConst = case elemIndex localVar callNode.output of
                        Nothing -> True
                        Just outputIx -> case constRetAssumptions callNode.functionName (toInteger outputIx) of
                            Nothing -> False
                            Just intputIx -> callNode.input `genericIndex` intputIx == varFromNameTyE localVar
                if isConst
                    then return Nothing
                    else throwNotConst
            NodeBasic basicNode -> do
                let updateExprs =
                        [ u.val
                        | u <- basicNode.varUpdates
                        , u.var == localVar
                        ]
                case updateExprs of
                    [] -> return Nothing
                    [Expr _ (ExprValueVar ident)] -> return $ Just ident
                    [_] -> throwNotConst
                    _ -> error "unexpected"
            _ -> return Nothing
        let preds = S.intersection loop.members $ swa.analysis.preds (Addr addr)
        for_ preds $ \predAddr -> do
            let predVar = (predName, predAddr)
            safe <- get
            unless (predVar `S.member` safe) $ do
                when (predAddr == split) throwNotConst
                go predVar
                modify $ S.insert predVar
    throwNotConst = throwError ()

--

computePreds :: ProblemSide -> NodeId -> S.Set NodeAddr
computePreds side = \nodeId -> M.findWithDefault S.empty nodeId m
  where
    m = M.fromListWith (<>) $ concat
        [ [ (cont, S.singleton nodeAddr)
          | cont <- node ^.. nodeConts
          ]
        | (nodeAddr, node) <- M.toList side.nodes
        ]

--

inlineScriptsEquivalent :: Tag t => InlineScript t -> InlineScript t -> Bool
inlineScriptsEquivalent = (==) `on` f
  where
    f script =
        let s = S.fromList script
         in ensure (S.size s == length script) s
