{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.Types.Extras.Problem
    ( ArgRenames
    , LoopDataMap
    , NodeGraph
    , ProblemAnalysis (..)
    , ProblemWithAnalysis (..)
    , analyzeProblem
    , problemArgRenames
    , augmentProblem
    , computePreds
    , createLoopDataMap
    , isReachableFrom
    , loopBodyInnerLoops
    , loopBodyOf
    , loopHeadOf
    , loopHeadsFrom
    , loopHeadsIncludingInner
    , loopHeadsOf
    , makeNodeGraph
    , nodeTagMap
    , pairingIdOfProblem
    , reachableFrom
    , varNamesOfProblem
    ) where

import BV.Core.Graph
import BV.Core.Types
import BV.Core.Types.Extras.Program
import BV.Utils

import Data.Function (applyWhen)
import Data.Functor (void)
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

computePreds :: Problem t -> NodeGraph -> ByTag t (M.Map NodeId (S.Set NodeAddr))
computePreds problem g = problem.sides <&> \side ->
    let nodes = S.fromList $ Ret : Err : side.entryPoint : reachableFrom g side.entryPoint
     in M.unionWith (<>) (M.fromSet (const S.empty) nodes) $ M.fromListWith (<>) $ concat
            [ [ (cont, S.singleton nodeAddr)
              | cont <- problem ^.. #nodes % at nodeAddr % unwrapped % nodeConts
              ]
            | Addr nodeAddr <- S.toList nodes
            ]

-- TODO more efficient but more opaque
computePreds' :: Tag t => Problem t -> (NodeAddr -> t) -> ByTag t (NodeId -> S.Set NodeAddr)
computePreds' problem nodeTag = withTags (void problem.sides) <&> \(WithTag tag ()) nodeId ->
    let f = applyWhen (not (is #_Addr nodeId)) (S.filter (\pred_ -> nodeTag pred_ == tag))
     in f $ clobbered M.! nodeId
  where
    clobbered = M.fromListWith (<>) $ concat
        [ [ (cont, S.singleton nodeAddr)
          | cont <- node ^.. nodeConts
          ]
        | (nodeAddr, node) <- M.toList problem.nodes
        ]

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
      , loopData :: LoopDataMap
      , preds :: ByTag t (NodeId -> S.Set NodeAddr)
      , vars :: S.Set Ident
      }
  deriving (Generic)

analyzeProblem :: Tag t => Problem t -> ProblemAnalysis t
analyzeProblem problem = ProblemAnalysis
    { nodeGraph
    , nodeTag
    , loopData = createLoopDataMap problem nodeGraph
    , preds = (M.!) <$> computePreds problem nodeGraph
    -- , preds = computePreds' problem nodeTag
    , vars = S.fromList $ toListOf varNamesOfProblem problem
    }
  where
    nodeGraph = makeNodeGraph problem.nodes
    nodeTag = (M.!) $ nodeTagMap problem nodeGraph

augmentProblem :: Tag t => Problem t -> ProblemWithAnalysis t
augmentProblem problem = ProblemWithAnalysis
    { problem
    , analysis = analyzeProblem problem
    }
