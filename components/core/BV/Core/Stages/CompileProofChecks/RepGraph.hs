{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BV.Core.Stages.CompileProofChecks.RepGraph
    ( FunctionSignature (..)
    , FunctionSignatures
    , MonadRepGraph (..)
    , RepGraphContext
    , RepGraphEnv
    , RepGraphState
    , initRepGraphEnv
    , initRepGraphState
    ) where

import BV.Core.Graph
import BV.Core.Stages.CompileProofChecks.Solver
import BV.Core.Types

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

type RepGraphContext m = (MonadReader RepGraphEnv m, MonadState RepGraphState m)

class MonadSolver m => MonadRepGraph m where
    liftRepGraph :: (forall n. RepGraphContext n => n a) -> m a

data RepGraphEnv
  = RepGraphEnv
      { functionSigs :: FunctionSignatures
      , pairings :: Pairings
      , problem :: Problem
      , nodeTag :: NodeAddr -> Tag
      , loopData :: Map NodeAddr LoopData
      , nodeGraph :: NodeGraph
      }
  deriving (Generic)

data RepGraphState
  = RepGraphState
      { smtDerivedOps :: Map (Op, Integer) String
      , namesUsed :: Set Ident
      , externalNames :: Set Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

data FunctionSignature
  = FunctionSignature
      { input :: [Argument]
      , output :: [Argument]
      }
  deriving (Eq, Generic, NFData, Ord, Show)

type FunctionSignatures = WithTag Ident -> FunctionSignature

data LoopData
  = LoopHead (Set NodeAddr)
  | LoopMember NodeAddr
  deriving (Eq, Generic, Ord, Show)

initRepGraphEnv :: FunctionSignatures -> Pairings -> Problem -> RepGraphEnv
initRepGraphEnv functionSigs pairings problem =
    RepGraphEnv
        { functionSigs
        , pairings
        , problem
        -- , nodeGraph = makeNodeGraph (map (_2 %~ view #node) (M.toAscList problem.nodes))
        , nodeGraph
        , nodeTag =
            let c = S.fromList . mapMaybe (preview #_Addr) $ reachableFrom nodeGraph problem.sides.c.entryPoint
             in \addr -> if addr `S.member` c then C else Asm
        , loopData =
            let heads = loopHeads nodeGraph [problem.sides.c.entryPoint, problem.sides.asm.entryPoint]
             in M.fromList $ flip foldMap heads $ \(loopHead, scc) ->
                    [(loopHead, LoopHead scc)] <> flip mapMaybe (S.toList scc) (\member ->
                        if member == loopHead then Nothing else Just (member, LoopMember loopHead))
        }
  where
    nodeGraph = makeNodeGraph (M.toAscList problem.nodes)

loopIdR :: MonadReader RepGraphEnv m => NodeAddr -> m (Maybe NodeAddr)
loopIdR addr = do
    loopData <- gview $ #loopData % at addr
    return (loopData <&> \case
        LoopHead _ -> addr
        LoopMember addr' -> addr')

loopHeadsR :: MonadReader RepGraphEnv m => m [NodeAddr]
loopHeadsR = do
    loopData <- gview #loopData
    return (mapMaybe (\(k, v) -> case v of
        LoopHead _ -> Just k
        LoopMember _ -> Nothing) (M.toList loopData))

initRepGraphState :: RepGraphState
initRepGraphState = RepGraphState
    { smtDerivedOps = M.empty
    , namesUsed = S.empty
    , externalNames = S.empty
    }
