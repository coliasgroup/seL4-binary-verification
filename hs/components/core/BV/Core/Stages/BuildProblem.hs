{-# OPTIONS_GHC -Wno-type-defaults #-}

module BV.Core.Stages.BuildProblem
    ( buildProblem
    ) where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

import BV.Core.Types
import BV.Core.Utils
import Control.Exception (assert)

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
      { nodes :: M.Map NodeAddr NodeWithMeta
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
    modify $ #nodes % at addr ?~ NodeWithMeta node (NodeMeta bySource)

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

getFreshName :: Ident -> State NodeMapBuilder Ident
getFreshName hint = do
    zoom #vars $ do
        taken <- get
        let name = chooseFreshName taken hint
        modify $ S.insert name
        return name
