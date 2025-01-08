{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Check
    ( Input
    ) where

import GHC.Generics (Generic)
import Optics.Core

import BV.Inputs
import BV.ObjDump
import BV.Pairing
import BV.Problem
import BV.Program
import BV.ProofChecks
import Control.Monad.Logger (MonadLogger)

data Input
  = Input
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      }
  deriving (Eq, Generic, Ord, Show)

data IntermediateArtifact
  = IntermediateArtifactFunctions Program
  | IntermediateArtifactPairings Pairings
  | IntermediateArtifactProblems Problems
  | IntermediateArtifactProofChecks (ProofChecks String)
  deriving (Eq, Generic, Ord, Show)

class ( Monad m
      , MonadLogger m
      , MonadRegisterIntermediateArtifacts m
      , MonadSneakyIO m
      ) => MonadCheckWriteOnly m where

class ( Monad m
      , MonadCheckWriteOnly m
      , MonadSolver m
      ) => MonadCheck m where

class Monad m => MonadRegisterIntermediateArtifacts m where
    registerIntermediateArtifact :: IntermediateArtifact -> m ()

class Monad m => MonadSneakyIO m where
    liftSneakyIO :: IO () -> m ()

class Monad m => MonadSolver m where
    -- TODO refine and abstract
    doSomethingWithSolver :: IO a -> m a
