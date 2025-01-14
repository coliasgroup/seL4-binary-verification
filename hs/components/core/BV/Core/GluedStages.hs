{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.GluedStages
    ( Input (..)
    , IntermediateArtifact (..)
    , MonadPureStages
    , MonadRegisterIntermediateArtifacts (..)
    , gluedStages
    ) where

import Control.Monad.Logger (MonadLogger)
import GHC.Generics (Generic)

import BV.Core.Stages
import BV.Core.Types

data Input
  = Input
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      , problemsAndProofs :: ProblemsAndProofs
      , asmIgnore :: [Ident]
      }
  deriving (Eq, Generic, Ord, Show)

data IntermediateArtifact
  = IntermediateArtifactFunctions Program
  | IntermediateArtifactPairings Pairings
  | IntermediateArtifactProblems Problems
  | IntermediateArtifactProofChecks (ProofChecks String)
  | IntermediateArtifactSMTProofChecks (SMTProofChecks ())
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadRegisterIntermediateArtifacts m where
    registerIntermediateArtifact :: IntermediateArtifact -> m ()

class ( Monad m
      , MonadLogger m
      , MonadRegisterIntermediateArtifacts m
      ) => MonadPureStages m where

gluedStages :: MonadPureStages m => Input -> m (SMTProofChecks ())
gluedStages = undefined
  where
    x = undefined
