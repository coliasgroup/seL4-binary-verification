{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.GluedStages
    ( Input (..)
    , IntermediateArtifact (..)
    , MonadPureStages
    , MonadRegisterIntermediateArtifacts (..)
    , gluedStages
    , gluedStages_
    ) where

import Control.Monad.Logger
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics

import BV.Core.Stages
import BV.Core.Types
import Control.Exception (assert)

data Input
  = Input
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      , problemsAndProofs :: ProblemsAndProofs
      , asmFunctionFilter :: Ident -> Bool
      }
  deriving (Generic)

data IntermediateArtifact
  = IntermediateArtifactFunctions Program
  | IntermediateArtifactPairings Pairings
  | IntermediateArtifactProblems Problems
  | IntermediateArtifactProofChecks (ProofChecks String)
  | IntermediateArtifactSMTProofChecks (SMTProofChecks ())
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadRegisterIntermediateArtifacts m where
    registerIntermediateArtifact :: IntermediateArtifact -> m ()

type MonadPureStages m =
    ( Monad m
    , MonadLogger m
    , MonadRegisterIntermediateArtifacts m
    )

gluedStages :: MonadPureStages m => Input -> m (SMTProofChecks ())
gluedStages = undefined

gluedStages_ :: MonadPureStages m => Input -> m ()
gluedStages_ input = do
    -- logDebugN "foo"
    registerIntermediateArtifact $ IntermediateArtifactFunctions collectedFunctions
    return ()
  where
    altered = fixupProgram <$> PairingOf
        { asm = input.programs.asm & #functions %~ M.filterWithKey (const . input.asmFunctionFilter)
        , c = pseudoCompile input.objDumpInfo input.programs.c
        }
    (inlineAsmPairings, alteredWithInlineAsm, _unhandledAsmFuns) = addInlineAssemblySpecs altered
    collectedFunctions = Program
        { structs = M.empty
        , constGlobals = M.empty
        , functions =
            assert (M.disjoint alteredWithInlineAsm.c.functions alteredWithInlineAsm.asm.functions) $
            M.union alteredWithInlineAsm.c.functions alteredWithInlineAsm.asm.functions
        }
