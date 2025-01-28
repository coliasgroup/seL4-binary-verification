{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module BV.Core.GluedStages
    ( Input (..)
    , IntermediateArtifact (..)
    , MonadPureStages
    , MonadRegisterIntermediateArtifacts (..)
    , gluedStages
    ) where

import Control.Exception (assert)
import Control.Monad.Logger
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

import BV.Core.Stages
import BV.Core.Types

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
  | IntermediateArtifactProofChecks (FlattenedProofChecks String)
  | IntermediateArtifactSMTProofChecks (FlattenedSMTProofChecks ())
  deriving (Eq, Generic, Ord, Show)

class Monad m => MonadRegisterIntermediateArtifacts m where
    registerIntermediateArtifact :: IntermediateArtifact -> m ()

type MonadPureStages m =
    ( Monad m
    , MonadLogger m
    , MonadRegisterIntermediateArtifacts m
    )

gluedStages :: MonadPureStages m => Input -> m (SMTProofChecks ())
gluedStages input = do
    logInfoN "x1"
    registerIntermediateArtifact $ IntermediateArtifactFunctions collectedFunctions
    logInfoN "x2"
    registerIntermediateArtifact $ IntermediateArtifactPairings pairings
    logInfoN "x3"
    registerIntermediateArtifact $ IntermediateArtifactProblems problems
    logInfoN "x4"
    registerIntermediateArtifact $ IntermediateArtifactProofChecks flattenedProofChecks
    logInfoN "x5"
    registerIntermediateArtifact $ IntermediateArtifactSMTProofChecks flattenedSMTProofChecks
    logInfoN "x6"
    return smtProofChecks

  where

    altered = fixupProgram <$> PairingOf
        { asm = input.programs.asm & #functions %~ M.filterWithKey (const . input.asmFunctionFilter)
        , c = pseudoCompile input.objDumpInfo input.programs.c
        }

    (inlineAsmPairings, alteredWithInlineAsm, _unhandledAsmFuns) = addInlineAssemblySpecs altered

    finalPrograms = alteredWithInlineAsm

    collectedFunctions = Program
        { structs = M.empty
        , constGlobals = M.empty
        , functions =
            assert (M.disjoint finalPrograms.c.functions finalPrograms.asm.functions) $
            M.union finalPrograms.c.functions finalPrograms.asm.functions
        }

    pairingIds = catMaybes . flip map (M.keys finalPrograms.asm.functions) $ \asmFunName ->
        let cFunName = asmFunNameToCFunName asmFunName
         in if M.member cFunName finalPrograms.c.functions
            then Just (PairingOf { c = cFunName, asm = asmFunName })
            else Nothing

    pairingsLessInlineAsm = flip M.fromSet (S.fromList pairingIds) $ \pairingId ->
        let stackBound = input.stackBounds.unwrap ! pairingId.asm
            cFun = finalPrograms.c.functions ! pairingId.c
         in formulatePairing stackBound cFun.input cFun.output

    pairings = Pairings $ pairingsLessInlineAsm `M.union` inlineAsmPairings.unwrap

    lookupFunctionForProblem tag funName = (pairingSide tag finalPrograms).functions ! funName

    problems = Problems $ flip M.mapMaybeWithKey pairings.unwrap $ \pairingId _pairing -> do
        let namedFuns = (\funName prog -> Named funName (prog.functions ! funName)) <$> pairingId <*> finalPrograms
        _ <- namedFuns.c.value.body
        _ <- namedFuns.asm.value.body
        -- guard $ namedFuns.asm.name == "updateCapData"
        -- guard $ namedFuns.asm.name == "Arch_configureIdleThread"
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        return $ buildProblem lookupFunctionForProblem inlineScript namedFuns

    problemsThatHaveProofs = Problems $ M.restrictKeys problems.unwrap (M.keysSet input.problemsAndProofs.unwrap)

    proofChecks = ProofChecks $ flip M.mapWithKey problemsThatHaveProofs.unwrap $ \pairingId problem ->
        let pairing = pairings `atPairingId` pairingId
            proofScript = (input.problemsAndProofs `atPairingId` pairingId).proof
            lookupOrigVarName quadrant mangledName =
                fromJust $ lookup mangledName (zip (map (.name) mangledArgs) (map (.name) origArgs))
              where
                fun = (pairingSide quadrant.tag finalPrograms).functions ! pairingSide quadrant.tag pairingId
                origArgs = case quadrant.direction of
                    PairingEqDirectionIn -> fun.input
                    PairingEqDirectionOut -> fun.output
                probSide = pairingSide quadrant.tag problem.sides
                mangledArgs = case quadrant.direction of
                    PairingEqDirectionIn -> probSide.input
                    PairingEqDirectionOut -> probSide.output
         in enumerateProofChecks lookupOrigVarName pairing problem proofScript

    flattenedProofChecks = flattenProofChecks proofChecks

    smtProofChecks = void . SMTProofChecks $ flip M.mapWithKey problemsThatHaveProofs.unwrap $ \pairingId problem ->
        let theseProofChecks = proofChecks `atPairingId` pairingId
         in compileProofChecks problem <$> theseProofChecks

    flattenedSMTProofChecks = flattenSMTProofChecks smtProofChecks


asmFunNameToCFunName :: Ident -> Ident
asmFunNameToCFunName = #unwrap %~ ("Kernel_C." ++)
