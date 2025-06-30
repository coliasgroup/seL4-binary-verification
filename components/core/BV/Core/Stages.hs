
{-# LANGUAGE DeriveAnyClass #-}

module BV.Core.Stages
    ( AsmFunctionFilter
    , module BV.Core.Stages.BuildProblem
    , module BV.Core.Stages.CompileProofChecks
    , module BV.Core.Stages.EnumerateProofChecks
    , module BV.Core.Stages.Fixup
    , module BV.Core.Stages.FormulatePairing
    , module BV.Core.Stages.InlineAssembly
    , module BV.Core.Stages.PseudoCompile
    , IntermediateStagesOutput (..)
    , ProofCheckMeta (..)
    , StagesInput (..)
    , StagesOutput (..)
    , StagesOutputChecks (..)
    , prettyProofCheckMeta
    , stages
    ) where

import BV.Core.DecorateProofScript
import BV.Core.Stages.BuildProblem
import BV.Core.Stages.CompileProofChecks
import BV.Core.Stages.EnumerateProofChecks
import BV.Core.Stages.Fixup
import BV.Core.Stages.FormulatePairing
import BV.Core.Stages.InlineAssembly
import BV.Core.Stages.PseudoCompile
import BV.Core.Types
import BV.Core.Types.Extras
import BV.Core.Utils

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Foldable (fold)
import Data.Functor (void)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)
import Optics

-- import Control.DeepSeq (liftRnf)
-- import Control.Parallel.Strategies (evalSeq, rdeepseq, rparWith, using)

data StagesInput
  = StagesInput
      { programs :: PairingOf Program
      , objDumpInfo :: ObjDumpInfo
      , rodata :: ROData
      , stackBounds :: StackBounds
      , inlineScripts :: InlineScripts
      , proofs :: Proofs ()
      , earlyAsmFunctionFilter :: IncludeExcludeFilter Ident
      }
  deriving (Eq, Generic, NFData, Ord, Show)

instance Binary StagesInput where

type AsmFunctionFilter = IncludeExcludeFilter Ident

data StagesOutput
  = StagesOutput
      { checks :: StagesOutputChecks
        -- report
      , unhandledInlineAssemblyFunctions :: [Ident]
      , unhandledInstructionFunctions :: [Ident]
        -- intermediate, for checking
      , intermediate :: IntermediateStagesOutput
      }
  deriving (Eq, Generic, NFData, Ord, Show)

newtype StagesOutputChecks
  = StagesOutputChecks { unwrap :: M.Map PairingId [SMTProofCheckGroup ProofCheckMeta] }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (NFData)

data ProofCheckMeta
  = ProofCheckMeta
      { path :: ProofScriptNodePath
      , desc :: ProofCheckDescription
      }
  deriving (Eq, Generic, NFData, Ord, Show)

prettyProofCheckMeta :: ProofCheckMeta -> String
prettyProofCheckMeta meta = prettyProofScriptNodePath meta.path ++ " >>> " ++ meta.desc

data IntermediateStagesOutput
  = IntermediateStagesOutput
      { functions :: Program
      , pairings :: Pairings
      , problems :: Problems
      , proofChecks :: ProofChecks ProofCheckDescription
      , compatProofChecks :: CompatProofChecks
      , compatSMTProofChecks :: CompatSMTProofChecks
      }
  deriving (Eq, Generic, NFData, Ord, Show)

stages :: StagesInput -> StagesOutput
stages input = StagesOutput
    { checks = finalChecks
    , unhandledInlineAssemblyFunctions = unhandledAsmFunctionNames.c
    , unhandledInstructionFunctions = unhandledAsmFunctionNames.asm
    , intermediate = IntermediateStagesOutput
        { functions = collectedFunctions
        , pairings
        , problems
        , proofChecks
        , compatProofChecks
        , compatSMTProofChecks
        }
    }

  where

    alteredPrograms = fixupProgram <$> PairingOf
        { asm = input.programs.asm & #functions %~ M.filterWithKey (\k _v ->
            applyIncludeExcludeFilter input.earlyAsmFunctionFilter k)
        , c = pseudoCompile input.objDumpInfo input.programs.c
        }

    (inlineAsmPairings, alteredProgramsWithInlineAsm, unhandledAsmFunctionNames) =
        addInlineAssemblySpecs alteredPrograms

    finalPrograms = alteredProgramsWithInlineAsm

    collectedFunctions = programFromFunctions $
        M.unionWith (error "not disjoint") finalPrograms.c.functions finalPrograms.asm.functions

    normalFunctionPairingIds = do
        asm <- M.keys finalPrograms.asm.functions
        let c = asmFunNameToCFunName asm
        guard $ c `M.member` finalPrograms.c.functions
        return $ PairingOf { c = asmFunNameToCFunName asm, asm }

    normalPairings = M.fromList
        [ let stackBound = input.stackBounds.unwrap ! pairingId.asm
              cFun = finalPrograms.c.functions ! pairingId.c
              pairing = formulatePairing stackBound cFun.input cFun.output
           in (pairingId, pairing)
        | pairingId <- normalFunctionPairingIds
        ]

    pairings = Pairings $ normalPairings `M.union` inlineAsmPairings.unwrap

    lookupFunction (WithTag tag funName) = (pairingSide tag finalPrograms).functions ! funName

    functionSigs = sig . lookupFunction
      where
        sig fun = FunctionSignature
            { input = fun.input
            , output = fun.output
            }

    -- TODO
    -- By doing this we lose laziness, and it's probably overkill anyways (reduces eval from ~8s -> ~4s)
    -- Also, since implementing compileProofChecks, takes up too much memory
    problems = problems'
    -- problems = using problems' $ traverseOf (#unwrap % traversed) (rparWith rdeepseq)

    problems' = Problems . M.fromList $ do
        pairingId <- normalFunctionPairingIds
        let namedFuns = (\funName prog -> Named funName (prog.functions ! funName)) <$> pairingId <*> finalPrograms
        guard $ isJust namedFuns.c.value.body
        guard $ isJust namedFuns.asm.value.body
        let inlineScript = M.findWithDefault [] pairingId input.inlineScripts.unwrap -- TODO
        let problem = buildProblem lookupFunction inlineScript namedFuns
        return (pairingId, problem)

    provenProblems = problems & #unwrap %~ \m -> M.restrictKeys m (M.keysSet input.proofs.unwrap)

    -- TODO (see above)
    proofChecks = proofChecks'
    -- proofChecks = using proofChecks' $ traverseOf (#unwrap % traversed) (rparWith (evalSeq (liftRnf (const ()))))
    -- proofChecks = using proofChecks' $ traverseOf (#unwrap % traversed) (rparWith rdeepseq)

    lookupOrigVarNameFor pairingId problem quadrant mangledName =
        fromJust $ lookup mangledName (zip (map (.name) mangledArgs) (map (.name) origArgs))
      where
        fun = lookupFunction (pairingSideWithTag quadrant.tag pairingId)
        origArgs = case quadrant.direction of
            PairingEqDirectionIn -> fun.input
            PairingEqDirectionOut -> fun.output
        probSide = pairingSide quadrant.tag problem.sides
        mangledArgs = case quadrant.direction of
            PairingEqDirectionIn -> probSide.input
            PairingEqDirectionOut -> probSide.output

    proofChecks' = ProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        let pairing = pairings `atPairingId` pairingId
            proofScript = input.proofs `atPairingId` pairingId
         in enumerateProofChecks (lookupOrigVarNameFor pairingId problem) pairing problem proofScript

    compatProofChecks = toCompatProofChecks proofChecks

    smtProofChecks = SMTProofChecks . flip M.mapWithKey provenProblems.unwrap $ \pairingId problem ->
        compileProofChecks input.programs.c.structs functionSigs pairings input.rodata (lookupOrigVarNameFor pairingId problem) problem
            <$> (proofChecks `atPairingId` pairingId)

    compatSMTProofChecks = toCompatSMTProofChecks (void smtProofChecks)

    finalChecks =
        let f = decorateProofScriptWithProofScriptNodePathsWith $ \path -> map (fmap (ProofCheckMeta path))
         in StagesOutputChecks $
                M.map (fold . f) smtProofChecks.unwrap


asmFunNameToCFunName :: Ident -> Ident
asmFunNameToCFunName = #unwrap %~ ("Kernel_C." ++)
