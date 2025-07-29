module BV.Core.Search.Inlining.All
    ( DiscoverAllInlineScriptInput (..)
    , discoverAllInlineScripts
    ) where

import BV.Core.ModelConfig (ModelConfig)
import BV.Core.Search.Inlining
import BV.Core.Stages
import BV.Core.Types
import BV.Core.Utils
import qualified BV.SMTLIB2.Monad as S

import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data DiscoverAllInlineScriptInput
  = DiscoverAllInlineScriptInput
      { programs :: ByTag' Program
      , objDumpInfo :: ObjDumpInfo
      , rodata :: ROData
      , earlyAsmFunctionFilter :: IncludeExcludeFilter Ident
      , asmFunctions :: S.Set Ident
      }
  deriving (Generic)

discoverAllInlineScripts
    :: (Monad m, S.MonadSolver n)
    => ((ModelConfig -> n a) -> m a)
    -> DiscoverAllInlineScriptInput
    -> m InlineScripts'
discoverAllInlineScripts run input = scripts

  where

    alteredPrograms = fixupProgram <$> byAsmRefineTag (ByAsmRefineTag
        { asm = getAsm input.programs & #functions %~ M.filterWithKey (\k _v ->
            applyIncludeExcludeFilter input.earlyAsmFunctionFilter k)
        , c = pseudoCompile input.objDumpInfo (getC input.programs)
        })

    (_inlineAsmPairings, alteredProgramsWithInlineAsm, _unhandledAsmFunctionNames) =
        addInlineAssemblySpecs alteredPrograms

    finalPrograms = alteredProgramsWithInlineAsm

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions M.! funName

    pairingIds = flip S.map input.asmFunctions $ \asm ->
        byAsmRefineTag (ByAsmRefineTag { asm, c = asmFunNameToCFunName asm})

    script pairingId = discoverInlineScript run $ DiscoverInlineScriptInput
        { structs = input.programs <&> (.structs)
        , rodata = input.rodata
        , functions = lookupFunction
        , pairingId
        }

    scripts = InlineScripts <$> sequenceA (M.fromSet script pairingIds)


asmFunNameToCFunName :: Ident -> Ident
asmFunNameToCFunName = #unwrap %~ ("Kernel_C." ++)
