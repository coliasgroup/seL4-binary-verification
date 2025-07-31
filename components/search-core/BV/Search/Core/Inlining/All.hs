module BV.Search.Core.Inlining.All
    ( DiscoverAllInlineScriptsInput (..)
    , prepareAllDiscoverInlineScriptInput
    ) where

import BV.Core.Stages
import BV.Core.Types
import BV.Core.Utils.IncludeExcludeFilter

import BV.Search.Core.Inlining

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Optics

data DiscoverAllInlineScriptsInput
  = DiscoverAllInlineScriptsInput
      { programs :: ByTag' Program
      , objDumpInfo :: ObjDumpInfo
      , rodata :: ROData
      , earlyAsmFunctionFilter :: AsmFunctionFilter
      , asmFunctions :: S.Set Ident
      , cFunctionPrefix :: String
      }
  deriving (Generic)

prepareAllDiscoverInlineScriptInput
    :: DiscoverAllInlineScriptsInput
    -> M.Map PairingId' DiscoverInlineScriptInput
prepareAllDiscoverInlineScriptInput input = scripts

  where

    alterProgramByTag = byAsmRefineTag (ByAsmRefineTag
        { asm = #functions %~ M.filterWithKey (\k _v ->
            applyIncludeExcludeFilter input.earlyAsmFunctionFilter k)
        , c = pseudoCompile input.objDumpInfo
        })

    alteredPrograms = fixupProgram <$> (alterProgramByTag <*> input.programs)

    (inlineAsmPairings, alteredProgramsWithInlineAsm, _unhandledAsmFunctionNames) =
        addInlineAssemblySpecs alteredPrograms

    finalPrograms = alteredProgramsWithInlineAsm

    lookupFunction (WithTag tag funName) = (viewAtTag tag finalPrograms).functions M.! funName

    requestedPairingIds = flip S.map input.asmFunctions $ \asm ->
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
         in byAsmRefineTag (ByAsmRefineTag { asm, c })

    normalFunctionPairingIds = do
        asm <- M.keys (getAsm finalPrograms).functions
        let c = asm & #unwrap %~ (input.cFunctionPrefix ++)
        guard $ c `M.member` (getC finalPrograms).functions
        return $ byAsmRefineTag (ByAsmRefineTag { asm, c })

    matches = S.fromList $ M.keys inlineAsmPairings.unwrap ++ normalFunctionPairingIds

    script pairingId = DiscoverInlineScriptInput
        { structs = input.programs <&> (.structs)
        , rodata = input.rodata
        , functions = lookupFunction
        , matches
        , pairingId
        }

    scripts = M.fromSet script requestedPairingIds
